{-
Binary serialization for .hie files.
-}

module GHC912.Iface.Ext.Binary
   (
     HieHeader
   , readHieFileHeader
   , readHieFileContents
   )
where

import Prelude hiding (span, mod)

import GHC.Builtin.Utils
import GHC.Settings.Utils         ( maybeRead )
import GHC912.Utils.Binary
import GHC.Iface.Ext.Types
import GHC.Types.Name
import GHC.Types.Name.Cache
import GHC.Types.Unique
import qualified GHC912.Utils.Binary as Binary
import GHC.Utils.Outputable hiding (char)
import GHC.Utils.Panic

import qualified Data.Array        as A
import qualified Data.Array.IO     as A
import qualified Data.Array.Unsafe as A
import Data.ByteString            ( ByteString )
import qualified Data.ByteString  as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Word                  ( Word8, Word32 )
import Control.Monad              ( replicateM, when, forM_, foldM )

-- | The header for HIE files - Capital ASCII letters \"HIE\".
hieMagic :: [Word8]
hieMagic = [72,73,69]

hieMagicLen :: Int
hieMagicLen = length hieMagic

initReadNameTable :: NameCache -> IO (ReaderTable Name)
initReadNameTable cache = do
  return $
    ReaderTable
      { getTable = \bh -> getSymbolTable bh cache
      , mkReaderFromTable = \tbl -> mkReader (getSymTabName tbl)
      }

type HieHeader = (Integer, ByteString)

readBinLine :: ReadBinHandle -> IO ByteString
readBinLine bh = BS.pack . reverse <$> loop []
  where
    loop acc = do
      char <- get bh :: IO Word8
      if char == 10 -- ASCII newline '\n'
      then return acc
      else loop (char : acc)

readHieFileHeader :: FilePath -> ReadBinHandle -> IO HieHeader
readHieFileHeader file bh0 = do
  -- Read the header
  magic <- replicateM hieMagicLen (get bh0)
  version <- BSC.unpack <$> readBinLine bh0
  case maybeRead version of
    Nothing ->
      panic $ unwords ["readHieFileHeader: hieVersion isn't an Integer:"
                      , show version
                      ]
    Just readHieVersion -> do
      ghcVersion <- readBinLine bh0

      -- Check if the header is valid
      when (magic /= hieMagic) $
        panic $ unwords ["readHieFileHeader: headers don't match for file:"
                        , file
                        , "Expected"
                        , show hieMagic
                        , "but got", show magic
                        ]
      return (readHieVersion, ghcVersion)

readHieFileContents :: ReadBinHandle -> NameCache -> IO HieFile
readHieFileContents bh0 name_cache = do
  fsReaderTable <- initFastStringReaderTable
  nameReaderTable <- initReadNameTable name_cache

  -- read the symbol table so we are capable of reading the actual data
  bh1 <-
    foldM (\bh tblReader -> tblReader bh) bh0
      -- The order of these deserialisation matters!
      --
      -- See Note [Order of deduplication tables during iface binary serialisation] for details.
      [ get_dictionary fsReaderTable
      , get_dictionary nameReaderTable
      ]

  -- load the actual data
  get bh1
  where
    get_dictionary tbl bin_handle = do
      fsTable <- Binary.forwardGetRel bin_handle (getTable tbl bin_handle)
      let
        fsReader = mkReaderFromTable tbl fsTable
        bhFs = addReaderToUserData fsReader bin_handle
      pure bhFs


getSymbolTable :: ReadBinHandle -> NameCache -> IO (SymbolTable Name)
getSymbolTable bh name_cache = do
  sz <- get bh
  mut_arr <- A.newArray_ (0, sz-1) :: IO (A.IOArray Int Name)
  forM_ [0..(sz-1)] $ \i -> do
    od_name <- getHieName bh
    name <- fromHieName name_cache od_name
    A.writeArray mut_arr i name
  A.unsafeFreeze mut_arr

getSymTabName :: SymbolTable Name -> ReadBinHandle -> IO Name
getSymTabName st bh = do
  i :: Word32 <- get bh
  return $ st A.! (fromIntegral i)

-- ** Converting to and from `HieName`'s

fromHieName :: NameCache -> HieName -> IO Name
fromHieName nc hie_name = do

  case hie_name of
    ExternalName mod occ span -> updateNameCache nc mod occ $ \cache -> do
      case lookupOrigNameCache cache mod occ of
        Just name -> pure (cache, name)
        Nothing   -> do
          uniq <- takeUniqFromNameCache nc
          let name       = mkExternalName uniq mod occ span
              new_cache  = extendOrigNameCache cache mod occ name
          pure (new_cache, name)

    LocalName occ span -> do
      uniq <- takeUniqFromNameCache nc
      -- don't update the NameCache for local names
      pure $ mkInternalName uniq occ span

    KnownKeyName u -> case lookupKnownKeyName u of
      Nothing -> pprPanic "fromHieName:unknown known-key unique"
                          (ppr u)
      Just n -> pure n

-- ** Reading and writing `HieName`'s

getHieName :: ReadBinHandle -> IO HieName
getHieName bh = do
  t <- getByte bh
  case t of
    0 -> do
      (modu, occ, span) <- get bh
      return $ ExternalName modu occ $ unBinSrcSpan span
    1 -> do
      (occ, span) <- get bh
      return $ LocalName occ $ unBinSrcSpan span
    2 -> do
      (c,i) <- get bh
      return $ KnownKeyName $ mkUnique c i
    _ -> panic "GHC.Iface.Ext.Binary.getHieName: invalid tag"
