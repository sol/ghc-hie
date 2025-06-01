module GHC.Iface.Ext.Binary.GHC912 (readHieFileContents) where

import Prelude hiding (span, mod)

import GHC.Builtin.Utils
import GHC.Iface.Ext.Binary.Utils
import GHC.Iface.Ext.Types
import GHC.Types.Name
import GHC.Types.Name.Cache
import GHC.Types.Unique
import qualified GHC.Iface.Ext.Binary.Utils as Binary
import GHC.Utils.Outputable hiding (char)
import GHC.Utils.Panic

import qualified Data.Array        as A
import qualified Data.Array.IO     as A
import qualified Data.Array.Unsafe as A
import Data.Word                  ( Word32 )
import Control.Monad              ( forM_, foldM )

initReadNameTable :: NameCache -> IO (ReaderTable Name)
initReadNameTable cache = do
  return $
    ReaderTable
      { getTable = \bh -> getSymbolTable bh cache
      , mkReaderFromTable = \tbl -> mkReader (getSymTabName tbl)
      }

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
