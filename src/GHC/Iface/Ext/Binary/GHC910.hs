module GHC.Iface.Ext.Binary.GHC910 (readHieFileContents) where

import Prelude hiding (mod, span)
import GHC.Iface.Ext.Binary.Utils hiding (SymbolTable)
import qualified GHC.Iface.Ext.Binary.Utils as Binary
import GHC.Types.Name
import GHC.Types.Name.Cache
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Builtin.Utils
import GHC.Types.Unique

import qualified Data.Array        as A
import qualified Data.Array.IO     as A
import qualified Data.Array.Unsafe as A
import Data.Word
import Data.Functor
import Control.Monad

import GHC.Iface.Ext.Types

type BinHandle = ReadBinHandle
type SymbolTable = Binary.SymbolTable Name
setUserData :: ReadBinHandle -> ReaderUserData -> ReadBinHandle
setUserData = setReaderUserData
tellBin :: ReadBinHandle -> IO (Bin a)
tellBin = tellBinReader
seekBin :: ReadBinHandle -> Bin a -> IO ()
seekBin = seekBinReader

readHieFileContents :: BinHandle -> NameCache -> IO HieFile
readHieFileContents bh0 name_cache = do
  dict <- get_dictionary bh0
  -- read the symbol table so we are capable of reading the actual data
  bh1 <- do
      let bh1 = setUserData bh0 $ newReadState (error "getSymtabName")
                                               (getDictFastString dict)
      symtab <- get_symbol_table bh1
      let bh1' = setUserData bh1
               $ newReadState (getSymTabName symtab)
                              (getDictFastString dict)
      return bh1'

  -- load the actual data
  get bh1 <&> hieFileGHC910
  where
    get_dictionary bin_handle = do
      dict_p <- get bin_handle
      data_p <- tellBin bin_handle
      seekBin bin_handle dict_p
      dict <- getDictionary bin_handle
      seekBin bin_handle data_p
      return dict

    get_symbol_table bh1 = do
      symtab_p <- get bh1
      data_p'  <- tellBin bh1
      seekBin bh1 symtab_p
      symtab <- getSymbolTable bh1 name_cache
      seekBin bh1 data_p'
      return symtab

getSymbolTable :: BinHandle -> NameCache -> IO SymbolTable
getSymbolTable bh name_cache = do
  sz <- get bh
  mut_arr <- A.newArray_ (0, sz-1) :: IO (A.IOArray Int Name)
  forM_ [0..(sz-1)] $ \i -> do
    od_name <- getHieName bh
    name <- fromHieName name_cache od_name
    A.writeArray mut_arr i name
  A.unsafeFreeze mut_arr

getSymTabName :: SymbolTable -> BinHandle -> IO Name
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

getHieName :: BinHandle -> IO HieName
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

newtype HieFileGHC910 = HieFileGHC910 { hieFileGHC910 :: HieFile }

instance Binary HieFileGHC910 where
  put_ bh (HieFileGHC910 hf) = do
    put_ bh $ hie_hs_file hf
    put_ bh $ hie_module hf
    put_ bh $ hie_types hf
    put_ bh $ hie_asts hf
    put_ bh $ hie_exports hf
    put_ bh $ hie_hs_src hf

  get bh = HieFile
    <$> get bh
    <*> get bh
    <*> get bh
    <*> get bh
    <*> get bh
    <*> get bh
    <*> pure mempty
    <&> HieFileGHC910
