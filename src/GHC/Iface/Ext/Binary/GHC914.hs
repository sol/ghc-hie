{-# LANGUAGE BlockArguments #-}
module GHC.Iface.Ext.Binary.GHC914 (
  readHieFile908
, readHieFile910
, readHieFile912
, readHieFile914
) where

import           Data.Typeable
import Prelude hiding (span, mod)

import GHC.Builtin.Utils
import GHC.Iface.Ext.Binary.Utils
import GHC.Iface.Ext.Types
import GHC.Types.Name
import GHC.Types.Name.Cache
import GHC.Types.Unique
import GHC.Utils.Outputable hiding (char)
import GHC.Utils.Panic
import GHC.Types.Avail (AvailInfo)
import GHC.Unit.Module (Module)
import GHC.Types.SrcLoc

import Data.Array (Array)
import qualified Data.Array        as A
import qualified Data.Array.IO     as A
import qualified Data.Array.Unsafe as A
import Data.Word                  ( Word32 )
import Data.ByteString (ByteString)
import Control.Monad

readHieFile908 :: ReadBinHandle -> NameCache -> IO HieFile
readHieFile908 = readHieFile908_910 GHC908

readHieFile910 :: ReadBinHandle -> NameCache -> IO HieFile
readHieFile910 = readHieFile908_910 GHC910

readHieFile908_910 :: GHC -> ReadBinHandle -> NameCache -> IO HieFile
readHieFile908_910 ghc bh0 name_cache = do
  dict_p <- get bh0
  symtab_p <- get bh0
  readHieFile ghc dict_p symtab_p (const mempty) bh0 name_cache

readHieFile912 :: ReadBinHandle -> NameCache -> IO HieFile
readHieFile912 = readHieFile912_914 GHC912

readHieFile914 :: ReadBinHandle -> NameCache -> IO HieFile
readHieFile914 = readHieFile912_914 GHC914

readHieFile912_914 :: GHC -> ReadBinHandle -> NameCache -> IO HieFile
readHieFile912_914 ghc bh0 name_cache = do
  dict_p <- makeAbsoluteBin <$> getRelBin bh0
  symtab_p <- makeAbsoluteBin <$> getRelBin bh0
  readHieFile ghc dict_p symtab_p get bh0 name_cache

initReadNameTable :: GHC -> Module -> NameCache -> IO (ReaderTable Name)
initReadNameTable ghc currentModule cache = do
  return $
    ReaderTable
      { getTable = \bh -> getSymbolTable ghc currentModule bh cache
      , mkReaderFromTable = \tbl -> mkReader (getSymTabName tbl)
      }

readHieFile :: GHC -> Bin () -> Bin () -> (ReadBinHandle -> IO NameEntityInfo) -> ReadBinHandle -> NameCache -> IO HieFile
readHieFile ghc dict_p symtab_p getNameEntityInfo bh0 name_cache = do

  fsReaderTable <- initFastStringReaderTable
  bh_dict <- get_dictionary dict_p fsReaderTable bh0

  file <- get @FilePath bh_dict
  currentModule <- get @Module bh_dict

  nameReaderTable <- initReadNameTable ghc currentModule name_cache
  bh_symtab <- get_dictionary symtab_p nameReaderTable bh_dict

  -- load the actual data
  HieFile file currentModule
    <$> get @(Array TypeIndex HieTypeFlat) bh_symtab
    <*> get @(HieASTs TypeIndex) bh_symtab
    <*> get @([AvailInfo]) bh_symtab
    <*> get @ByteString bh_symtab
    <*> getNameEntityInfo bh_symtab
  where
    get_dictionary :: forall a. Typeable a => Bin () -> ReaderTable a -> ReadBinHandle -> IO ReadBinHandle
    get_dictionary p tbl bin_handle = withRestore do
      seekBinReader bin_handle p
      fsTable :: SymbolTable a <- getTable tbl bin_handle
      let
        fsReader :: BinaryReader a
        fsReader = mkReaderFromTable tbl fsTable

        bhFs :: ReadBinHandle
        bhFs = addReaderToUserData fsReader bin_handle
      pure bhFs

    withRestore :: IO a -> IO a
    withRestore action = do
      backup <- tellBinReader bh0
      action <* seekBinReader bh0 backup

getSymbolTable :: GHC -> Module -> ReadBinHandle -> NameCache -> IO (SymbolTable Name)
getSymbolTable ghc currentModule bh name_cache = do
  sz <- get bh
  mut_arr <- A.newArray_ (0, sz-1) :: IO (A.IOArray Int Name)
  forM_ [0..(sz-1)] $ \i -> do
    od_name <- getHieName bh
    name <- fromHieName ghc currentModule name_cache od_name
    A.writeArray mut_arr i name
  A.unsafeFreeze mut_arr

getSymTabName :: SymbolTable Name -> ReadBinHandle -> IO Name
getSymTabName st bh = do
  i :: Word32 <- get bh
  return $ st A.! (fromIntegral i)

-- ** Converting to and from `HieName`'s

fromHieName :: GHC -> Module -> NameCache -> HieName -> IO Name
fromHieName ghc currentModule nc hie_name = do

  case hie_name of
    ExternalName mod occ span -> updateNameCache nc mod occ $ \cache -> do
      case lookupOrigNameCache cache mod occ of
        Just old_name -> case nameSrcSpan old_name of
          UnhelpfulSpan {} -> update
          RealSrcSpan {}
            | mod == currentModule -> update
            | otherwise -> keep
          where
            new_name = mkExternalName uniq mod occ span
            new_cache = extendOrigNameCache cache mod occ new_name
            uniq = nameUnique old_name

            update = pure (new_cache, new_name)
            keep = pure (cache, old_name)

        Nothing   -> do
          uniq <- takeUniqFromNameCache nc
          let name       = mkExternalName uniq mod occ span
              new_cache  = extendOrigNameCache cache mod occ name
          pure (new_cache, name)

    LocalName occ span -> do
      uniq <- takeUniqFromNameCache nc
      -- don't update the NameCache for local names
      pure $ mkInternalName uniq occ span

    KnownKeyName u -> case lookupKnownKeyName ghc u of
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
