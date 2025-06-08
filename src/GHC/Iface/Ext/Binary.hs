{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
module GHC.Iface.Ext.Binary (
  readHieFile
, readHieFileEither
, HieHeader
, HieFileResult(..)
, extractSourceFileName
) where

import Data.List (intercalate)
import Data.ByteString (ByteString)

import GHC.Types.Name.Cache

import GHC.Iface.Ext.Types

import GHC.Iface.Ext.Binary.Utils
import GHC.Iface.Ext.Binary.GHC912 qualified as HieFile
import GHC.Iface.Ext.Binary.Header (HieHeader, readHieFileHeader)
import GHC.Iface.Ext.Binary.Header qualified as Header

#if __GLASGOW_HASKELL__ == 908 || __GLASGOW_HASKELL__ == 910 || __GLASGOW_HASKELL__ == 912
supported :: [Integer]
supported = supported908 ++ supported910 ++ supported912
#endif

supported908 :: [Integer]
supported908 = [9081 .. 9084]

supported910 :: [Integer]
supported910 = [9101 .. 9102]

supported912 :: [Integer]
supported912 = [9121 .. 9122]

-- | Read a `HieFile` from a `FilePath`. Can use an existing `NameCache`.
readHieFile :: NameCache -> FilePath -> IO HieFileResult
readHieFile name_cache file = readHie (unsupportedVersion file) id name_cache file

extractSourceFileName :: FilePath -> IO FilePath
extractSourceFileName file = readBinMem file >>= Header.extractSourceFileName

unsupportedVersion :: FilePath -> HieHeader -> IO a
unsupportedVersion file = fail . unsupportedVersionError file

unsupportedVersionError :: FilePath -> HieHeader -> String
unsupportedVersionError file (show -> version, _) =
  "Unsupported HIE version " <> version <> " for file " <> file <> ", supported versions: " <> supportedVersions
  where
    supportedVersions :: String
    supportedVersions = intercalate ", " . reverse $ map show supported

-- | Read a `HieFile` from a `FilePath`. Can use an existing `NameCache`.
-- `Left` case returns the failing header versions.
readHieFileEither :: NameCache -> FilePath -> IO (Either HieHeader HieFileResult)
readHieFileEither = readHie (return . Left) Right

readHie :: (HieHeader -> IO a) -> (HieFileResult -> a) -> NameCache -> FilePath -> IO a
readHie left right name_cache file = do
  bh0 <- readBinMem file
  header@(version, ghcVersion) <- readHieFileHeader file bh0
  let hieFileResult = right . HieFileResult version ghcVersion
  if
    | version `elem` supported908 -> hieFileResult <$> HieFile.readHieFile908 bh0 name_cache
    | version `elem` supported910 -> hieFileResult <$> HieFile.readHieFile910 bh0 name_cache
    | version `elem` supported912 -> hieFileResult <$> HieFile.readHieFile912 bh0 name_cache
    | otherwise -> left header
{-# INLINE readHie #-}

data HieFileResult = HieFileResult {
  hie_file_result_version :: Integer
, hie_file_result_ghc_version :: ByteString
, hie_file_result :: HieFile
}
