{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
module GHC.Iface.Ext.Binary (
  readHieFile
, readHieFileEither
, HieHeader
, HieFileResult(..)
) where

import Data.List (intercalate)
import Data.ByteString (ByteString)

import GHC.Types.Name.Cache

import GHC.Iface.Ext.Types

import GHC910.Iface.Ext.Binary qualified as GHC910
import GHC912.Utils.Binary
import GHC912.Iface.Ext.Binary (HieHeader, readHieFileHeader)
import GHC912.Iface.Ext.Binary qualified as GHC912

supported :: [Integer]
supported = supported910 ++ supported912

supported910 :: [Integer]
supported910 = [9101, 9102]

supported912 :: [Integer]
supported912 = [9121]

-- | Read a `HieFile` from a `FilePath`. Can use an existing `NameCache`.
readHieFile :: NameCache -> FilePath -> IO HieFileResult
readHieFile name_cache file = do
  readHieFileEither name_cache file >>= either (unsupportedVersion file) return

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
readHieFileEither name_cache file = do
  bh0 <- readBinMem file
  header@(version, ghcVersion) <- readHieFileHeader file bh0
  let
    hieFileResult :: HieFile -> Either HieHeader HieFileResult
    hieFileResult = Right . HieFileResult version ghcVersion
  if
    | version `elem` supported910 -> hieFileResult <$> GHC910.readHieFileContents bh0 name_cache
    | version `elem` supported912 -> hieFileResult <$> GHC912.readHieFileContents bh0 name_cache
    | otherwise -> return (Left header)

data HieFileResult = HieFileResult {
  hie_file_result_version :: Integer
, hie_file_result_ghc_version :: ByteString
, hie_file_result :: HieFile
}
