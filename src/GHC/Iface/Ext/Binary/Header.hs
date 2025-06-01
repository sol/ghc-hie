module GHC.Iface.Ext.Binary.Header (
  extractSourceFileName
, HieHeader
, readHieFileHeader
) where

import Control.Monad
import Data.Word
import Data.ByteString (ByteString, pack)
import Data.ByteString.Internal (w2c)
import GHC.Settings.Utils (maybeRead)

import GHC.Iface.Ext.Binary.Utils

hieMagic :: [Word8]
hieMagic = [72,73,69]

hieMagicLen :: Int
hieMagicLen = 3

newline :: Word8
newline = 10

extractSourceFileName :: ReadBinHandle -> IO FilePath
extractSourceFileName bh0 = do
  advance bh0 hieMagicLen
  skipLine
  skipLine
  advance bh0 8
  get @FilePath bh0
  where
    skipLine :: IO ()
    skipLine = do
      c <- get @Word8 bh0
      if c == newline
      then return ()
      else skipLine

type HieHeader = (Integer, ByteString)

readHieFileHeader :: FilePath -> ReadBinHandle -> IO HieHeader
readHieFileHeader file bh0 = do
  magic <- replicateM hieMagicLen (get bh0)
  version <- map w2c <$> takeLine
  case maybeRead version of
    Nothing -> do
      fail $ "readHieFileHeader: hieVersion isn't an Integer: " <> show version
    Just hieVersion -> do
      ghcVersion <- pack <$> takeLine
      when (magic /= hieMagic) . fail $ unwords [
          "readHieFileHeader: headers don't match for file:"
        , file
        , "Expected"
        , show hieMagic
        , "but got", show magic
        ]
      return (hieVersion, ghcVersion)
  where
    takeLine :: IO [Word8]
    takeLine = reverse <$> loop []
      where
        loop :: [Word8] -> IO [Word8]
        loop acc = do
          c <- get @Word8 bh0
          if c == newline
          then return acc
          else loop (c : acc)
