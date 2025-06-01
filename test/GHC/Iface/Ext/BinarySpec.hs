{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module GHC.Iface.Ext.BinarySpec (spec) where

import Test.Hspec

import Data.String
import Data.Functor
import Data.Foldable

import System.Process
import System.FilePath
import System.IO.Temp
import System.Environment.Blank (unsetEnv)

import GHC.Unit.Types
import GHC.Types.Name.Cache (initNameCache)
import Language.Haskell.Syntax.Module.Name

import GHC.Iface.Ext.Binary
import GHC.Iface.Ext.Types
import GHC.Iface.Ext.Utils ()
import GHC.Iface.Ext.Debug ()

run :: CreateProcess -> IO ()
run p = withCreateProcess p \ _ _ _ -> void . waitForProcess

deriving instance Show unit => Show (GenModule unit)
deriving newtype instance IsString ModuleName
deriving newtype instance IsString UnitId

instance IsString Unit where
  fromString = RealUnit . Definite . fromString

withHieFile :: String -> (FilePath -> IO b) -> IO b
withHieFile ghc action = do
  unsetEnv "GHC_ENVIRONMENT"
  withSystemTempDirectory "hspec" \ dir -> do
    writeFile (dir </> "Foo.hs") $ unlines [
        "module Foo where"
      , "foo :: Int"
      , "foo = 23"
      ]
    run (proc ghc ["-v0", "-fwrite-ide-info", "Foo.hs"]) { cwd = Just dir }
    action $ dir </> "Foo.hie"

supported :: [(String, Integer)]
supported = [
    ("9.10.1", 9101)
  , ("9.10.2", 9102)
  , ("9.12.1", 9121)
  ]

spec :: Spec
spec = do
  describe "readHieFile" do
    it "rejects HIE-files created with GHC 9.8.4" do
      withHieFile "ghc-9.8.4" \ hieFile -> do
        extractSourceFileName hieFile `shouldReturn` "Foo.hs"
        let
          message =
               "Unsupported HIE version 9084 for file "
            <> hieFile
            <> ", supported versions: 9121, 9102, 9101"
          expected = userError message
        envNameCache <- initNameCache 'r' mempty
        readHieFile envNameCache hieFile `shouldThrow` (== expected)

    for_ supported \ (ghcVersion, hieVersion) -> do
      it ("accepts HIE-files created with GHC " <> ghcVersion) do
        unsetEnv "GHC_ENVIRONMENT"
        withHieFile ("ghc-" <> ghcVersion) \ hieFile -> do
          extractSourceFileName hieFile `shouldReturn` "Foo.hs"
          envNameCache <- initNameCache 'r' mempty
          r <- readHieFile envNameCache hieFile
          r.hie_file_result_version `shouldBe` hieVersion
          r.hie_file_result_ghc_version `shouldBe` fromString ghcVersion
          r.hie_file_result.hie_hs_file `shouldBe` "Foo.hs"
          r.hie_file_result.hie_module `shouldBe` Module "main" "Foo"
