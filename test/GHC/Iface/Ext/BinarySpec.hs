{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module GHC.Iface.Ext.BinarySpec (spec) where

import Test.Hspec

import Data.String
import Control.Monad

import System.Process
import System.FilePath
import System.IO.Temp
import System.Environment.Blank (unsetEnv)

import GHC.Unit.Types
import GHC.Types.Name.Cache (initNameCache)
import Language.Haskell.Syntax.Module.Name

import GHC912.Iface.Ext.Binary
import GHC912.Iface.Ext.Types
import GHC912.Iface.Ext.Utils ()
import GHC912.Iface.Ext.Debug ()

run :: CreateProcess -> IO ()
run p = withCreateProcess p \ _ _ _ -> void . waitForProcess

deriving instance Show unit => Show (GenModule unit)
deriving newtype instance IsString ModuleName
deriving newtype instance IsString UnitId

instance IsString Unit where
  fromString = RealUnit . Definite . fromString

spec :: Spec
spec = do
  describe "readHieFile" do
    it "can read HIE-files that were created with GHC 9.12.1" do
      unsetEnv "GHC_ENVIRONMENT"
      withSystemTempDirectory "hspec" \ dir -> do
        writeFile (dir </> "Foo.hs") $ unlines [
            "module Foo where"
          , "foo :: Int"
          , "foo = 23"
          ]
        run (proc "ghc-9.12.1" ["-v0", "-fwrite-ide-info", "Foo.hs"]) { cwd = Just dir }

        envNameCache <- initNameCache 'r' mempty
        r <- readHieFile envNameCache $ dir </> "Foo.hie"

        r.hie_file_result_version `shouldBe` 9121
        r.hie_file_result_ghc_version `shouldBe` "9.12.1"
        r.hie_file_result.hie_hs_file `shouldBe` "Foo.hs"
        r.hie_file_result.hie_module `shouldBe` Module "main" "Foo"
