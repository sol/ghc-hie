{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module GHC.Iface.Ext.BinarySpec (spec) where

import Test.Hspec

import Control.Concurrent (readMVar)
import Data.Maybe
import Data.String
import Data.Functor
import Data.Foldable
import Data.Map qualified as Map
import Data.ByteString.Char8 qualified as B

import System.Process
import System.FilePath
import System.IO.Temp
import System.Environment.Blank (unsetEnv)

import GHC.Unit.Types
import GHC.Types.SrcLoc
import GHC.Types.Name
import GHC.Types.Name.Cache
import GHC.Unit.Module.Env
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
  withSystemTempDirectory "hspec" \ dir -> do
    createHieFile ghc dir "Foo.hs" [
        "module Foo where"
      , "foo :: Int"
      , "foo = 23"
      ]
    action $ dir </> "Foo.hie"

createHieFile :: FilePath -> FilePath -> FilePath -> [String] -> IO ()
createHieFile ghc dir name contents = do
  writeFile (dir </> name) $ unlines contents
  unsetEnv "GHC_ENVIRONMENT"
  run (proc ghc ["-v0", "-fwrite-ide-info", name]) { cwd = Just dir }

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
        nameCache <- initNameCache 'r' mempty
        readHieFile nameCache hieFile `shouldThrow` (== expected)

    for_ supported \ (ghcVersion, hieVersion) -> do

      let ghc = "ghc-" <> ghcVersion

      context ("with " <> ghc) do

        it "accepts HIE-files" do
          withHieFile ghc \ hieFile -> do
            extractSourceFileName hieFile `shouldReturn` "Foo.hs"

            nameCache <- initNameCache 'r' mempty
            result <- readHieFile nameCache hieFile

            result.hie_file_result_version `shouldBe` hieVersion
            result.hie_file_result_ghc_version `shouldBe` fromString ghcVersion

            let hie_file = result.hie_file_result

            hie_file.hie_hs_file `shouldBe` "Foo.hs"
            hie_file.hie_module `shouldBe` Module "main" "Foo"
            length hie_file.hie_types `shouldBe` 1
            Map.keys hie_file.hie_asts.getAsts `shouldBe` [HiePath "Foo.hs"]
            length hie_file.hie_exports `shouldBe` 1
            hie_file.hie_hs_src `shouldBe` B.unlines [
                "module Foo where"
              , "foo :: Int"
              , "foo = 23"
              ]
            length hie_file.hie_entity_infos `shouldBe` if hieVersion < 9121 then 0 else 3

        it "maintains precise locations in NameCache" do
          withSystemTempDirectory "hspec" \ dir -> do
            createHieFile ghc dir "Foo.hs" [
                "module Foo (foo) where"
              , ""
              , ""
              , "foo :: Int"
              , "foo = 23"
              ]
            createHieFile ghc dir "Bar.hs" [
                "module Bar where"
              , "import qualified Foo"
              , "bar :: Int"
              , "bar = Foo.foo"
              ]
            createHieFile ghc dir "Foo.hs" [
                "module Foo (foo) where"
              , "foo :: Int"
              , "foo = 23"
              ]
            nameCache <- initNameCache 'r' mempty
            _ <- readHieFile nameCache $ dir </> "Bar.hie"
            _ <- readHieFile nameCache $ dir </> "Foo.hie"

            names <- moduleEnvElts <$> readMVar (nsNames nameCache)

            let
              lookupFoo :: OccEnv Name -> Maybe Name
              lookupFoo = flip lookupOccEnv $ mkOccName varName "foo"

              candidates :: [Name]
              candidates = mapMaybe lookupFoo names

              expectedLocation :: SrcSpan
              expectedLocation = mkSrcSpan (mkSrcLoc "Foo.hs" 3 1) (mkSrcLoc "Foo.hs" 3 4)

            map nameSrcSpan candidates `shouldBe` [expectedLocation]
