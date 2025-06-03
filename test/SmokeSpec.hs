{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module SmokeSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Foreign (Ptr, peek)
import Data.Word
import Data.Foldable
import Control.Exception (bracket_)
import System.Environment (lookupEnv)
import System.Process

import GHC.Types.Name.Cache
import GHC.Settings.Config (cProjectUnitId)
import GHC.Types.Unique.Supply (initUniqSupply)

import GHC.Iface.Ext.Binary
import GHC.Iface.Ext.BinarySpec ()
import qualified GHC.Iface.Ext.Upstream as Upstream

foreign import ccall unsafe "&ghc_unique_counter64" ghc_unique_counter64 :: Ptr Word64
foreign import ccall unsafe "&ghc_unique_inc"       ghc_unique_inc       :: Ptr Int

withDeterministicUniqueSupply :: IO a -> IO a
withDeterministicUniqueSupply action = do
  counter <- peek ghc_unique_counter64
  increment <- peek ghc_unique_inc
  bracket_ (initUniqSupply 0 1) (initUniqSupply counter increment) action

findHieFiles :: IO [FilePath]
findHieFiles = lookupEnv "CI" >>= \ case
  Nothing -> return []; Just
    _ -> lines <$> readCreateProcess (shell $ "find " <> store <> " -name '*.hie'") ""
  where
    store = "~/.local/state/cabal/store/" <> cProjectUnitId

spec :: Spec
spec = do
  describe "withDeterministicUniqueSupply" do
    it "runs an action with a deterministic unique supply" do
      withDeterministicUniqueSupply do
        peek ghc_unique_counter64 `shouldReturn` 0
        peek ghc_unique_inc `shouldReturn` 1

    it "restores the original unique supply when done" do
      counter <- peek ghc_unique_counter64
      increment <- peek ghc_unique_inc
      withDeterministicUniqueSupply do
        initUniqSupply 23 42
        peek ghc_unique_counter64 `shouldReturn` 23
        peek ghc_unique_inc `shouldReturn` 42
      peek ghc_unique_counter64 `shouldReturn` counter
      peek ghc_unique_inc `shouldReturn` increment

  describe "smoke tests" do
    runIO findHieFiles >>= traverse_ \ hieFile -> do
      it hieFile do
        theirs <- withDeterministicUniqueSupply do
          nameCache <- initNameCache 'r' mempty
          Upstream.readHieFile hieFile nameCache
        mine <- withDeterministicUniqueSupply do
          nameCache <- initNameCache 'r' mempty
          hie_file_result <$> readHieFile nameCache hieFile
        Blind mine `shouldBe` Blind theirs
