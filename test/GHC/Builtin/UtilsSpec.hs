{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module GHC.Builtin.UtilsSpec (spec) where

import Data.ByteString.Builder qualified as Builder
import Data.ByteString qualified as B
import GHC.Settings.Config (cProjectUnitId)
import Prelude hiding (mod, span)

import GHC.Builtin.Imports

import Control.Monad (when)
import Control.Exception (try)
import Data.List qualified as List
import           Language.Haskell.Syntax.Module.Name
import Data.Word
import GHC.Data.FastString
import GHC.Utils.Outputable hiding ((<>))
import           GHC.Types.Unique
import           GHC.Types.TyThing
import           Test.Hspec

import "ghc" GHC.Builtin.Utils qualified as GHC


#if __GLASGOW_HASKELL__ == 908
import GHC.Builtin.Names.GHC908
#elif __GLASGOW_HASKELL__ == 910
import GHC.Builtin.Names.GHC910
#elif __GLASGOW_HASKELL__ == 912
import GHC.Builtin.Names.GHC912
#elif __GLASGOW_HASKELL__ == 914
import GHC.Builtin.Names.GHC914
#else
knownKeyNames :: [Name]
knownKeyNames = undefined
#endif

tryReadFile :: FilePath -> IO (Either IOError B.ByteString)
tryReadFile = try . B.readFile

ensureFile :: FilePath -> String -> IO ()
ensureFile name (B.toStrict . Builder.toLazyByteString . Builder.stringUtf8 -> new) = do
  old <- tryReadFile name
  when (old /= Right new) $ do
    B.writeFile name new

newtype Exact a = Exact a

instance Show (Exact Name) where
  show (Exact name) = showPprUnsafe name

instance Eq (Exact Name) where
  Exact a == Exact b =
        nameUnique a == nameUnique b
    && nameOccName a == nameOccName b
    && nameSrcSpan a == nameSrcSpan b

    && isExternalName a == isExternalName b
    -- && isWiredInName a == isWiredInName b
    && isInternalName a == isInternalName b
    && isSystemName a == isSystemName b

    && nameModule_maybe a == nameModule_maybe b
    -- && wiredInNameTyThing_maybe a == wiredInNameTyThing_maybe b
    -- && isBuiltInSyntax a == isBuiltInSyntax b

_foo :: Bool
_foo = case undefined :: BuiltInSyntax of
  -- If this ever gets more constructors, then we need to update the Eq instance
  BuiltInSyntax -> True
  UserSyntax -> True

deriving instance Eq TyThing

foo :: Name -> Name
foo name = external unique unit modName nameSpace occname span
  where
    nameSpace :: NameSpace
    nameSpace = case fieldOcc_maybe occ of
      Just field -> fieldName field
      Nothing -> case occNameSpace occ of
        n
          | n == varName -> varName
          | n == dataName -> dataName
          | n == tvName -> tvName
          | n == tcClsName -> tcClsName
          | otherwise -> error $ showSDocUnsafe $ pprNameSpace n

    unique :: Word64
    unique = getKey $ nameUnique name

    modName :: FastString
    (uu, modName) = case nameModule name of
      Module   (RealUnit (Definite (UnitId u))) (ModuleName n) ->
        (u, n)
        -- Module thisGhcUnit (ModuleName n)
      _ -> undefined

    unit :: Unit
    unit = case uu of
      "ghc-prim" -> primUnit
      "ghc-internal" -> ghcInternalUnit
      "ghc-bignum" -> bignumUnit
      "base" -> baseUnit
      "template-haskell" -> thUnit
      u | u == fsLit cProjectUnitId -> thisGhcUnit
      u -> error $ show u



    span :: SrcSpan
    span = case nameSrcSpan name of
      UnhelpfulSpan UnhelpfulWiredIn -> wiredInSrcSpan
      UnhelpfulSpan UnhelpfulNoLocationInfo -> noSrcSpan
      _ -> undefined

    occ :: OccName
    occ = nameOccName name

    occname :: FastString
    occname = occNameFS occ

foo_ :: Name -> String
foo_ name = unwords ["external"
  , show @Word64 unique
  , unit
  , show @FastString modName
  , nameSpace
  , show @FastString occname
  , span
  ]
  where
    nameSpace :: String
    nameSpace = case fieldOcc_maybe occ of
      Just field -> "(fieldName " <> show field <> ")"
      Nothing -> case occNameSpace occ of
        n
          | n == varName -> "varName"
          | n == dataName -> "dataName"
          | n == tvName -> "tvName"
          | n == tcClsName -> "tcClsName"
          | otherwise -> error $ showSDocUnsafe $ pprNameSpace n

    unique :: Word64
    unique = getKey $ nameUnique name

    modName :: FastString
    (uu, modName) = case nameModule name of
      Module   (RealUnit (Definite (UnitId u))) (ModuleName n) ->
        (u, n)
        -- Module thisGhcUnit (ModuleName n)
      _ -> undefined

    unit :: String
    unit = case uu of
      "ghc-prim" -> "primUnit"
      "ghc-internal" -> "ghcInternalUnit"
      "ghc-bignum" -> "bignumUnit"
      "base" -> "baseUnit"
      "template-haskell" -> "thUnit"
      u | u == fsLit cProjectUnitId -> "thisGhcUnit"
      u -> error $ show u



    span :: String
    span = case nameSrcSpan name of
      UnhelpfulSpan UnhelpfulWiredIn -> "wiredInSrcSpan"
      UnhelpfulSpan UnhelpfulNoLocationInfo -> "noSrcSpan"
      _ -> undefined

    occ :: OccName
    occ = nameOccName name

    occname :: FastString
    occname = occNameFS occ

known :: [Name]
known = List.sortOn (getKey . nameUnique) GHC.knownKeyNames

spec :: Spec
spec = do
  describe "reverse" $ do
    xit "reverses a list" $ do
      let
        m = "GHC" <> show @Int __GLASGOW_HASKELL__

      ensureFile ("src/GHC/Builtin/Names/" <> m <> ".hs") $ unlines [
          "{-# LANGUAGE OverloadedStrings #-}"
        , "module GHC.Builtin.Names." <> m <> " where"
        , ""
        , "import GHC.Builtin.Imports"
        , ""
        , "knownKeyNames :: [Name]"
        , "knownKeyNames = ["
        , "    " <> List.intercalate "\n  , " (map foo_ known)
        , "  ]"
        ]

      map (Exact . foo) (take 10 $ known) `shouldBe` (take 10 $ map Exact known)
      map (Exact . foo) (known) `shouldBe` map Exact known

      map Exact (take 10 $ knownKeyNames) `shouldBe` (take 10 $ map Exact known)
      map Exact (knownKeyNames) `shouldBe` map Exact known

{-
    it "reverses a list" $ do
      print $ length $ known
      print $ length $ filter isExternalName known

      print $ length $ filter isWiredInName GHC.knownKeyNames
      print $ length $ [ty | Just ty <- map wiredInNameTyThing_maybe GHC.knownKeyNames]
      print $ length $ [() | Just (AnId _) <- map wiredInNameTyThing_maybe GHC.knownKeyNames]
      print $ length $ [() | Just (ATyCon _) <- map wiredInNameTyThing_maybe GHC.knownKeyNames]
      print $ length $ [() | Just (AConLike _) <- map wiredInNameTyThing_maybe GHC.knownKeyNames]

      putStrLn "*************************"
      print $ length $ [() | Just (ACoAxiom _) <- map wiredInNameTyThing_maybe GHC.knownKeyNames]
      print $ length $ filter isSystemName GHC.knownKeyNames


      let
        name :: Name
        name : _ = GHC.knownKeyNames

        occ :: OccName
        occ = nameOccName name

        occname :: FastString
        occname = occNameFS occ

        nameSpace :: NameSpace
        nameSpace = occNameSpace occ

        uniq :: Word64
        uniq = getKey $ nameUnique name

        loc :: SrcSpan
        loc = nameSrcSpan name


        mod = nameModule name
        tyThing = wiredInNameTyThing_maybe name

      print loc
      print $ showPprUnsafe occ
      print occname
      print $ showSDocUnsafe $ pprNameSpace nameSpace
      print $ isTcClsNameSpace nameSpace
      print $ mod
      print $ showPprUnsafe tyThing

      let
        actual = mkSystemNameAt
          (mkUniqueGrimily uniq)
          (mkOccNameFS tcClsName occname)
          (UnhelpfulSpan UnhelpfulWiredIn)

      pending
      -- Exact actual `shouldBe` Exact name
      -- -}
