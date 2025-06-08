{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
module GHC.Builtin.Utils (GHC(..), isKnownKeyName, lookupKnownKeyName) where

import Data.Maybe
import Control.Applicative

import GHC.Types.Unique
import GHC.Types.Unique.FM

import GHC.Builtin.Imports
import GHC.Builtin.Uniques
import GHC.Builtin.Names.GHC908 qualified as GHC908
import GHC.Builtin.Names.GHC910 qualified as GHC910
import GHC.Builtin.Names.GHC912 qualified as GHC912

data GHC =
    GHC908
  | GHC910
  | GHC912

lookupKnownKeyName :: GHC -> Unique -> Maybe Name
lookupKnownKeyName = \ case
  GHC908 -> lookupKnownKeyName908
  GHC910 -> lookupKnownKeyName910
  GHC912 -> lookupKnownKeyName912

lookupKnownKeyName908 :: Unique -> Maybe Name
lookupKnownKeyName908 u = knownUniqueName u <|> lookupUFM_Directly knownKeysMap908 u

lookupKnownKeyName910 :: Unique -> Maybe Name
lookupKnownKeyName910 u = knownUniqueName u <|> lookupUFM_Directly knownKeysMap910 u

lookupKnownKeyName912 :: Unique -> Maybe Name
lookupKnownKeyName912 u = knownUniqueName u <|> lookupUFM_Directly knownKeysMap912 u

knownKeysMap908 :: UniqFM Name Name
knownKeysMap908 = listToIdentityUFM GHC908.knownKeyNames

knownKeysMap910 :: UniqFM Name Name
knownKeysMap910 = listToIdentityUFM GHC910.knownKeyNames

knownKeysMap912 :: UniqFM Name Name
knownKeysMap912 = listToIdentityUFM GHC912.knownKeyNames

isKnownKeyName :: Name -> Bool
isKnownKeyName n = isJust (knownUniqueName $ nameUnique n) || elemUFM n knownKeys

#if __GLASGOW_HASKELL__ == 908 || __GLASGOW_HASKELL__ == 910 || __GLASGOW_HASKELL__ == 912
knownKeys :: UniqFM Name ()
knownKeys = () <$ knownKeysMap908 <> knownKeysMap910 <> knownKeysMap912
#endif
