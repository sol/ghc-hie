module GHC.Builtin.Utils (isKnownKeyName, lookupKnownKeyName) where

import Data.Maybe
import Control.Applicative

import GHC.Types.Unique
import GHC.Types.Unique.FM

import GHC.Builtin.Imports
import GHC.Builtin.Uniques
import GHC.Builtin.Names

lookupKnownKeyName :: Unique -> Maybe Name
lookupKnownKeyName u = knownUniqueName u <|> lookupUFM_Directly knownKeysMap u

knownKeysMap :: UniqFM Name Name
knownKeysMap = listToIdentityUFM knownKeyNames

isKnownKeyName :: Name -> Bool
isKnownKeyName n = isJust (knownUniqueName $ nameUnique n) || elemUFM n knownKeysMap
