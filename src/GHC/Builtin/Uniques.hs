{-# LANGUAGE CPP #-}
module GHC.Builtin.Uniques (knownUniqueName) where

import GHC.Prelude

import GHC.Builtin.Types
import GHC.Core.TyCon
import GHC.Core.DataCon
import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.Basic
import GHC.Types.Unique

import GHC.Utils.Outputable
import GHC.Utils.Panic

import Data.Maybe
import GHC.Utils.Word64 (word64ToInt)

#if __GLASGOW_HASKELL__ == 910
#if __GLASGOW_HASKELL_PATCHLEVEL1__ == 1 || __GLASGOW_HASKELL_PATCHLEVEL1__ == 2
knownUniqueName :: Unique -> Maybe Name
knownUniqueName u =
    case tag of
      'z' -> Just $ getUnboxedSumName n
      '4' -> Just $ getTupleTyConName Boxed n
      '5' -> Just $ getTupleTyConName Unboxed n
      '7' -> Just $ getTupleDataConName Boxed n
      '8' -> Just $ getTupleDataConName Unboxed n
      'j' -> Just $ getCTupleSelIdName n
      'k' -> Just $ getCTupleTyConName n
      'm' -> Just $ getCTupleDataConName n
      _   -> Nothing
  where
    (tag, n') = unpkUnique u
    -- Known unique names are guaranteed to fit in Int, so we don't need the whole Word64.
    n = assert (isValidKnownKeyUnique u) (word64ToInt n')

getUnboxedSumName :: Int -> Name
getUnboxedSumName n
  | n .&. 0xfc == 0xfc
  = case tag of
      0x0 -> tyConName $ sumTyCon arity
      0x1 -> getRep $ sumTyCon arity
      _   -> pprPanic "getUnboxedSumName: invalid tag" (ppr tag)
  | tag == 0x0
  = dataConName $ sumDataCon (alt + 1) arity
  | tag == 0x1
  = getName $ dataConWrapId $ sumDataCon (alt + 1) arity
  | tag == 0x2
  = getRep $ promoteDataCon $ sumDataCon (alt + 1) arity
  | otherwise
  = pprPanic "getUnboxedSumName" (ppr n)
  where
    arity = n `shiftR` 8
    alt = (n .&. 0xfc) `shiftR` 2
    tag = 0x3 .&. n
    getRep tycon =
        fromMaybe (pprPanic "getUnboxedSumName(getRep)" (ppr tycon))
        $ tyConRepName_maybe tycon

getCTupleTyConName :: Int -> Name
getCTupleTyConName n =
    case n `divMod` 2 of
      (arity, 0) -> cTupleTyConName arity
      (arity, 1) -> mkPrelTyConRepName $ cTupleTyConName arity
      _          -> panic "getCTupleTyConName: impossible"

getCTupleDataConName :: Int -> Name
getCTupleDataConName n =
    case n `divMod` 3 of
      (arity,  0) -> cTupleDataConName arity
      (arity,  1) -> getName $ dataConWrapId $ cTupleDataCon arity
      (arity,  2) -> mkPrelTyConRepName $ cTupleDataConName arity
      _           -> panic "getCTupleDataConName: impossible"

getCTupleSelIdName :: Int -> Name
getCTupleSelIdName n = cTupleSelIdName (sc_pos + 1) arity
  where
    arity  = n `shiftR` cTupleSelIdArityBits
    sc_pos = n .&. cTupleSelIdPosBitmask

cTupleSelIdArityBits :: Int
cTupleSelIdArityBits = 8

cTupleSelIdPosBitmask :: Int
cTupleSelIdPosBitmask = 0xff

getTupleTyConName :: Boxity -> Int -> Name
getTupleTyConName boxity n =
    case n `divMod` 2 of
      (arity, 0) -> tyConName $ tupleTyCon boxity arity
      (arity, 1) -> fromMaybe (panic "getTupleTyConName")
                    $ tyConRepName_maybe $ tupleTyCon boxity arity
      _          -> panic "getTupleTyConName: impossible"

getTupleDataConName :: Boxity -> Int -> Name
getTupleDataConName boxity n =
    case n `divMod` 3 of
      (arity, 0) -> dataConName $ tupleDataCon boxity arity
      (arity, 1) -> idName $ dataConWorkId $ tupleDataCon boxity arity
      (arity, 2) -> fromMaybe (panic "getTupleDataCon")
                    $ tyConRepName_maybe $ promotedTupleDataCon boxity arity
      _          -> panic "getTupleDataConName: impossible"
#endif
#endif
