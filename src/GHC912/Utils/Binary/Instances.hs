{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module GHC912.Utils.Binary.Instances where

import GHC.Types.Unique (getUnique)
import GHC.Types.Unique.DSet (unionManyUniqDSets)
import Control.Monad
import Data.Proxy
import GHC.Types.Name
import GHC.Types.Avail
import GHC.Types.Basic
import GHC.Types.Var hiding (varName)
import GHC.Unit.Types
import GHC.Iface.Type

import GHC912.Utils.Binary

import GHC.Utils.Panic.Plain (panic)

instance Binary Name where
   put_ bh name =
      case findUserDataWriter Proxy bh of
        tbl -> putEntry tbl bh name

   get bh =
      case findUserDataReader Proxy bh of
        tbl -> getEntry tbl bh

instance Binary a => Binary (GenModule a) where
  put_ bh (Module p n) = put_ bh p >> put_ bh n
  -- Module has strict fields, so use $! in order not to allocate a thunk
  get bh = do p <- get bh; n <- get bh; return $! Module p n

instance Binary Unit where
  put_ bh (RealUnit def_uid) = do
    putByte bh 0
    put_ bh def_uid
  put_ bh (VirtUnit indef_uid) = do
    putByte bh 1
    put_ bh indef_uid
  put_ bh HoleUnit =
    putByte bh 2
  get bh = do b <- getByte bh
              u <- case b of
                0 -> fmap RealUnit (get bh)
                1 -> fmap VirtUnit (get bh)
                _ -> pure HoleUnit
              -- Unit has strict fields that need forcing; otherwise we allocate a thunk.
              pure $! u

deriving newtype instance Binary unit => Binary (Definite unit)

instance Binary InstantiatedUnit where
  put_ bh indef = do
    put_ bh (instUnitInstanceOf indef)
    put_ bh (instUnitInsts indef)
  get bh = do
    cid   <- get bh
    insts <- get bh
    let fs = mkInstantiatedUnitHash cid insts
    -- InstantiatedUnit has strict fields, so use $! in order not to allocate a thunk
    return $! InstantiatedUnit {
                instUnitInstanceOf = cid,
                instUnitInsts = insts,
                instUnitHoles = unionManyUniqDSets (map (moduleFreeHoles.snd) insts),
                instUnitFS = fs,
                instUnitKey = getUnique fs
              }

instance Binary UnitId where
  put_ bh (UnitId fs) = put_ bh fs
  get bh = do fs <- get bh; return (UnitId fs)

instance Binary AvailInfo where
    put_ bh (Avail aa) = do
            putByte bh 0
            put_ bh aa
    put_ bh (AvailTC ab ac) = do
            putByte bh 1
            put_ bh ab
            put_ bh ac
    get bh = do
            h <- getByte bh
            case h of
              0 -> do aa <- get bh
                      return (Avail aa)
              _ -> do ab <- get bh
                      ac <- get bh
                      return (AvailTC ab ac)

instance Binary IfaceTyCon where
  put_ bh (IfaceTyCon n i) = put_ bh n >> put_ bh i

  get bh = do
    n <- get bh
    i <- get bh
    return (IfaceTyCon n i)

instance Binary IfaceTyConSort where
   put_ bh IfaceNormalTyCon             = putByte bh 0
   put_ bh (IfaceTupleTyCon arity sort) = putByte bh 1 >> put_ bh arity >> put_ bh sort
   put_ bh (IfaceSumTyCon arity)        = putByte bh 2 >> put_ bh arity
   put_ bh IfaceEqualityTyCon           = putByte bh 3

   get bh = do
       n <- getByte bh
       case n of
         0 -> return IfaceNormalTyCon
         1 -> IfaceTupleTyCon <$> get bh <*> get bh
         2 -> IfaceSumTyCon <$> get bh
         _ -> return IfaceEqualityTyCon

instance Binary IfaceTyConInfo where
   put_ bh (IfaceTyConInfo i s) = put_ bh i >> put_ bh s

   get bh = mkIfaceTyConInfo <$!> get bh <*> get bh
    -- We want to make sure, when reading from disk, as the most common case
    -- is supposed to be shared. Any thunk adds an additional indirection
    -- making sharing less useful.
    --
    -- See !12200 for how this bang and the one in 'IfaceTyCon' reduces the
    -- residency by ~10% when loading 'mi_extra_decls' from disk.

instance Binary TupleSort where
    put_ bh BoxedTuple      = putByte bh 0
    put_ bh UnboxedTuple    = putByte bh 1
    put_ bh ConstraintTuple = putByte bh 2
    get bh = do
      h <- getByte bh
      case h of
        0 -> return BoxedTuple
        1 -> return UnboxedTuple
        _ -> return ConstraintTuple

instance Binary PromotionFlag where
   put_ bh NotPromoted = putByte bh 0
   put_ bh IsPromoted  = putByte bh 1

   get bh = do
       n <- getByte bh
       case n of
         0 -> return NotPromoted
         1 -> return IsPromoted
         _ -> fail "Binary(IsPromoted): fail)"

instance Binary ForAllTyFlag where
  put_ bh Required  = putByte bh 0
  put_ bh Specified = putByte bh 1
  put_ bh Inferred  = putByte bh 2

  get bh = do
    h <- getByte bh
    case h of
      0 -> return Required
      1 -> return Specified
      _ -> return Inferred

instance Binary IfaceTyLit where
  put_ bh (IfaceNumTyLit n)   = putByte bh 1 >> put_ bh n
  put_ bh (IfaceStrTyLit n)   = putByte bh 2 >> put_ bh n
  put_ bh (IfaceCharTyLit n)  = putByte bh 3 >> put_ bh n

  get bh =
    do tag <- getByte bh
       case tag of
         1 -> do { n <- get bh
                 ; return (IfaceNumTyLit n) }
         2 -> do { n <- get bh
                 ; return (IfaceStrTyLit n) }
         3 -> do { n <- get bh
                 ; return (IfaceCharTyLit n) }
         _ -> panic ("get IfaceTyLit " ++ show tag)

putAllTables :: WriteBinHandle -> [WriterTable] -> IO b -> IO ([Int], b)
putAllTables _ [] act = do
  a <- act
  pure ([], a)
putAllTables bh (x : xs) act = do
  (r, (res, a)) <- forwardPutRel bh (const $ putTable x bh) $ do
    putAllTables bh xs act
  pure (r : res, a)

instance Binary OccName where
    put_ bh name = do
            put_ bh (occNameSpace name)
            put_ bh (occNameFS name)
    get bh = do
          aa <- get bh
          ab <- get bh
          return (mkOccNameFS aa ab)

instance Binary NameSpace where
    put_ _ _ = undefined
    get bh = do
            h <- getByte bh
            case h of
              0 -> return varName
              1 -> return dataName
              2 -> return tvName
              3 -> return tcClsName
              _ -> do
                parent <- get bh
                return $ fieldName parent
