{-# LANGUAGE CPP #-}
module GHC.Iface.Ext.Compat where

#if !MIN_VERSION_ghc(9,12,1)
mkIfLclName :: a -> a
mkIfLclName = id
#endif
