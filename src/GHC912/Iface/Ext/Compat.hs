{-# LANGUAGE CPP #-}
module GHC912.Iface.Ext.Compat where

#if !MIN_VERSION_ghc(9,12,1)
mkIfLclName :: a -> a
mkIfLclName = id
#endif
