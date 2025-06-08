{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module GHC.Builtin.Imports (
  module GHC.Builtin.Imports
, module Imports
) where

import Data.Word
import Language.Haskell.Syntax.Module.Name
import GHC.Data.FastString
import GHC.Types.Unique
import GHC.Unit.Types as Imports
import GHC.Types.Name as Imports
import GHC.Types.SrcLoc as Imports

external :: Word64 -> Unit -> FastString -> NameSpace -> FastString -> SrcSpan -> Name
external unique unit module_ t name =
  mkExternalName (mkUniqueGrimily unique) (Module unit $ ModuleName module_) (mkOccNameFS t name)

#if __GLASGOW_HASKELL__ >= 912
baseUnit :: Unit
baseUnit = fsToUnit "base"

thUnit :: Unit
thUnit = fsToUnit "template-haskell"
#endif

#if __GLASGOW_HASKELL__ == 908
ghcInternalUnit :: Unit
ghcInternalUnit = fsToUnit "ghc-internal"
#endif
