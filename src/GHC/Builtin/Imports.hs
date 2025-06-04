module GHC.Builtin.Imports (
  external
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
  mkExternalName (mkUniqueGrimily unique ) (Module unit (ModuleName module_)) (mkOccNameFS t name)
