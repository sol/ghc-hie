{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module GHC.Iface.Ext.Upstream (readHieFile) where

import Prelude hiding (span)
import Data.Set qualified as Set
import Data.Map qualified as Map
import Data.Array.Base qualified as A

import GHC.Types.Avail
import GHC.Types.Name.Cache
import "ghc" GHC.Iface.Ext.Types qualified as GHC
import "ghc" GHC.Iface.Ext.Binary qualified as GHC

import GHC.Iface.Ext.Types

deriving instance Eq AvailInfo
deriving instance Eq HieFile

readHieFile :: FilePath -> NameCache -> IO HieFile
readHieFile hieFile nameCache = do
  fromHieFile . GHC.hie_file_result <$> GHC.readHieFile nameCache hieFile

fromHieFile :: GHC.HieFile -> HieFile
fromHieFile GHC.HieFile{..} = HieFile {
  hie_hs_file
, hie_module
, hie_types = A.amap fromHieType hie_types
, hie_asts = fromHieASTs hie_asts
, hie_exports
, hie_hs_src
#if __GLASGOW_HASKELL__ >= 912
, hie_entity_infos = Map.map (Set.map fromEntityInfo) hie_entity_infos
#else
, hie_entity_infos = mempty
#endif
}

#if __GLASGOW_HASKELL__ >= 912
fromEntityInfo :: GHC.EntityInfo -> EntityInfo
fromEntityInfo = \ case
  GHC.EntityVariable -> EntityVariable
  GHC.EntityFunction -> EntityFunction
  GHC.EntityDataConstructor -> EntityDataConstructor
  GHC.EntityTypeVariable -> EntityTypeVariable
  GHC.EntityClassMethod -> EntityClassMethod
  GHC.EntityPatternSynonym -> EntityPatternSynonym
  GHC.EntityTypeConstructor -> EntityTypeConstructor
  GHC.EntityTypeClass -> EntityTypeClass
  GHC.EntityTypeSynonym -> EntityTypeSynonym
  GHC.EntityTypeFamily -> EntityTypeFamily
  GHC.EntityRecordField -> EntityRecordField
#endif

fromHieType :: GHC.HieType a -> HieType a
fromHieType = \ case
  GHC.HTyVarTy name -> HTyVarTy name
  GHC.HAppTy a hieArgs -> HAppTy a (fromHieArgs hieArgs)
  GHC.HTyConApp ifaceTyCon hieArgs -> HTyConApp ifaceTyCon (fromHieArgs hieArgs)
  GHC.HForAllTy as a -> HForAllTy as a
  GHC.HFunTy a b c -> HFunTy a b c
  GHC.HQualTy a b -> HQualTy a b
  GHC.HLitTy ifaceTyLit -> HLitTy ifaceTyLit
  GHC.HCastTy a -> HCastTy a
  GHC.HCoercionTy -> HCoercionTy

fromHieArgs :: GHC.HieArgs a -> HieArgs a
fromHieArgs = \ case
  GHC.HieArgs xs -> HieArgs xs

fromHieASTs :: GHC.HieASTs a -> HieASTs a
fromHieASTs = \ case
  GHC.HieASTs asts -> HieASTs $ Map.map fromHieAST asts

fromHieAST :: GHC.HieAST a -> HieAST a
fromHieAST GHC.Node{..} = Node {
  sourcedNodeInfo = fromSourcedNodeInfo sourcedNodeInfo
, nodeSpan
, nodeChildren = map fromHieAST nodeChildren
}

fromSourcedNodeInfo :: GHC.SourcedNodeInfo a -> SourcedNodeInfo a
fromSourcedNodeInfo =
  SourcedNodeInfo . Map.map fromNodeInfo . Map.mapKeys fromNodeOrigin . GHC.getSourcedNodeInfo

fromNodeOrigin :: GHC.NodeOrigin -> NodeOrigin
fromNodeOrigin = \ case
  GHC.SourceInfo -> SourceInfo
  GHC.GeneratedInfo -> GeneratedInfo

fromNodeInfo :: GHC.NodeInfo a -> NodeInfo a
fromNodeInfo GHC.NodeInfo{..} = NodeInfo {
  nodeAnnotations = Set.map fromNodeAnnotation nodeAnnotations
, nodeType
, nodeIdentifiers = fromNodeIdentifiers nodeIdentifiers
}

fromNodeAnnotation :: GHC.NodeAnnotation -> NodeAnnotation
fromNodeAnnotation GHC.NodeAnnotation{..} = NodeAnnotation{..}

fromNodeIdentifiers :: GHC.NodeIdentifiers a -> NodeIdentifiers a
fromNodeIdentifiers = Map.map fromIdentifierDetails

fromIdentifierDetails :: GHC.IdentifierDetails a -> IdentifierDetails a
fromIdentifierDetails GHC.IdentifierDetails{..} = IdentifierDetails {
  identType
, identInfo = Set.map fromContextInfo identInfo
}

fromBindType :: GHC.BindType -> BindType
fromBindType = \ case
  GHC.RegularBind -> RegularBind
  GHC.InstanceBind -> InstanceBind

fromDeclType :: GHC.DeclType -> DeclType
fromDeclType = \ case
  GHC.FamDec -> FamDec
  GHC.SynDec -> SynDec
  GHC.DataDec -> DataDec
  GHC.InstDec -> InstDec
  GHC.PatSynDec -> PatSynDec
  GHC.ClassDec -> ClassDec
  GHC.ConDec -> ConDec

fromTyVarScope :: GHC.TyVarScope -> TyVarScope
fromTyVarScope = \case
  GHC.ResolvedScopes scopes -> ResolvedScopes $ map fromScope scopes
  GHC.UnresolvedScope a b -> UnresolvedScope a b

fromRecFieldContext :: GHC.RecFieldContext -> RecFieldContext
fromRecFieldContext = \ case
  GHC.RecFieldDecl -> RecFieldDecl
  GHC.RecFieldAssign -> RecFieldAssign
  GHC.RecFieldMatch -> RecFieldMatch
  GHC.RecFieldOcc -> RecFieldOcc

fromContextInfo :: GHC.ContextInfo -> ContextInfo
fromContextInfo = \ case
  GHC.Use -> Use
  GHC.MatchBind -> MatchBind
  GHC.IEThing iEType -> IEThing (fromIEType iEType)
  GHC.TyDecl -> TyDecl
  GHC.ValBind bindType scope span -> ValBind (fromBindType bindType) (fromScope scope) span
  GHC.PatternBind scopeA scopeB span -> PatternBind (fromScope scopeA) (fromScope scopeB) span
  GHC.ClassTyDecl span -> ClassTyDecl span
  GHC.Decl declType span -> Decl (fromDeclType declType) span
  GHC.TyVarBind scope tyVarScope -> TyVarBind (fromScope scope) (fromTyVarScope tyVarScope)
  GHC.RecField recFieldContext span -> RecField (fromRecFieldContext recFieldContext) span
  GHC.EvidenceVarBind evVarSource scope span -> EvidenceVarBind (fromEvVarSource evVarSource) (fromScope scope) span
  GHC.EvidenceVarUse -> EvidenceVarUse

fromIEType :: GHC.IEType -> IEType
fromIEType = \ case
  GHC.Import -> Import
  GHC.ImportAs -> ImportAs
  GHC.ImportHiding -> ImportHiding
  GHC.Export -> Export

fromScope :: GHC.Scope -> Scope
fromScope = \ case
  GHC.NoScope -> NoScope
  GHC.LocalScope span -> LocalScope span
  GHC.ModuleScope -> ModuleScope

fromEvVarSource :: GHC.EvVarSource -> EvVarSource
fromEvVarSource = \ case
  GHC.EvPatternBind -> EvPatternBind
  GHC.EvSigBind -> EvSigBind
  GHC.EvWrapperBind -> EvWrapperBind
  GHC.EvImplicitBind -> EvImplicitBind
  GHC.EvInstBind { isSuperInst, cls } -> EvInstBind { isSuperInst, cls }
  GHC.EvLetBind evBindDeps -> EvLetBind (fromEvBindDeps evBindDeps)

fromEvBindDeps :: GHC.EvBindDeps -> EvBindDeps
fromEvBindDeps = \ case
  GHC.EvBindDeps xs -> EvBindDeps xs
