----------------------------------------------------------------------------
-- |
-- Module      :  Server.Tags.Search
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskell.Language.Server.Tags.Search
  ( findSymbol
  , findSymbolByRegexp
  ) where

import Prelude hiding (mod)

import Control.Arrow (second)
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Except.Ext
import Control.Monad.Reader
import Control.Monad.State
import Control.Parallel.Strategies.Ext

import Data.Foldable.Ext
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.Traversable
import Prettyprinter qualified as PP
import Prettyprinter.Ext

import Control.Monad.Filesystem (MonadFS)
import Control.Monad.Logging
import Data.CompiledRegex
import Data.ErrorMessage
import Data.Path
import Data.SubkeyMap qualified as SubkeyMap
import Data.SymbolMap (SymbolMap)
import Data.SymbolMap qualified as SM
import Data.Symbols
import Haskell.Language.Lexer.Types qualified as Types
import Haskell.Language.Server.Tags.LoadModule
import Haskell.Language.Server.Tags.Types
import Haskell.Language.Server.Tags.Types.Imports
import Haskell.Language.Server.Tags.Types.Modules

findSymbol
  :: (WithCallStack, MonadError ErrorMessage m, MonadState LoadState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m, MonadBase IO m)
  => NameResolutionScope
  -> FullPath 'File
  -> SymbolName             -- ^ Symbol to find. Can be either qualified, unqualified, ascii name/utf name/operator.
  -> m (Set ResolvedSymbol) -- ^ Found tags, may be empty when nothing was found.
findSymbol scope filename sym = do
  logVerboseDebug $
    "[findSymbol] searching for" <+> pretty sym <+> "within" <+> pretty filename
  currMod <- do
    name           <- fileNameToModuleName filename
    nameResolution <- asks tsconfNameResolution
    resolveModule nameResolution checkLoadingModules loadModule =<<
      readFileAndLoad (Just name) filename
  case scope of
    ScopeCurrentModule -> findInModule sym currMod
    ScopeAllModules    ->
      foldMapPar (SM.lookup sym') <$> gets (scopeFromAllModules currMod)
      where
        (_, sym') = splitQualifiedPart sym

findSymbolByRegexp
  :: (WithCallStack, MonadError ErrorMessage m, MonadState LoadState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => NameResolutionScope
  -> FullPath 'File
  -> CompiledRegex          -- ^ Regexp to look for.
  -> m (Set ResolvedSymbol) -- ^ Found tags, may be empty when nothing was found.
findSymbolByRegexp scope filename re = do
  logVerboseDebug $
    "[findSymbolByRegexp] searching for" <+> pretty re <+> "within" <+> pretty filename
  name           <- fileNameToModuleName filename
  nameResolution <- asks tsconfNameResolution
  currMod        <-
    resolveModule nameResolution checkLoadingModules loadModule =<<
      readFileAndLoad (Just name) filename
  (mods :: NonEmpty SymbolMap) <-
    case scope of
      ScopeCurrentModule -> do
        importNames <-
          visibleNamesFromImports
            AllNames
            (mhModName (modHeader currMod))
            (SubkeyMap.toList (mhImports (modHeader currMod)))
        pure $ modAllSymbols currMod :| importNames
      ScopeAllModules    -> gets $ scopeFromAllModules currMod
  pure $
    foldMapPar
      (S.fromList . filter (reMatches re . unqualSymNameText . resolvedSymbolName) . SM.toList)
      mods

scopeFromAllModules :: Module a -> LoadState -> NonEmpty SymbolMap
scopeFromAllModules currMod LoadState{lsLoadedModules, lsUnloadedFiles} =
  modAllSymbols currMod :| go lsLoadedModules <> go lsUnloadedFiles
  where
    currFile = modFile currMod
    go :: (Foldable f, Foldable g) => f (g (Module a)) -> [SymbolMap]
    go = foldMap (fmap modAllSymbols . filter ((/= currFile) . modFile) . toList)

foldMapPar
  :: (Foldable f, Monoid b)
  => (a -> b)
  -> f a
  -> b
foldMapPar f xs = runEval $
  foldPar =<< parTraversable rseq (f <$> toList xs)

-- | Try to find out what @sym@ refers to in the context of module @mod@.
findInModule
  :: forall m. (WithCallStack, MonadError ErrorMessage m, MonadState LoadState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => SymbolName
  -> ResolvedModule
  -> m (Set ResolvedSymbol)
findInModule sym mod = do
  logVerboseDebug $
    "[findInModule] qualifier for" <+> pretty sym <> ":" <+> pretty qualifier
  case qualifier of
    -- Unqualified name
    Nothing -> do
      let localSyms :: Set ResolvedSymbol
          localSyms = S.fromList $ lookUpInSymbolMap sym' $ modAllSymbols mod
      importedSyms <- lookUpInImportedModules
        OnlyUnqualifiedNames
        currModName
        sym'
        (SubkeyMap.toList (mhImports header))
      pure $ localSyms <> importedSyms

      -- (relevantImports :: [SymbolMap]) <-
      --   visibleNamesFromImports currModName (SubkeyMap.toList (mhImports header))
      -- logVerboseDebug $
      --   "[findInModule] relevant imports:" ## pretty relevantImports
      -- pure $ localSyms <> foldMapPar (S.fromList . lookUpInSymbolMap sym') relevantImports

    -- Qualified name
    Just qualifier' -> do
      resolvedSpecs <- resolveQualifier qualifier' header
      case resolvedSpecs of
        Nothing     ->
          throwErrorWithCallStack $ "Qualifier" <+> PP.squotes (pretty qualifier') <+>
            "not listed among module's import qualifiers:" ##
            ppMapWith pretty ppNE (mhImportQualifiers header)
        Just specs -> do
          logVerboseDebug $
            "[findInModule] resolved qualifier" <+> pretty qualifier' <+> "to modules:" ## pretty specs
          lookUpInImportedModules AllNames currModName sym' (toList specs)
  where
    qualifier :: Maybe ImportQualifier
    sym'      :: UnqualifiedSymbolName
    (qualifier, sym') = splitQualifiedPart sym
    currModName = mhModName header
    header :: ModuleHeader
    header = modHeader mod

data AllowedNamesKind = OnlyUnqualifiedNames | AllNames
  deriving (Eq, Ord, Show, Enum, Bounded)

visibleNamesFromImports
  :: forall m. (WithCallStack, MonadError ErrorMessage m, MonadState LoadState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => AllowedNamesKind
  -> ModuleName
  -> [(ImportKey, NonEmpty ImportSpec)] -- ^ Imports of a module
  -> m [SymbolMap]
visibleNamesFromImports namesToConsider currMod imports = do
  TagsServerConf{tsconfNameResolution} <- ask
  foldForA imports $ \(impKey, impSpecs) -> do
    mods <- loadModule' impKey
    let combinedNames :: SymbolMap
        combinedNames = foldMap modAllExportedNames mods
        impSpecs' :: [(ImportSpec, SymbolMap)]
        impSpecs'     = case namesToConsider of
          AllNames             -> (, combinedNames) <$> toList impSpecs
          OnlyUnqualifiedNames ->
            mapMaybe (\spec -> (spec,) <$> importBringsUnqualifiedNames combinedNames spec) $ toList impSpecs
    for impSpecs' $ \(spec, names) ->
      case visibleNamesFromImportSpec tsconfNameResolution currMod names spec of
        Left err -> throwError err
        Right x  -> pure x

lookUpInImportedModules
  :: forall m. (WithCallStack, MonadError ErrorMessage m, MonadState LoadState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => AllowedNamesKind
  -> ModuleName
  -> UnqualifiedSymbolName
  -> [(ImportKey, NonEmpty ImportSpec)]
  -> m (Set ResolvedSymbol)
lookUpInImportedModules names currModName name imports = do
  logDebug $ ppFoldableHeader
    ("[lookUpInImportedModules] searching for name" <+> pretty name <+> "in modules")
    (second (fmap (ikModuleName . ispecImportKey)) <$> imports)
  foldMapPar (S.fromList . lookUpInSymbolMap name) <$>
    visibleNamesFromImports names currModName imports
    -- traverse (loadModule' . ispecImportKey) specs

lookUpInSymbolMap :: UnqualifiedSymbolName -> SymbolMap -> [ResolvedSymbol]
lookUpInSymbolMap sym sm
  -- Just syms -> toList syms
  -- If a name refers to both constructor and type and construcutor constructs
  -- values for the type in question then
  | (redundant@(_ : _), other) <- L.partition isRedundantConstructor syms'
  , (_reallyRedundant, haveNoCorrespondingParents) <-
      let otherFiles = S.fromList $ map resolvedSymbolFile other in
      L.partition ((`S.member` otherFiles) . resolvedSymbolFile) redundant
  = haveNoCorrespondingParents ++ other
  | otherwise
  = syms'
  where
    syms = SM.lookup sym sm

    syms' = toList syms
    isRedundantConstructor :: ResolvedSymbol -> Bool
    isRedundantConstructor x =
      case (resolvedSymbolType x, resolvedSymbolParentName x) of
        (Types.Constructor, Just p) -> p == resolvedSymbolName x
        _                           -> False
