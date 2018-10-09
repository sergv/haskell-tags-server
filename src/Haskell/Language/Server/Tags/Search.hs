----------------------------------------------------------------------------
-- |
-- Module      :  Server.Tags.Search
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskell.Language.Server.Tags.Search
  ( findSymbol
  , findSymbolByRegexp
  ) where

import Prelude hiding (mod)

import Control.Monad.Base
import Control.Monad.Except.Ext
import Control.Monad.Reader
import Control.Monad.State
import Control.Parallel.Strategies.Ext

import Data.Foldable.Ext (toList)
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Ext

import Control.Monad.Filesystem (MonadFS)
import qualified Control.Monad.Filesystem as MonadFS
import Control.Monad.Logging
import Data.CompiledRegex
import Data.ErrorMessage
import Data.Path
import Data.SymbolMap (SymbolMap)
import qualified Data.SymbolMap as SM
import Data.Symbols
import qualified Haskell.Language.Lexer.FastTags as FastTags
import Haskell.Language.Server.Tags.LoadModule
import Haskell.Language.Server.Tags.Types
import Haskell.Language.Server.Tags.Types.Imports
import Haskell.Language.Server.Tags.Types.Modules

findSymbol
  :: (WithCallStack, MonadError ErrorMessage m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m, MonadBase IO m)
  => (MonadFS n, MonadError ErrorMessage n, MonadLog n)
  => (forall a. n a -> m a)
  -> NameResolutionScope
  -> FullPath 'File
  -> SymbolName             -- ^ Symbol to find. Can be either qualified, unqualified, ascii name/utf name/operator.
  -> m (Set ResolvedSymbol) -- ^ Found tags, may be empty when nothing was found.
findSymbol liftN scope filename sym = do
  logVerboseDebug $
    "[findSymbol] searching for" <+> pretty sym <+> "within" <+> pretty filename
  currMod <- do
    modifTime <- MonadFS.getModificationTime filename
    name      <- fileNameToModuleName filename
    resolveModule checkLoadingModules (loadModule liftN) =<<
      readFileAndLoad (Just name) modifTime filename
  case scope of
    ScopeCurrentModule -> findInModule sym currMod
    ScopeAllModules    ->
      foldMapPar (foldMap (S.fromList . toList) . SM.lookup sym') <$> gets (scopeFromAllModules currMod)
      where
        (_, sym') = splitQualifiedPart sym

findSymbolByRegexp
  :: (WithCallStack, MonadError ErrorMessage m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => (MonadFS n, MonadError ErrorMessage n, MonadLog n)
  => (forall a. n a -> m a)
  -> NameResolutionScope
  -> FullPath 'File
  -> CompiledRegex          -- ^ Regexp to look for.
  -> m (Set ResolvedSymbol) -- ^ Found tags, may be empty when nothing was found.
findSymbolByRegexp liftN scope filename re = do
  logVerboseDebug $
    "[findSymbolByRegexp] searching for" <+> pretty re <+> "within" <+> pretty filename
  modifTime <- MonadFS.getModificationTime filename
  name      <- fileNameToModuleName filename
  currMod   <-
    resolveModule checkLoadingModules (loadModule liftN) =<<
    readFileAndLoad (Just name) modifTime filename
  (mods :: NonEmpty SymbolMap) <-
    case scope of
      ScopeCurrentModule -> pure $
        modAllSymbols currMod :| foldMap (fmap ispecImportedNames . toList) (mhImports (modHeader currMod))
      ScopeAllModules    -> gets $ scopeFromAllModules currMod
  pure $
    foldMapPar
      (S.fromList . filter (reMatches re . unqualSymNameText . resolvedSymbolName) . SM.toList)
      mods

scopeFromAllModules :: Module a -> TagsServerState -> NonEmpty SymbolMap
scopeFromAllModules currMod TagsServerState{tssLoadedModules, tssUnloadedFiles} =
  modAllSymbols currMod :| go tssLoadedModules <> go tssUnloadedFiles
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
  :: forall m. (WithCallStack, MonadError ErrorMessage m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => SymbolName
  -> ResolvedModule
  -> m (Set ResolvedSymbol)
findInModule sym mod = do
  logVerboseDebug $
    "[findSymbol] qualifier for" <+> pretty sym <> ":" <+> pretty qualifier
  case qualifier of
    -- Unqualified name
    Nothing -> do
      let localSyms :: Set ResolvedSymbol
          localSyms       = S.fromList $ lookUpInSymbolMap sym' $ modAllSymbols mod
          relevantImports :: [SymbolMap]
          relevantImports = mapMaybe importBringsUnqualifiedNames
                          $ foldMap toList
                          $ mhImports header
      logVerboseDebug $
        "[findInModule] relevant imports:" ## pretty relevantImports
      pure $ localSyms <> foldMapPar (S.fromList . lookUpInSymbolMap sym') relevantImports
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
            "[lookUpInImportedModules] resolved qualifier" <+> pretty qualifier' <+> "to modules:" ## pretty specs
          lookUpInImportedModules sym' specs
  where
    qualifier :: Maybe ImportQualifier
    sym'      :: UnqualifiedSymbolName
    (qualifier, sym') = splitQualifiedPart sym
    header :: ResolvedModuleHeader
    header = modHeader mod

lookUpInImportedModules
  :: forall m f. (WithCallStack, MonadError ErrorMessage m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => (Functor f, Foldable f)
  => UnqualifiedSymbolName
  -> f ResolvedImportSpec
  -> m (Set ResolvedSymbol)
lookUpInImportedModules name specs = do
  logDebug $ ppFoldableHeader
    ("[lookUpInImportedModules] searching for name" <+> pretty name <+> "in modules")
    (ikModuleName . ispecImportKey <$> specs)
  pure $ foldMapPar (S.fromList . lookUpInSymbolMap name . ispecImportedNames) specs

lookUpInSymbolMap :: UnqualifiedSymbolName -> SymbolMap -> [ResolvedSymbol]
lookUpInSymbolMap sym sm =
  case SM.lookup sym sm of
    Nothing -> []
    -- Just syms -> toList syms
    -- If a name refers to both constructor and type and construcutor constructs
    -- values for the type in question then
    Just syms
      | (redundant@(_ : _), other) <- L.partition isRedundantConstructor syms'
      , (_reallyRedundant, haveNoCorrespondingParents) <-
          let otherFiles = S.fromList $ map resolvedSymbolFile other in
          L.partition ((`S.member` otherFiles) . resolvedSymbolFile) redundant
      -> haveNoCorrespondingParents ++ other
      | otherwise
      -> syms'
      where
        syms' = toList syms
        isRedundantConstructor :: ResolvedSymbol -> Bool
        isRedundantConstructor x =
          case (resolvedSymbolType x, resolvedSymbolParent x) of
            (FastTags.Constructor, Just p) -> p == resolvedSymbolName x
            _                              -> False
