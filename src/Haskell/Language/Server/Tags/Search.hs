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
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskell.Language.Server.Tags.Search (findSymbol) where

import Prelude hiding (mod)

import Control.Monad.Except.Ext
import Control.Monad.Reader
import Control.Monad.State

import Data.Foldable.Ext (toList, foldMapA, foldForA)
import qualified Data.List as L
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Ext

import Control.Monad.Filesystem (MonadFS)
import qualified Control.Monad.Filesystem as MonadFS
import Control.Monad.Logging
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
  :: (WithCallStack, MonadError ErrorMessage m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => (MonadFS n, MonadError ErrorMessage n, MonadLog n)
  => (forall a. n a -> m a)
  -> FullPath 'File         -- ^ File name
  -> SymbolName             -- ^ Symbol to find. Can be either qualified, unqualified, ascii name/utf name/operator
  -> m (Set ResolvedSymbol) -- ^ Found tags, may be empty when nothing was found.
findSymbol liftN filename sym = do
  logVerboseDebug $
    "[findSymbol] searching for" <+> pretty sym <+> "within" <+> pretty filename
  modifTime <- MonadFS.getModificationTime filename
  name      <- fileNameToModuleName filename
  findInModule liftN sym =<<
    resolveModule checkLoadingModules (loadModule liftN) =<<
      readFileAndLoad (Just name) modifTime filename

-- | Try to find out what @sym@ refers to in the context of module @mod@.
findInModule
  :: forall m n. (WithCallStack, MonadError ErrorMessage m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => (MonadFS n, MonadError ErrorMessage n, MonadLog n)
  => (forall a. n a -> m a)
  -> SymbolName
  -> ResolvedModule
  -> m (Set ResolvedSymbol)
findInModule liftN sym mod = do
  logVerboseDebug $
    "[findSymbol] qualifier for" <+> pretty sym <> ":" <+> pretty qualifier
  case qualifier of
    -- Unqualified name
    Nothing -> do
      let localSyms :: Set ResolvedSymbol
          localSyms       = S.fromList $ lookUpInSymbolMap sym' $ modAllSymbols mod
          relevantImports = filter importBringsUnqualifiedNames
                          $ foldMap toList
                          $ mhImports header
      (localSyms <>) <$> lookUpInImportedModules liftN sym' relevantImports
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
          lookUpInImportedModules liftN sym' specs
  where
    qualifier :: Maybe ImportQualifier
    sym'      :: UnqualifiedSymbolName
    (qualifier, sym') = splitQualifiedPart sym
    header :: ResolvedModuleHeader
    header = modHeader mod

lookUpInImportedModules
  :: forall m n f. (WithCallStack, MonadError ErrorMessage m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => (MonadFS n, MonadError ErrorMessage n, MonadLog n)
  => (Functor f, Foldable f)
  => (forall a. n a -> m a)
  -> UnqualifiedSymbolName
  -> f ResolvedImportSpec
  -> m (Set ResolvedSymbol)
lookUpInImportedModules liftN name specs = do
  logDebug $ ppFoldableHeader
    ("[lookUpInImportedModules] searching for name" <+> pretty name <+> "in modules")
    (ikModuleName . ispecImportKey <$> specs)
  foldForA specs $ \ImportSpec{ispecImportKey, ispecImportedNames} -> do
    let nameVisible = SM.member name ispecImportedNames
    if nameVisible
    then do
      mods <- loadModule liftN ispecImportKey
      fmap S.fromList $ foldForA mods $ foldMapA $ \mod -> do
        logDebug $ "[lookUpInImportedModules] searching for name" <+> pretty name <+> "in module" <+> pretty (ikModuleName ispecImportKey)
        lookUpInImportedModule name mod
    else pure mempty

lookUpInImportedModule
  :: forall m. (WithCallStack, MonadError ErrorMessage m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => UnqualifiedSymbolName
  -> ResolvedModule
  -> m [ResolvedSymbol]
lookUpInImportedModule name importedMod =
  pure $ lookUpInSymbolMap name $ modAllExportedNames importedMod

lookUpInSymbolMap :: UnqualifiedSymbolName -> SymbolMap -> [ResolvedSymbol]
lookUpInSymbolMap sym sm =
  -- If a name refers to both constructor and type and construcutor constructs
  -- values for the type in question then
  case SM.lookup sym sm of
    Nothing -> []
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
