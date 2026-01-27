----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Server.Tags.LoadFiles
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Wednesday, 12 October 2016
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskell.Language.Server.Tags.LoadFiles
  ( loadAllFilesIntoState
  ) where

import Control.Arrow ((&&&))
import Control.Monad.Except.Ext
import Control.Monad.State.Strict

import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Prettyprinter qualified as PP
import Prettyprinter.Ext

import Control.Monad.Logging
import Data.ErrorMessage
import Data.Map.NonEmpty (NonEmptyMap)
import Data.Map.NonEmpty qualified as NEMap
import Data.Path
import Haskell.Language.Server.Tags.LoadModule (resolveModule)
import Haskell.Language.Server.Tags.Types
import Haskell.Language.Server.Tags.Types.Imports
import Haskell.Language.Server.Tags.Types.Modules

{-# INLINE loadAllFilesIntoState #-}
-- | A quick way to resolve all known modules, assuming there're no other
-- modules we don't know about.
--
-- Most of the speed comes from not touching the file system.
loadAllFilesIntoState
  :: forall m. (WithCallStack, MonadError ErrorMessage m, MonadLog m, MonadState LoadState m)
  => Map ImportKey (NonEmpty UnresolvedModule)
  -> TagsServerConf
  -> m (Map ImportKey (NonEmpty ResolvedModule))
loadAllFilesIntoState unresolvedModules TagsServerConf{tsconfNameResolution} =
  flip M.traverseMaybeWithKey unresolvedModules $ \importKey _ ->
    doResolve unresolvedModules tsconfNameResolution importKey

doResolve
  :: forall m. (WithCallStack, MonadState LoadState m, MonadError ErrorMessage m, MonadLog m)
  => Map ImportKey (NonEmpty UnresolvedModule)
  -> NameResolutionStrictness
  -> ImportKey
  -> m (Maybe (NonEmpty ResolvedModule))
doResolve allKnownModules nameResolution = go
  where
    go :: ImportKey -> m (Maybe (NonEmpty ResolvedModule))
    go key = do
      resolveState <- get
      case M.lookup key $ lsLoadedModules resolveState of
        Just resolved -> pure $ Just resolved
        Nothing       -> do
          logInfo $ "[loadAllFilesIntoState.doResolve] Resolving" <+> PP.dquotes (pretty (ikModuleName key))
          let currentlyLoading = lsLoadsInProgress resolveState
          if key `M.member` currentlyLoading
          then
            throwErrorWithCallStack $ PP.hsep
              [ "[loadAllFilesIntoState.doResolve] found import loop: module"
              , PP.dquotes (pretty key)
              , "was required while being loaded"
              ]
          else
            case M.lookup key allKnownModules of
              Nothing         -> do
                let msg = PP.hsep
                      [ "[loadAllFilesIntoState.doResolve] imported module"
                      , PP.dquotes (pretty key)
                      , "not found"
                      ]
                case nameResolution of
                  NameResolutionLax -> do
                    logWarning msg
                    pure Nothing
                  NameResolutionStrict ->
                    throwErrorWithCallStack msg
              Just unresolved -> do
                let unresolvedMap :: NonEmptyMap (FullPath 'File) UnresolvedModule
                    unresolvedMap = NEMap.fromNonEmpty $ (modFile &&& id) <$> unresolved
                logDebug $ "[loadAllFilesIntoState.doResolve] currently loading:" ## ppMapWith pretty (ppNE . NEMap.keysNE) currentlyLoading
                modify $ \s ->
                  s { lsLoadsInProgress = M.insertWith NEMap.union key unresolvedMap $ lsLoadsInProgress s }
                -- logDebug $ "[loadAllFilesIntoState.doResolve] files:" ## ppNE (modFile <$> unresolved)
                resolved <- traverse (resolveModule nameResolution checkLoadingModules go) unresolved
                modify $ \s -> s
                  { lsLoadsInProgress =
                      M.update (`NEMap.difference` unresolvedMap) key $ lsLoadsInProgress s
                  , lsLoadedModules   =
                      M.insertWith (<>) key resolved $ lsLoadedModules s
                  }
                logInfo $ "[loadAllFilesIntoState.doResolve] Resolved" <+> PP.dquotes (pretty (ikModuleName key))
                pure $ Just resolved

    checkLoadingModules
      :: ImportKey
      -> m (Maybe (NonEmpty UnresolvedModule, [ResolvedModule]))
    checkLoadingModules key = do
      LoadState{lsLoadsInProgress, lsLoadedModules} <- get
      pure $ case M.lookup key lsLoadsInProgress of
        Just modules -> Just (NEMap.elemsNE modules, loadedMods)
          where
            loadedMods :: [ResolvedModule]
            loadedMods = foldMap toList $ M.lookup key lsLoadedModules
        Nothing      -> Nothing
