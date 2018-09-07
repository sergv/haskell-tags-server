----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Server.Tags.LoadFiles
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Wednesday, 12 October 2016
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskell.Language.Server.Tags.LoadFiles
  ( loadAllFilesIntoState
  ) where

import Prelude hiding (mod, readFile)

import Control.Arrow ((&&&))
import Control.Monad.Catch
import Control.Monad.Except.Ext
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Semigroup as Semigroup
import qualified Data.Set as S
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Ext
import Data.Traversable

import Control.Monad.Filesystem (MonadFS)
import qualified Control.Monad.Filesystem as MonadFS
import Control.Monad.Filesystem.FileSearch
import Control.Monad.Logging
import Data.ErrorMessage
import Data.Map.NonEmpty (NonEmptyMap)
import qualified Data.Map.NonEmpty as NEMap
import Data.Path
import Haskell.Language.Server.Tags.LoadModule (loadModuleFromSource, resolveModule)
import Haskell.Language.Server.Tags.Types
import Haskell.Language.Server.Tags.Types.Imports
import Haskell.Language.Server.Tags.Types.Modules

-- todo: handle header files here
classifyPath :: TagsServerConf -> FindEntry -> Maybe (ImportTarget, FullPath)
classifyPath TagsServerConf{tsconfVanillaExtensions, tsconfHsBootExtensions} entry
  | ext `S.member` tsconfVanillaExtensions = Just (VanillaModule, path)
  | ext `S.member` tsconfHsBootExtensions  = Just (HsBootModule, path)
  | otherwise                              = Nothing
  where
    path = findEntryFullPath entry
    ext  = bpExtension $ findEntryBasePath entry

data ResolveState = ResolveState
  { rsLoadingModules :: !(Map ImportKey (NonEmptyMap FullPath UnresolvedModule))
  , rsLoadedModules  :: !(Map ImportKey (NonEmpty ResolvedModule))
  }

loadAllFilesIntoState
  :: forall m. (HasCallStack, MonadCatch m, MonadError ErrorMessage m, MonadLog m, MonadFS m)
  => TagsServerConf
  -> m (Map ImportKey (NonEmpty ResolvedModule))
loadAllFilesIntoState conf = do
  allFiles <- runFileSearchT (tsconfSearchDirs conf) $ findRec (classifyPath conf)

  unresolvedModules <- for allFiles $ \(importType, filename) -> do
    modTime       <- MonadFS.getModificationTime filename
    source        <- MonadFS.readFile filename
    logInfo $ "[loadAllFilesIntoState] Loading" <+> PP.dquotes (pretty filename)
    unresolvedMod <- loadModuleFromSource Nothing modTime filename source
    unresolvedMod `seq` pure (importType, unresolvedMod)
  let mkImportKey :: (ImportTarget, UnresolvedModule) -> ImportKey
      mkImportKey (target, mod) = ImportKey
        { ikImportTarget = target
        , ikModuleName   = mhModName $ modHeader mod
        }
      unresolvedModulesMap :: Map ImportKey (NonEmpty UnresolvedModule)
      unresolvedModulesMap = M.fromListWith (Semigroup.<>)
                           $ map (mkImportKey &&& ((:| []) . snd)) unresolvedModules
      initState :: ResolveState
      initState = ResolveState
        { rsLoadingModules = mempty
        , rsLoadedModules  = mempty
        }
      checkLoadingModules
        :: forall n. MonadState ResolveState n
        => ImportKey
        -> n (Maybe (NonEmpty UnresolvedModule))
      checkLoadingModules key = do
        s <- get
        pure $ NEMap.elemsNE <$> M.lookup key (rsLoadingModules s)

      doResolve
        :: forall n. (HasCallStack, MonadState ResolveState n, MonadError ErrorMessage n, MonadLog n)
        => ImportKey
        -> n (Maybe (NonEmpty ResolvedModule))
      doResolve key = do
        resolveState <- get
        case M.lookup key $ rsLoadedModules resolveState of
          Just resolved -> pure $ Just resolved
          Nothing       -> do
            logInfo $ "[loadAllFilesIntoState.doResolve] Resolving" <+> PP.dquotes (pretty key)
            let currentlyLoading = rsLoadingModules resolveState
            if key `M.member` currentlyLoading
            then
              throwErrorWithCallStack $ PP.hsep
                [ "[loadAllFilesIntoState.doResolve] found import loop: module"
                , PP.dquotes (pretty key)
                , "was required while being loaded"
                ]
            else
              case M.lookup key unresolvedModulesMap of
                Nothing         -> do
                  let msg = PP.hsep
                        [ "[loadAllFilesIntoState.doResolve] imported module"
                        , PP.dquotes (pretty key)
                        , "not found"
                        ]
                  case tsconfNameResolution conf of
                    NameResolutionLax -> do
                      logWarning msg
                      pure Nothing
                    NameResolutionStrict -> throwErrorWithCallStack msg
                Just unresolved -> do
                  let unresolvedMap = NEMap.fromNonEmpty $ (modFile &&& id) <$> unresolved
                  logDebug $ "[loadAllFilesIntoState.doResolve] currently loading:" ## ppMapWith pretty (ppNE . NEMap.keysNE) currentlyLoading
                  modify $ \s ->
                    s { rsLoadingModules = M.insert key unresolvedMap $ rsLoadingModules s }
                  -- logInfo $ "[loadAllFilesIntoState.doResolve] files:" ## ppNE (modFile <$> unresolved)
                  resolved <- flip runReaderT conf $
                    traverse (resolveModule checkLoadingModules doResolve) unresolved
                  modify $ \s -> s
                    { rsLoadingModules =
                        M.delete key $ rsLoadingModules s
                    , rsLoadedModules  =
                        M.insertWith (Semigroup.<>) key resolved $ rsLoadedModules s
                    }
                  logInfo $ "[loadAllFilesIntoState.doResolve] resolved" <+> PP.dquotes (pretty key)
                  pure $ Just resolved

  flip evalStateT initState $
    flip M.traverseMaybeWithKey unresolvedModulesMap $ \importKey _ ->
      doResolve importKey
