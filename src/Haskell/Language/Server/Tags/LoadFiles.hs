----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Server.Tags.LoadFiles
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Wednesday, 12 October 2016
----------------------------------------------------------------------------

{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskell.Language.Server.Tags.LoadFiles
  ( loadAllFilesIntoState
  ) where

import Prelude hiding (readFile)

import Control.Arrow ((&&&))
import Control.Monad.Catch
import Control.Monad.Except.Ext
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
        :: forall m. (MonadState ResolveState m)
        => ImportKey
        -> m (Maybe (NonEmpty UnresolvedModule))
      checkLoadingModules key = do
        s <- get
        pure $ NEMap.elemsNE <$> M.lookup key (rsLoadingModules s)

      doResolve
        :: forall m. (HasCallStack, MonadState ResolveState m, MonadError ErrorMessage m, MonadLog m)
        => ImportKey
        -> m (NonEmpty ResolvedModule)
      doResolve key = do
        logInfo $ "[loadAllFilesIntoState.doResolve] Resolving" <+> PP.dquotes (pretty key)
        state <- get
        case M.lookup key $ rsLoadedModules state of
          Just resolved -> pure resolved
          Nothing       -> do
            let currentlyLoading = rsLoadingModules state
            if M.member key currentlyLoading
            then
              throwErrorWithCallStack $ PP.hsep
                [ "[loadAllFilesIntoState.doResolve] found import loop: module"
                , PP.dquotes (pretty key)
                , "was required while being loaded"
                ]
            else
              case M.lookup key unresolvedModulesMap of
                Nothing         ->
                  throwErrorWithCallStack $ PP.hsep
                    [ "[loadAllFilesIntoState.doResolve] imported module"
                    , PP.dquotes (pretty key)
                    , "not found"
                    ]
                Just unresolved -> do
                  let unresolvedMap = NEMap.fromNonEmpty $ (modFile &&& id) <$> unresolved
                  logDebug $ "[loadAllFilesIntoState.doResolve] currentlyLoading =" <+> ppMapWith pretty (ppNE . NEMap.keysNE) currentlyLoading
                  modify $ \s ->
                    s { rsLoadingModules = M.insert key unresolvedMap $ rsLoadingModules s }
                  logInfo $ "[loadAllFilesIntoState.doResolve] files: " <+> ppNE (modFile <$> unresolved)
                  resolved <- traverse (resolveModule checkLoadingModules doResolve) unresolved
                  modify $ \s -> s
                    { rsLoadingModules =
                        M.delete key $ rsLoadingModules s
                    , rsLoadedModules  =
                        M.insertWith (Semigroup.<>) key resolved $ rsLoadedModules s
                    }
                  logInfo $ "[loadAllFilesIntoState.doResolve] resolved" <+> PP.dquotes (pretty key)
                  pure resolved

  flip evalStateT initState $
    flip M.traverseWithKey unresolvedModulesMap $ \importKey _ ->
      doResolve importKey


