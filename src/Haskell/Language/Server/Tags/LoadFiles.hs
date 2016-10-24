----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Server.Tags.LoadFiles
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Wednesday, 12 October 2016
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskell.Language.Server.Tags.LoadFiles
  ( loadAllFilesIntoState
  ) where

import Prelude hiding (readFile)

import Control.Arrow ((&&&))
import Control.Arrow.Ext
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Foldable.Ext
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Semigroup as Semigroup
import qualified Data.Set as S
import Data.Traversable
import System.FilePath
import qualified Text.PrettyPrint.Leijen.Text as PP
import Text.PrettyPrint.Leijen.Text.Utils

import Control.Monad.Filesystem (MonadFS)
import qualified Control.Monad.Filesystem as MonadFS
import Control.Monad.Logging
import Data.Map.NonEmpty (NonEmptyMap)
import qualified Data.Map.NonEmpty as NEMap
import Haskell.Language.Server.Tags.LoadModule (loadModuleFromSource, resolveModule, vanillaExtensions, hsBootExtensions)
import Haskell.Language.Server.Tags.Types

classifyPath :: FilePath -> Maybe (ImportTarget, FilePath)
classifyPath path
  | ext `S.member` vanillaExtensions = Just (VanillaModule, path)
  | ext `S.member` hsBootExtensions  = Just (HsBootModule, path)
  | otherwise                        = Nothing
  where
    ext = takeExtension path

data ResolveState = ResolveState
  { rsLoadingModules :: !(Map ImportKey (NonEmptyMap FilePath UnresolvedModule))
  , rsLoadedModules  :: !(Map ImportKey (NonEmpty ResolvedModule))
  }

loadAllFilesIntoState
  :: forall m. (MonadCatch m, MonadError Doc m, MonadLog m, MonadFS m)
  => TagsServerConf
  -> m (Map ImportKey (NonEmpty ResolvedModule))
loadAllFilesIntoState conf = do
  allFiles <- flip foldMapA (tsconfRecursiveSourceDirectories conf <> tsconfSourceDirectories conf) $
    traverse (secondM MonadFS.canonicalizePath) <=< MonadFS.findRec MonadFS.isNotIgnoredDir classifyPath
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
        :: forall m. (MonadState ResolveState m, MonadError Doc m, MonadLog m)
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
              throwError $ "[loadAllFilesIntoState.doResolve] found import loop: module" <+> PP.dquotes (pretty key) <+>
                "was required while being loaded"
            else
              case M.lookup key unresolvedModulesMap of
                Nothing         ->
                  throwError $ "[loadAllFilesIntoState.doResolve] imported module" <+> PP.dquotes (pretty key) <+> "not found"
                Just unresolved -> do
                  let unresolvedMap = NEMap.fromNonEmpty $ (modFile &&& id) <$> unresolved
                  logDebug $ "[loadAllFilesIntoState.doResolve] currentlyLoading =" <+> ppMap (ppNE . NEMap.keysNE <$> currentlyLoading)
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


