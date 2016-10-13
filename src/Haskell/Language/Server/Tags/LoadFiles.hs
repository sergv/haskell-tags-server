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
import Data.Set (Set)
import qualified Data.Set as S
import Data.Traversable
import System.FilePath
import Text.PrettyPrint.Leijen.Text (Doc, (<+>), pretty)

import Control.Monad.Filesystem (MonadFS)
import qualified Control.Monad.Filesystem as MonadFS
import Control.Monad.Logging
import Haskell.Language.Server.Tags.LoadModule (loadModuleFromSource, resolveModuleExports, vanillaExtensions, hsBootExtensions)
import Haskell.Language.Server.Tags.Types

classifyPath :: FilePath -> Maybe (ImportTarget, FilePath)
classifyPath path
  | ext `S.member` vanillaExtensions = Just (VanillaModule, path)
  | ext `S.member` hsBootExtensions  = Just (HsBootModule, path)
  | otherwise                        = Nothing
  where
    ext = takeExtension path

data ResolveState = ResolveState
  { rsLoadingModules :: !(Set ImportKey)
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
      doResolve
        :: forall m. (MonadState ResolveState m, MonadError Doc m, MonadLog m)
        => ImportKey
        -> m (NonEmpty ResolvedModule)
      doResolve k = do
        currentlyLoading <- gets rsLoadingModules
        if S.member k currentlyLoading
        then
          throwError $ "[loadAllFilesIntoState.doResolve] found import loop: module" <+> pretty k <+>
            "was required while being loaded"
        else do
          modify $ \s -> s { rsLoadingModules = S.insert k $ rsLoadingModules s }
          case M.lookup k unresolvedModulesMap of
            Nothing         ->
              throwError $ "[loadAllFilesIntoState.doResolve] imported module" <+> pretty k <+> "not found"
            Just unresolved -> do
              resolved <- traverse (resolveModuleExports doResolve) unresolved
              modify $ \s -> s
                 { rsLoadingModules =
                     S.delete k $ rsLoadingModules s
                 , rsLoadedModules  =
                     M.insertWith (Semigroup.<>) k resolved $ rsLoadedModules s
                 }
              pure resolved

  flip evalStateT initState $
    flip M.traverseWithKey unresolvedModulesMap $ \importKey _ ->
      doResolve importKey


