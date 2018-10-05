----------------------------------------------------------------------------
-- |
-- Module      :  Server.Tags
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Monday, 15 August 2016
-- The actual server that handles tags
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskell.Language.Server.Tags
  ( startTagsServer
  , stopTagsServer
  , waitForTagsServerFinish
  , TagsServer(tsRequestHandler)
  , RequestHandler
  , TagsServerConf(..)
  , defaultTagsServerConf
  , TagsServerState(..)
  , emptyTagsServerState
  ) where

import Control.Concurrent
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except.Ext
import Control.Monad.Trans.Control

import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as M
import Data.Semigroup.Foldable
import qualified Data.Set as S
import Data.Text.Prettyprint.Doc.Ext (Pretty(..), (##))

import qualified Data.Promise as Promise

import Control.Monad.Filesystem (MonadFS(..), SearchCfg(..))
import qualified Control.Monad.Filesystem as MonadFS
import Control.Monad.Logging
import Data.CompiledRegex
import Data.ErrorMessage
import Data.Map.NonEmpty (NonEmptyMap)
import qualified Data.Map.NonEmpty as NEMap
import Data.Path
import Data.Symbols (fileNameToModuleName)
import Haskell.Language.Server.Tags.LoadFiles
import Haskell.Language.Server.Tags.LoadModule
import Haskell.Language.Server.Tags.Search
import Haskell.Language.Server.Tags.SearchM
import Haskell.Language.Server.Tags.Types
import Haskell.Language.Server.Tags.Types.Imports
import Haskell.Language.Server.Tags.Types.Modules

data TagsServer = TagsServer
  { -- | The way to communicate with running tags server: send requests and
    -- get promises of responses.
    tsRequestHandler :: RequestHandler
    -- | Lock that becomes available when server exits.
  , tsFinishState    :: MVar TagsServerState
    -- | Id of the requent serving thread.
  , tsThreadId       :: !ThreadId
  }

stopTagsServer :: MonadBase IO m => TagsServer -> m ()
stopTagsServer = liftBase . killThread . tsThreadId

-- | Block until tags server stops.
waitForTagsServerFinish :: MonadBase IO m => TagsServer -> m TagsServerState
waitForTagsServerFinish = liftBase . readMVar . tsFinishState

preloadFiles
  :: (WithCallStack, MonadError ErrorMessage m, MonadLog m, MonadFS m)
  => SearchCfg
  -> TagsServerConf
  -> TagsServerState
  -> m TagsServerState
preloadFiles searchCfg@SearchCfg{scIgnoredGlobs} conf initState = do
  ignoredGlobsRE <- fileGlobsToRegex (scIgnoredGlobs <> MonadFS.defaultInoredGlobs)
  knownFiles     <- MonadFS.findRec searchCfg ignoredGlobsRE (loadMod conf)
  if tsconfEagerTagging conf
  then do
    logInfo "[preloadFiles] collecting tags eagerly..."
    initState' <- loadAllFilesIntoState (fold1 . NEMap.elemsNE <$> knownFiles) conf initState
    logInfo "[preloadFiles] collecting tags eagerly... OK"
    pure initState'
  else pure initState
    { tssUnloadedFiles =
      M.unionWith (<>) (fold1 . NEMap.elemsNE <$> knownFiles) (tssUnloadedFiles initState)
    }

-- | Start new tags server thread that will serve requests supplied via returned
-- RequestHandler.
startTagsServer
  :: forall m. (WithCallStack, MonadBase IO m, MonadBaseControl IO m, MonadCatch m, MonadError ErrorMessage m, MonadLog m, MonadFS m, MonadMask m)
  => SearchCfg
  -> TagsServerConf
  -> m TagsServer
startTagsServer searchCfg conf = do
  initState <- preloadFiles searchCfg conf emptyTagsServerState
  reqChan   <- liftBase newChan
  lock      <- liftBase newEmptyMVar
  tid       <- liftBaseDiscard forkIO $ handleRequests lock reqChan initState
  let tsRequestHandler :: RequestHandler
      tsRequestHandler = \case
        req@QueryReq{} -> do
          respPromise <- Promise.newPromise
          writeChan reqChan (SomeRequest (UserReq req) respPromise)
          pure respPromise
        req@DirReq{} -> do
          respPromise <- Promise.newPromise
          writeChan reqChan (SomeRequest (UserReq req) respPromise)
          pure respPromise
  pure TagsServer
    { tsRequestHandler
    , tsFinishState = lock
    , tsThreadId    = tid
    }
  where
    handleRequests
      :: MVar TagsServerState
      -> Chan SomeRequest
      -> TagsServerState
      -> m ()
    handleRequests lock reqChan = go
      where
        go s = go =<< (handleReq reqChan s `onException` liftBase (putMVar lock s))
    handleReq
      :: Chan SomeRequest
      -> TagsServerState
      -> m TagsServerState
    handleReq reqChan serverState = do
      req <- liftBase $ readChan reqChan
      case req of
        SomeRequest FSNotifyReq{} () ->
          undefined
        SomeRequest (UserReq request) respPromise -> do
          -- (request, responsePromise) <- liftBase $ readChan reqChan
          logInfo $ "[startTagsServer.handleReq] request:" ## pretty request
          case request of
            QueryReq filename request' -> do
              ensureFileExists filename
              (response, serverState') <- runSearchT conf serverState $
                case request' of
                  FindSymbol symbol -> do
                    symbols <- findSymbol id filename symbol
                    pure $ case toList symbols of
                      []   -> NotFound
                      s:ss -> Found $ s :| ss
                  FindSymbolByRegex scope regexp -> do
                    symbols <- findSymbolByRegexp id scope filename regexp
                    pure $ case toList symbols of
                      []   -> NotFound
                      s:ss -> Found $ s :| ss
              logInfo $ "[startTagsServer.handleReq] response:" ## either pretty pretty response
              Promise.putValue respPromise response
              pure serverState'
            DirReq (AddShallowRecursiveIgnored scShallowPaths scRecursivePaths scIgnoredGlobs) -> do
              let searchCfg' = SearchCfg
                    { scShallowPaths
                    , scRecursivePaths
                    , scIgnoredDirs = scIgnoredDirs searchCfg
                    , scIgnoredGlobs
                    }
              serverState' <- preloadFiles searchCfg' conf serverState
              Promise.putValue respPromise (Right ())
              pure serverState'

ensureFileExists
  :: (WithCallStack, MonadFS m, MonadError ErrorMessage m)
  => FullPath 'File -> m ()
ensureFileExists _path = -- do
  -- NB FullPath is guaranteed to exist by construction.
  pure ()
  -- exists <- doesFileExist path
  -- unless exists $
  --   throwErrorWithCallStack $ "Error: file" <+> PP.dquotes (pretty path) <+> "does not exist"

-- todo: handle header files here
classifyPath :: TagsServerConf -> FullPath 'File -> Maybe ImportTarget
classifyPath TagsServerConf{tsconfVanillaExtensions, tsconfHsBootExtensions} path
  | ext `S.member` tsconfVanillaExtensions = Just VanillaModule
  | ext `S.member` tsconfHsBootExtensions  = Just HsBootModule
  | otherwise                              = Nothing
  where
    ext  = takeExtension path

loadMod
  :: (MonadFS m, MonadError ErrorMessage m, MonadLog m)
  => TagsServerConf
  -> FullPath 'File
  -> m (Maybe (ImportKey, NonEmptyMap (FullPath 'File) (NonEmpty UnresolvedModule)))
loadMod conf filename =
  case classifyPath conf filename of
    Nothing         -> pure Nothing
    Just importType -> do
      modTime       <- MonadFS.getModificationTime filename
      suggestedName <- fileNameToModuleName filename
      unresolvedMod@Module{modHeader = ModuleHeader{mhModName}} <-
        readFileAndLoad (Just suggestedName) modTime filename
      unresolvedMod `seq` pure (Just (ImportKey importType mhModName, NEMap.singleton filename (unresolvedMod :| [])))

