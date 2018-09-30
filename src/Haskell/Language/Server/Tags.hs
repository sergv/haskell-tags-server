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
{-# LANGUAGE FlexibleContexts    #-}
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

import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup.Foldable
import qualified Data.Set as S
import Data.Text.Prettyprint.Doc.Ext (Pretty(..), (##))

import Data.Promise (Promise)
import qualified Data.Promise as Promise

import Control.Monad.Filesystem (MonadFS(..))
import qualified Control.Monad.Filesystem as MonadFS
import Control.Monad.Logging
import Data.ErrorMessage
import Data.Map.NonEmpty (NonEmptyMap)
import qualified Data.Map.NonEmpty as NEMap
import Data.Path
import qualified Data.SubkeyMap as SubkeyMap
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

-- | Start new tags server thread that will serve requests supplied via returned
-- RequestHandler.
startTagsServer
  :: forall m. (WithCallStack, MonadBase IO m, MonadBaseControl IO m, MonadCatch m, MonadError ErrorMessage m, MonadLog m, MonadFS m, MonadMask m)
  => TagsServerConf
  -> m TagsServer
startTagsServer conf = do
  knownFiles <- MonadFS.findRec (tsconfSearchDirs conf) (loadMod conf)
  state' <-
    if tsconfEagerTagging conf
    then do
      logInfo "[startTagsServer] collecting tags eagerly..."
      tssLoadedModules <- SubkeyMap.fromMap <$> loadAllFilesIntoState (fold1 . NEMap.elemsNE <$> knownFiles) conf
      logInfo "[startTagsServer] collecting tags eagerly... OK"
      pure TagsServerState
        { tssLoadedModules
        , tssLoadsInProgress = mempty
        , tssUnloadedFiles   = mempty
        }
    else pure TagsServerState
      { tssLoadedModules   = mempty
      , tssLoadsInProgress = mempty
      , tssUnloadedFiles   = fold1 . NEMap.elemsNE <$> knownFiles
      }
  reqChan <- liftBase newChan
  lock    <- liftBase newEmptyMVar
  tid     <- liftBaseDiscard forkIO $ handleRequests lock reqChan state'
  let requestHandler req = do
        respPromise <- Promise.newPromise
        writeChan reqChan (req, respPromise)
        pure respPromise
  pure TagsServer
    { tsRequestHandler = requestHandler
    , tsFinishState    = lock
    , tsThreadId       = tid
    }
  where
    handleRequests
      :: MVar TagsServerState
      -> Chan (Request, Promise (Either ErrorMessage Response))
      -> TagsServerState
      -> m ()
    handleRequests lock reqChan = go
      where
        go s =
          go =<< (handleReq reqChan s `onException` liftBase (putMVar lock s))
    handleReq
      :: Chan (Request, Promise (Either ErrorMessage Response))
      -> TagsServerState
      -> m TagsServerState
    handleReq reqChan serverState = do
      (request, responsePromise) <- liftBase $ readChan reqChan
      logInfo $ "[startTagsServer.handleReq] request:" ## pretty request
      (response, state') <- runSearchT conf serverState $
        case request of
          FindSymbol filename symbol -> do
            ensureFileExists filename
            symbols <- findSymbol id filename symbol
            case symbols of
              []   -> pure $ NotFound symbol
              s:ss -> pure $ Found $ s :| ss
          FindSymbolByRegexp filename _ -> do
            ensureFileExists filename
            throwErrorWithCallStack "Search by regexp is not implemented yet"
      logInfo $ "[startTagsServer.handleReq] response:" ## either pretty pretty response
      Promise.putValue responsePromise response
      pure state'

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

