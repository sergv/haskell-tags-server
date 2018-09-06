----------------------------------------------------------------------------
-- |
-- Module      :  Server.Tags
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Monday, 15 August 2016
-- The actual server that handles tags
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
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
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except.Ext
import Control.Monad.Trans.Control
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup
import Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Ext (Pretty(..), (<+>), (##))

import Data.Promise (Promise)
import qualified Data.Promise as Promise

import Control.Monad.Filesystem (MonadFS(..))
import Control.Monad.Logging
import Data.ErrorMessage
import Data.Path (FullPath)
import qualified Data.SubkeyMap as SubkeyMap
import Haskell.Language.Server.Tags.LoadFiles
import Haskell.Language.Server.Tags.Search
import Haskell.Language.Server.Tags.SearchM
import Haskell.Language.Server.Tags.Types

data TagsServer = TagsServer
  { tsRequestHandler :: RequestHandler
    -- | Lock that becomes available when server exits.
  , tsFinishState    :: MVar TagsServerState
    -- | Id of thread serving requests.
  , tsThreadId       :: ThreadId
  }

stopTagsServer :: MonadBase IO m => TagsServer -> m ()
stopTagsServer = liftBase . killThread . tsThreadId

-- | Block until tags server stops.
waitForTagsServerFinish :: MonadBase IO m => TagsServer -> m TagsServerState
waitForTagsServerFinish = liftBase . readMVar . tsFinishState

-- | Start new tags server thread that will serve requests supplied via returned
-- RequestHandler.
startTagsServer
  :: forall m. (HasCallStack, MonadBase IO m, MonadBaseControl IO m, MonadCatch m, MonadError ErrorMessage m, MonadLog m, MonadFS m)
  => TagsServerConf
  -> TagsServerState
  -> m TagsServer
startTagsServer conf state = do
  state' <-
    if tsconfEagerTagging conf
    then do
      logDebug "[startTagsServer] collecting tags eagerly"
      modules <- loadAllFilesIntoState conf
      logDebug $ "[startTagsServer] finished collecting tags eagerly, processed" <+> pretty (getSum $ foldMap (Sum . length) modules) <+> "modules"
      pure $ state { tssLoadedModules = SubkeyMap.fromMap modules <> tssLoadedModules state }
    else pure state
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
    handleRequests lock reqChan serverState = do
      state' <- handleReq reqChan serverState `onException` liftBase (putMVar lock serverState)
      handleRequests lock reqChan state'
    handleReq
      :: Chan (Request, Promise (Either ErrorMessage Response))
      -> TagsServerState
      -> m TagsServerState
    handleReq reqChan serverState = do
      (request, responsePromise) <- liftBase $ readChan reqChan
      logDebug $ "[startTagsServer.handleReq] got request:" ## pretty request
      (response, state') <- runSearchT conf serverState $
        case request of
          FindSymbol filename symbol -> do
            ensureFileExists filename
            symbols <- findSymbol filename symbol
            case symbols of
              []   -> pure $ NotFound symbol
              s:ss -> pure $ Found $ s :| ss
          FindSymbolByRegexp filename _ -> do
            ensureFileExists filename
            throwErrorWithCallStack "Search by regexp is not implemented yet"
      logDebug $ "[startTagsServer.handleReq] got response:" <+> either pretty pretty response
      Promise.putValue responsePromise response
      pure state'

ensureFileExists
  :: (HasCallStack, MonadFS m, MonadError ErrorMessage m)
  => FullPath -> m ()
ensureFileExists path = do
  exists <- doesFileExist path
  unless exists $
    throwErrorWithCallStack $ "Error: file" <+> PP.dquotes (pretty path) <+> "does not exist"
