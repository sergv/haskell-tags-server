----------------------------------------------------------------------------
-- |
-- Module      :  Server.Tags
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Monday, 15 August 2016
-- Stability   :
-- Portability :
--
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
  , emptyTagsServerConf
  , canonicalizeConfPaths
  , addRecursiveRootsToConf
  , TagsServerState(..)
  , emptyTagsServerState
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Trans.Control
import Data.List.NonEmpty (NonEmpty(..))
import Text.PrettyPrint.Leijen.Text (Doc)

import Data.Promise (Promise)
import qualified Data.Promise as Promise

import Control.Monad.Filesystem (MonadFS)
import Control.Monad.Logging
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

stopTagsServer :: (MonadBase IO m) => TagsServer -> m ()
stopTagsServer = liftBase . killThread . tsThreadId

-- | Block until tags server stops.
waitForTagsServerFinish :: (MonadBase IO m) => TagsServer -> m TagsServerState
waitForTagsServerFinish = liftBase . readMVar . tsFinishState

-- | Start new tags server thread that will serve requests supplied via returned
-- RequestHandler.
startTagsServer
  :: forall m. (MonadBase IO m, MonadBaseControl IO m, MonadCatch m, MonadError Doc m, MonadLog m, MonadFS m)
  => TagsServerConf
  -> TagsServerState
  -> m TagsServer
startTagsServer conf state = do
  unless (tsconfLazyTagging conf) $
    throwError "NOT IMPLEMENTED YET: eager tags collection"
  reqChan <- liftBase newChan
  lock    <- liftBase newEmptyMVar
  tid     <- liftBaseDiscard forkIO $ handleRequests lock reqChan state
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
      -> Chan (Request, Promise (Either Doc Response))
      -> TagsServerState
      -> m ()
    handleRequests lock reqChan state = do
      state' <- handleReq reqChan state `onException` liftBase (putMVar lock state)
      handleRequests lock reqChan state'
    handleReq
      :: Chan (Request, Promise (Either Doc Response))
      -> TagsServerState
      -> m TagsServerState
    handleReq reqChan state = do
      (request, responsePromise) <- liftBase $ readChan reqChan
      case request of
        FindSymbol filename symbol -> do
          (symbols, state') <- runSearchT conf state $ findSymbol filename symbol
          Promise.putValue responsePromise $
            case symbols of
              Left err     -> Left err
              Right []     -> Right $ NotFound symbol
              Right (s:ss) -> Right $ Found $ s :| ss
          pure state'
        FindSymbolByRegexp _ _ -> do
          Promise.putValue responsePromise $ Left "Search by regexp is not implemented yet"
          pure state

