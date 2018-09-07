----------------------------------------------------------------------------
-- |
-- Module      :  LogCollectingServer
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- TCPServer that can be stopped with a binary flag flip. Also stores all
-- its logs in an IORef instead of writing them to stderr.
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeFamilies     #-}

module ServerTests.LogCollectingServer
  ( LogCollectingServer
  , mkLogCollectingServer
  , stopLogCollectingServer
  , waitUntilStart
  , getLogs
  ) where

import Control.Concurrent.MVar
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except.Ext
import Control.Monad.Trans.Control
import Data.Foldable (toList)
import Data.Text.Prettyprint.Doc (Doc)
import Data.Void (Void)
import Network.Socket (PortNumber)

import Control.Monad.Filesystem (MonadFS)
import Control.Monad.Logging
import Control.Monad.Logging.Simple
import Data.ErrorMessage
import Haskell.Language.Server.BERT
import Haskell.Language.Server.Tags

data LogCollectingServer = LogCollectingServer
  { lcsLogs       :: MVar [Doc Void]
  , lcsTagsServer :: TagsServer
  , lcsBertServer :: BertServer
  }

mkLogCollectingServer
  :: (MonadBaseControl IO m, MonadError ErrorMessage m, MonadCatch m, MonadFS m)
  => TagsServerConf -> PortNumber -> m LogCollectingServer
mkLogCollectingServer conf port = do
  logOutputVar <- liftBase $ newMVar mempty
  let addLogEntry x = modifyMVar_ logOutputVar (pure . (x :))
  tagsServer <- runSimpleLoggerT (Just (Custom (liftBase . addLogEntry))) VerboseDebug
              $ startTagsServer conf emptyTagsServerState
  bertServer <- liftBase
                  $ runSimpleLoggerT (Just (Custom addLogEntry)) VerboseDebug
                  $ runBertServer port $ tsRequestHandler tagsServer
  pure LogCollectingServer
    { lcsLogs       = logOutputVar
    , lcsTagsServer = tagsServer
    , lcsBertServer = bertServer
    }

stopLogCollectingServer
  :: (HasCallStack, MonadBase IO m)
  => LogCollectingServer -> m ()
stopLogCollectingServer LogCollectingServer{lcsTagsServer, lcsBertServer} = do
  stopBertServer lcsBertServer
  stopTagsServer lcsTagsServer

-- | Block current thread until server will be started and will listen on
-- its port.
waitUntilStart :: MonadBase IO m => LogCollectingServer -> m ()
waitUntilStart = waitForBertServerStart . lcsBertServer

getLogs :: LogCollectingServer -> IO [Doc Void]
getLogs serv = reverse . toList <$> readMVar (lcsLogs serv)
