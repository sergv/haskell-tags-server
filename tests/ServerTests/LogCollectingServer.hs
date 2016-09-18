----------------------------------------------------------------------------
-- |
-- Module      :  LogCollectingServer
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
-- TCPServer that can be stopped with a binary flag flip. Also stores all
-- its logs in an IORef instead of writing them to stderr.
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeFamilies     #-}

module ServerTests.LogCollectingServer
  ( LogCollectingServer
  , mkLogCollectingServer
  , waitUntilStart
  , getLogs
  ) where

import Control.Concurrent.MVar
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Trans.Control
import Data.Foldable (toList)
import Network.Socket (PortNumber)
import Text.PrettyPrint.Leijen.Text (Doc)

import Control.Monad.Logging
import Control.Monad.Logging.Simple
import Server.Tags
import Server.BERT

data LogCollectingServer = LogCollectingServer
  { lcsLogs       :: MVar [Doc]
  , lcsTagsServer :: TagsServer
  , lcsBertServer :: BertServer
  }

mkLogCollectingServer
  :: (MonadBaseControl IO m, MonadError Doc m, MonadCatch m)
  => TagsServerConf -> PortNumber -> m LogCollectingServer
mkLogCollectingServer conf port = do
  logOutputVar <- liftBase $ newMVar mempty
  let log x = liftBase $ modifyMVar_ logOutputVar (pure . (x :))
  tagsServer   <- runSimpleLoggerT (Just (Custom log)) Debug
                $ startTagsServer conf emptyTagsServerState
  bertServer <- liftBase $ runBertServer port $ tsRequestHandler tagsServer
  pure LogCollectingServer
    { lcsLogs       = logOutputVar
    , lcsTagsServer = tagsServer
    , lcsBertServer = bertServer
    }

-- | Block current thread until server will be started and will listen on
-- its port.
waitUntilStart :: LogCollectingServer -> IO ()
waitUntilStart = waitForBertServerStart . lcsBertServer

getLogs :: LogCollectingServer -> IO [Doc]
getLogs serv = reverse . toList <$> readMVar (lcsLogs serv)

