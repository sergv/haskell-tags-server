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

{-# LANGUAGE TypeFamilies #-}

module LogCollectingServer
  ( LogCollectingServer
  , logCollectingServer
  , waitUntilStart
  , getLogs
  )
where

import Control.Applicative
import Control.Concurrent.STM
import Data.DList (DList)
import qualified Data.DList as DL
import Network.Socket (PortNumber)

import Network.BERT.Transport (TCP(..), TCPServer(..), Server(..), tcpServer)
import System.Log.Formatter (nullFormatter)
import System.Log.Handler.Simple
import System.Log.Logger

import Logging (initLogger)

data LogCollectingServer = LogCollectingServer
  { _lcServer :: TCPServer
  , lcLogs    :: TVar (DList String)
  , lcStarted :: TVar Bool
  }

instance Server LogCollectingServer where
  type ServerTransport LogCollectingServer = TCP
  runServer (LogCollectingServer serv _ isStartedVar) handle = do
    atomically $ writeTVar isStartedVar True
    runServer serv handle
  cleanup (LogCollectingServer serv _ _) = cleanup serv

logCollectingServer :: PortNumber -> IO LogCollectingServer
logCollectingServer port = do
  serv         <- tcpServer port
  logOutputVar <- newTVarIO DL.empty
  startedVar   <- newTVarIO False
  let handler = ioRefWriter logOutputVar
  -- This is a bit of a hack: we're tweaking global logger
  -- so that all log messages will arrive to our handler.
  initLogger (setHandlers [handler] . setLevel DEBUG)
  return $ LogCollectingServer serv logOutputVar startedVar

-- Block current thread until server will be started and will listen on
-- its port.
waitUntilStart :: LogCollectingServer -> IO ()
waitUntilStart serv = atomically go
  where
    go :: STM ()
    go = do
      isStarted <- readTVar (lcStarted serv)
      if isStarted
        then return ()
        else retry

getLogs :: LogCollectingServer -> IO [String]
getLogs serv = DL.toList <$> readTVarIO (lcLogs serv)

ioRefWriter :: TVar (DList String) -> GenericHandler (TVar (DList String))
ioRefWriter var = GenericHandler
  { priority  = DEBUG
  , formatter = nullFormatter
  , privData  = var
  , writeFunc = \v msg -> atomically $ modifyTVar' v (`DL.snoc` msg)
  , closeFunc = const $ return ()
  }
