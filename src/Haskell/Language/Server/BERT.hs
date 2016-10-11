----------------------------------------------------------------------------
-- |
-- Module      :  Server.BERT
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Monday, 15 August 2016
-- Stability   :
-- Portability :
--
-- BERT frontend for tag server.
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Haskell.Language.Server.BERT
  ( defaultPort
  , BertServer
  , stopBertServer
  , waitForBertServerStart
  , runBertServer
  ) where

import Control.Concurrent
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Trans.Control
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import Network.Socket
import Text.PrettyPrint.Leijen.Text.Utils

import Data.BERT
import FastTags (SrcPos(..), Type, Line(..))
import qualified Network.BERT.Server as BERT
import qualified Network.BERT.Transport as BERT

import Control.Monad.Logging
import Data.CompiledRegex
import Data.Condition
import qualified Data.Promise as Promise
import Data.Symbols
import Haskell.Language.Server.Tags.Types


defaultPort :: PortNumber
defaultPort = 10000

decodeUtf8 :: (MonadError Doc m) => Doc -> UTF8.ByteString -> m T.Text
decodeUtf8 thing = either (throwError . mkErr) pure . TE.decodeUtf8' . C8.toStrict
  where
    mkErr = (msg <+>) . showDoc
    msg :: Doc
    msg = "Invalid utf8 encoding of" <+> thing <> ":"

-- | Bert transport that can be waiter for.
data SynchronizedTransport = SynchronizedTransport
  { stTransport   :: BERT.TCPServer
  , stStartedLock :: Condition
  }

instance BERT.Server SynchronizedTransport where
  type ServerTransport SynchronizedTransport = BERT.TCP
  runServer SynchronizedTransport{stTransport, stStartedLock} handle = do
    let sock = BERT.getTcpListenSocket stTransport
    listen sock sOMAXCONN
    setCondition stStartedLock
    forever $ do
      (clientsock, _) <- accept sock
      setSocketOption clientsock NoDelay 1
      handle $ BERT.TCP clientsock
  cleanup = BERT.cleanup . stTransport

data BertServer = BertServer
  { bsThreadId  :: ThreadId
  , bsTransport :: SynchronizedTransport
  }

stopBertServer :: (MonadBase IO m) => BertServer -> m ()
stopBertServer = liftBase . killThread . bsThreadId

-- | After this function returns the server is guaranteed to be ready
-- to receive new connections.
waitForBertServerStart :: (MonadBase IO m) => BertServer -> m ()
waitForBertServerStart = waitForCondition . stStartedLock . bsTransport

runBertServer
  :: forall m. (MonadBase IO m, MonadBaseControl IO m, MonadLog m, StM m BERT.DispatchResult ~ BERT.DispatchResult)
  => PortNumber
  -> RequestHandler
  -> m BertServer
runBertServer port reqHandler = do
  syncTransport <- liftBase $ SynchronizedTransport
                                <$> BERT.tcpServer port
                                <*> newUnsetCondition
  tid  <- liftBaseWith $ \runInBase ->
            forkIO $ BERT.serve syncTransport (\x y z -> runInBase $ go x y z)
  pure BertServer
    { bsThreadId  = tid
    , bsTransport = syncTransport
    }
  where
    go :: String -> String -> [Term] -> m BERT.DispatchResult
    go mod func args = do
      logDebug $ "[runBertServer.go] got request" <+> pretty mod <> ":" <> pretty func
      res <- runExceptT $ go' mod func args
      case res of
        Left err ->
          pure $ BERT.Success $ TupleTerm
            [ AtomTerm "error"
            , BinaryTerm $ TLE.encodeUtf8 $ displayDoc err
            ]
        Right x -> pure x
    go' :: String -> String -> [Term] -> ExceptT Doc m BERT.DispatchResult
    go' "haskell-tags-server" "find-regexp" args =
      case args of
        [BinaryTerm filename, BinaryTerm regexp] -> do
          request  <- FindSymbolByRegexp
                        <$> (T.unpack <$> decodeUtf8 "filename" filename)
                        <*> (compileRegex False . T.unpack =<< decodeUtf8 "regexp" regexp)
          response <- liftBase $ Promise.getPromisedValue =<< reqHandler request
          BERT.Success <$> either throwError (pure . responseToTerm) response
        _                                    ->
          throwError $ "Expected 2 arguments but got:" <+> showDoc args
    go' "haskell-tags-server" "find" args =
      case args of
        [BinaryTerm filename, BinaryTerm symbol] -> do
          request  <- FindSymbol
                        <$> (T.unpack <$> decodeUtf8 "filename" filename)
                        <*> (mkSymbolName <$> decodeUtf8 "symbol to find" symbol)
          response <- liftBase $ Promise.getPromisedValue =<< reqHandler request
          BERT.Success <$> either throwError (pure . responseToTerm) response
        _                                    ->
          throwError $ "Expected 2 arguments but got:" <+> showDoc args
    go' "haskell-tags-server" _ _ = pure BERT.NoSuchFunction
    go' _                     _ _ = pure BERT.NoSuchModule

responseToTerm :: Response -> Term
responseToTerm = \case
  NotFound _ -> AtomTerm "not_found"
  Found (sym :| []) ->
    TupleTerm
      [ AtomTerm "loc_known"
      , symbolToBERT sym
      ]
  Found symbols ->
    TupleTerm
      [ AtomTerm "loc_ambiguous"
      , ListTerm $ toList $ symbolToBERT <$> symbols
      ]

symbolToBERT :: ResolvedSymbol -> Term
symbolToBERT sym =
  TupleTerm
    [ BinaryTerm $ UTF8.fromString posFile
    , IntTerm $ unLine posLine
    , AtomTerm $ show typ
    ]
  where
    SrcPos{posFile, posLine} = resolvedSymbolPosition sym
    typ :: Type
    typ = resolvedSymbolType sym
