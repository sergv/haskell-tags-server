----------------------------------------------------------------------------
-- |
-- Module      :  Server.BERT
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Monday, 15 August 2016
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

import Prelude hiding (mod)

import Control.Concurrent
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.ErrorExcept
import Control.Monad.Except (throwError)
import Control.Monad.Except.Ext
import Control.Monad.Trans.Control

import qualified Data.ByteString.Lazy.Char8 as CL8
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Text.Prettyprint.Doc.Ext
import Data.Void (Void, vacuous)
import qualified Network.Socket as Network

import Data.BERT
import qualified Network.BERT.Server as BERT
import qualified Network.BERT.Transport as BERT

import Control.Monad.Logging
import Data.CompiledRegex
import Data.Condition
import Data.ErrorMessage
import Data.Path
import qualified Data.Promise as Promise
import Data.Symbols
import Haskell.Language.Lexer.FastTags (Type, Line(..))
import Haskell.Language.Server.Tags.Types

defaultPort :: Network.PortNumber
defaultPort = 10000

decodeUtf8
  :: (WithCallStack, MonadError ErrorMessage m)
  => Doc Void -> UTF8.ByteString -> m T.Text
decodeUtf8 thing =
  either (throwErrorWithCallStack . mkErr) pure . TE.decodeUtf8' . CL8.toStrict
  where
    mkErr = (msg <+>) . ppShow
    msg :: Doc ann
    msg = "Invalid utf8 encoding of" <+> vacuous thing <> ":"

extractString
  :: (WithCallStack, MonadError ErrorMessage m)
  => Doc Void -> Term -> m T.Text
extractString thing = \case
  BinaryTerm str -> decodeUtf8 thing str
  invalid        -> throwErrorWithCallStack $
    "Expected a string for" <+> vacuous thing <> ", but got:" <+> ppShow invalid


-- | Bert transport that can be waiter for.
data SynchronizedTransport = SynchronizedTransport
  { stTransport   :: BERT.TCPServer
  , stStartedLock :: Condition
  }

instance BERT.Server SynchronizedTransport where
  type ServerTransport SynchronizedTransport = BERT.TCP
  runServer SynchronizedTransport{stTransport, stStartedLock} f = do
    let sock = BERT.getTcpListenSocket stTransport
    Network.listen sock Network.maxListenQueue
    setCondition stStartedLock
    forever $ do
      (clientsock, _) <- Network.accept sock
      Network.setSocketOption clientsock Network.NoDelay 1
      f $ BERT.TCP clientsock
  cleanup = BERT.cleanup . stTransport

data BertServer = BertServer
  { bsThreadId  :: ThreadId
  , bsTransport :: SynchronizedTransport
  }

stopBertServer :: MonadBase IO m => BertServer -> m ()
stopBertServer = liftBase . killThread . bsThreadId

-- | After this function returns the server is guaranteed to be ready
-- to receive new connections.
waitForBertServerStart :: MonadBase IO m => BertServer -> m ()
waitForBertServerStart = waitForCondition . stStartedLock . bsTransport

runBertServer
  :: forall m. (WithCallStack, MonadBase IO m, MonadBaseControl IO m, MonadLog m, MonadCatch m, StM m BERT.DispatchResult ~ BERT.DispatchResult)
  => Network.PortNumber
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
    go :: WithCallStack => String -> String -> [Term] -> m BERT.DispatchResult
    go mod func args = do
      logInfo $ "[runBertServer.go] got request" <+> pretty mod <> ":" ## pretty func
      case args of
        [callId, TupleTerm args'] -> do
          res <- runErrorExceptT $ go' mod func args'
          pure $ case res of
            Left err ->
              BERT.Success $ TupleTerm
                [ callId
                , TupleTerm [AtomTerm "error", BinaryTerm err']
                ]
              where
                err' = TLE.encodeUtf8 $ displayDoc $ pretty err
            Right (BERT.Success x) ->
              BERT.Success $ TupleTerm
                [ callId
                , TupleTerm [AtomTerm "ok", x]
                ]
            Right x -> x
        _ -> pure $
          BERT.Success $ TupleTerm
            [ NilTerm -- dummy call id
            , TupleTerm
                [ AtomTerm "error"
                , BinaryTerm "Call argument must be of the form {call-id, actual-args-tuple}."
                ]
            ]
    go'
      :: WithCallStack
      => String -> String -> [Term] -> ErrorExceptT ErrorMessage m BERT.DispatchResult
    go' "haskell-tags-server" "add-shallow-recursive-ignored-entries" args =
      case args of
        [ListTerm shallowDirs, ListTerm recursiveDirs, ListTerm ignoredGlobs] -> do
          request <- DirReq <$>
            (AddShallowRecursiveIgnored
              <$> (S.fromList <$> traverse (mkFullPath <=< extractString "shallow directory") shallowDirs)
              <*> (S.fromList <$> traverse (mkFullPath <=< extractString "recursive directory") recursiveDirs)
              <*> (S.fromList <$> traverse (extractString "ignore glob") ignoredGlobs))
          res <- liftBase $ Promise.getPromisedValue =<< reqHandler request
          BERT.Success <$> either throwError (\() -> pure $ AtomTerm "ok") res
        _ ->
          throwErrorWithCallStack $
            "Expected 3 arguments but got:" ## ppShow args
    go' "haskell-tags-server" "find" args =
      case args of
        [BinaryTerm filename, BinaryTerm symbol, scope] -> do
          filename' <- mkFullPath =<< decodeUtf8 "filename" filename
          scope'    <- decodeScope scope
          request   <- QueryReq filename'
            <$> (FindSymbol scope' <$> (mkSymbolName <$> decodeUtf8 "symbol to find" symbol))
          response <- liftBase $ Promise.getPromisedValue =<< reqHandler request
          BERT.Success <$> either throwError (pure . responseToTerm) response
        _ ->
          throwErrorWithCallStack $
            "Expected 3 arguments but got:" ## ppShow args
    go' "haskell-tags-server" "find-regex" args =
      case args of
        [BinaryTerm filename, BinaryTerm regexp, scope] -> do
          filename' <- mkFullPath =<< decodeUtf8 "filename" filename
          scope'    <- decodeScope scope
          request   <- QueryReq filename'
            <$> (FindSymbolByRegex scope' <$> (compileRegex =<< decodeUtf8 "regexp" regexp))
          response <- liftBase $ Promise.getPromisedValue =<< reqHandler request
          BERT.Success <$> either throwError (pure . responseToTerm) response
        _ ->
          throwErrorWithCallStack $
            "Expected 3 arguments but got:" ## ppShow args
    go' "haskell-tags-server" _ _ = pure BERT.NoSuchFunction
    go' _                     _ _ = pure BERT.NoSuchModule

decodeScope :: MonadError ErrorMessage m => Term -> m NameResolutionScope
decodeScope = \case
  AtomTerm "local"  -> pure ScopeCurrentModule
  AtomTerm "global" -> pure ScopeAllModules
  invalid           ->
    throwErrorWithCallStack $
      "Invalid scope specification:" ## ppShow invalid

responseToTerm :: QueryResponse -> Term
responseToTerm = \case
  NotFound -> AtomTerm "not_found"
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
    [ BinaryTerm $ CL8.fromStrict $ TE.encodeUtf8 $ unqualSymNameText $ resolvedSymbolName sym
    , BinaryTerm $ CL8.fromStrict $ fullPathAsUtf8 file
    , IntTerm $ unLine line
    , AtomTerm $ show typ
    ]
  where
    (file, line) = resolvedSymbolPosition sym
    typ :: Type
    typ = resolvedSymbolType sym
