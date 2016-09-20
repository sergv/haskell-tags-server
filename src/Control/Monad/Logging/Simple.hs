----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Logging.Simple
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Tuesday, 30 August 2016
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Control.Monad.Logging.Simple
  ( SimpleLoggerT
  , Destination(..)
  , runSimpleLoggerT
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Coerce
import qualified Data.Text.Lazy.IO as TLIO
import System.IO

import Text.PrettyPrint.Leijen.Text (Doc)
import Text.PrettyPrint.Leijen.Text.Utils

import Control.Monad.Logging
import Control.Monad.Filesystem

data SimpleLoggerCfg m = SimpleLoggerCfg
  { logSink     :: Doc -> m ()
  , logSeverity :: Severity
  }

newtype SimpleLoggerT m a = SimpleLoggerT (ReaderT (SimpleLoggerCfg m) m a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadBase b
    , MonadError e
    , MonadThrow
    , MonadCatch
    , MonadFS
    )

instance MonadTrans SimpleLoggerT where
  lift = SimpleLoggerT . lift

-- instance MonadTransControl SimpleLoggerT where
--   type StT SimpleLoggerT a = StT (ReaderT (SimpleLoggerCfg m)) a

instance (MonadBaseControl n m) => MonadBaseControl n (SimpleLoggerT m) where
  type StM (SimpleLoggerT m) a = StM (ReaderT (SimpleLoggerCfg m) m) a
  -- liftBaseWith f = SimpleLoggerT $ liftBaseWith (\g -> f (g . unSimpleLoggerT))
  liftBaseWith :: forall a. (RunInBase (SimpleLoggerT m) n -> n a) -> SimpleLoggerT m a
  liftBaseWith = coerce (liftBaseWith :: (RunInBase (ReaderT (SimpleLoggerCfg m) m) n -> n a) -> ReaderT (SimpleLoggerCfg m) m a)
  restoreM :: forall a. StM (SimpleLoggerT m) a -> SimpleLoggerT m a
  restoreM = coerce . (restoreM :: StM (ReaderT (SimpleLoggerCfg m) m) a -> ReaderT (SimpleLoggerCfg m) m a)

instance (Monad m) => MonadLog (SimpleLoggerT m) where
  logDoc severity msg = SimpleLoggerT $ do
    SimpleLoggerCfg{logSink, logSeverity} <- ask
    when (severity >= logSeverity) $
      lift $ logSink msg

data Destination m = Stderr | Stdout | Custom (Doc -> m ())

runSimpleLoggerT
  :: forall m a. (MonadBase IO m)
  => Maybe (Destination m)
  -> Severity
  -> SimpleLoggerT m a
  -> m a
runSimpleLoggerT dest severity (SimpleLoggerT action) =
  runReaderT action cfg
  where
    cfg :: SimpleLoggerCfg m
    cfg = SimpleLoggerCfg
      { logSink     =
          case dest of
            Nothing    -> \_ -> pure ()
            Just dest' ->
              case dest' of
                Stderr   -> liftBase . TLIO.hPutStrLn stderr . displayDoc
                Stdout   -> liftBase . TLIO.hPutStrLn stdout . displayDoc
                Custom f -> f
      , logSeverity = severity
      }
