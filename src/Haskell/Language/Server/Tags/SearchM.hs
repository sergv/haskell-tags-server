----------------------------------------------------------------------------
-- |
-- Module      :  Server.Tags.SearchM
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Tuesday, 23 August 2016
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Haskell.Language.Server.Tags.SearchM
  ( SearchT
  , runSearchT
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Control.Monad.Filesystem (MonadFS)
import Control.Monad.Logging (MonadLog)
import Data.ErrorMessage
import Haskell.Language.Server.Tags.Types

-- | Monad for carrying out symbol search operations.
newtype SearchT m a = SearchM (ExceptT ErrorMessage (StateT TagsServerState (ReaderT TagsServerConf m)) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState TagsServerState
    , MonadReader TagsServerConf
    , MonadError ErrorMessage
    , MonadLog
    , MonadFS
    )

runSearchT :: TagsServerConf -> TagsServerState -> SearchT m a -> m (Either ErrorMessage a, TagsServerState)
runSearchT conf state (SearchM action)
  = flip runReaderT conf
  $ flip runStateT state
  $ runExceptT action


