----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Logging
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Wednesday, 24 August 2016
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Logging
  ( MonadLog(..)
  , Severity(..)
  , logError
  , logWarning
  , logInfo
  , logDebug
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.State.Strict as SS
import Control.Monad.Writer
import Data.Text.Prettyprint.Doc (Doc)
import Data.Void (Void)

data Severity = Debug | Info | Warning | Error
  deriving (Eq, Ord, Show)

class Monad m => MonadLog m where
  logDoc :: Severity -> Doc Void -> m ()

{-# INLINE logError #-}
logError :: MonadLog m => Doc Void -> m ()
logError = logDoc Error

{-# INLINE logWarning #-}
logWarning :: MonadLog m => Doc Void -> m ()
logWarning = logDoc Warning

{-# INLINE logInfo #-}
logInfo :: MonadLog m => Doc Void -> m ()
logInfo = logDoc Info

{-# INLINE logDebug #-}
logDebug :: MonadLog m => Doc Void -> m ()
logDebug = logDoc Debug

instance MonadLog m => MonadLog (ExceptT e m) where
  {-# INLINE logDoc #-}
  logDoc s = lift . logDoc s

instance MonadLog m => MonadLog (ReaderT r m) where
  {-# INLINE logDoc #-}
  logDoc s = lift . logDoc s

instance MonadLog m => MonadLog (StateT s m) where
  {-# INLINE logDoc #-}
  logDoc s = lift . logDoc s

instance (MonadLog m, Monoid w) => MonadLog (WriterT w m) where
  {-# INLINE logDoc #-}
  logDoc s = lift . logDoc s

instance MonadLog m => MonadLog (SS.StateT s m) where
  {-# INLINE logDoc #-}
  logDoc s = lift . logDoc s
