----------------------------------------------------------------------------
-- |
-- Module      :  Data.Promise
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Sunday, 14 August 2016
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Data.Promise
  ( Promise
  , newPromise
  , getPromisedValue
  , putValue
  ) where

import Control.Concurrent.MVar
import Control.Monad.Base

-- | Promise to return some result. Once result becomes available, it stays so.
newtype Promise a = Promise (MVar a)

-- | Create empty promise
newPromise :: (MonadBase IO m) => m (Promise a)
newPromise = liftBase $ Promise <$> newEmptyMVar

-- | Obtain value from a promise, if it's available. Block until value becomes
-- available.
getPromisedValue :: (MonadBase IO m) => Promise a -> m a
getPromisedValue (Promise var) = liftBase $ readMVar var

-- | Add value to a promise, thus fulfilling the promise. This should be done
-- only once, or thread puting the value would block indefinitely.
putValue :: (MonadBase IO m) => Promise a -> a -> m ()
putValue (Promise var) x = liftBase $ putMVar var x

