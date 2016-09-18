----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Logging
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Wednesday, 24 August 2016
-- Stability   :
-- Portability :
--
--
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
import Text.PrettyPrint.Leijen.Text (Doc)

data Severity = Debug | Info | Warning | Error
  deriving (Show, Eq, Ord)

class (Monad m) => MonadLog m where
  logDoc :: Severity -> Doc -> m ()

logError :: (MonadLog m) => Doc -> m ()
logError = logDoc Error

logWarning :: (MonadLog m) => Doc -> m ()
logWarning = logDoc Warning

logInfo :: (MonadLog m) => Doc -> m ()
logInfo = logDoc Info

logDebug :: (MonadLog m) => Doc -> m ()
logDebug = logDoc Debug

instance (MonadLog m) => MonadLog (ExceptT e m) where
  logDoc s msg = lift $ logDoc s msg
