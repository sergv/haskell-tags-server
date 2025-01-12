----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Except.Ext
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :   4 June 2017
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Control.Monad.Except.Ext
  ( throwErrorWithCallStack
  , WithCallStack
  , module Control.Monad.Except
  ) where

import qualified Control.Monad.Except as CME
import Control.Monad.Except hiding (throwError)
import Data.Void (Void)
import GHC.Stack.Ext (WithCallStack, callStack)
import Prettyprinter.Ext

import Data.ErrorMessage

throwErrorWithCallStack
  :: (WithCallStack, MonadError ErrorMessage m)
  => Doc Void -> m a
throwErrorWithCallStack msg = CME.throwError ErrorMessage
  { errorMessageBody      = msg
  , errorMessageBacktrace = callStack
  }
