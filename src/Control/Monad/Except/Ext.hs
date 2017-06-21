----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Except.Ext
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :   4 June 2017
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts  #-}

module Control.Monad.Except.Ext
  ( throwErrorWithCallStack
  , HasCallStack
  , module ExceptExport
  ) where

import Control.Monad.Except
import qualified Control.Monad.Except as ExceptExport hiding (throwError)
import GHC.Stack (HasCallStack, callStack)
import Text.PrettyPrint.Leijen.Text.Ext

import Data.ErrorMessage

throwErrorWithCallStack
  :: (HasCallStack, MonadError ErrorMessage m)
  => Doc -> m a
throwErrorWithCallStack msg = throwError ErrorMessage
  { errorMessageBody      = msg
  , errorMessageBacktrace = callStack
  }


