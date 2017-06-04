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
{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.Except.Ext
  ( throwErrorWithCallStack
  , HasCallStack
  , module ExceptExport
  ) where

import Control.Monad.Except
import qualified Control.Monad.Except as ExceptExport hiding (throwError)
import GHC.Stack (HasCallStack)
import qualified Text.PrettyPrint.Leijen.Text as PP
import Text.PrettyPrint.Leijen.Text.Ext

throwErrorWithCallStack :: MonadError Doc m => Doc -> m a
throwErrorWithCallStack msg = throwError $
  msg PP.<$> PP.nest 2 ("Backtrace:" PP.<$> ppCallStack)


