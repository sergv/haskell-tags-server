----------------------------------------------------------------------------
-- |
-- Module      :  Data.ErrorMessage
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :   5 June 2017
----------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.ErrorMessage (ErrorMessage(..)) where

import Data.String
import Data.Text.Prettyprint.Doc.Ext
import Data.Void (Void, vacuous)
import GHC.Stack.Ext

data ErrorMessage = ErrorMessage
  { errorMessageBody      :: Doc Void
  , errorMessageBacktrace :: CallStack
  }

instance Pretty ErrorMessage where
  pretty ErrorMessage{errorMessageBody, errorMessageBacktrace} =
    vacuous errorMessageBody ##
      "Backtrace:" ##
        ppCallStack errorMessageBacktrace

instance IsString ErrorMessage where
  fromString msg = ErrorMessage
    { errorMessageBody      = docFromString msg
    , errorMessageBacktrace = callStack
    }
