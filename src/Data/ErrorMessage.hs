----------------------------------------------------------------------------
-- |
-- Module      :  Data.ErrorMessage
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :   5 June 2017
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.ErrorMessage (ErrorMessage(..)) where

import Data.String
import Data.Text.Prettyprint.Doc.Ext (Pretty(..), Doc)
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Ext as PP
import Data.Void (Void, vacuous)
import GHC.Stack

data ErrorMessage = ErrorMessage
  { errorMessageBody      :: Doc Void
  , errorMessageBacktrace :: CallStack
  }

instance Pretty ErrorMessage where
  pretty ErrorMessage{errorMessageBody, errorMessageBacktrace} =
    vacuous errorMessageBody <> PP.line <>
    PP.nest 2 ("Backtrace:" <> PP.line <> PP.ppCallStack errorMessageBacktrace)

instance IsString ErrorMessage where
  fromString msg = ErrorMessage
    { errorMessageBody      = PP.docFromString msg
    , errorMessageBacktrace = callStack
    }
