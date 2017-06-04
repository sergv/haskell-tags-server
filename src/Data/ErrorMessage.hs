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
import GHC.Stack
import qualified Text.PrettyPrint.Leijen.Text as PP
import Text.PrettyPrint.Leijen.Text.Ext (Pretty(..), Doc)
import qualified Text.PrettyPrint.Leijen.Text.Ext as PP

data ErrorMessage = ErrorMessage
  { errorMessageBody      :: Doc
  , errorMessageBacktrace :: CallStack
  }

instance Pretty ErrorMessage where
  pretty ErrorMessage{errorMessageBody, errorMessageBacktrace} =
    errorMessageBody PP.<$>
    PP.nest 2 ("Backtrace:" PP.<$> PP.ppCallStack errorMessageBacktrace)

instance IsString ErrorMessage where
  fromString msg = ErrorMessage
    { errorMessageBody      = PP.docFromString msg
    , errorMessageBacktrace = callStack
    }
