----------------------------------------------------------------------------
-- |
-- Module      :  Data.CompiledRegex
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Tuesday, 16 August 2016
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.CompiledRegex
  ( CompiledRegex
  , source
  , regex
  , compileRegex
  ) where

import Control.Monad.Except.Ext
import Data.Function (on)
import qualified Data.Text.Lazy as TL
import Text.PrettyPrint.Leijen.Text (Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP
import Text.PrettyPrint.Leijen.Text.Ext ((<+>))
import Text.Regex.TDFA

-- | Wrapper around Regex that provides dummy Show, Eq and Ord instances
data CompiledRegex = CompiledRegex
  { source :: String
  , regex  :: Regex
  }

instance Show CompiledRegex where
  show CompiledRegex{source} = "CompiledRegex " ++ show source

instance Eq CompiledRegex where
  (==) = (==) `on` source

instance Ord CompiledRegex where
  compare = compare `on` source

compileRegex
  :: (HasCallStack, MonadError Doc m)
  => Bool -> String -> m CompiledRegex
compileRegex captureGroups src =
  case makeRegexOptsM compOpt execOpt src of
    Left err -> throwErrorWithCallStack $
      "Invalid regexp. Error:" <+> PP.text (TL.pack err)
    Right re -> pure CompiledRegex
      { source = src
      , regex  = re
      }
  where
    compOpt :: CompOption
    compOpt = defaultCompOpt
      { newSyntax      = True
      , multiline      = True
      , caseSensitive  = True
      , lastStarGreedy = not captureGroups
      }
    execOpt :: ExecOption
    execOpt = defaultExecOpt
      { captureGroups = captureGroups
      }
