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

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.CompiledRegex
  ( CompiledRegex
  , source
  , regex
  , compileRegex
  ) where

import Control.Monad.Except.Ext
import Data.Function (on)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Show
import Text.Regex.TDFA

import Data.ErrorMessage

-- | Wrapper around Regex that provides dummy Show, Eq and Ord instances
data CompiledRegex = CompiledRegex
  { source :: String
  , regex  :: Regex
  }

instance Pretty CompiledRegex where
  pretty = ppShow

instance Show CompiledRegex where
  show CompiledRegex{source} = "CompiledRegex " ++ show source

instance Eq CompiledRegex where
  (==) = (==) `on` source

instance Ord CompiledRegex where
  compare = compare `on` source

compileRegex
  :: (HasCallStack, MonadError ErrorMessage m)
  => Bool -> String -> m CompiledRegex
compileRegex captureGroups src =
  case makeRegexOptsM compOpt execOpt src of
    Left (err :: String) -> throwErrorWithCallStack $
      "Invalid regexp. Error:" <+> pretty err
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
