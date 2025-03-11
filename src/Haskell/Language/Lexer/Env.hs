----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer.Env
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  20 June 2017
----------------------------------------------------------------------------

module Haskell.Language.Lexer.Env
  ( AlexEnv(..)
  , mkAlexEnv
  ) where

import Data.Void (Void)
import Haskell.Language.Lexer.Types

-- | Environment for user rule predicates.
newtype AlexEnv = AlexEnv
  { aeLiterateMode :: LitMode Void
  } deriving (Eq, Ord, Show)

mkAlexEnv :: LitMode Void -> AlexEnv
mkAlexEnv mode = AlexEnv
  { aeLiterateMode = mode
  }

