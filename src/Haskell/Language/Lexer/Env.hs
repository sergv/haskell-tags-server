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
data AlexEnv = AlexEnv
  { aeFilename     :: FilePath
  , aeLiterateMode :: LitMode Void
  } deriving (Eq, Ord, Show)

mkAlexEnv :: FilePath -> LitMode Void -> AlexEnv
mkAlexEnv filename mode = AlexEnv
  { aeFilename     = filename
  , aeLiterateMode = mode
  }

