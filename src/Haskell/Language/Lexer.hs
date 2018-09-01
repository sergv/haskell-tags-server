----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Thursday,  3 November 2016
----------------------------------------------------------------------------

module Haskell.Language.Lexer
  ( tokenize
  -- , tokenizeM
  -- , LiterateMode(..)
  , LiterateLocation(..)
  ) where

-- import Data.Functor.Identity
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import System.FilePath

import Data.ErrorMessage
-- import Haskell.Language.Lexer.FastTags (Token)
-- import Haskell.Language.Lexer.Lexer (tokenizeM)
-- import Haskell.Language.Lexer.Types (LiterateMode(..))

import Haskell.Language.Lexer.FastTags (Token)
import qualified Haskell.Language.LexerSimple.Lexer as SimpleLexer
import Haskell.Language.LexerSimple.Types (LiterateLocation(..))

tokenize :: HasCallStack => FilePath -> Text -> Either ErrorMessage [Token]
-- tokenize filename = runIdentity . tokenizeM filename mode
  -- where
  --   mode :: LiterateMode
  --   mode
  --     | takeExtension filename == ".lhs" = Literate
  --     | otherwise                        = Vanilla
tokenize filename = SimpleLexer.tokenize filename mode
  where
    mode :: LiterateLocation a
    mode
      | takeExtension filename == ".lhs" = LiterateOutside
      | otherwise                        = Vanilla
