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
  , LitMode(..)
  ) where

-- import Data.Functor.Identity
import Data.ByteString qualified as BS
import GHC.Stack.Ext (WithCallStack)
import System.FilePath

-- import Haskell.Language.Lexer.Lexer (tokenizeM)
-- import Haskell.Language.Lexer.Types (Token, LiterateMode(..))

import Haskell.Language.Lexer.Types
import Haskell.Language.LexerSimple.Lexer qualified as SimpleLexer

tokenize :: WithCallStack => FilePath -> BS.ByteString -> [Pos ServerToken]
-- tokenize filename = runIdentity . tokenizeM filename mode
  -- where
  --   mode :: LiterateMode
  --   mode
  --     | takeExtension filename == ".lhs" = Literate
  --     | otherwise                        = Vanilla
tokenize filename = SimpleLexer.tokenize mode
  where
    mode :: LitMode a
    mode
      | takeExtension filename `elem` [".lhs", ".lhs-boot"]
      = LitOutside
      | otherwise
      = LitVanilla
