----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Thursday,  3 November 2016
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

module Haskell.Language.Lexer
  ( tokenize
  , tokenizeM
  , LiterateMode(..)
  ) where

import Data.Functor.Identity
import Data.Text (Text)
import System.FilePath
import Text.PrettyPrint.Leijen.Text.Ext (Doc)
import FastTags.Token (Token)

import Haskell.Language.Lexer.Lexer (tokenizeM)
import Haskell.Language.Lexer.LexerTypes (LiterateMode(..))

tokenize :: FilePath -> Text -> Either Doc [Token]
tokenize filename = runIdentity . tokenizeM filename mode
  where
    mode :: LiterateMode
    mode
      | takeExtension filename == ".lhs" = Literate
      | otherwise                        = Vanilla