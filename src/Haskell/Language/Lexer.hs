----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Thursday,  3 November 2016
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Haskell.Language.Lexer
  ( tokenize
  , modeFromFilename
  , LitMode(..)
  ) where

import Data.ByteString qualified as BS
import Data.ErrorMessage
-- import Data.Functor.Identity
-- import Data.Text.Encoding qualified as T
import Data.Void (Void)
import GHC.Stack.Ext (WithCallStack)

import Data.Path
-- import Haskell.Language.Lexer.Lexer (tokenizeM)
import Haskell.Language.LexerSimple.Lexer qualified as SimpleLexer

import Haskell.Language.Lexer.Types

modeFromFilename :: TakeExtension a => a -> LitMode b
modeFromFilename filename
  | takeExtension filename `elem` [mkExtension ".lhs", mkExtension ".lhs-boot"]
  = LitOutside
  | otherwise
  = LitVanilla

tokenize :: WithCallStack => LitMode Void -> BS.ByteString -> Either ErrorMessage [Pos ServerToken]
-- tokenize mode = runIdentity . tokenizeM mode . T.decodeUtf8
tokenize = SimpleLexer.tokenize
