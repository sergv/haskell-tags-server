----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer.TokenisationUtils
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Haskell.Language.Lexer.TokenisationUtils
  ( filename
  , testFullTagsWithoutPrefixes
  , testTagNames
  , untag
  , tokenize'
  , module Haskell.Language.Lexer.Types
  ) where

import Test.Tasty

import Control.Arrow ((***))

import Data.List (sort)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Void (Void)
import GHC.Stack.Ext (WithCallStack)
import Prettyprinter.Ext qualified as PP

import Haskell.Language.Lexer (LitMode(..), tokenize)
import Haskell.Language.Lexer.Types (PragmaType(..), Token(..))
import Haskell.Language.Tags.Analyze (processTokens, ProcessMode(..))
import Haskell.Language.Tags.Types (TagVal(..), Pos(..))

import TestUtils (makeTest)

filename :: FilePath
filename = "/foo/bar/fn.hs"

testFullTagsWithoutPrefixes
  :: WithCallStack
  => LitMode Void -> T.Text -> [Pos TagVal] -> TestTree
testFullTagsWithoutPrefixes mode = \source tags ->
  makeTest ((sort *** map PP.renderStringWide) . processTokens ProcessVanilla . tokenize' mode) source (tags, warnings)
  where
    warnings :: [String]
    warnings = []

testTagNames
  :: WithCallStack
  => LitMode Void -> T.Text -> [String] -> TestTree
testTagNames mode source tags =
  makeTest process source (tags, warnings)
  where
    warnings :: [String]
    warnings = []

    process :: T.Text -> ([String], [String])
    process =
      (sort . map untag *** map PP.renderStringWide) . processTokens ProcessVanilla . tokenize' mode

untag :: Pos TagVal -> String
untag (Pos _ (TagVal name _ _)) = T.unpack name

tokenize'
  :: WithCallStack
  => LitMode Void -> T.Text -> [Pos Token]
tokenize' mode
  = either (error . PP.renderStringWide . PP.pretty) id
  . tokenize mode
  . TE.encodeUtf8
