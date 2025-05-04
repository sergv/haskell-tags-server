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
  , stripServerTokens'
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
import TestUtils (makeTest)

import Haskell.Language.Lexer.Types
  ( PragmaType(..)
  , ServerToken(..)
  , TokenVal
  , TagVal(..)
  , Pos(..)
  , Type(..)
  , SrcPos(..)
  , Line(..)
  , whereBlock
  , processTokens
  , UnstrippedTokens(..)
  , unstrippedTokensOf
  , stripServerTokens
  , embedServerToken
  )

filename :: FilePath
filename = "/foo/bar/fn.hs"

testFullTagsWithoutPrefixes
  :: WithCallStack
  => LitMode Void -> T.Text -> [Pos TagVal] -> TestTree
testFullTagsWithoutPrefixes mode = \source tags ->
  makeTest ((sort *** map PP.renderStringWide) . processTokens . tokenize' mode) source (tags, warnings)
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
      (sort . map untag *** map PP.renderStringWide) . processTokens . tokenize' mode

untag :: Pos TagVal -> String
untag (Pos _ (TagVal name _ _)) = T.unpack name

tokenize'
  :: WithCallStack
  => LitMode Void -> T.Text -> [Pos ServerToken]
tokenize' mode
  = either (error . PP.renderStringWide . PP.pretty) id
  . tokenize mode
  . TE.encodeUtf8

stripServerTokens' :: [Pos ServerToken] -> [Pos TokenVal]
stripServerTokens' ts =
  case stripServerTokens ts of
    (ts', [])       -> ts'
    (_,   es@(_:_)) -> error $ PP.renderStringWide $
      PP.ppFoldableHeaderWith id "Errors while stripping server tokens:" es
