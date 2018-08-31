----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer.TokenisationUtils
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Haskell.Language.Lexer.TokenisationUtils
  ( filename
  , testFullTagsWithoutPrefixes
  , testTagNames
  , untag
  , tokenize'
  , module Haskell.Language.Lexer.FastTags
  ) where

import Test.Tasty

import Control.Arrow (first)
import Data.Functor.Identity
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc.Ext as PP
import GHC.Stack (HasCallStack)

import Haskell.Language.Lexer (LiterateMode(..))
import qualified Haskell.Language.Lexer as Lexer
import TestUtils (makeTest)

import Haskell.Language.Lexer.FastTags
  ( TokenVal(..)
  , PragmaType(..)
  , Token
  , TagVal(..)
  , Pos(..)
  , UnstrippedTokens(..)
  , Type(..)
  , SrcPos(..)
  , Line(..)
  , breakBlocks
  , unstrippedTokensOf
  , processTokens
  )

filename :: FilePath
filename = "fn.hs"

testFullTagsWithoutPrefixes
  :: HasCallStack
  => FilePath -> LiterateMode -> Text -> [Pos TagVal] -> TestTree
testFullTagsWithoutPrefixes fn mode = \source tags ->
  makeTest (first sort . processTokens . tokenize' fn mode) source (tags, warnings)
  where
    warnings :: [String]
    warnings = []

testTagNames
  :: HasCallStack
  => FilePath -> LiterateMode -> Text -> [String] -> TestTree
testTagNames fn mode source tags =
  makeTest process source (tags, warnings)
  where
    warnings :: [String]
    warnings = []

    process :: Text -> ([String], [String])
    process = first (sort . map untag) . processTokens . tokenize' fn mode

untag :: Pos TagVal -> String
untag (Pos _ (TagVal name _ _)) = T.unpack name

tokenize'
  :: HasCallStack
  => FilePath -> LiterateMode -> Text -> [Token]
tokenize' fn mode =
    either (error . PP.displayDocString . PP.pretty) id
  . runIdentity
  . Lexer.tokenizeM fn mode

-- tokenize''
--   :: FilePath
--   -> LiterateMode
--   -> [(PathFragment, Text)]
--   -> Text
--   -> [Token]
-- tokenize'' fn mode includes =
--   either (error . show) id . runIdentity . Lexer.tokenizeM fn mode
