----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Main (main) where

import Test.Tasty

import qualified Haskell.Language.Lexer.Tests as LexerTests
import qualified Haskell.Language.Lexer.Preprocessor.Tests as PreprocessorTests
import qualified Haskell.Language.Server.Tags.AnalyzeHeaderTests as AnalyzeHeaderTests
import qualified Haskell.Language.Server.Tags.TypesTests as TypesTests
import qualified ServerTests

main :: IO ()
main = do
  let tests = testGroup "Tests"
        [ ServerTests.tests
        , TypesTests.tests
        , AnalyzeHeaderTests.tests
        , LexerTests.tests
        , PreprocessorTests.tests
        ]
  defaultMainWithIngredients defaultIngredients tests
