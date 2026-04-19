----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

module TestMain (main) where

import System.IO
import Test.Tasty

import Data.Map.NonEmpty.Tests qualified as Data.Map.NonEmptyTests
import Haskell.Language.Lexer.Preprocessor.Tests qualified as PreprocessorTests
import Haskell.Language.Lexer.Tests qualified as LexerTests
import Haskell.Language.ModuleTests qualified as ModuleTests
import Haskell.Language.Server.Tags.AnalyzeHeaderTests qualified as AnalyzeHeaderTests
import Haskell.Language.Server.Tags.TypesTests qualified as TypesTests
import SearchTests qualified

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  let tests = testGroup "Tests"
        [ Data.Map.NonEmptyTests.tests
        , SearchTests.tests
        , TypesTests.tests
        , AnalyzeHeaderTests.tests
        , LexerTests.tests
        , PreprocessorTests.tests
        , ModuleTests.tests
        ]
  defaultMainWithIngredients defaultIngredients tests
