----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

module TestMain (main) where

import Network.Socket (withSocketsDo)
import System.IO
import Test.Tasty

#ifndef mingw32_HOST_OS
import System.Posix (installHandler, sigPIPE, Handler(Ignore))
#endif

import Data.Map.NonEmpty.Tests qualified as Data.Map.NonEmptyTests
import Haskell.Language.Lexer.Tests qualified as LexerTests
import Haskell.Language.Lexer.Preprocessor.Tests qualified as PreprocessorTests
import Haskell.Language.Server.Tags.AnalyzeHeaderTests qualified as AnalyzeHeaderTests
import Haskell.Language.Server.Tags.TypesTests qualified as TypesTests
import Haskell.Language.ModuleTests qualified as ModuleTests
import ServerTests qualified

main :: IO ()
main = withSocketsDo $ do

#ifndef mingw32_HOST_OS
  _ <- installHandler sigPIPE Ignore Nothing
#endif

  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  let tests = testGroup "Tests"
        [ Data.Map.NonEmptyTests.tests
        , ServerTests.tests
        , TypesTests.tests
        , AnalyzeHeaderTests.tests
        , LexerTests.tests
        , PreprocessorTests.tests
        , ModuleTests.tests
        ]
  defaultMainWithIngredients defaultIngredients tests
