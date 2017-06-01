----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer.Preprocessor.Tests
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  29 May 2017
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Haskell.Language.Lexer.Preprocessor.Tests (tests) where

import Test.Tasty
import Test.Tasty.HUnit (testCase)

import Control.Arrow (left)
import Data.List.NonEmpty (NonEmpty(..))
-- import Data.Text (Text)

import Haskell.Language.Lexer.Preprocessor
import TestUtils (makeAssertion)
import Text.PrettyPrint.Leijen.Text.Ext (displayDocString)

tests :: TestTree
tests = testGroup "Preprocessor tests"
  [ defineTests
  , undefTests
  ]

defineTests :: TestTree
defineTests = testGroup "#define"
  [ testGroup "Constants"
      [ testCase "Vanilla define" $
          "#define foo bar"
          ==> Right ("foo", PreprocessorConstant "bar")
      , testCase "Define with some spaces" $
          "# define FOO qu ux"
          ==> Right ("FOO", PreprocessorConstant "qu ux")
      , testCase "Define with lots of continuation lines" $
          "# \\\n\
          \ define \\\n\
          \ FOO \\\n\
          \ qu \\\n\
          \ ux"
          ==> Right ("FOO", PreprocessorConstant "qu  ux")
      , testCase "Define with name split by continuation line" $
          "#define FO\\\n\
          \O bar"
          ==> Right ("FOO", PreprocessorConstant "bar")
      ]
  , testGroup "Macro functions"
      [ testGroup "Single argument"
          [ testCase "Vanilla define" $
              "#define FOO(x) x"
              ==> Right ("FOO", PreprocessorFunction ("x" :| []) "x")
          , testCase "Define with some spaces" $
              "# define FOO( x ) x"
              ==> Right ("FOO", PreprocessorFunction ("x" :| []) "x")
          ]
      , testGroup "Two arguments"
          [ testCase "Vanilla define" $
              "#define FOO(x, y) (x < y)"
              ==> Right ("FOO", PreprocessorFunction ("x" :| ["y"]) "(x < y)")
          , testCase "Define with with some spaces" $
              "# define FOO( x , y ) ( x < y )"
              ==> Right ("FOO", PreprocessorFunction ("x" :| ["y"]) "( x < y )")
          , testCase "Define with lots of continuation lines" $
              "# \\\n\
              \ define \\\n\
              \ FOO( \\\n\
              \ x \\\n\
              \ , \\\n\
              \ y \\\n\
              \ ) \\\n\
              \ ( x < \\\n\
              \ y )"
              ==> Right ("FOO", PreprocessorFunction ("x" :| ["y"]) "( x <  y )")
      , testCase "Define with name split by continuation line" $
          "#define FO\\\n\
          \O(x \\\n\
          \ , \\\n\
          \ y) \\\n\
          \                                (y -\\\n\
          \x)"
          ==> Right ("FOO", PreprocessorFunction ("x" :| ["y"]) "(y -x)")
          ]
      ]
  ]
  where
    (==>) = makeAssertion (left displayDocString . parsePreprocessorDefine)
    -- f :: Text -> Either Doc (Text, PreprocessorMacro) -> Assertion
    -- f input expected =
    --   parsePreprocessorDefine input == expected

undefTests :: TestTree
undefTests = testGroup "#undef"
  [ testCase "Vanilla undef" $
      "#undef FOO"
      ==> Right "FOO"
  , testCase "Undef with some spaces" $
      "#   undef    FOO"
      ==> Right "FOO"
  , testCase "Undef with lots of continuation lines" $
      "# \\\n\
      \  undef  \\\n\
      \  F\\\n\
      \OO"
      ==> Right "FOO"
  ]
  where
    (==>) = makeAssertion (left displayDocString . parsePreprocessorUndef)
