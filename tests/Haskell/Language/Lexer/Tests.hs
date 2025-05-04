----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer.Tests
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Thursday,  3 November 2016
----------------------------------------------------------------------------

{-# LANGUAGE MultilineStrings  #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskell.Language.Lexer.Tests (tests) where

import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit (testCase)

import FastTags.Tag qualified as FastTags

import Haskell.Language.Lexer (LitMode(..))

import Haskell.Language.Lexer.CppTypes qualified as Cpp
import Haskell.Language.Lexer.Tokenisation qualified as Tokenisation
import Haskell.Language.Lexer.TokenisationUtils
import TestUtils (makeAssertion, makeTest)

tests :: TestTree
tests = testGroup "Lexer tests"
  [ Tokenisation.tests
  , testTokenizeCpp
  , testFullPipeline
  ]

testTokenizeCpp :: TestTree
testTokenizeCpp = testGroup "Tokenize with preprocessor"
  [ testCase "Stripping of #define" $
    """
    #define FOO foo
    bar :: a -> a
    bar x = x
    """
    ==>
    [ Newline 0
    , Cpp $ Cpp.Define "FOO"
    , Newline 0
    , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
    , T "bar", T "x", Equals, T "x", Newline 0
    ]
  , testCase "Stripping of multi-line #define #1" $
    """
    #define \\
        FOO \\
      foo
    bar :: a -> a
    bar x = x
    """
    ==>
    [ Newline 0
    , Cpp $ Cpp.Define "FOO"
    , Newline 0
    , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
    , T "bar", T "x", Equals, T "x", Newline 0
    ]
  , testCase "Stripping of multi-line #define #2" $
    """
    #define FOO(T) \\
    {- hello there -} ;\\
    foo :: T -> T ;\\
    foo x = x
    bar :: a -> a
    bar x = x
    """
    ==>
    [ Newline 0
    , Cpp $ Cpp.Define "FOO"
    , Newline 0
    , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
    , T "bar", T "x", Equals, T "x", Newline 0
    ]
  , testCase "Regression in 'text-show' package" $
    textShowSource
    ==>
    [ Newline 0
    , Cpp $ Cpp.Define "GTEXT_SHOW"
    , Newline 0
    , Newline 0
    , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
    , T "bar", T "x", Equals, T "x", Newline 0
    ]
  ]
  where
    (==>) = makeAssertion f
    f = map valOf . tokenize' LitVanilla

_testTokenizeCppDefines :: TestTree
_testTokenizeCppDefines = testGroup "#define"
  [ constants
  , functions
  , concatenation
  , functionsAndConstants
  ]
  where
    (==>) = makeAssertion f
    f = map valOf . tokenize' LitVanilla

    constants :: TestTree
    constants = testGroup "Constants"
      [ testCase "Vanilla define" $
          """
          #define FOO foo
          FOO :: a -> a
          FOO x = x
          """
          ==>
          [ Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "foo", T "x", Equals, T "x", Newline 0
          ]
      , testCase "Haskell-style define names with primes and backticks" $
          """
          #define FOO'Bar` foo
          FOO'Bar` :: a -> a
          FOO'Bar` x = x
          """
          ==>
          [ Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "foo", T "x", Equals, T "x", Newline 0
          ]
      , testCase "Define with lots of continuation lines and indentation" $
         """
         #  \\
           define \\
             FOO      \\
                foo
         FOO :: a -> a
         FOO x = x
         """
          ==>
          [ Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "foo", T "x", Equals, T "x", Newline 0
          ]
      , testCase "Define with lots of continuation lines, no indentation and some spaces" $
          """
          #  \\
          define \\
          FOO      \\
          foo
          FOO :: a -> a
          FOO x = x
          """
          ==>
          [ Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "foo", T "x", Equals, T "x", Newline 0
          ]
      , testCase "Define with lots of continuation lines, no indentation and minimum spaces" $
          """
          #\\
          define \\
          FOO \\
          foo
          FOO :: a -> a
          FOO x = x
          """
          ==>
          [ Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "foo", T "x", Equals, T "x", Newline 0
          ]
      , testCase "Try to use constant define as a macro" $
          """
          #define FOO 1
          quux = FOO(2)
          """
          ==>
          [ Newline 0
          , Newline 0
          , T "quux", Equals, T "1", LParen, T "2", RParen, Newline 0
          ]
      , testCase "Define constant that spans boundaries of a string token" $
          """
          #define BAZ "foo bar

          bar = BAZ FOO quux"

          foo ::  a -> b
          """
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , T "bar", Equals, String, Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "b", Newline 0
          ]
      , testCase "Define multiline constant that spans boundaries of a string token" $
          """
          #define BAZ2 "foo \\
            bar

          bar = BAZ2 FOO quux"

          foo ::  a -> b
          """
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , T "bar", Equals, String, Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "b", Newline 0
          ]
      , testCase "Stripping of empty c-style comments" $
          """
          #define TEST foo/**/bar

          concatTest :: a -> a
          concatTest x =
            x + TEST + "foobar"
          """
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , T "concatTest", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "concatTest", T "x", Equals, Newline 2
          , T "x", T "+", T "foobar", T "+", String, Newline 0
          ]
      , testCase "Stripping of non-empty c-style comments" $
          """
          #define TEST foo/* hello world! */bar

          concatTest :: a -> a
          concatTest x =
            x + TEST + "foobar"
          """
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , T "concatTest", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "concatTest", T "x", Equals, Newline 2
          , T "x", T "+", T "foobar", T "+", String, Newline 0
          ]
      , testCase "Define after use" $
          """
          foo = X

          #define X Y

          bar = X
          """
          ==>
          [ Newline 0
          , T "foo", Equals, T "X", Newline 0
          , Newline 0
          , Newline 0
          , Newline 0
          , T "bar", Equals, T "Y", Newline 0
          ]
      , testCase "Redefine" $
          """
          #define X Y

          foo = X

          #define X Z

          bar = X
          """
          ==>
          [ Newline 0
          , Newline 0
          , T "foo", Equals, T "Y", Newline 0
          , Newline 0
          , Newline 0
          , Newline 0
          , T "bar", Equals, T "Z", Newline 0
          ]
      , testCase "Expand vanilla multi-token #define" $
          """
          #define FOO a + b
          foo :: Int -> Int -> Int
          foo a b c = FOO + c * (FOO)
          """
          ==>
          [ Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "Int", Arrow, T "Int", Arrow, T "Int", Newline 0
          , T "foo", T "a", T "b", T "c", Equals, T "a", T "+", T "b", T "+", T "c", T "*", LParen, T "a", T "+", T "b", RParen, Newline 0
          ]
      , testCase "Expand #define that expands to another #define'd name" $
          """
          #define FOO foo
          #define BAR FOO

          BAR :: a -> b
          BAR x = x
          """
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "b", Newline 0
          , T "foo", T "x", Equals, T "x", Newline 0
          ]
      , testCase "Expand #define that expands to name #define'd later in the program" $
          """
          #define BAR FOO
          #define FOO foo

          BAR :: a -> b
          BAR x = x
          """
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "b", Newline 0
          , T "foo", T "x", Equals, T "x", Newline 0
          ]
      ]

    functions :: TestTree
    functions = testGroup "Functions"
      [ testCase "Function of 0 arguments without whitespace" $
          """
          #define foo() bar
          test :: a -> a
          test x = foo()baz
          """
          ==>
          [ Newline 0
          , Newline 0
          , T "test", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "test", T "x", Equals, T "bar", T "baz", Newline 0
          ]
      , testCase "Function of 0 arguments with whitespace in argument list at definition site" $
          """
          #define foo(           ) bar
          test :: a -> a
          test x = foo()baz
          """
          ==>
          [ Newline 0
          , Newline 0
          , T "test", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "test", T "x", Equals, T "bar", T "baz", Newline 0
          ]
      , testCase "Function of 0 arguments and whitespace in argument list at call site" $
          """
          #define foo() bar
          test :: a -> a
          test x = foo(           )baz
          """
          ==>
          [ Newline 0
          , Newline 0
          , T "test", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "test", T "x", Equals, T "bar", T "baz", Newline 0
          ]
      , testCase "Function of 0 arguments not equivalent to macro name" $
          """
          #define foo() bar
          test :: a -> a
          test x = foo baz
          """
          ==>
          [ Newline 0
          , Newline 0
          , T "test", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "test", T "x", Equals, T "foo", T "baz", Newline 0
          ]
      , testCase "Function of 1 argument" $
          """
          #define MKLENS(x) x
          MKLENS(bar) :: a -> a
          MKLENS(bar) x = x
          """
          ==>
          [ Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "bar", T "x", Equals, T "x", Newline 0
          ]
      , testCase "Function of 1 arguments not expanded instead of constant" $
          """
          #define foo() bar
          test :: a -> a
          test x = foo baz
          """
          ==>
          [ Newline 0
          , Newline 0
          , T "test", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "test", T "x", Equals, T "foo", T "baz", Newline 0
          ]
      , testCase "Function of 2 vanilla arguments" $
          """
          #define TEST(x, y) x + y
          bar :: a -> a
          bar x = TEST(x, x)
          """
          ==>
          [ Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "bar", T "x", Equals, T "x", T "+",T "x", Newline 0
          ]
      , testCase "Function of 2 arguments when one argument contains comma within single quotes" $
          """
          #define TEST(x, y) x + y
          bar :: a -> a
          bar x = TEST(',', x)
          """
          ==>
          [ Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "bar", T "x", Equals, Character, T "+",T "x", Newline 0
          ]
      , testCase "Function of 2 arguments when one argument contains comma within double quotes" $
          """
          #define TEST(x, y) x + y
          bar :: a -> a
          bar x = TEST("x, x", x)
          """
          ==>
          [ Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "bar", T "x", Equals, String, T "+",T "x", Newline 0
          ]
      , testCase "Function of 2 arguments when one argument contains comma within balanced parentheses" $
          """
          #define TEST(x, y) x + y
          bar :: a -> a
          bar x = TEST((x, x), x)
          """
          ==>
          [ Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "bar", T "x", Equals, LParen, T "x", Comma, T "x", RParen, T "+",T "x", Newline 0
          ]
      , testCase "Function of 2 arguments when one argument contains balanced parentheses surrounded by other text" $
          """
          #define TEST(x, y) x + y
          bar :: a -> a
          bar x = TEST(y (x, x) * z, x)
          """
          ==>
          [ Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "bar", T "x", Equals, T "y", LParen, T "x", Comma, T "x", RParen, T "*", T "z", T "+", T "x", Newline 0
          ]
      , testCase "Function of 2 arguments when one argument contains comma within many balanced parentheses" $
          """
          #define TEST(x, y) x + y
          bar :: a -> a
          bar x = TEST(((((((((((x, x)))))))))), x)
          """
          ==>
          ([ Newline 0
           , Newline 0
           , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
           , T "bar", T "x", Equals
           ] ++
           replicate 10 LParen ++ [T "x", Comma, T "x"] ++ replicate 10 RParen ++
           [T "+",T "x", Newline 0])
      , testCase "Function of 2 arguments - balanced brackets do not change semantics of comma" $
          """
          #define TEST(x, y) x + y
          bar :: a -> a
          bar x = TEST([x, x])
          """
          ==>
          [ Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "bar", T "x", Equals, LBracket, T "x", T "+", T "x", RBracket, Newline 0
          ]
      , testCase "Function of 4 arguments with some arguments empty" $
          """
          #define TEST(x, y, z, w) x + y + z + w
          bar :: a -> a
          bar x = TEST(x * x,   ,, 2 )
          """
          ==>
          [ Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "bar", T "x", Equals, T "x", T "*", T "x", T "+", T "+", T "+", T "2", Newline 0
          ]
      , testCase "Expand multiline function-like macro" $
          """
          #define TEST(name, tvar, var) \\
            name :: tvar -> tvar \\
            name var = var

          foo :: a -> a
          foo x = x
          TEST(bar, b, y)

          TEST(baz, c, z)
          """
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "foo", T "x", Equals, T "x", Newline 0
          , T "bar", DoubleColon, T "b", Arrow, T "b"
          , T "bar", T "y", Equals, T "y", Newline 0
          , Newline 0
          , Newline 0
          , T "baz", DoubleColon, T "c", Arrow, T "c"
          , T "baz", T "z", Equals, T "z", Newline 0
          ]
      , testCase "Expand invalid function-like macro" $
          """
          #define FOO () foo()

          bar :: a -> ()
          bar = FOO() + 1
          """
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, LParen, RParen, Newline 0
          , T "bar", Equals, LParen, RParen, T "foo", LParen, RParen, LParen, RParen, T "+", T "1", Newline 0
          ]
      , testCase "Stringization with # without spaces" $
          """
          #define TEST(x) x (#x)
          bar :: a -> a
          bar x = TEST(x)
          """
          ==>
          [ Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "bar", T "x", Equals, T "x", LParen, String, RParen, Newline 0
          ]
      , testCase "Stringization with # surrounded with spaces" $
          """
          #define TEST(x) x (  #  x  )
          bar :: a -> a
          bar x = TEST(x)
          """
          ==>
          [ Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "bar", T "x", Equals, T "x", LParen, String, RParen, Newline 0
          ]
      , testCase "Redefine" $
          """
          #define X(a) Y

          foo = X(1)

          #define X(a) Z

          bar = X(1)
          """
          ==>
          [ Newline 0
          , Newline 0
          , T "foo", Equals, T "Y", Newline 0
          , Newline 0
          , Newline 0
          , Newline 0
          , T "bar", Equals, T "Z", Newline 0
          ]
      , testCase "Function macro passed as argument to different function macro and applied there" $
          """
          #define BAR(x) 1
          #define BAZ(x) 2

          #define FOO(BAR, X) BAR(X)

          foo :: a -> a
          foo = FOO(BAZ, 3)
          """
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "foo", Equals, T "2", Newline 0
          ]
      ]

    concatenation :: TestTree
    concatenation = testGroup "Concatenation"
      [ -- Must do this because gcc and cpphs do this.
        -- Clang doesn't do this right, but that would be definitely
        -- nonportable, so don't aim for Clang.
        testCase "Via c-style comments" $
          """
          #define CONCAT_TEST(name) name/**/Test

          concatTest :: a -> a
          concatTest x =
            x + CONCAT_TEST(bar) + "foobar"
          """
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , T "concatTest", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "concatTest", T "x", Equals, Newline 2
          , T "x", T "+", T "barTest", T "+", String, Newline 0
          ]
        -- Gcc does not support this, but cpphs does and it's pretty standard
        -- in the C world.
      , testCase "Via ##" $
          """
          #define CONCAT_TEST(name) name##Test

          concatTest :: a -> a
          concatTest x =
            x + CONCAT_TEST(bar) + "foobar"
          """
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , T "concatTest", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "concatTest", T "x", Equals, Newline 2
          , T "x", T "+", T "barTest", T "+", String, Newline 0
          ]
      , testCase "Via ## with spaces" $
          """
          #define CONCAT_TEST(name) name ## Test

          concatTest :: a -> a
          concatTest x =
            x + CONCAT_TEST(bar) + "foobar"
          """
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , T "concatTest", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "concatTest", T "x", Equals, Newline 2
          , T "x", T "+", T "barTest", T "+", String, Newline 0
          ]
      ]

    functionsAndConstants :: TestTree
    functionsAndConstants = testGroup "Functions and constants"
      [ testCase "Function redefines constant" $
          """
          #define X Y

          foo = X

          #define X(a) Z

          bar = X(1)
          """
          ==>
          [ Newline 0
          , Newline 0
          , T "foo", Equals, T "Y", Newline 0
          , Newline 0
          , Newline 0
          , Newline 0
          , T "bar", Equals, T "Z", Newline 0
          ]
      , testCase "Function argument takes precedence over other defined macros" $
          """
          #define FOO(BAR) BAR

          #define BAR 15

          baz :: a -> a
          baz x = FOO(x) + 1
          """
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , Newline 0
          , T "baz", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "baz", T "X", Equals, T "x", T "+", T "1", Newline 0
          ]
      , testCase "Function macro passed as argument to another function macro but not aplied there" $
          """
          #define BAR(x) 1
          #define BAZ(x) 2

          #define FOO(BAR, X) BAR

          foo :: a -> a
          foo = FOO(BAZ, 3)
          """
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "foo", Equals, T "BAZ", Newline 0
          ]
      , testCase "Constant macro passed as argument to another function macro gets expanded" $
          """
          #define BAR(x) 1
          #define BAZ 2

          #define FOO(BAR, X) BAR

          foo :: a -> a
          foo = FOO(BAZ, 3)
          """
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "foo", Equals, T "2", Newline 0
          ]
      ]

_testTokenizeCppConditionals :: TestTree
_testTokenizeCppConditionals = testGroup "Conditionals"
  [ testCase "Expand #ifdef-#endif" $
      """
      #ifdef FOO
      foo :: a -> a
      foo x = x
      #endif
      bar :: b -> b
      bar y = y


      """
      ==>
      [ Newline 0
      , Newline 0
      , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
      , T "foo", T "x", Equals, T "x", Newline 0
      , Newline 0
      , T "bar", DoubleColon, T "b", Arrow, T "b", Newline 0
      , T "bar", T "y", Equals, T "y", Newline 0
      , Newline 0
      , Newline 0
      ]
  , testCase "Expand both branches of #ifdef-#else-#endif" $
      """
      #ifdef FOO
      foo :: a -> a
      foo x = x
      #else
      bar :: b -> b
      bar y = y
      #endif
      """
      ==>
      [ Newline 0
      , Newline 0
      , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
      , T "foo", T "x", Equals, T "x", Newline 0
      , Newline 0
      , T "bar", DoubleColon, T "b", Arrow, T "b", Newline 0
      , T "bar", T "y", Equals, T "y", Newline 0
      , Newline 0
      ]
  , testCase "Expand all branches in #if-#elif-#else-#endif" $
      """
      #if defined(FOO)
      foo :: a -> a
      foo x = x
      #elif defined(BAR)
      bar :: b -> b
      bar y = y
      #else
      baz :: c -> c
      baz z = z
      #endif
      """
      ==>
      [ Newline 0
      , Newline 0
      , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
      , T "foo", T "x", Equals, T "x", Newline 0
      , Newline 0
      , T "bar", DoubleColon, T "b", Arrow, T "b", Newline 0
      , T "bar", T "y", Equals, T "y", Newline 0
      , Newline 0
      , T "baz", DoubleColon, T "c", Arrow, T "c", Newline 0
      , T "baz", T "z", Equals, T "z", Newline 0
      , Newline 0
      ]
  ]
  where
    (==>) = makeAssertion f
    f = map valOf . tokenize' LitVanilla

_testTokenizeCppDefinesWithinConditionals :: TestTree
_testTokenizeCppDefinesWithinConditionals =
  testGroup "Defines within conditionals"
    [ testCase "Define same constant within conditional branches" $
        """
        #if defined(FOO)
        #define BAR x
        #else
        #define BAR y
        #endif

        BAR :: a -> b
        """
        ==>
        [ Newline 0
        , Newline 0
        , Newline 0
        , Newline 0
        , Newline 0
        , Newline 0
        , Newline 0
        , T "x", DoubleColon, T "a", Arrow, T "b", Newline 0
        , T "y", DoubleColon, T "a", Arrow, T "b", Newline 0
        ]
    , testCase "Define same function within conditional branches" $
        """
        #if defined(FOO)
        #define BAR(a) x
        #else
        #define BAR(a) y
        #endif

        BAR(1) :: a -> b
        """
        ==>
        [ Newline 0
        , Newline 0
        , Newline 0
        , Newline 0
        , Newline 0
        , Newline 0
        , Newline 0
        , T "x", DoubleColon, T "a", Arrow, T "b", Newline 0
        , T "y", DoubleColon, T "a", Arrow, T "b", Newline 0
        ]
    , testCase "Define constant and function with the same name within conditional branches" $
        """
        #if defined(FOO)
        #define BAR x
        #else
        #define BAR(a) y
        #endif

        BAR :: a -> b
        BAR(1) :: a -> b
        """
        ==>
        [ Newline 0
        , Newline 0
        , Newline 0
        , Newline 0
        , Newline 0
        , Newline 0
        , Newline 0
        , T "x", DoubleColon, T "a", Arrow, T "b", Newline 0
        , T "y", DoubleColon, T "a", Arrow, T "b", Newline 0
        ]
    ]
  where
    (==>) = makeAssertion f
    f = map valOf . tokenize' LitVanilla

testFullPipeline :: TestTree
testFullPipeline = testGroup "Full processing pipeline"
  [ ["data X", "module X"]
    ==>
    [ Pos (SrcPos (Line 1) 0 mempty mempty) (TagVal "X" Type Nothing)
    , Pos (SrcPos (Line 1) 0 mempty mempty) (TagVal "X" Module Nothing)
    ]
  -- Type goes ahead of Module.
  , [ """
      module X where
      data X
      """
    ]
    ==>
    [ Pos (SrcPos (Line 1) 0 mempty mempty) (TagVal "X" Module Nothing)
    , Pos (SrcPos (Line 2) 0 mempty mempty) (TagVal "X" Type Nothing)
    ]
  , [ """
      module Z where
      data X = Y
      """
    ]
    ==>
    [ Pos (SrcPos (Line 1) 0 mempty mempty) (TagVal "Z" Module Nothing)
    , Pos (SrcPos (Line 2) 0 mempty mempty) (TagVal "X" Type Nothing)
    , Pos (SrcPos (Line 2) 0 mempty mempty) (TagVal "Y" Constructor (Just (FastTags.ParentTag "X" Type)))
    ]
  , [ """
      module Z where
      data X a =
        Y a
      """
    ]
    ==>
    [ Pos (SrcPos (Line 1) 0 mempty mempty) (TagVal "Z" Module Nothing)
    , Pos (SrcPos (Line 2) 0 mempty mempty) (TagVal "X" Type Nothing)
    , Pos (SrcPos (Line 3) 0 mempty mempty) (TagVal "Y" Constructor (Just (FastTags.ParentTag "X" Type)))
    ]
  , [ """
      newtype A f a b = A
        { unA :: f (a -> b) }
      """
    ]
    ==>
    [ Pos (SrcPos (Line 1) 0 mempty mempty) (TagVal "A" Type Nothing)
    , Pos (SrcPos (Line 1) 0 mempty mempty) (TagVal "A" Constructor (Just (FastTags.ParentTag "A" Type)))
    , Pos (SrcPos (Line 2) 0 mempty mempty) (TagVal "unA" Function (Just (FastTags.ParentTag "A" Type)))
    ]
  ]
  where
    (==>) = makeTest f'
    f' :: [Text] -> [Pos TagVal]
    f' = sort
       . concatMap (fst . processTokens . tokenize' LitVanilla)

textShowSource :: T.Text
textShowSource =
  """
  #define GTEXT_SHOW(text_type,show_funs,no_show_funs,show1_funs,one_hash,two_hash,hash_prec,gtext_show,gshow_prec,gtext_show_con,gshow_prec_con,show_prec,lift_show_prec,show_space,show_paren,show_list_with,from_char,from_string) \\
  {- | A 'show_funs' value either stores nothing (for 'TextShow') or it stores            \\
  the two function arguments that show occurrences of the type parameter (for             \\
  'TextShow1').                                                                           \\
                                                                                          \\
  /Since: 3.4/                                                                            \\
  -};                                                                                     \\
  data show_funs arity a where {                                                          \\
      no_show_funs :: show_funs Zero a                                                    \\
    ; show1_funs   :: (Int -> a -> text_type) -> ([a] -> text_type) -> show_funs One a    \\
   } deriving Typeable;                                                                   \\
                                                                                          \\
  instance Contravariant (show_funs arity) where {                                        \\
      contramap _ no_show_funs       = no_show_funs                                       \\
    ; contramap f (show1_funs sp sl) = show1_funs (\\p -> sp p . f) (sl . map f)

  bar :: a -> a
  bar x = x
  """
