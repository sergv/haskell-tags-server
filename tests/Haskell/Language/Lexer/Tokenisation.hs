----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer.Tokenisation
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE MultilineStrings          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

{-# OPTIONS_GHC -Wno-x-tests-only #-}

module Haskell.Language.Lexer.Tokenisation (tests) where

import GHC.Stack.Types (HasCallStack)
import Test.Tasty

import Data.Foldable (toList)
import Data.List qualified as L
import Data.Text qualified as T
import Data.Void (Void)

import Haskell.Language.Blocks
import Haskell.Language.Lexer.CppTypes qualified as Cpp
import Haskell.Language.Lexer.TokenisationUtils
import Haskell.Language.Lexer.Types
import Haskell.Language.Tags.Analyze
import Haskell.Language.Tags.Types
import TestUtils (makeTest)

tests :: TestTree
tests = testGroup "Basic tokenisation"
  [ testTokenise
  , testTokeniseWithNewlines
  , testStripComments
  , testBreakBlocks
  , testWhereBlock
  , testProcess
  ]

testTokenise :: TestTree
testTokenise = testGroup "Tokenise"
  [ "xyz  -- abc"       ==> [T "xyz", Newline 0]
  , "xyz  --- abc"      ==> [T "xyz", Newline 0]
  , "  {-   foo -}"     ==> [Newline 0]
  , """
      {-   foo

    -}
    """
    ==>
    [Newline 0]
  , "  {- foo {- bar-} -}" ==> [Newline 0]
  , "  {-# INLINE #-}"  ==> [Newline 0]
  , "a::b->c"           ==>
    [T "a", DoubleColon, T "b", Arrow, T "c", Newline 0]
  , "a∷b→c"
    ==>
    [T "a", DoubleColon, T "b", Arrow, T "c", Newline 0]
  , "x{-\n  bc#-}\n"    ==> [T "x", Newline 0, Newline 0]
  , "X.Y"               ==> [T "X.Y", Newline 0]
  , "a=>b"              ==> [T "a", Implies, T "b", Newline 0]
  , "a ⇒ b"             ==> [T "a", Implies, T "b", Newline 0]
  , "x9"                ==> [T "x9", Newline 0]
    -- , "9x" ==> ["nl 0", "9", "x"]
  , "x :+: y"           ==> [T "x", T ":+:", T "y", Newline 0]
  , "(#$)"              ==> [LParen, T "#$", RParen, Newline 0]
  , "Data.Map.map"      ==> [T "Data.Map.map", Newline 0]
  , "Map.map"           ==> [T "Map.map", Newline 0]
  , "forall a. f a"     ==> [T "forall", T "a", Dot, T "f", T "a", Newline 0]
  , "forall a . Foo"    ==> [T "forall", T "a", Dot, T "Foo", Newline 0]
  , "forall a. Foo"     ==> [T "forall", T "a", Dot, T "Foo", Newline 0]
  , "forall a .Foo"     ==> [T "forall", T "a", Dot, T "Foo", Newline 0]
  , "forall a.Foo"      ==> [T "forall", T "a", Dot, T "Foo", Newline 0]
  , "$#-- hi"           ==> [T "$#--", T "hi", Newline 0]
  , "(*), (-)"          ==>
    [LParen, T "*", RParen, Comma, LParen, T "-", RParen, Newline 0]
  , "(.::)"             ==> [LParen, T ".::", RParen, Newline 0]
    -- we rely on this behavior
  , "data (:+) a b"     ==>
    [KWData, LParen, T ":+", RParen, T "a", T "b", Newline 0]
  , "data (.::+::) a b" ==>
    [KWData, LParen, T ".::+::", RParen, T "a", T "b", Newline 0]
    -- string tokenization
  , "foo \"bar\" baz"   ==> [T "foo", String, T "baz", Newline 0]
    -- multiline string
  , """
    foo \"bar\\
      \\bar\" baz
    """
    ==>
    [T "foo", String, T "baz", Newline 0]
    -- multiline string with \r
  , """
    foo \"bar\\\r
      \\bar\" baz
    """
    ==>
    [T "foo", String, T "baz", Newline 0]
    -- multiline string with zero indentation
  , """
    foo \"bar\\\r
    \\bar\" baz
    """
    ==>
    [T "foo", String, T "baz", Newline 0]
  , "(\\err -> case err of Foo -> True; _ -> False)" ==>
    [ LParen, LambdaBackslash, T "err", Arrow, KWCase, T "err", KWOf, T "Foo"
    , Arrow, T "True", Semicolon, T "_", Arrow, T "False", RParen, Newline 0
    ]
  , """
    foo = "foo\\n\\\r
      \\" bar
    """
    ==>
    [T "foo", Equals, String, T "bar", Newline 0]
  , """
    foo = "foo\\n\\
      \\x" bar
    """
    ==>
    [ T "foo", Equals, String, T "bar", Newline 0]

  , """
    {-   ___   -}import Data.Char;main=putStr$do{c<-"/1 AA A A;9+ )11929 )1191A 2C9A ";e\r
     {- {- | -} -}  {- {- || -} -}{- {- || -} -}{- {- || -} -} {--}.(`divMod`8).(+(-32)).ord$c};f(0,0)="\\n";f(m,n)=m?"  "++n?"_/"\r
    {- {- | -} -}n?x=do{[1..n];x}                                    --- obfuscated\r
    {-\\_/ on Fairbairn, with apologies to Chris Brown. Above is / Haskell 98 -}
    """
    ==>
    [ KWImport, T "Data.Char", Semicolon
    , T "main", Equals , T "putStr", T "$", KWDo, LBrace, T "c", T "<-"
    , String, Semicolon, T "e", Newline 4
    , Dot, LParen, Backtick, T "divMod", Backtick, Number, RParen, Dot
    , LParen, T "+", LParen, Number, RParen, RParen, Dot
    , T "ord", T "$", T "c", RBrace, Semicolon
    , T "f", LParen, Number, Comma, Number, RParen, Equals, String, Semicolon
    , T "f", LParen, T "m", Comma, T "n", RParen, Equals, T "m", T "?"
    , String, T "++", T "n", T "?", String, Newline 0
    , T "n", T "?", T "x", Equals, KWDo, LBrace, LBracket, Number
    , T "..", T "n", RBracket, Semicolon, T "x", RBrace, Newline 0
    , Newline 0
    ]

  , """
    one_hash, two_hash :: text_type
    hash_prec :: Int -> Int
    one_hash  = from_char '#'
    two_hash  = from_string "##"
    hash_prec = const 0
    """
    ==>
    [ T "one_hash", Comma, T "two_hash", DoubleColon, T "text_type", Newline 0
    , T "hash_prec", DoubleColon, T "Int", Arrow, T "Int", Newline 0
    , T "one_hash", Equals, T "from_char", Character, Newline 0
    , T "two_hash",  Equals, T "from_string", String, Newline 0
    , T "hash_prec", Equals, T "const", Number, Newline 0
    ]
  , "showComplexFloat x 0.0 = showFFloat Nothing x \"\""
    ==>
    [ T "showComplexFloat", T "x", Number, Equals
    , T "showFFloat", T "Nothing", T "x", String, Newline 0
    ]
  , "deriveJSON defaultOptions ''CI.CI"
    ==>
    [T "deriveJSON", T "defaultOptions", T "''CI.CI", Newline 0]
  , "--:+: :+: :+:"
    ==>
    [T "--:+:", T ":+:", T ":+:", Newline 0]

  , "\\x -> y" ==> [LambdaBackslash, T "x", Arrow, T "y", Newline 0]
  , """
    f :: G -> N -> R
    f g =
    \\n -> case lookup n info' of
    \tNothing -> []
    \tJust c  -> c
      where
    """
    ==>
    [ T "f", DoubleColon, T "G", Arrow, T "N", Arrow, T "R", Newline 0
    , T "f", T "g", Equals, Newline 0
    , LambdaBackslash, T "n", Arrow
    , KWCase, T "lookup", T "n", T "info'", KWOf, Newline 1
    , T "Nothing", Arrow, LBracket, RBracket, Newline 1
    , T "Just", T "c", Arrow, T "c", Newline 2
    , KWWhere, Newline 0
    ]

  , tokenizeSplices
  , "import Foo hiding (Bar)" ==>
    [KWImport, T "Foo", T "hiding", LParen, T "Bar", RParen, Newline 0]

  , """
    foo
    #{enum Bar, Baz }
    quux
    """
    ==>
    [ T "foo", Newline 0, HSCEnum, T "Bar", Comma, T "Baz", RBrace, Newline 0
    , T "quux", Newline 0
    ]

  , """
    newtype ControlOp' = ControlOp' CInt

    #{enum ControlOp, ControlOp
     , controlOpAdd    = EPOLL_CTL_ADD
     , controlOpModify = EPOLL_CTL_MOD
     , controlOpDelete = EPOLL_CTL_DEL
     }
    """ ==>
    [ KWNewtype, T "ControlOp'", Equals, T "ControlOp'", T "CInt", Newline 0
    , Newline 0
    , HSCEnum, T "ControlOp", Comma, T "ControlOp", Newline 1
    , Comma, T "controlOpAdd", Equals, T "EPOLL_CTL_ADD", Newline 1
    , Comma, T "controlOpModify", Equals, T "EPOLL_CTL_MOD", Newline 1
    , Comma, T "controlOpDelete", Equals, T "EPOLL_CTL_DEL", Newline 1
    , RBrace, Newline 0
    ]
  , "a # b = apply a b" ==>
    [T "a", T "#", T "b", Equals, T "apply", T "a", T "b", Newline 0]

  , "\"\""         ==> [String, Newline 0]
  , "\"a\""        ==> [String, Newline 0]
  , "\"abc\""      ==> [String, Newline 0]
  , "\"abc\\n\""   ==> [String, Newline 0]
  , "\"abc\\d\""   ==> [String, Newline 0]
  , "\"abc\\\\d\"" ==> [String, Newline 0]
  , "\"\\\\d\""    ==> [String, Newline 0]
  , "\"\\\\\""     ==> [String, Newline 0]

  , "\"a\" ++ \"b\""  ==> [String, T "++", String, Newline 0]
  , "\"\\\\\" ++ \"b\"" ==> [String, T "++", String, Newline 0]

  , "'a'"       ==> [Character, Newline 0]
  , "'\\''"     ==> [Character, Newline 0]
  , "'\"'"      ==> [Character, Newline 0]
  , "'\\\\'"    ==> [Character, Newline 0]
  , "foo' bar'" ==> [T "foo'", T "bar'", Newline 0]
  , "'\\n'"     ==> [Character, Newline 0]

  , "argsOf s = map (init . (drop $ 2 + length s)) . filter ((\"\\\\\" ++ s ++ \"{\") `isPrefixOf`)"
    ==>
    [ T "argsOf", T "s", Equals, T "map", LParen, T "init", Dot
    , LParen, T "drop", T "$", Number, T "+"
    , T "length", T "s", RParen, RParen , Dot, T "filter"
    , LParen, LParen, String, T "++", T "s", T "++", String, RParen
    , Backtick, T "isPrefixOf", Backtick, RParen, Newline 0
    ]

  , """
    foo
    # 17 "/usr/include/stdc-predef.h" 3 4
    bar
    """
    ==>
    [ T "foo", Newline 0
    , Newline 0
    , T "bar", Newline 0
    ]
  , """
    foo
    #{get_area "Queue.T"}
    bar
    """
    ==>
    [ T "foo", Newline 0
    , Newline 0
    , T "bar", Newline 0
    ]
  , """
    foo
    #ccall apr_atomic_init, Ptr <apr_pool_t> -> IO <apr_status_t>
    bar
    """
    ==>
    [ T "foo", Newline 0
    , Newline 0
    , T "bar", Newline 0
    ]
  , """
    type Foo = #{type int64_t}
    """
    ==>
    [ KWType, T "Foo", Equals, HSCDirectiveBraced, T "int64_t", RBrace, Newline 0
    ]
  , tokenizePreprocessor
  ]
  where
    (==>) :: HasCallStack => T.Text -> [Token] -> TestTree
    (==>) = makeTest f
    f :: T.Text -> [Token]
    f = L.drop 1 -- strip uninteresting initial newline
      . map valOf
      . tokenize' LitVanilla

    tokenizeSplices = testGroup "tokenize splices"
      [ "$(foo)"                                  ==>
        [SpliceStart, T "foo", RParen, Newline 0]
      , "$(foo [| baz |])"                        ==>
        [ SpliceStart, T "foo", QuasiquoterStart, QuasiquoterEnd, RParen
        , Newline 0
        ]
      , "$(foo ⟦ baz ⟧)"                          ==>
        [ SpliceStart, T "foo", QuasiquoterStart, QuasiquoterEnd, RParen
        , Newline 0
        ]
      , "$(foo [bar| baz |])"                     ==>
        [ SpliceStart, T "foo", QuasiquoterStart, QuasiquoterEnd, RParen
        , Newline 0
        ]
      , "$(foo [Foo.bar| baz ⟧)"                  ==>
        [ SpliceStart, T "foo", QuasiquoterStart, QuasiquoterEnd, RParen
        , Newline 0
        ]
      , "$(foo [$bar| baz |])"                    ==>
        [ SpliceStart, T "foo", QuasiquoterStart, QuasiquoterEnd, RParen
        , Newline 0
        ]
      , "$(foo [$Foo.bar| baz ⟧)"                 ==>
        [ SpliceStart, T "foo", QuasiquoterStart, QuasiquoterEnd, RParen
        , Newline 0
        ]
      , "$(foo [bar| bar!\nbaz!\n $(baz) |])"     ==>
        [ SpliceStart, T "foo", QuasiquoterStart, SpliceStart, T "baz"
        , RParen, QuasiquoterEnd, RParen, Newline 0
        ]
      , "$(foo [Foo.bar| bar!\nbaz!\n $(baz) ⟧)"  ==>
        [ SpliceStart, T "foo", QuasiquoterStart, SpliceStart, T "baz"
        , RParen, QuasiquoterEnd, RParen, Newline 0
        ]
      , "$(foo [$bar| bar!\nbaz!\n $(baz) |])"    ==>
        [ SpliceStart, T "foo", QuasiquoterStart, SpliceStart, T "baz"
        , RParen, QuasiquoterEnd, RParen, Newline 0
        ]
      , "$(foo [$Foo.bar| bar!\nbaz!\n $(baz) ⟧)" ==>
        [ SpliceStart, T "foo", QuasiquoterStart, SpliceStart, T "baz"
        , RParen, QuasiquoterEnd, RParen, Newline 0
        ]
      , "foo [$bar| baz |]"                       ==>
        [ T "foo", QuasiquoterStart, QuasiquoterEnd, Newline 0 ]
      , """
        foo [$bar|
         baz |]
        """                                 ==>
        [ T "foo", QuasiquoterStart, QuasiquoterEnd, Newline 0 ]
      , """
        foo [$bar|
         baz
        |]
        """                                      ==>
        [ T "foo", QuasiquoterStart, QuasiquoterEnd, Newline 0 ]
      , """
        foo
        $bar
        baz
        """                                     ==>
        [ T "foo", Newline 0
        , ToplevelSplice, Newline 0
        , T "baz", Newline 0
        ]
      , """
        foo
        f $ x = f x
        baz
        """                   ==>
        [ T "foo", Newline 0
        , T "f", T "$", T "x", Equals, T "f", T "x", Newline 0
        , T "baz", Newline 0
        ]
      ]

    tokenizePreprocessor = testGroup "tokenize preprocessor"
      [ """
        #define FOO 1
        """
        ==>
        [ Cpp $ Cpp.Define "FOO", Newline 0
        ]
      , """
        #include "foo.h"
        """
        ==>
        [ Cpp $ Cpp.Include "foo.h", Newline 0
        ]
      , """
        #include <foo.h>
        """
        ==>
        [ Cpp $ Cpp.Include "foo.h", Newline 0
        ]
      , """
        #include <bar/foo.h>
        """
        ==>
        [ Cpp $ Cpp.Include "bar/foo.h", Newline 0
        ]
      , """
        #undef FOO
        """
        ==>
        [ Cpp $ Cpp.Undef "FOO" , Newline 0
        ]
      , """
        #ifdef FOO
        """
        ==>
        [ Cpp $ Cpp.Ifdef "FOO" , Newline 0
        ]
      , """
        #ifndef FOO
        """
        ==>
        [ Cpp $ Cpp.Ifndef "FOO" , Newline 0
        ]
      , """
        #if defined(FOO)
        """
        ==>
        [ Cpp $ Cpp.If "defined(FOO)", Newline 0
        ]
      , """
        #if defined(FOO) && \\
          defined(BAR)
        """
        ==>
        [ Cpp $ Cpp.If "defined(FOO) &&    defined(BAR)", Newline 0
        ]
      , """
        #if 0
        foobar
        #endif
        """
        ==>
        [ Cpp $ Cpp.If "0", Newline 0
        , T "foobar", Newline 0
        , Cpp $ Cpp.Endif, Newline 0
        ]
      , """
        #elif defined(FROB)
        """
        ==>
        [ Cpp $ Cpp.Elif "defined(FROB)", Newline 0
        ]
      , """
        #elif defined(FOO) && \\
          defined(BAR)
        """
        ==>
        [ Cpp $ Cpp.Elif "defined(FOO) &&    defined(BAR)", Newline 0
        ]
      , """
        #else
        """
        ==>
        [ Cpp $ Cpp.Else, Newline 0
        ]
      , """
        #endif
        """
        ==>
        [ Cpp $ Cpp.Endif, Newline 0
        ]
      , """
        #line 100
        """
        ==>
        [ Newline 0
        ]
      , """
        #warning "OK"
        """
        ==>
        [ Newline 0
        ]
      , """
        #error "OK"
        """
        ==>
        [ Newline 0
        ]
      ]

testTokeniseWithNewlines :: TestTree
testTokeniseWithNewlines = testGroup "Tokenise with newlines"
  [ testGroup "vanilla"
    [ "x\ny\n"
      ==> [Newline 0, T "x", Newline 0, T "y", Newline 0, Newline 0]
    , " xx\n yy\n"
      ==> [Newline 1, T "xx", Newline 1, T "yy", Newline 0, Newline 0]
    , """
      class (X x) => C a b where
      \tm :: a->b
      \tn :: c
      """
      ==>
      [ Newline 0
      , KWClass, LParen, T "X", T "x", RParen, Implies
      , T "C", T "a", T "b", KWWhere, Newline 1
      , T "m", DoubleColon, T "a", Arrow, T "b", Newline 1
      , T "n", DoubleColon, T "c"
      , Newline 0
      ]
    ]
  , testGroup "literate"
    [ "foo bar baz"
      |=>
      []
    , "foo bar baz\nquux fizz buzz"
      |=>
      []
    , "> foo = 1"
      |=>
      [Newline 1, T "foo", Equals, Number, Newline 0]
    , """
      This is a factorial function

      > f :: Integer -> Integer
      > f 0 = 1
      > f n =
      >   n * (f $ n - 1)

      And that's it !
      """
      |=>
      [ Newline 1
      , T "f", DoubleColon, T "Integer", Arrow, T "Integer", Newline 1
      , T "f", Number, Equals, Number, Newline 1
      , T "f", T "n", Equals, Newline 3
      , T "n", T "*", LParen, T "f", T "$"
      , T "n", T "-", Number, RParen, Newline 0
      ]
    , """
      This is a factorial function

      > f :: Integer -> Integer
      > f 0 = 1
      > f n =
      >   n * (f $ n - 1)

      And another function:

      > foo :: a -> a
      > foo x = x
      """
      |=>
      [ Newline 1
      , T "f", DoubleColon, T "Integer", Arrow, T "Integer", Newline 1
      , T "f", Number, Equals, Number, Newline 1
      , T "f", T "n", Equals, Newline 3
      , T "n", T "*", LParen, T "f", T "$"
      , T "n", T "-", Number, RParen, Newline 0
      , Newline 1
      , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 1
      , T "foo", T "x", Equals, T "x", Newline 0
      ]

    , """
          This is a factorial function

      > f :: Integer -> Integer
      > f 0 = 1
      > f n =
      >   n * (f $ n - 1)

          And another function:

      > foo :: a -> a
      > foo x = x
      """
      |=>
      [ Newline 1
      , T "f", DoubleColon, T "Integer", Arrow, T "Integer", Newline 1
      , T "f", Number, Equals, Number, Newline 1
      , T "f", T "n", Equals, Newline 3
      , T "n", T "*", LParen, T "f", T "$"
      , T "n", T "-", Number, RParen, Newline 0
      , Newline 1
      , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 1
      , T "foo", T "x", Equals, T "x", Newline 0
      ]

    , """
      This is a factorial function
      \\begin{code}
      f :: Integer -> Integer
      f 0 = 1
      f n =
        n * (f $ n - 1)
      \\end{code}
      And that's it !
      """
      |=>
      [ Newline 0
      , T "f", DoubleColon, T "Integer", Arrow, T "Integer", Newline 0
      , T "f", Number, Equals, Number, Newline 0
      , T "f", T "n", Equals, Newline 2
      , T "n", T "*", LParen, T "f", T "$", T "n", T "-", Number, RParen
      ]
    , """
      This is a 'factorial' function
      \\begin{code}
      f :: Integer -> Integer
      f 0 = 1
      f n =
        n * (f $ n - 1)
      \\end{code}
      But that's not it yet! Here's another function:
      \\begin{code}
      foo :: a -> a
      foo x = x
      \\end{code}
      And that's it !
      """
      |=>
      [ Newline 0
      , T "f", DoubleColon, T "Integer", Arrow, T "Integer", Newline 0
      , T "f", Number, Equals, Number, Newline 0
      , T "f", T "n", Equals, Newline 2
      , T "n", T "*", LParen, T "f", T "$"
      , T "n", T "-", Number, RParen, Newline 0
      , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
      , T "foo", T "x", Equals, T "x"
      ]
    , """
      This is a 'factorial' function
      \\begin{code}
        f :: Integer -> Integer
        f 0 = 1
        f n =
          n * (f $ n - 1)
      \\end{code}
      But that's not it yet! Here's another function:
      \\begin{code}
        foo :: a -> a
        foo x = x
      \\end{code}
      And that's it !
      """
      |=>
      [ Newline 2
      , T "f", DoubleColon, T "Integer", Arrow, T "Integer", Newline 2
      , T "f", Number, Equals, Number, Newline 2
      , T "f", T "n", Equals, Newline 4
      , T "n", T "*", LParen, T "f", T "$", T "n", T "-", Number, RParen

      , Newline 2
      , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 2
      , T "foo", T "x", Equals, T "x"
      ]
    , """
      Test
      \\begin{code}
      class (X x) => C a b where
        m :: a->b
        n :: c
      \\end{code}
      """
      |=>
      [ Newline 0
      , KWClass, LParen, T "X", T "x", RParen, Implies
      , T "C", T "a", T "b", KWWhere, Newline 2
      , T "m", DoubleColon, T "a", Arrow, T "b", Newline 2
      , T "n", DoubleColon, T "c"
      ]
    , """
      Test
      \\begin{code}
      class (X x) => C a b where
      \tm :: a->b
      \tn :: c
      \\end{code}
      """
      |=>
      [ Newline 0
      , KWClass, LParen, T "X", T "x", RParen, Implies
      , T "C", T "a", T "b", KWWhere, Newline 1
      , T "m", DoubleColon, T "a", Arrow, T "b", Newline 1
      , T "n", DoubleColon, T "c"
      ]
    ]
  ]
  where
    (==>) = makeTest (f LitVanilla)
    (|=>) = makeTest (f LitOutside)
    f mode =
      map valOf
      . tokenize' mode


testStripComments :: TestTree
testStripComments = testGroup "Strip comments"
  [ "hello -- there"
    ==>
    [Newline 0, T "hello", Newline 0]
  , "hello --there"
    ==>
    [Newline 0, T "hello", Newline 0]
  , "hello {- there -} fred"
    ==>
    [Newline 0, T "hello", T "fred", Newline 0]
  , """
    hello -- {- there -}
    fred
    """
    ==>
    [Newline 0, T "hello", Newline 0, T "fred", Newline 0]
  , "{-# LANG #-} hello {- there {- nested -} comment -} fred"
    ==>
    [Newline 1, T "hello", T "fred", Newline 0]
  , """
    hello {-
    there
    ------}
     fred
    """
    ==>
    [Newline 0, T "hello", Newline 1, T "fred", Newline 0]
  , """
    hello {-
    there
      ------}
     fred
    """
    ==>
    [Newline 0, T "hello", Newline 1, T "fred", Newline 0]
  , """
    hello {-
    there
    -----}
     fred
    """
    ==>
    [Newline 0, T "hello", Newline 1, T "fred", Newline 0]
  , """
    hello {-
    there
      -----}
     fred
    """
    ==>
    [Newline 0, T "hello", Newline 1, T "fred", Newline 0]
  , """
    hello {-
    -- there -}
    """
    ==>
    [Newline 0, T "hello", Newline 0]
  , """
    foo --- my comment
    --- my other comment
    bar
    """
    ==>
    [Newline 0, T "foo", Newline 0, Newline 0, T "bar", Newline 0]
  ]
  where
    (==>) = makeTest f
    f = map valOf . tokenize' LitVanilla

testBreakBlocks :: TestTree
testBreakBlocks = testGroup "Break blocks"
  [ testGroup "vanilla"
    [ """
      a
      b
      """
      ==>
      [ [T "a"]
      , [T "b"]
      ]
    , """
      a
       a
      b
      """
      ==>
      [ [T "a", Newline 1, T "a"]
      , [T "b"]
      ]
    , """
      a
       a
       a
      b
      """
      ==>
      [ [T "a", Newline 1, T "a", Newline 1, T "a"]
      , [T "b"]
      ]
      -- CPP defines are ignored
    , """
      a
       a
      #define FOO
       a
      b
      """
      ==>
      [ [T "a", Newline 1, T "a", Newline 0, Cpp $ Cpp.Define "FOO", Newline 1, T "a"]
      , [T "b"]
      ]
      -- intervening blank lines are ignored
    , """
      a
       a

       a
      b
      """
      ==>
      [ [T "a", Newline 1, T "a", Newline 1, T "a"]
      , [T "b"]
      ]
    , """
      a


       a
      b
      """
      ==>
      [ [T "a", Newline 1, T "a"]
      , [T "b"]
      ]
    , """
      a
       aa
       aa
      """
      ==>
      [[T "a", Newline 1, T "aa", Newline 1, T "aa"]]
    , """
       aa
       aa
      """
      ==>
      [ [T "aa"]
      , [T "aa"]
      ]

    , """
      one_hash, two_hash :: text_type
      hash_prec :: Int -> Int
      one_hash  = from_char '#'
      two_hash  = from_string "##"
      hash_prec = const 0
      """
      ==>
      [ [T "one_hash", Comma, T "two_hash", DoubleColon, T "text_type"]
      , [T "hash_prec", DoubleColon, T "Int", Arrow, T "Int"]
      , [T "one_hash", Equals, T "from_char", Character]
      , [T "two_hash",  Equals, T "from_string", String]
      , [T "hash_prec", Equals, T "const", Number]
      ]
    , """
      one_hash, two_hash :: text_type; \
      \hash_prec :: Int -> Int; \
      \one_hash  = from_char '#'; \
      \two_hash  = from_string "##"; \
      \hash_prec = const 0
      """
      ==>
      [ [T "one_hash", Comma, T "two_hash", DoubleColon, T "text_type"]
      , [T "hash_prec", DoubleColon, T "Int", Arrow, T "Int"]
      , [T "one_hash", Equals, T "from_char", Character]
      , [T "two_hash",  Equals, T "from_string", String]
      , [T "hash_prec", Equals, T "const", Number]
      ]
    , """
      {
        data F f :: * ; -- foo
                        -- bar
                        -- baz
        mkF  :: f -> F f ; getF :: F f -> f ;
      } ;
      """
      ==>
      [ [ LBrace, Newline 2, KWData, T "F", T "f", DoubleColon
        , T "*", Semicolon, Newline 2
        , T "mkF", DoubleColon, T "f", Arrow, T "F", T "f", Semicolon
        , T "getF", DoubleColon, T "F", T "f", Arrow, T "f", Semicolon
        , Newline 0
        , RBrace
        ]
      ]
    ]
  , testGroup "literate"
    [ """
      > a
      >
      >
      >  a
      > b
      """
      |=>
      [ [T "a", Newline 2, T "a"]
      , [T "b"]
      ]
    , """
      > a
      >
      >
      >  a
      > b
      """
      |=>
      [ [T "a", Newline 2, T "a"]
      , [T "b"]
      ]
    , """
      > a
      >  aa
      >  aa
      """
      |=>
      [[T "a", Newline 2, T "aa", Newline 2, T "aa"]]
    , """
      > a
      >  aa
      >
      >  aa
      """
      |=>
      [[T "a", Newline 2, T "aa", Newline 2, T "aa"]]
    ]
  ]
  where
    (==>) :: HasCallStack => T.Text -> [[Token]] -> TestTree
    (==>) = makeTest (f LitVanilla)
    (|=>) :: HasCallStack => T.Text -> [[Token]] -> TestTree
    (|=>) = makeTest (f LitOutside)
    f :: LitMode Void -> T.Text -> [[Token]]
    f mode
      = map (map valOf . toList)
      . breakBlocks StripDirectives
      . tokenize' mode

testWhereBlock :: TestTree
testWhereBlock = testGroup "whereBlock"
  [ """
    class A f where
      data F f :: * -- foo
                    -- bar
                    -- baz
      mkF  :: f -> F f
      getF :: F f -> f
    """
    ==>
    [ [KWData, T "F", T "f", DoubleColon, T "*"]
    , [T "mkF", DoubleColon, T "f", Arrow, T "F", T "f"]
    , [T "getF", DoubleColon, T "F", T "f", Arrow, T "f"]
    ]
  , """
    class A f where {
      data F f :: * ; -- foo
                      -- bar
                      -- baz
      mkF  :: f -> F f ;
      getF :: F f -> f ;
    } ;
    """
    ==>
    [ [KWData, T "F", T "f", DoubleColon, T "*"]
    , [T "mkF", DoubleColon, T "f", Arrow, T "F", T "f"]
    , [T "getF", DoubleColon, T "F", T "f", Arrow, T "f"]
    ]
  ]
  where
    (==>) = makeTest f
    f = map (map valOf . unUnstrippedTokens)
      . whereBlock
      . UnstrippedTokens
      . tokenize' LitVanilla

testProcess :: TestTree
testProcess = testGroup "Process"
  [ testMeta
  , testData
  , testGADT
  , testFamilies
  , testFunctions
  , testClass
  , testInstance
  , testLiterate
  , testPatterns
  , testFFI
  , testDefine
  , testHSC2HS
  , testUnicode
  ]

testMeta :: TestTree
testMeta = testGroup "prefix, suffix and offset tracking"
  [ "module Bar.Foo where\n" ==>
    [Pos (SrcPos 1 0 "" "") (TagVal "Foo" Module Nothing)]
  , """
    newtype Foo a b =
    \tBar x y z
    """
    ==>
    [ Pos (SrcPos 1 0 "" "") (TagVal "Foo" Type Nothing)
    , Pos (SrcPos 2 0 "" "") (TagVal "Bar" Constructor (Just (ParentTag "Foo" Type)))
    ]
  , """
    data Foo a b =
    \tBar x y z
    """
    ==>
    [ Pos (SrcPos 1 0 "" "") (TagVal "Foo" Type Nothing)
    , Pos (SrcPos 2 0 "" "") (TagVal "Bar" Constructor (Just (ParentTag "Foo" Type)))
    ]
  , """
    f :: A -> B
    g :: C -> D
    data D = C {
    \tf :: A
    \t}
    """
    ==>
    [ Pos (SrcPos 1 0 "" "") (TagVal "f" Function Nothing)
    , Pos (SrcPos 2 0 "" "") (TagVal "g" Function Nothing)
    , Pos (SrcPos 3 0 "" "") (TagVal "C" Constructor (Just (ParentTag "D" Type)))
    , Pos (SrcPos 3 0 "" "") (TagVal "D" Type Nothing)
    , Pos (SrcPos 4 0 "" "") (TagVal "f" Function (Just (ParentTag "D" Type)))
    ]
  , """
    instance Foo Bar where
      newtype FooFam Bar = BarList [Int]
    """
    ==>
    [ Pos (SrcPos 2 0 "" "") (TagVal "BarList" Constructor (Just (ParentTag "FooFam" Family)))
    ]
  , """
    instance Foo Bar where
      newtype FooFam Bar = BarList { getBarList :: [Int] }
    """ ==>
    [ Pos (SrcPos 2 0 "" "") (TagVal "BarList" Constructor (Just (ParentTag "FooFam" Family)))
    , Pos (SrcPos 2 0 "" "") (TagVal "getBarList" Function (Just (ParentTag "FooFam" Family)))
    ]
  , """
    instance Foo Bar where
      data (Ord a) => FooFam Bar a = BarList { getBarList :: [a] }
                                   | BarMap { getBarMap :: Map a Int }
    """
    ==>
    [ Pos (SrcPos 2 0 "" "") (TagVal "BarList" Constructor (Just (ParentTag "FooFam" Family)))
    , Pos (SrcPos 2 0 "" "") (TagVal "getBarList" Function (Just (ParentTag "FooFam" Family)))
    , Pos (SrcPos 3 0 "" "") (TagVal "BarMap" Constructor (Just (ParentTag "FooFam" Family)))
    , Pos (SrcPos 3 0 "" "") (TagVal "getBarMap" Function (Just (ParentTag "FooFam" Family)))
    ]
  , "newtype instance FooFam Bar = BarList { getBarList :: [Int] }" ==>
    [ Pos (SrcPos 1 0 "" "") (TagVal "BarList" Constructor (Just (ParentTag "FooFam" Family)))
    , Pos (SrcPos 1 0 "" "") (TagVal "getBarList" Function (Just (ParentTag "FooFam" Family)))
    ]
  , """
    data instance (Ord a) => FooFam Bar a = BarList { getBarList :: [a] }
                                          | BarMap { getBarMap :: Map a Int }
    """
    ==>
    [ Pos (SrcPos 1 0 "" "") (TagVal "BarList" Constructor (Just (ParentTag "FooFam" Family)))
    , Pos (SrcPos 1 0 "" "") (TagVal "getBarList" Function (Just (ParentTag "FooFam" Family)))
    , Pos (SrcPos 2 0 "" "") (TagVal "BarMap" Constructor (Just (ParentTag "FooFam" Family)))
    , Pos (SrcPos 2 0 "" "") (TagVal "getBarMap" Function (Just (ParentTag "FooFam" Family)))
    ]
  ]
  where
    (==>) = testFullTagsWithoutPrefixes LitVanilla

testData :: TestTree
testData = testGroup "data"
  [ "data X\n"                            ==> ["X"]
  , "data X = X Int\n"                    ==> ["X", "X"]
  , "data Foo = Bar | Baz"                ==> ["Bar", "Baz", "Foo"]
  , "data Foo =\n\tBar\n\t| Baz"          ==> ["Bar", "Baz", "Foo"]
    -- Records.
  , "data Foo a = Bar { field :: Field }" ==> ["Bar", "Foo", "field"]
  , "data R = R { a::X, b::Y }"           ==> ["R", "R", "a", "b"]
  , "data R = R { a, b::X }"              ==> ["R", "R", "a", "b"]
  , "data R = R { a∷X, b∷Y }"             ==> ["R", "R", "a", "b"]
  , "data R = R { a, b∷X }"               ==> ["R", "R", "a", "b"]
  , "data R = R {\n\ta::X\n\t, b::Y\n\t}" ==> ["R", "R", "a", "b"]
  , "data R = R {\n\ta,b::X\n\t}"         ==> ["R", "R", "a", "b"]
    -- Record operators
  , "data Foo a b = (:*:) { foo :: a, bar :: b }" ==>
    [":*:", "Foo", "bar", "foo"]

  , """
    data R = R {
        a :: !RealTime
      , b :: !RealTime
    }
    """
    ==>
    ["R", "R", "a", "b"]
  , """
    data Rec = Rec {
      a :: Int
    , b :: !Double
    , c :: Maybe Rec\
    \\n\
    \}
    """
    ==>
    ["Rec", "Rec", "a", "b", "c"]

  , "data X = X !Int"                            ==> ["X", "X"]
  , "data X = Y !Int !X | Z"                     ==> ["X", "Y", "Z"]
  , "data X = Y :+: !Z | !X `Mult` X"            ==> [":+:", "Mult", "X"]
  , "data X = !Y `Add` !Z"                       ==> ["Add", "X"]

  , "data X = forall a. Y a"                     ==> ["X", "Y"]
  , "data X = forall a . Y a"                    ==> ["X", "Y"]
  , "data X = forall a .Y a"                     ==> ["X", "Y"]
  , "data X = forall a.Y a"                      ==> ["X", "Y"]
  , "data X = forall a. Eq a => Y a"             ==> ["X", "Y"]
  , "data X = forall a. (Eq a) => Y a"           ==> ["X", "Y"]
  , "data X = forall (a :: Nat). (Eq' a) => Y a" ==> ["X", "Y"]
  , "data X = forall a. (Eq a, Ord a) => Y a"    ==> ["X", "Y"]
  , "data X = forall a. Ref :<: a => Y a"        ==> ["X", "Y"]
  , "data X = forall a. (:<:) Ref a => Y a"      ==> ["X", "Y"]
  , "data X = forall a. ((:<:) Ref a) => Y a"    ==> ["X", "Y"]
  , "data X = forall a. Y !a"                    ==> ["X", "Y"]
  , "data X = forall a. (Eq a, Ord a) => Y !a"   ==> ["X", "Y"]

  , """
    data Foo a =
        Plain Int
      | forall a. Bar a Int
      | forall a b. Baz b a
      | forall a . Quux a
      | forall a .Quuz a
    """
    ==>
    ["Bar", "Baz", "Foo", "Plain", "Quux", "Quuz"]

  , "data X a = Add a "                     ==> ["Add", "X"]
  , "data Eq a => X a = Add a"              ==> ["Add", "X"]
  , "data (Eq a) => X a = Add a"            ==> ["Add", "X"]
  , "data (Eq a, Ord a) => X a = Add a"     ==> ["Add", "X"]
  , "data (Eq (a), Ord (a)) => X a = Add a" ==> ["Add", "X"]

  , "data Ref :<: f => X f = RRef f"                                  ==>
    ["RRef", "X"]
  , "data a :<: b => X a b = Add a"                                   ==>
    ["Add", "X"]
  , "newtype Ref :<: f => X f = RRef f"                               ==>
    ["RRef", "X"]
  , "newtype a :<: b => X a b = Add a"                                ==>
    ["Add", "X"]
  , "data Ref :<: [f] => X f = RRef f"                                ==>
    ["RRef", "X"]
  , "data [a] :<: b => X a b = Add a"                                 ==>
    ["Add", "X"]
  , "newtype Ref :<: [f] => X f = RRef f"                             ==>
    ["RRef", "X"]
  , "newtype [a] :<: b => X a b = Add a"                              ==>
    ["Add", "X"]

  , "data (a :<: b) => X a b = Add a"                                 ==>
    ["Add", "X"]
  , "data a :><: b = a :>|<: b"                                       ==>
    [":><:", ":>|<:"]
  , "data (:><:) a b = (:>|<:) a b"                                   ==>
    [":><:", ":>|<:"]
  , "data (:><:) a b = Foo b | (:>|<:) a b"                           ==>
    [":><:", ":>|<:", "Foo"]
  , "data (:><:) a b = Foo b | forall c. (:>|<:) a c"                 ==>
    [":><:", ":>|<:", "Foo"]
  , "data (:><:) a b = Foo b | forall c. (Eq c) => (:>|<:) a c"       ==>
    [":><:", ":>|<:", "Foo"]
  , "data (:><:) a b = Foo b | forall c. Eq c => (:>|<:) a c"         ==>
    [":><:", ":>|<:", "Foo"]

  , "newtype Eq a => X a = Add a"                                     ==>
    ["Add", "X"]
  , "newtype (Eq a) => X a = Add a"                                   ==>
    ["Add", "X"]
  , "newtype (Eq a, Ord a) => X a = Add a"                            ==>
    ["Add", "X"]
  , "newtype () => X a = Add a"                                       ==>
    ["Add", "X"]

  , "newtype (u :*: v) z = X"                                         ==>
    [":*:", "X"]
  , "data (u :*: v) z = X"                                            ==>
    [":*:", "X"]
  , "type (u :*: v) z = (u, v, z)"                                    ==>
    [":*:"]

  , "newtype ((u :: (* -> *) -> *) :*: v) z = X"                      ==>
    [":*:", "X"]
  , "data ((u :: (* -> *) -> *) :*: v) z = X"                         ==>
    [":*:", "X"]
  , "type ((u :: (* -> *) -> *) :*: v) z = (u, v, z)"                 ==>
    [":*:"]

  , "newtype () => ((u :: (* -> *) -> *) :*: v) z = X"                ==>
    [":*:", "X"]
  , "data () => ((u :: (* -> *) -> *) :*: v) z = X"                   ==>
    [":*:", "X"]
  , "type () => ((u :: (* -> *) -> *) :*: v) z = (u, v, z)"           ==>
    [":*:"]

  , "newtype (Eq (v z)) => ((u :: (* -> *) -> *) :*: v) z = X"        ==>
    [":*:", "X"]
  , "data (Eq (v z)) => ((u :: (* -> *) -> *) :*: v) z = X"           ==>
    [":*:", "X"]
  , "type (Eq (v z)) => ((u :: (* -> *) -> *) :*: v) z = (u, v, z)"   ==>
    [":*:"]

  , "newtype Eq (v z) => ((u :: (* -> *) -> *) :*: v) z = X"          ==>
    [":*:", "X"]
  , "data Eq (v z) => ((u :: (* -> *) -> *) :*: v) z = X"             ==>
    [":*:", "X"]
  , "type Eq (v z) => ((u :: (* -> *) -> *) :*: v) z = (u, v, z)"     ==>
    [":*:"]

  , "newtype (Eq (v z)) => ((u :: (* -> *) -> *) `Foo` v) z = X"      ==>
    ["Foo", "X"]
  , "data (Eq (v z)) => ((u :: (* -> *) -> *) `Foo` v) z = X"         ==>
    ["Foo", "X"]
  , "type (Eq (v z)) => ((u :: (* -> *) -> *) `Foo` v) z = (u, v, z)" ==>
    ["Foo"]
  , "type (Eq (v z)) => ((u ∷ (* -> *) -> *) `Foo` v) z = (u, v, z)"  ==>
    ["Foo"]

  , "newtype Eq (v z) => ((u :: (* -> *) -> *) `Foo` v) z = X"        ==>
    ["Foo", "X"]
  , "data Eq (v z) => ((u :: (* -> *) -> *) `Foo` v) z = X"           ==>
    ["Foo", "X"]
  , "type Eq (v z) => ((u :: (* -> *) -> *) `Foo` v) z = (u, v, z)"   ==>
    ["Foo"]
  , "type Eq (v z) ⇒ ((u ∷ (* → *) → *) `Foo` v) z = (u, v, z)"       ==>
    ["Foo"]

  , "data (:*:) u v z = X"                                            ==>
    [":*:", "X"]
  , "data (Eq (u v), Ord (z)) => (:*:) u v z = X"                     ==>
    [":*:", "X"]
  , "data (u `W` v) z = X"                                            ==>
    ["W", "X"]
  , "data (Eq (u v), Ord (z)) => (u `W` v) z = X"                     ==>
    ["W", "X"]

  , """
    newtype X a = Z {
     -- TODO blah
     foo :: [a] }
    """
    ==>
    ["X", "Z", "foo"]
  , """
    newtype (u :*: v) z = X {
     -- my insightful comment
     extract :: (u (v z)) }
    """
    ==>
    [":*:", "X", "extract"]
  , """
    newtype (u :*: v) z = X {
     -- my insightful comment
     pattern :: (u (v z)) }
    """
    ==>
    [":*:", "X", "pattern"]

  , "data Hadron a b = Science { f :: a, g :: a, h :: b }"
    ==>
    ["Hadron", "Science", "f", "g", "h"]
  , "data Hadron a b = Science { f :: a, g :: a, pattern :: b }"
    ==>
    ["Hadron", "Science", "f", "g", "pattern"]
  , "data Hadron a b = Science { f :: a, g :: (a, b), h :: b }"
    ==>
    ["Hadron", "Science", "f", "g", "h"]
  , "data Hadron a b = forall x. Science { f :: x, h :: b }"
    ==>
    ["Hadron", "Science", "f", "h"]
  , "data Hadron a b = forall x. Science { f :: [x], h :: b }"
    ==>
    ["Hadron", "Science", "f", "h"]
  , "data Hadron a b = forall x y. Science { f :: (x, y), h :: b }"
    ==>
    ["Hadron", "Science", "f", "h"]
  , "data Hadron a b = forall x y. Science { f :: [(x, y)], h :: b }"
    ==>
    ["Hadron", "Science", "f", "h"]
  , "data Hadron a b = \
    \forall x y. Science { f :: [(Box x, Map x y)], h :: b }"
    ==>
    ["Hadron", "Science", "f", "h"]
  , "data Hadron a b = forall x y. Science { f ∷ [(Box x, Map x y)], h ∷ b }"
    ==>
    ["Hadron", "Science", "f", "h"]
  , """
    data Hadron a b = forall x y z. Science
      { f :: x
      , g :: [(Box x, Map x y, z)] \
    \  }
    """
    ==>
    ["Hadron", "Science", "f", "g"]
  , """
    data Hadron a b = forall x y z. Science
      { f :: x
      , g :: [(Box x, Map x y, z)] \
    \  , h :: b
      }
    """
    ==>
    ["Hadron", "Science", "f", "g", "h"]
  , "data Hadron a b = Science { h :: b }"
    ==>
    ["Hadron", "Science", "h"]
  , """
    data Test a b =
        Foo a
      | Bar [(Maybe a, Map a b, b)]
    """
    ==>
    ["Bar", "Foo", "Test"]
  , """
    data Test a b =
        Foo a
      | [(Maybe b, Map b a, a)] `Bar` [(Maybe a, Map a b, b)]
    """
    ==>
    ["Bar", "Foo", "Test"]
  , "data IO a = IO (World->(a,World))"
    ==>
    ["IO", "IO"]

  , """
    data SubTransformTriple a =
            SubTransformTriple
               (forall sh. (Shape sh, Slice sh) => Transform (sh:.Int) a)
               (forall sh. (Shape sh, Slice sh) => Transform (sh:.Int) a)
               (forall sh. (Shape sh, Slice sh) => Transform (sh:.Int) a)
    """
    ==>
    ["SubTransformTriple", "SubTransformTriple"]

  , """
    data TestCase
        = forall a prop . (Testable prop, Data a)
        => TestCase  (((String, a, a) -> Property) -> prop)
            deriving (Typeable)
    """
    ==>
    ["TestCase", "TestCase"]

  , """
    -- | Binding List
    data BindingList v a = Variable v => BindingList {source :: Source v a -- ^ the list's binding source
                                                    , list   :: v [v a]    -- ^ the bound list
                                                    , pos    :: v Int}     -- ^ the current position
    """
    ==>
    ["BindingList", "BindingList", "list", "pos", "source"]

  , """
    data Tester a = Tester
        {(===) :: [String] -> a -> IO ()
        ,fails :: [String] -> IO ()
        ,isHelp :: [String] -> [String] -> IO ()
        ,isHelpNot :: [String] -> [String] -> IO ()
        ,isVersion :: [String] -> String -> IO ()
        ,isVerbosity :: [String] -> Verbosity -> IO ()
        ,completion :: [String] -> (Int,Int) -> [Complete] -> IO ()
        }
    """
    ==>
    ["===", "Tester", "Tester", "completion", "fails", "isHelp", "isHelpNot", "isVerbosity", "isVersion"]

  , """
    data Tester a = Tester
        {(===) :: [String] -> a -> IO ()
        ,fails :: [String] -> IO ()
        ,isHelp, isHelpNot :: [String] -> [String] -> IO ()
        ,isVersion :: [String] -> String -> IO ()
        ,isVerbosity :: [String] -> Verbosity -> IO ()
        ,completion :: [String] -> (Int,Int) -> [Complete] -> IO ()
        }
    """
    ==>
    ["===", "Tester", "Tester", "completion", "fails", "isHelp", "isHelpNot", "isVerbosity", "isVersion"]

  , """
    -- | View of the right end of a sequence.
    data ViewR s a
        = EmptyR
        | s a :> a
    """
    ==>
    [":>", "EmptyR", "ViewR"]
  , """
    -- | View of the right end of a sequence.
    data ViewR s a
        = s a :> a
        | EmptyR
    """
    ==>
    [":>", "EmptyR", "ViewR"]

  , "data [] a = [] | a : [a]"
    ==> [":", "[]", "[]"]
  , "data () = ()"
    ==> ["()", "()"]
  , "data (,) a b = (,) a b"
    ==> ["(,)", "(,)"]
  , "data (,,) a b c = (,,) a b c"
    ==> ["(,,)", "(,,)"]
  , "data (a, b) = (a, b)"
    ==> ["(,)", "(,)"]
  , "data (a, b, c) = (a, b, c)"
    ==> ["(,,)", "(,,)"]
  ]
  where
    (==>) = testTagNames LitVanilla

testGADT :: TestTree
testGADT = testGroup "gadt"
  [ "data X where A :: X\n"              ==> ["A", "X"]
  , "data X where\n\tA :: X\n"           ==> ["A", "X"]
  , "data X where\n\tA :: X\n\tB :: X\n" ==> ["A", "B", "X"]
  , "data X where\n\tA, B :: X\n"        ==> ["A", "B", "X"]
  , """
    data X :: * -> * -> * where
      A, B :: Int -> Int -> X
    """
    ==>
    ["A", "B", "X"]
  , """
    data X ∷ * → * → * where
      A, B ∷ Int → Int → X
    """
    ==>
    ["A", "B", "X"]
  , """
    data Vec ix where
      Nil   :: Int -> Foo Int
      (:::) :: Int -> Vec Int -> Vec Int
      (:+.) :: Int -> Int -> Vec Int -> Vec Int
    """
    ==>
    [":+.", ":::", "Nil", "Vec"]
  , """
    data Vec ix where
      Nil   :: Int -> Foo Int
      -- foo
      (:::) :: Int -> Vec Int -> Vec Int
    -- bar
      (:+.) :: Int     ->
               -- ^ baz
               Int     ->
               Vec Int ->
    Vec Int
    """
    ==>
    [":+.", ":::", "Nil", "Vec"]
  , """
    data NatSing (n :: Nat) where
      ZeroSing :: 'Zero
      SuccSing :: NatSing n -> NatSing ('Succ n)
    """
    ==>
    ["NatSing", "SuccSing", "ZeroSing"]
  , """
    data Rec a where
      C :: { foo :: Int } -> Rec a
    """
    ==> ["C", "Rec", "foo"]
  , """
    data Rec a where
      C :: { foo :: Int, bar :: Int -> Int } -> Rec a
    """
    ==> ["C", "Rec", "bar", "foo"]
  , """
    data Rec a where
      C :: { foo :: Int, bar :: Int -> Int } -> Rec a
      D :: { baz :: (Int -> Int) -> Int, bar :: (((Int) -> (Int))) } -> Rec a
    """
    ==> ["C", "D", "Rec", "bar", "bar", "baz", "foo"]
  , """
    newtype TyConProxy a b where
        TyConProxy :: () -> TyConProxy a b
      deriving ( Arbitrary
               , Show
               , Generic
    #if defined(__LANGUAGE_DERIVE_GENERIC1__)
               , Generic1
    #endif
               )
    """
    ==> ["TyConProxy", "TyConProxy"]
  , """
    data Foo a where
      Bar :: Baz a => { foo :: Int, bar :: a } -> Foo a
    """
    ==> ["Bar", "Foo", "bar", "foo"]

  , "type role Map nominal representational"
    ==> []
  ]
  where
    (==>) = testTagNames LitVanilla

testFamilies :: TestTree
testFamilies = testGroup "families"
  [ "type family X :: *\n"      ==> ["X"]
  , "data family X :: * -> *\n" ==> ["X"]
  , "data family a ** b"        ==> ["**"]

  , "type family a :<: b\n"                               ==> [":<:"]
  , "type family (a :: Nat) :<: (b :: Nat) :: Nat\n"      ==> [":<:"]
  , "type family (a :: Nat) `Family` (b :: Nat) :: Nat\n" ==> ["Family"]
  , "type family (m :: Nat) <=? (n :: Nat) :: Bool"       ==> ["<=?"]
  , "type family (m ∷ Nat) <=? (n ∷ Nat) ∷ Bool"         ==> ["<=?"]

  , "data instance X a b = Y a | Z { unZ :: b }"                     ==>
    ["Y", "Z", "unZ"]
  , "data instance (Eq a, Eq b) => X a b = Y a | Z { unZ :: b }"     ==>
    ["Y", "Z", "unZ"]
  , "data instance (Eq a, Eq b) => X a b = Y a | Z { pattern :: b }" ==>
    ["Y", "Z", "pattern"]
  , "data instance XList Char = XCons !Char !(XList Char) | XNil"    ==>
    ["XCons", "XNil"]
  , "newtype instance Cxt x => T [x] = A (B x) deriving (Z,W)"       ==> ["A"]
  , "type instance Cxt x => T [x] = A (B x)"                         ==> []
  , """
    data instance G [a] b where
       G1 :: c -> G [Int] b
       G2 :: G [a] Bool
    """
    ==>
    ["G1", "G2"]
  , "class C where\n\ttype X y :: *\n" ==> ["C", "X"]
  , "class C where\n\tdata X y :: *\n" ==> ["C", "X"]
  , "class C where\n\ttype X y ∷ *\n"  ==> ["C", "X"]
  , "class C where\n\tdata X y ∷ *\n"  ==> ["C", "X"]
  ]
  where
    (==>) = testTagNames LitVanilla

testFunctions :: TestTree
testFunctions = testGroup "functions"
  -- Multiple declarations.
  [ "a,b::X"          ==> ["a", "b"]
    -- With an operator.
  , "(+), a :: X"     ==> ["+", "a"]
    -- Unicode operator.
  , "(•) :: X"        ==> ["•"]
    -- Don't get fooled by literals.
  , "1 :: Int"        ==> []
    -- Don't confuse _ wildcard for an operator.
  , """
    f :: Int -> Int
    f _ = 1
    """
    ==>
    ["f"]

    -- plain functions and operators
  , "(.::) :: X -> Y" ==> [".::"]
  , "(+::) :: X -> Y" ==> ["+::"]
  , "(->:) :: X -> Y" ==> ["->:"]
  , "(--+) :: X -> Y" ==> ["--+"]
  , "(=>>) :: X -> Y" ==> ["=>>"]

    -- Multi-line with semicolons at the end
  , """
    one_hash, two_hash :: text_type;
    hash_prec :: Int -> Int;
    one_hash  = from_char '#';
    two_hash  = from_string "##";
    hash_prec = const 0
    """
    ==>
    ["hash_prec", "one_hash", "two_hash"]
    -- Single-line separated by semicolons - e.g. result of a macro expansion
  , """
    one_hash, two_hash :: text_type; \
    \hash_prec :: Int -> Int; \
    \one_hash  = from_char '#'; \
    \two_hash  = from_string "##"; \
    \hash_prec = const 0
    """
    ==>
    ["hash_prec", "one_hash", "two_hash"]

  , """
    assertDataFormatError :: DecompressError -> IO String
    assertDataFormatError (DataFormatError detail) = return detail
    assertDataFormatError _                        = assertFailure "expected DataError"
                                                  >> return ""
    """
    ==>
    ["assertDataFormatError"]

  , """
    instance PartialComparison Double where
        type PartialCompareEffortIndicator Double = ()
        pCompareEff _ a b = Just $ toPartialOrdering $ Prelude.compare a b
    --        case (isNaN a, isNaN b) of
    --           (False, False) -> Just $ toPartialOrdering $ Prelude.compare a b
    --           (True, True) -> Just EQ
    --           _ -> Just NC
        pCompareDefaultEffort _ = ()

    pComparePreludeCompare _ a b =
        Just $ toPartialOrdering $ Prelude.compare a b

    propPartialComparisonReflexiveEQ ::
        (PartialComparison t) =>
        t ->
        (PartialCompareEffortIndicator t) ->
        (UniformlyOrderedSingleton t) ->
        Bool
    propPartialComparisonReflexiveEQ _ effort (UniformlyOrderedSingleton e) =
        case pCompareEff effort e e of Just EQ -> True; Nothing -> True; _ -> False
    """
    ==>
    ["pComparePreludeCompare", "propPartialComparisonReflexiveEQ"]

  , """
    hexQuad :: Z.Parser Int
    hexQuad = do
      s <- Z.take 4
      let hex n | w >= C_0 && w <= C_9 = w - C_0
                | w >= C_a && w <= C_f = w - 87
                | w >= C_A && w <= C_F = w - 55
                | otherwise          = 255
            where w = fromIntegral $ B.unsafeIndex s n
          a = hex 0; b = hex 1; c = hex 2; d = hex 3
      if (a .|. b .|. c .|. d) /= 255
        then return $! d .|. (c `shiftL` 4) .|. (b `shiftL` 8) .|. (a `shiftL` 12)
        else fail "invalid hex escape"
    """
    ==>
    ["hexQuad"]

    -- Semicolon within instance does not produce toplevel tags
  , "instance Path  T.Text      where readPath = Just; pathRep _ = typeRep (Proxy :: Proxy Text)"
    ==>
    []

  , "prop_bounds2 o1 w1 o2 w2 = let n1 = fromW8 w1 ; n2 = fromW8 w2 ; bs = ((o1, o2), (o1 + n1, o2 + n2)) in bs == bounds (listArray bs (take ((n1 + 1) * (n2 + 1)) (cycle [False, True, True])))"
    ==>
    ["prop_bounds2"]

  , """
    string :: String -> ReadP r String
    -- ^ Parses and returns the specified string.
    string this = do s <- look; scan this s
     where
      scan []     _               = do return this
      scan (x:xs) (y:ys) | x == y = do get >> scan xs ys
      scan _      _               = do pfail
    """
    ==>
    ["string"]

  , """
    -- | Get every node of a tree, put it into a list.
    climb :: Tree a -> [a]
    climb x = case x of (Empty) -> [];(Branch a Empty b) -> a : climb b;
                        (Branch a b Empty) -> a : climb b;
                        (Branch a b d) -> a : climb b ++ climb d
    """
    ==>
    ["climb"]

  , """
    addend hid a (NotM (App uid okh elr as)) = NotM $ App uid okh elr (f as)
     where f (NotM ALNil) = NotM $ ALCons hid a (NotM $ ALNil)
           f (NotM (ALCons hid a as)) = NotM $ ALCons hid a (f as)
           f _ = __IMPOSSIBLE__
    addend _ _ _ = __IMPOSSIBLE__
    copyarg _ = False
    """
    ==>
    ["addend", "copyarg"]

  , """
    realWorldTc :: TyCon; \\
    realWorldTc = mkTyCon3 "ghc-prim" \"GHC.Types" \"RealWorld"; \\
    instance Typeable RealWorld where { typeOf _ = mkTyConApp realWorldTc [] }
    """
    ==>
    ["realWorldTc"]

  , "(r `mkQ` br) a = _" ==> ["mkQ"]

  , "_?_ = return unsafePerformIO" ==> ["?"]

  , """
    (+) :: DebMap -> String -> DebianVersion
    m + k = maybe (error ("No version number for " ++ show k ++ " in " ++ show (Map.map (maybe Nothing (Just . prettyDebianVersion)) m))) id (Map.findWithDefault Nothing k m)
    """
    ==>
    ["+"]

  , """
    (!) :: DebMap -> String -> DebianVersion
    _ ! k = maybe (error ("No version number for " ++ show k ++ " in " ++ show (Map.map (maybe Nothing (Just . prettyDebianVersion)) m))) id (Map.findWithDefault Nothing k m)
    """
    ==>
    ["!"]

  , """
    (.) :: DebMap -> String -> DebianVersion
    m . k = maybe (error ("No version number for " ++ show k ++ " in " ++ show (Map.map (maybe Nothing (Just . prettyDebianVersion)) m))) id (Map.findWithDefault Nothing k m)
    """
    ==>
    ["."]

  , """
    (.) :: DebMap -> String -> DebianVersion
    m . k x = maybe (error ("No version number for " ++ show k ++ " in " ++ show (Map.map (maybe Nothing (Just . prettyDebianVersion)) m))) id (Map.findWithDefault Nothing k m)
    """
    ==>
    ["."]

  , """
    {- 123___ -}import Data.Char;main=putStr$do{c<-"/1 AA A A;9+ )11929 )1191A 2C9A ";e
    {-  |  -}    .(`divMod`8).(+(-32)).ord$c};f(0,0)="\\n";f(m,n)=m?"  "++n?"_/"
    {-  |  -}n?x=do{[1..n];x}                                    --- obfuscated
    {-\\_/ on Fairbairn, with apologies to Chris Brown. Above is / Haskell 98 -}
    """
    ==>
    ["?", "f", "main"]
  , """
    {-   456___   -}import Data.Char;main=putStr$do{c<-"/1 AA A A;9+ )11929 )1191A 2C9A ";e
     {- {- | -} -}  {- {- || -} -}{- {- || -} -}{- {- || -} -} {--}.(`divMod`8).(+(-32)).ord$c};f(0,0)="\\n";f(m,n)=m?"  "++n?"_/"
    {- {- | -} -}n?x=do{[1..n];x}                                    --- obfuscated
    {-\\_/ on Fairbairn, with apologies to Chris Brown. Above is / Haskell 98 -}
    """
    ==>
    ["?", "f", "main"]

  , """
    showComplexFloat :: Double -> Double -> String
    showComplexFloat x 0.0 = showFFloat Nothing x ""
    showComplexFloat 0.0 y = showFFloat Nothing y "i\"
    showComplexFloat x y = (showFFloat Nothing x "") ++ (if y > 0 then "+\" else "") ++ (showFFloat Nothing y "i\")
    """
    ==>
    ["showComplexFloat"]

  , """
    createAlert            :<|>
     getAlert              :<|>
     deleteAlert           :<|>
     setAlertStatus        :<|>
     tagAlert              :<|>
     untagAlert            :<|>
     updateAlertAttributes = client (Proxy :: Proxy AlertApi)
    """
    ==>
    ["createAlert"]

  , "_g :: X -> Y" ==> ["_g"]
  , "(f . g) x = f (g x)" ==> ["."]
  , """
    (#) :: TransArray c => c -> (b -> IO r) -> Trans c b -> IO r
    a # b = apply a b
    {-# INLINE (#) #-}

    """
    ==>
    ["#"]

  , """
    escape :: FilePath -> FilePath
    escape s = "\\\"\" ++ concatMap esc s ++ "\\\"\"
      where
      esc c | c `elem` ['\\\\', '"']   = '\\\\' : [c]
            | isAscii c && isPrint c = [c]
            | otherwise              = "\\\\x" ++ showHex (fromEnum c) "\\\\ "

    ------------------------------------------------------------------------
    -- Compiling Emacs Lisp files

    -- | The Agda mode's Emacs Lisp files, given in the order in which
    -- they should be compiled.

    emacsLispFiles :: [FilePath]
    emacsLispFiles =
      [ "agda2-abbrevs.el"
      , "annotation.el"
      , "agda2-queue.el"
      , "eri.el"
      , "agda2.el"
      , "agda-input.el"
      , "agda2-highlight.el"
      , "agda2-mode.el"
      ]
    """
    ==>
    ["emacsLispFiles", "escape"]

  , """
    happyError = \tks i -> error (
    \t"Parse error in line " ++ show (i::Int) ++ "\\n")
    """
    ==>
    ["happyError"]
  , """
    > happyError = \tks i -> error (
    >\t"Parse error in line " ++ show (i::Int) ++ "\\n")
    """
    |=>
    ["happyError"]

  , """
    module UnindentedImportList(
    foo,
    (++),
    bar\
    \,
    quux
    )
    where

    import Test

    baz :: a -> a
    baz x = x

    """
    ==>
    ["UnindentedImportList", "baz"]

  , """


    #define FOO


      module UnindentedImportList(
      foo,
      (++),
      bar\
    \  ,
      quux
      )
      where

      import Test

      baz :: a -> a
      baz x = x

    """
    ==>
    ["FOO", "UnindentedImportList", "baz"]

  , """

    {-# LANGAUGE TemplateHaskell #-}
    module Example where

    import Local.TH
    concat <$> sequence [thFunc1, thFunc2]

    foo :: a -> a
    foo x = x
    """
    ==>
    ["Example", "foo"]
  , """

    {-# LANGAUGE TemplateHaskell #-}
    module Example where

    import Local.TH
    concat <$> sequence [thFunc1, thFunc2]

    """
    ==>
    ["Example"]

  , toplevelFunctionsWithoutSignatures

  ]
  where
    (==>) = testTagNames LitVanilla
    (|=>) = testTagNames LitOutside
    toplevelFunctionsWithoutSignatures =
      testGroup "toplevel functions without signatures"
        [ "$(return . map sumDeclaration $ [0..15])" ==> []
        , "$( fmap (reverse . concat) . traverse prismsForSingleType $ [1..15] )" ==> []
        , "T.makeInstances [2..6]\nx" ==> []
        , "infix 5 |+|"  ==> []
        , "infixl 5 |+|" ==> []
        , "infixr 5 |+|" ==> []
        , "f = g"        ==> ["f"]
          -- Relies on RepeatableTag.
        , """
          f :: a -> b -> a
          f x y = x
          """
          ==>
          ["f"]
        , "f x y = x"               ==> ["f"]
        , "f (x :+: y) z = x"       ==> ["f"]
        , "(x :+: y) `f` z = x"     ==> ["f"]
        , "(x :+: y) *: z = x"     ==> ["*:"]
        , "((:+:) x y) *: z = x"   ==> ["*:"]
        , "(*:) (x :+: y) z = x"   ==> ["*:"]
        , "(*:) ((:+:) x y) z = x" ==> ["*:"]
        , strictMatchTests
        , lazyMatchTests
        , atPatternsTests
        , """
          f x Nothing = x
          f x (Just y) = y
          """
          ==>
          ["f"]
        , """
          x `f` Nothing = x
          x `f` (Just y) = y
          """
          ==>
          ["f"]
        , """
          f x y = g x
            where
              g _ = y
          """
          ==>
          ["f"]
        , "x `f` y = x"   ==> ["f"]
        , "(|+|) x y = x" ==> ["|+|"]
        , "x |+| y = x"   ==> ["|+|"]
        , "(!) x y = x" ==> ["!"]
        , "--- my comment" ==> []
        , """
          foo :: Rec -> Bar
          foo Rec{..} = Bar (recField + 1)
          """
          ==>
          ["foo"]
        , """
          foo :: Rec -> Bar
          foo Rec { bar = Baz {..}} = Bar (recField + 1)
          """
          ==>
          ["foo"]
          -- Functions named "pattern"
        , "pattern :: Int -> String -> [Int]"           ==> ["pattern"]
        , "pattern x () = x + x"                        ==> ["pattern"]
        , "pattern, foo :: Int -> String -> [Int]"      ==> ["foo", "pattern"]
        , "foo, pattern :: Int -> String -> [Int]"      ==> ["foo", "pattern"]
        , "foo, pattern, bar :: Int -> String -> [Int]" ==> ["bar", "foo", "pattern"]
        , "pattern x = x "                              ==> ["pattern"]
        , "pattern x y = x + y"                         ==> ["pattern"]
        , "x `pattern` y = x + y"                       ==> ["pattern"]
        , "pattern x y z = x + y + z"                   ==> ["pattern"]
          -- Arguments named "forall
        , "f forall = forall + 1"                       ==> ["f"]
        , "a # b = apply a b"                           ==> ["#"]
        ]
    strictMatchTests = testGroup "strict match (!)"
      [ "f !x y = x"                ==> ["f"]
      , "f x !y = x"                ==> ["f"]
      , "f !x !y = x"               ==> ["f"]
      , "f ! x y = x"               ==> ["f"]
        -- this one is a bit controversial but it seems to be the way ghc
        -- parses it
      , "f ! x = x"                 ==> ["f"]
      , "(*:) !(x :+: y) z = x"    ==> ["*:"]
      , "(*:) !(!x :+: !y) !z = x" ==> ["*:"]
      , "(*:) !((:+:) x y) z = x"  ==> ["*:"]
      , "(*:) !((:+:) !x !y) !z = x" ==> ["*:"]
      , """
        (!) :: a -> b -> a
        (!) x y = x
        """
        ==>
        ["!"]
        -- this is a degenerate case since even ghc treats ! here as
        -- a BangPatterns instead of operator
      , "x ! y = x" ==> ["x"]
      ]
    lazyMatchTests = testGroup "lazy match (~)"
      [ "f ~x y = x"  ==> ["f"]
      , "f x ~y = x"  ==> ["f"]
      , "f ~x ~y = x" ==> ["f"]
      , "f ~ x y = x" ==> ["f"]
        -- this one is a bit controversial but it seems to be the way ghc
        -- parses it
      , "f ~ x = x"   ==> ["f"]
      , "(*:) ~(x :+: y) z = x" ==> ["*:"]
      , "(*:) ~(~x :+: ~y) ~z = x" ==> ["*:"]
      , "(*:) ~((:+:) x y) z = x" ==> ["*:"]
      , "(*:) ~((:+:) ~x ~y) ~z = x" ==> ["*:"]
      , """
        (~) :: a -> b -> a
        (~) x y = x
        """
        ==>
        ["~"]
        -- this is a degenerate case since even ghc treats ~ here as
        -- a BangPatterns instead of operator
      , "x ~ y = x" ==> ["x"]
      ]
    atPatternsTests = testGroup "at patterns (@)"
      [ "f z@x y    = z"                        ==> ["f"]
      , "f x   z'@y = z'"                       ==> ["f"]
      , "f z@x z'@y = z"                        ==> ["f"]
      , "f z@(Foo _) z'@y = z"                  ==> ["f"]
      , "f z@(Foo _) z'@(Bar _) = z"            ==> ["f"]
      , "f z @ x y  = z"                        ==> ["f"]
      , "f z @ (x : xs) = z: [x: xs]"           ==> ["f"]
      , "f z @ (x : zs @ xs) = z: [x: zs]"      ==> ["f"]
      , "f z @ (zz @x : zs @ xs) = z: [zz: zs]" ==> ["f"]

      , "(*:) zzz@(x :+: y) z = x"             ==> ["*:"]
      , "(*:) zzz@(zx@x :+: zy@y) zz@z = x"    ==> ["*:"]
      , "(*:) zzz@((:+:) x y) z = x"           ==> ["*:"]
      , "(*:) zzz@((:+:) zs@x zs@y) zz@z = x"  ==> ["*:"]

      , "f z@(!x) ~y = x"                       ==> ["f"]
      ]

testClass :: TestTree
testClass = testGroup "class"
  [ """
    class (X x) => C a b where
    \tm :: a->b
    \tn :: c
    """
    ==>
    ["C", "m", "n"]
  , """
    class (X x) ⇒ C a b where
    \tm ∷ a→b
    \tn ∷ c
    """
    ==>
    ["C", "m", "n"]
  , """
    class (X x) => C a b | a -> b where
    \tm :: a->b
    \tn :: c
    """
    ==>
    ["C", "m", "n"]
  , """
    class (X x) ⇒ C a b | a → b where
    \tm ∷ a→b
    \tn ∷ c
    """
    ==>
    ["C", "m", "n"]
  , """
    class A a where f :: X
    """
    ==>
    ["A", "f"]
    -- indented inside where
  , """
    class X where
    \ta, (+) :: X
    """
    ==>
    ["+", "X", "a"]
  , """
    class X where
    \ta :: X
    \tb, c :: Y
    """
    ==>
    ["X", "a", "b", "c"]
  , """
    class X
    \twhere
    \ta :: X
    \tb, c :: Y
    """
    ==>
    ["X", "a", "b", "c"]
  , """
    class X
    \twhere
    \ta ::
    \t\tX
    \tb :: Y
    """
    ==>
    ["X", "a", "b"]

  , """
    class a :<: b where
        f :: a -> b
    """
    ==>
    [":<:", "f"]
  , """
    class (:<:) a b where
        f :: a -> b
    """
    ==>
    [":<:", "f"]
  , """
    class Eq a => a :<: b where
        f :: a -> b
    """
    ==>
    [":<:", "f"]
  , """
    class a ~ 'Foo => a :<: b where
        f :: a -> b
    """
    ==>
    [":<:", "f"]
  , """
    class 'Foo ~ a => a :<: b where
        f :: a -> b
    """
    ==>
    [":<:", "f"]
  , """
    class (Eq a) => a :<: b where
        f :: a -> b
    """
    ==>
    [":<:", "f"]
  , """
    class (a ~ 'Foo) => a :<: b where
        f :: a -> b
    """
    ==>
    [":<:", "f"]
  , """
    class ('Foo ~ a) => a :<: b where
        f :: a -> b
    """
    ==>
    [":<:", "f"]
  , """
    class a :<<<: b => a :<: b where
        f :: a -> b
    """
    ==>
    [":<:", "f"]
  , """
    class (a :<<<: b) => a :<: b where
        f :: a -> b
    """
    ==>
    [":<:", "f"]
  , """
    class (a :<<<: b) ⇒ a :<: b where
        f ∷ a → b
    """
    ==>
    [":<:", "f"]
  , """
    class (Eq a, Ord b) => a :<: b where
        f :: a -> b
    """
    ==>
    [":<:", "f"]
  , """
    class (Eq a, Ord b) => (a :: (* -> *) -> *) :<: b where
        f :: a -> b
    """
    ==>
    [":<:", "f"]
    -- this is bizzarre
  , "class (Eq (a), Ord (f a [a])) => f `Z` a" ==> ["Z"]

  , """
    class A f where
      data F f :: *
      g :: a -> f a
      h :: f a -> a
    """
    ==>
    ["A", "F", "g", "h"]
  , """
    class A f where
      data F f :: *
      mkF :: f -> F f
      getF :: F f -> f
    """
    ==>
    ["A", "F", "getF", "mkF"]
  , """
    class A f where
      data F f :: * -- foo
                    -- bar
                    -- baz
      mkF  :: f -> F f
      getF :: F f -> f
    """
    ==>
    ["A", "F", "getF", "mkF"]
    -- Not confused by a class context on a method.
  , """
    class X a where
    \tfoo :: Eq a => a -> a
    """ ==> ["X", "foo"]
  , """
    class Category cat where
        -- | the identity morphism
        id :: cat a a

        -- | morphism composition
        (.) :: cat b c -> cat a b -> cat a c
    """
    ==>
    [".", "Category", "id"]
  , """
    class Match a b where
        pattern :: Pattern a b
    """ ==> ["Match", "pattern"]
  , "class a ~~ b => (a :: k) ~ (b :: k) | a -> b, b -> a"
    ==>
    ["~"]
  , "class a ~~ b => (a :: k) ! (b :: k) | a -> b, b -> a"
    ==>
    ["!"]
  , """
    class A f where {
      data F f :: * ; -- foo
                      -- bar
                      -- baz
      mkF  :: f -> F f ; getF :: F f -> f ;
    } ;
    """
    ==>
    ["A", "F", "getF", "mkF"]
  ]
  where
    (==>) = testTagNames LitVanilla

testInstance :: TestTree
testInstance = testGroup "instance"
  [ """
    instance Foo Quux where
      data Bar Quux a = QBar { frob :: a }
                      | QBaz { fizz :: String }
                      deriving (Show)
    """
    ==>
    ["QBar", "QBaz", "fizz", "frob"]
  , """
    instance Foo Quux where
      data Bar Quux a = QBar a | QBaz String deriving (Show)
    """
    ==>
    ["QBar", "QBaz"]
  , """
    instance Foo Quux where
      data Bar Quux a = QBar { frob :: a }
                      | QBaz { fizz :: String }
                      deriving (Show)
      data IMRuunningOutOfNamesHere Quux = Whatever
    """
    ==>
    ["QBar", "QBaz", "Whatever", "fizz", "frob"]
    -- in this test foo function should not affect tags found
  , """
    instance Foo Quux where
      data Bar Quux a = QBar { frob :: a }
                      | QBaz { fizz :: String }
                      deriving (Show)

      foo _ = QBaz "hey there"
    """
    ==>
    ["QBar", "QBaz", "fizz", "frob"]
  , "instance Foo Int where foo _ = 1"
    ==>
    []
  , """
    instance Foo Quux where
      newtype Bar Quux a = QBar a
                         deriving (Show)

      foo _ = QBaz "hey there"
    """
    ==>
    ["QBar"]
  , """
    instance Foo Quux where
      newtype Bar Quux a = QBar { frob :: a }
    """
    ==>
    ["QBar", "frob"]
  , """
    instance (Monoid w, MBC b m) => MBC b (JournalT w m) where
       newtype StM (JournalT w m) a =
           StMJournal { unStMJournal :: ComposeSt (JournalT w) m a }
       liftBaseWith = defaultLiftBaseWith StMJournal
       restoreM     = defaultRestoreM   unStMJournal
       {-# INLINE liftBaseWith #-}
       {-# INLINE restoreM #-}

    """
    ==>
    ["StMJournal", "unStMJournal"]
  ]
  where
    (==>) = testTagNames LitVanilla

testLiterate :: TestTree
testLiterate = testGroup "Literate"
  [ """
    > class (X x) => C a b where
    >\tm :: a->b
    >\tn :: c
    """
    ==>
    ["C", "m", "n"]
  , """
    Test
    \\begin{code}
    class (X x) => C a b where
    \tm :: a->b
    \tn :: c
    \\end{code}
    """
    ==>
    ["C", "m", "n"]
  , """
    > precalcClosure0 :: Grammar -> Name -> RuleList
    > precalcClosure0 g =
    >\t\\n -> case lookup n info' of
    >\t\tNothing -> []
    >\t\tJust c  -> c
    >  where
    """
    ==>
    ["precalcClosure0"]
  , """
    New Resolutions by Jean-Luc Ponty, Scott O'Neil, and John Garvin\r
    \r
    > module Euterpea.Examples.NewResolutions where\r
    > import Euterpea\r
    \r
    > nrContext = Context {cTime = 0,\r
    >                      cPlayer = fancyPlayer,\r
    >                      cInst = Marimba,\r
    >                      cDur = 1.0,\r
    >                      cPch = 0,\r
    >                      cKey = (C,Major),\r
    >                      cVol = 100}\r
    >\r
    > tNewRes m = makeMidi (m, nrContext, defUpm)\r
    \r
    > root, minThird, fifth, octave :: Pitch -> Dur -> Music Pitch\r
    > root       p dur = Prim $ Note dur p\r
    > minThird   p dur = Prim $ Note dur (trans 3 p)\r
    > majThird   p dur = Prim $ Note dur (trans 4 p)\r
    > fifth      p dur = Prim $ Note dur (trans 7 p)\r
    > majSixth   p dur = Prim $ Note dur (trans 9 p)\r
    > minSeventh p dur = Prim $ Note dur (trans 10 p)\r
    > octave     p dur = Prim $ Note dur (trans 12 p)\r
    > oMinThird  p dur = Prim $ Note dur (trans 15 p)\r
    > oFifth     p dur = Prim $ Note dur (trans 19 p)
    """
    ==>
    [ "NewResolutions"
    , "fifth"
    , "majSixth"
    , "majThird"
    , "minSeventh"
    , "minThird"
    , "nrContext"
    , "oFifth"
    , "oMinThird"
    , "octave"
    , "root"
    , "tNewRes"
    ]
  ]
  where
    (==>) = testTagNames LitOutside

testPatterns :: TestTree
testPatterns = testGroup "patterns"
  [ "pattern Arrow a b = ConsT \"->\" [a, b]"
    ==>
    ["Arrow"]
  , """
    pattern Arrow a b = ConsT "->" [a, b]
    pattern Pair a b = [a, b]
    """
    ==>
    ["Arrow", "Pair"]
  , """
    pattern Sub a b = Op '-' [a, b]
    pattern Pair a b = [a, b]
    """
    ==>
    ["Pair", "Sub"]
  , "pattern (:++) x y = [x, y]"
    ==>
    [":++"]
  , "pattern x :** y = [x, y]"
    ==>
    [":**"]
  , """
    pattern Nil :: Vec2 a
    pattern Nil = Vec2 []
    """
    ==>
    ["Nil", "Nil"]
  , """
    pattern (:>) x xs <- ((\\ys -> (head $ unvec2 ys,Vec2 . tail $ unvec2 ys)) -> (x,xs))
    where
       (:>) x xs = Vec2 (x:unvec2 xs)
    """
    ==>
    [":>"]
  , """

    data Foo = Foo_ { _foo :: !(Last String) } deriving (Eq)
    n\
    \pattern Bar :: A -> B
    pattern Bar { foo } = Foo_ (Last foo)
    {-# COMPLETE Bar #-}
    """
    ==>
    ["Bar", "Foo", "Foo_", "_foo", "foo"]
  , """

    data Foo = Foo_ { _foo :: !(Last String), _bar :: !(Last String) } deriving (Eq)
    n\
    \pattern Bar :: A -> B
    pattern Bar { foo, bar } = Foo_ (Last foo) (Last bar)
    {-# COMPLETE Bar #-}
    """
    ==>
    ["Bar", "Foo", "Foo_", "_bar", "_foo", "bar", "foo"]
  ]
  where
    (==>) = testTagNames LitVanilla

testFFI :: TestTree
testFFI = testGroup "ffi"
  [ "foreign import ccall foo :: Double -> IO Double"            ==> ["foo"]
  , "foreign import unsafe java foo :: Double -> IO Double"      ==> ["foo"]
  , "foreign import safe stdcall foo :: Double -> IO Double"     ==> ["foo"]
  , "foreign import safe stdcall pattern :: Double -> IO Double" ==> ["pattern"]
  ]
  where
    (==>) = testTagNames LitVanilla

testDefine :: TestTree
testDefine = testGroup "preprocessor defines"
  [ "#define FOO 1" ==>
    ["FOO"]
  , """
    #define FOO
     1
    """ ==>
    ["FOO"]
  , """
    #define FOO
    1
    """ ==>
    ["FOO"]
  , "#define FOO(x) (x + x)" ==>
    ["FOO"]
  , """
    #if X
    #define FOO 1
    #else
    #define FOO 2
    #endif
    """ ==>
    ["FOO"]
  , """
    #if X

    #define FOO 1


    #else


    #define FOO 2

    #endif
    """ ==>
    ["FOO"]
  , "#define FOO(x) (x + x)" ==>
    ["FOO"]
  , "#let BAR x y z = \"x + y + z\"" ==>
    ["BAR"]
  ]
  where
    (==>) = testTagNames LitVanilla

testHSC2HS :: TestTree
testHSC2HS = testGroup "hsc2hs"
  [ """
    #{enum ControlOp, ControlOp
     , controlOpAdd    = EPOLL_CTL_ADD
     , controlOpModify = EPOLL_CTL_MOD
     , controlOpDelete = EPOLL_CTL_DEL
     }
    """
    ==>
    [ "controlOpAdd", "controlOpDelete", "controlOpModify"
    ]
  , """
    #{
    enum ControlOp, ControlOp
     , controlOpAdd    = EPOLL_CTL_ADD
     , controlOpModify = EPOLL_CTL_MOD
     , controlOpDelete = EPOLL_CTL_DEL
     }
    """
    ==>
    [ "controlOpAdd", "controlOpDelete", "controlOpModify"
    ]
  , """
    #{enum Test1, Test2
     , foo
     , foo_bar
     , BAR_BAZ
     , BAR_BAZquux
     }
    """
    ==>
    [ "barBaz", "barBazquux", "foo", "fooBar"
    ]
  , """
    #enum Mask, UserSpace, IN_ACCESS, IN_MODIFY, IN_ATTRIB, IN_CLOSE_WRITE
    #enum Mask, UserSpace, IN_CLOSE_NOWRITE, IN_OPEN, IN_MOVED_FROM, IN_MOVED_TO
    #enum Mask, UserSpace, IN_CREATE, IN_DELETE, IN_DELETE_SELF, IN_MOVE_SELF
    """
    ==>
    [ "inAccess", "inAttrib", "inCloseNowrite", "inCloseWrite"
    , "inCreate", "inDelete", "inDeleteSelf", "inModify"
    , "inMoveSelf", "inMovedFrom", "inMovedTo", "inOpen"
    ]

  , """
    #enum ExecOption,ExecOption, \\
      execAnchored = PCRE_ANCHORED, \\
      execNotBOL = PCRE_NOTBOL, \\
      execNotEOL = PCRE_NOTEOL, \\
      execNotEmpty = PCRE_NOTEMPTY, \\
      execNoUTF8Check = PCRE_NO_UTF8_CHECK, \\
      execPartial = PCRE_PARTIAL
    """
    ==>
    [ "execAnchored"
    , "execNoUTF8Check"
    , "execNotBOL"
    , "execNotEOL"
    , "execNotEmpty"
    , "execPartial"
    ]

  , """
    #enum ReturnCode,ReturnCode, \\
      retNoMatch = PCRE_ERROR_NOMATCH, \\
      retNull = PCRE_ERROR_NULL, \\
      retBadOption = PCRE_ERROR_BADOPTION, \\
      retBadMagic = PCRE_ERROR_BADMAGIC, \\
      retUnknownNode = PCRE_ERROR_UNKNOWN_NODE, \\
      retNoMemory = PCRE_ERROR_NOMEMORY, \\
      retNoSubstring = PCRE_ERROR_NOSUBSTRING
    """
    ==>
    [ "retBadMagic"
    , "retBadOption"
    , "retNoMatch"
    , "retNoMemory"
    , "retNoSubstring"
    , "retNull"
    , "retUnknownNode"
    ]

  , """
    #{
    define hsc_patsyn(l, typ, cons, hprefix, recmac) { \\
      struct { const char *s; unsigned n; } *p, list[] = { LLVM_HS_FOR_EACH_ ## l(recmac) }; \\
      for(p = list; p < list + sizeof(list)/sizeof(list[0]); ++p) { \\
        hsc_printf("pattern " #hprefix "%s :: " #typ "\\n", p->s); \\
        hsc_printf("pattern " #hprefix "%s =  " #cons " %u\\n", p->s, p->n); \\
      }\\
    }\\
    }\\

    foo x = x
    """
    ==>
    [ "foo"
    ]
  ]
  where
    (==>) = testTagNames LitVanilla

testUnicode :: TestTree
testUnicode = testGroup "Unicode"
  [ "foo = ()" ==> ["foo"]
  , "привет = ()" ==> ["привет"]
  , "猫 = ()" ==> ["猫"]
  , "foo x = x * x" ==> ["foo"]
  , "привет x = x * x" ==> ["привет"]
  , "自乗 x = x * x" ==> ["自乗"]
  ]
  where
    (==>) = testTagNames LitVanilla
