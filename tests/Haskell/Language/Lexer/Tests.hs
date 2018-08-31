----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer.Tests
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Thursday,  3 November 2016
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Haskell.Language.Lexer.Tests (tests) where

import Test.Tasty
import Test.Tasty.HUnit (testCase)

import Control.Arrow (first)
import Data.Functor.Identity
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc.Ext as PP
import GHC.Stack (HasCallStack)

import Haskell.Language.Lexer.FastTags (TokenVal(..), PragmaType(..), Token, TagVal(..), Pos(..), UnstrippedTokens(..), Type(..), SrcPos(..), Line(..))
import qualified Haskell.Language.Lexer.FastTags as Tag

import Haskell.Language.Lexer (LiterateMode(..))
import qualified Haskell.Language.Lexer as Lexer
import TestUtils (makeAssertion, makeTest)

tests :: TestTree
tests = testGroup "Lexer tests"
  [ testTokenize
  , testTokenizeWithNewlines
  , testStripComments
  , testBreakBlocks
  , testProcess
  , testTokenizeCpp
  , testFullPipeline
  ]

testTokenize :: TestTree
testTokenize = testGroup "Tokenize"
  [ "xyz  -- abc"          ==> [T "xyz", Newline 0]
  , "xyz  --- abc"         ==> [T "xyz", Newline 0]
  , "  {-   foo -}"        ==> [Newline 0]
  , "  {-   foo \n\
    \\n\
    \-}"                   ==> [Newline 0]
  , "  {- foo {- bar-} -}" ==> [Newline 0]
  , "  {-# INLINE #-}"     ==> [Newline 0]
  , "a::b->c"              ==> [T "a", DoubleColon, T "b", Arrow, T "c", Newline 0]
  , "a∷b→c"                ==> [T "a", DoubleColon, T "b", Arrow, T "c", Newline 0]
  , "x{-\n  bc#-}\n"       ==> [T "x", Newline 0]
  , "X.Y"                  ==> [T "X.Y", Newline 0]
  , "a=>b"                 ==> [T "a", Implies, T "b", Newline 0]
  , "a ⇒ b"                ==> [T "a", Implies, T "b", Newline 0]
  , "x9"                   ==> [T "x9", Newline 0]
  -- , "9x"                ==> ["nl 0", "9", "x"]
  , "x :+: y"              ==> [T "x", T ":+:", T "y", Newline 0]
  , "(#$)"                 ==> [LParen, T "#$", RParen, Newline 0]
  , "Data.Map.map"         ==> [T "Data.Map.map", Newline 0]
  , "Map.map"              ==> [T "Map.map", Newline 0]
  , "forall a. f a"        ==> [T "forall", T "a", Dot, T "f", T "a", Newline 0]
  , "forall a . Foo"       ==> [T "forall", T "a", Dot, T "Foo", Newline 0]
  , "forall a. Foo"        ==> [T "forall", T "a", Dot, T "Foo", Newline 0]
  , "forall a .Foo"        ==> [T "forall", T "a", Dot, T "Foo", Newline 0]
  , "forall a.Foo"         ==> [T "forall", T "a", Dot, T "Foo", Newline 0]
  , "$#-- hi"              ==> [T "$#--", T "hi", Newline 0]
  , "(*), (-)"             ==> [LParen, T "*", RParen, Comma, LParen, T "-", RParen, Newline 0]
  , "(.::)"                ==> [LParen, T ".::", RParen, Newline 0]
  -- we rely on this behavior
  , "data (:+) a b"        ==> [KWData, LParen, T ":+", RParen, T "a", T "b", Newline 0]
  , "data (.::+::) a b"    ==> [KWData, LParen, T ".::+::", RParen, T "a", T "b", Newline 0]
  -- string tokenization
  , "foo \"bar\" baz"      ==> [T "foo", String, T "baz", Newline 0]
  -- multiline string
  , "foo \"bar\\\n\
    \  \\bar\" baz"        ==> [T "foo", String, T "baz", Newline 0]
  , "(\\err -> case err of Foo -> True; _ -> False)"
    ==>
    [LParen, T "\\", T "err", Arrow, KWCase, T "err", KWOf, T "Foo", Arrow, T "True", T "_", Arrow, T "False", RParen, Newline 0]
  , "foo = \"foo\\n\\\n\
    \  \\\" bar"
    ==>
    [T "foo", Equals, String, T "bar", Newline 0]
  , "foo = \"foo\\n\\\n\
    \  \\x\" bar"
    ==>
    [T "foo", Equals, String, T "bar", Newline 0]
  , tokenizeSplices
  , "import Foo hiding (Bar)"                ==>
    [KWImport, T "Foo", T "hiding", LParen, T "Bar", RParen, Newline 0]
  , "import {-# SOURCE #-} Foo hiding (Bar)" ==>
    [KWImport, Pragma SourcePragma, T "Foo", T "hiding", LParen, T "Bar", RParen, Newline 0]
  , "import {-# source #-} Foo hiding (Bar)" ==>
    [KWImport, Pragma SourcePragma, T "Foo", T "hiding", LParen, T "Bar", RParen, Newline 0]
  , "import {-# SoUrCe #-} Foo hiding (Bar)" ==>
    [KWImport, Pragma SourcePragma, T "Foo", T "hiding", LParen, T "Bar", RParen, Newline 0]
  ]
  where
    (==>) = makeTest f
    f :: Text -> [TokenVal]
    f = tail -- strip uninteresting initial newline
      . map valOf
      . tokenize' filename Vanilla

    tokenizeSplices = testGroup "Splices"
      [ "$(foo)"                              ==>
        [SpliceStart, T "foo", RParen, Newline 0]
      , "$(foo [| baz |])"                    ==>
        [SpliceStart, T "foo", QuasiquoterStart, QuasiquoterEnd, RParen, Newline 0]
      , "$(foo [bar| baz |])"                 ==>
        [SpliceStart, T "foo", QuasiquoterStart, QuasiquoterEnd, RParen, Newline 0]
      , "$(foo [bar| bar!\nbaz!\n $(baz) |])" ==>
        [SpliceStart, T "foo", QuasiquoterStart, SpliceStart, T "baz", RParen, QuasiquoterEnd, RParen, Newline 0]
      ]

testTokenizeWithNewlines :: TestTree
testTokenizeWithNewlines = testGroup "Tokenize with newlines"
  [ "1\n2\n"     ==> [Newline 0, T "1", Newline 0, T "2", Newline 0]
  , " 11\n 11\n" ==> [Newline 1, T "11", Newline 1, T "11", Newline 0]
  , "foo bar baz"
    |=>
    []
  , "foo bar baz\nquux fizz buzz"
    |=>
    []
  , "> foo = 1"
    |=>
    [Newline 1, T "foo", Equals, T "1", Newline 0]
  , "This is a factorial function\n\
    \\n\
    \> factorial :: Integer -> Integer\n\
    \> factorial 0 = 1\n\
    \> factorial n = \n\
    \>   n * (factorial $ n - 1)\n\
    \\n\
    \And that's it !"
    |=>
    -- TODO: maybe keep track of minimal newline indent across whole file
    -- and subtract it from all newlines at the end?
    [ Newline 1
    , T "factorial", DoubleColon, T "Integer", Arrow, T "Integer", Newline 1
    , T "factorial", T "0", Equals, T "1", Newline 1
    , T "factorial", T "n", Equals, Newline 3
    , T "n", T "*", LParen, T "factorial", T "$", T "n", T "-", T "1", RParen, Newline 0
    ]
  , "This is a factorial function\n\
    \\n\
    \> factorial :: Integer -> Integer\n\
    \> factorial 0 = 1\n\
    \> factorial n = \n\
    \>   n * (factorial $ n - 1)\n\
    \\n\
    \And another function:\n\
    \\n\
    \> foo :: a -> a\n\
    \> foo x = x"
    |=>
    -- TODO: maybe keep track of minimal newline indent across whole file
    -- and subtract it from all newlines at the end?
    [ Newline 1
    , T "factorial", DoubleColon, T "Integer", Arrow, T "Integer", Newline 1
    , T "factorial", T "0", Equals, T "1", Newline 1
    , T "factorial", T "n", Equals, Newline 3
    , T "n", T "*", LParen, T "factorial", T "$", T "n", T "-", T "1", RParen, Newline 0
    , Newline 1
    , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 1
    , T "foo", T "x", Equals, T "x", Newline 0
    ]
  , "This is a factorial function\n\
    \\\begin{code}\n\
    \factorial :: Integer -> Integer\n\
    \factorial 0 = 1\n\
    \factorial n = \n\
    \  n * (factorial $ n - 1)\n\
    \\\end{code}\n\
    \And that's it !"
    |=>
    [ Newline 1
    , T "factorial", DoubleColon, T "Integer", Arrow, T "Integer", Newline 0
    , T "factorial", T "0", Equals, T "1", Newline 0
    , T "factorial", T "n", Equals, Newline 2
    , T "n", T "*", LParen, T "factorial", T "$", T "n", T "-", T "1", RParen, Newline 0
    ]
  , "This is a factorial function\n\
    \\\begin{code}\n\
    \factorial :: Integer -> Integer\n\
    \factorial 0 = 1\n\
    \factorial n = \n\
    \  n * (factorial $ n - 1)\n\
    \\\end{code}\n\
    \But that's not it yet! Here's another function:\n\
    \\\begin{code}\n\
    \foo :: a -> a\n\
    \foo x = x\n\
    \\\end{code}\n\
    \And that's it !"
    |=>
    [ Newline 1
    , T "factorial", DoubleColon, T "Integer", Arrow, T "Integer", Newline 0
    , T "factorial", T "0", Equals, T "1", Newline 0
    , T "factorial", T "n", Equals, Newline 2
    , T "n", T "*", LParen, T "factorial", T "$", T "n", T "-", T "1", RParen, Newline 0
    , Newline 1
    , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
    , T "foo", T "x", Equals, T "x", Newline 0
    ]
  ]
  where
    (==>) = makeTest (f Vanilla)
    (|=>) = makeTest (f Literate)
    f mode =
        map valOf
      . tokenize' filename mode

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
  , "hello -- {- there -}\n\
    \fred"
    ==>
    [Newline 0, T "hello", Newline 0, T "fred", Newline 0]
  , "{-# LANG #-} hello {- there {- nested -} comment -} fred"
    ==>
    [Newline 0, T "hello", T "fred", Newline 0]
  , "hello {-\n\
    \there\n\
    \------}\n\
    \ fred"
    ==>
    [Newline 0, T "hello", Newline 1, T "fred", Newline 0]
  , "hello {-  \n\
    \there\n\
    \  ------}  \n\
    \ fred"
    ==>
    [Newline 0, T "hello", Newline 1, T "fred", Newline 0]
  , "hello {-\n\
    \there\n\
    \-----}\n\
    \ fred"
    ==>
    [Newline 0, T "hello", Newline 1, T "fred", Newline 0]
  , "hello {-  \n\
    \there\n\
    \  -----}  \n\
    \ fred"
    ==>
    [Newline 0, T "hello", Newline 1, T "fred", Newline 0]
  , "hello {-\n\
    \-- there -}"
    ==>
    [Newline 0, T "hello", Newline 0]
  , "foo --- my comment\n\
    \--- my other comment\n\
    \bar"
    ==>
    [Newline 0, T "foo", Newline 0, Newline 0, T "bar", Newline 0]
  ]
  where
    (==>) = makeTest f
    f = map valOf . tokenize' filename Vanilla

testBreakBlocks :: TestTree
testBreakBlocks = testGroup "Break blocks"
  [ testGroup "non-literate"
    [ "a\n\
      \b\n"
      ==>
      [ [T "a"]
      , [T "b"]
      ]
    , "a\n\
      \ a\n\
      \b\n"
      ==>
      [ [T "a", Newline 1, T "a"]
      , [T "b"]
      ]
    , "a\n\
      \ a\n\
      \ a\n\
      \b\n"
      ==>
      [ [T "a", Newline 1, T "a", Newline 1, T "a"]
      , [T "b"]
      ]
    -- intervening blank lines are ignored
    , "a\n\
      \ a\n\
      \\n\
      \ a\n\
      \b\n"
      ==>
      [ [T "a", Newline 1, T "a", Newline 1, T "a"]
      , [T "b"]
      ]
    , "a\n\
      \\n\
      \\n\
      \ a\n\
      \b\n"
      ==>
      [ [T "a", Newline 1, T "a"]
      , [T "b"]
      ]
    , "a\n\
      \ aa\n\
      \ aa\n"
      ==>
      [[T "a", Newline 1, T "aa", Newline 1, T "aa"]]
    , " aa\n\
      \ aa\n"
      ==>
      [ [T "aa"]
      , [T "aa"]
      ]
    ]
  , testGroup "literate"
    [ "> a\n\
      \>\n\
      \>\n\
      \>  a\n\
      \> b\n"
      |=>
      [ [T "a", Newline 2, T "a"]
      , [T "b"]
      ]
    , "> a\n\
      \> \n\
      \> \n\
      \>  a\n\
      \> b\n"
      |=>
      [ [T "a", Newline 2, T "a"]
      , [T "b"]
      ]
    , "> a\n\
      \>  aa\n\
      \>  aa\n"
      |=>
      [[T "a", Newline 2, T "aa", Newline 2, T "aa"]]
    , "> a\n\
      \>  aa\n\
      \>\n\
      \>  aa\n"
      |=>
      [[T "a", Newline 2, T "aa", Newline 2, T "aa"]]
    ]
  ]
  where
    (==>) = makeTest (f Vanilla)
    (|=>) = makeTest (f Literate)
    f mode =
        map (map valOf . Tag.unstrippedTokensOf)
      . Tag.breakBlocks
      . UnstrippedTokens
      . tokenize' filename mode

testProcess :: TestTree
testProcess = testGroup "Process"
  [ testPrefixes
  , testData
  , testGADT
  , testFamilies
  , testFunctions
  , testClass
  , testInstance
  , testLiterate
  , testPatterns
  , testFFI
  ]

testPrefixes :: TestTree
testPrefixes = testGroup "Prefix tracking"
  [ "module Bar.Foo where\n"
    ==>
    [Pos (SrcPos fn 1 "") (TagVal "Foo" Module Nothing)]
  , "newtype Foo a b =\n\
    \\tBar x y z\n"
    ==>
    [ Pos (SrcPos fn 1 "") (TagVal "Foo" Type Nothing)
    , Pos (SrcPos fn 2 "") (TagVal "Bar" Constructor (Just "Foo"))
    ]
  , "data Foo a b =\n\
    \\tBar x y z\n"
    ==>
    [ Pos (SrcPos fn 1 "") (TagVal "Foo" Type Nothing)
    , Pos (SrcPos fn 2 "") (TagVal "Bar" Constructor (Just "Foo"))
    ]
  , "f :: A -> B\n\
    \g :: C -> D\n\
    \data D = C {\n\
    \\tf :: A\n\
    \\t}\n"
    ==>
    [ Pos (SrcPos fn 1 "") (TagVal "f" Function Nothing)
    , Pos (SrcPos fn 2 "") (TagVal "g" Function Nothing)
    , Pos (SrcPos fn 3 "") (TagVal "C" Constructor (Just "D"))
    , Pos (SrcPos fn 3 "") (TagVal "D" Type Nothing)
    , Pos (SrcPos fn 4 "") (TagVal "f" Function (Just "D"))
    ]
  , "instance Foo Bar where\n\
    \  newtype FooFam Bar = BarList [Int]"
    ==>
    [ Pos (SrcPos fn 2 "") (TagVal "BarList" Constructor (Just "FooFam"))
    ]
  , "instance Foo Bar where\n\
    \  newtype FooFam Bar = BarList { getBarList :: [Int] }"
    ==>
    [ Pos (SrcPos fn 2 "") (TagVal "BarList" Constructor (Just "FooFam"))
    , Pos (SrcPos fn 2 "") (TagVal "getBarList" Function (Just "FooFam"))
    ]
  , "instance Foo Bar where\n\
    \  data (Ord a) => FooFam Bar a = BarList { getBarList :: [a] }\n\
    \                               | BarMap { getBarMap :: Map a Int }"
    ==>
    [ Pos (SrcPos fn 2 "") (TagVal "BarList" Constructor (Just "FooFam"))
    , Pos (SrcPos fn 2 "") (TagVal "getBarList" Function (Just "FooFam"))
    , Pos (SrcPos fn 3 "") (TagVal "BarMap" Constructor (Just "FooFam"))
    , Pos (SrcPos fn 3 "") (TagVal "getBarMap" Function (Just "FooFam"))
    ]
  , "newtype instance FooFam Bar = BarList { getBarList :: [Int] }"
    ==>
    [ Pos (SrcPos fn 1 "")              (TagVal "BarList" Constructor (Just "FooFam"))
    , Pos (SrcPos fn 1 "") (TagVal "getBarList" Function (Just "FooFam"))
    ]
  , "data instance (Ord a) => FooFam Bar a = BarList { getBarList :: [a] }\n\
    \                                      | BarMap { getBarMap :: Map a Int }"
    ==>
    [ Pos (SrcPos fn 1 "") (TagVal "BarList" Constructor (Just "FooFam"))
    , Pos (SrcPos fn 1 "") (TagVal "getBarList" Function (Just "FooFam"))
    , Pos (SrcPos fn 2 "") (TagVal "BarMap" Constructor (Just "FooFam"))
    , Pos (SrcPos fn 2 "") (TagVal "getBarMap" Function (Just "FooFam"))
    ]
  ]
  where
    (==>) = testFullTagsWithoutPrefixes fn Vanilla
    fn = filename

testData :: TestTree
testData = testGroup "Data"
  [ "data X\n"                            ==> ["X"]
  , "data X = X Int\n"                    ==> ["X", "X"]
  , "data Foo = Bar | Baz"                ==> ["Bar", "Baz", "Foo"]
  , "data Foo =\n\tBar\n\t| Baz"          ==> ["Bar", "Baz", "Foo"]
  -- Records.
  , "data Foo a = Bar { field :: Field }" ==> ["Bar", "Foo", "field"]
  , "data R = R { a::X, b::Y }"           ==> ["R", "R", "a", "b"]
  , "data R = R { a∷X, b∷Y }"             ==> ["R", "R", "a", "b"]
  , "data R = R {\n\ta::X\n\t, b::Y\n\t}" ==> ["R", "R", "a", "b"]
  , "data R = R {\n\ta,b::X\n\t}"         ==> ["R", "R", "a", "b"]
    -- Record operators
    , "data Foo a b = (:*:) { foo :: a, bar :: b }" ==>
        [":*:", "Foo", "bar", "foo"]

  , "data R = R {\n\
    \    a :: !RealTime\n\
    \  , b :: !RealTime\n\
    \}"
    ==>
    ["R", "R", "a", "b"]
  , "data Rec = Rec {\n\
    \  a :: Int\n\
    \, b :: !Double\n\
    \, c :: Maybe Rec\
    \\n\
    \}"
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

  , "data Foo a = \n\
    \    Plain Int\n\
    \  | forall a. Bar a Int\n\
    \  | forall a b. Baz b a\n\
    \  | forall a . Quux a \
    \  | forall a .Quuz a"
    ==>
    ["Bar", "Baz", "Foo", "Plain", "Quux", "Quuz"]

  , "data X a = Add a "                                               ==> ["Add", "X"]
  , "data Eq a => X a = Add a"                                        ==> ["Add", "X"]
  , "data (Eq a) => X a = Add a"                                      ==> ["Add", "X"]
  , "data (Eq a, Ord a) => X a = Add a"                               ==> ["Add", "X"]
  , "data (Eq (a), Ord (a)) => X a = Add a"                           ==> ["Add", "X"]

  , "data Ref :<: f => X f = RRef f"                                  ==> ["RRef", "X"]
  , "data a :<: b => X a b = Add a"                                   ==> ["Add", "X"]
  , "newtype Ref :<: f => X f = RRef f"                               ==> ["RRef", "X"]
  , "newtype a :<: b => X a b = Add a"                                ==> ["Add", "X"]
  , "data Ref :<: [f] => X f = RRef f"                                ==> ["RRef", "X"]
  , "data [a] :<: b => X a b = Add a"                                 ==> ["Add", "X"]
  , "newtype Ref :<: [f] => X f = RRef f"                             ==> ["RRef", "X"]
  , "newtype [a] :<: b => X a b = Add a"                              ==> ["Add", "X"]

  , "data (a :<: b) => X a b = Add a"                                 ==> ["Add", "X"]
  , "data a :><: b = a :>|<: b"                                       ==> [":><:", ":>|<:"]
  , "data (:><:) a b = (:>|<:) a b"                                   ==> [":><:", ":>|<:"]
  , "data (:><:) a b = Foo b | (:>|<:) a b"                           ==> [":><:", ":>|<:", "Foo"]
  , "data (:><:) a b = Foo b | forall c. (:>|<:) a c"                 ==> [":><:", ":>|<:", "Foo"]
  , "data (:><:) a b = Foo b | forall c. (Eq c) => (:>|<:) a c"       ==> [":><:", ":>|<:", "Foo"]
  , "data (:><:) a b = Foo b | forall c. Eq c => (:>|<:) a c"         ==> [":><:", ":>|<:", "Foo"]

  , "newtype Eq a => X a = Add a"                                     ==> ["Add", "X"]
  , "newtype (Eq a) => X a = Add a"                                   ==> ["Add", "X"]
  , "newtype (Eq a, Ord a) => X a = Add a"                            ==> ["Add", "X"]

  , "newtype (u :*: v) z = X"                                         ==> [":*:", "X"]
  , "data (u :*: v) z = X"                                            ==> [":*:", "X"]
  , "type (u :*: v) z = (u, v, z)"                                    ==> [":*:"]

  , "newtype ((u :: (* -> *) -> *) :*: v) z = X"                      ==> [":*:", "X"]
  , "data ((u :: (* -> *) -> *) :*: v) z = X"                         ==> [":*:", "X"]
  , "type ((u :: (* -> *) -> *) :*: v) z = (u, v, z)"                 ==> [":*:"]

  , "newtype (Eq (v z)) => ((u :: (* -> *) -> *) :*: v) z = X"        ==> [":*:", "X"]
  , "data (Eq (v z)) => ((u :: (* -> *) -> *) :*: v) z = X"           ==> [":*:", "X"]
  , "type (Eq (v z)) => ((u :: (* -> *) -> *) :*: v) z = (u, v, z)"   ==> [":*:"]

  , "newtype Eq (v z) => ((u :: (* -> *) -> *) :*: v) z = X"          ==> [":*:", "X"]
  , "data Eq (v z) => ((u :: (* -> *) -> *) :*: v) z = X"             ==> [":*:", "X"]
  , "type Eq (v z) => ((u :: (* -> *) -> *) :*: v) z = (u, v, z)"     ==> [":*:"]

  , "newtype (Eq (v z)) => ((u :: (* -> *) -> *) `Foo` v) z = X"      ==> ["Foo", "X"]
  , "data (Eq (v z)) => ((u :: (* -> *) -> *) `Foo` v) z = X"         ==> ["Foo", "X"]
  , "type (Eq (v z)) => ((u :: (* -> *) -> *) `Foo` v) z = (u, v, z)" ==> ["Foo"]
  , "type (Eq (v z)) => ((u ∷ (* -> *) -> *) `Foo` v) z = (u, v, z)"  ==> ["Foo"]

  , "newtype Eq (v z) => ((u :: (* -> *) -> *) `Foo` v) z = X"        ==> ["Foo", "X"]
  , "data Eq (v z) => ((u :: (* -> *) -> *) `Foo` v) z = X"           ==> ["Foo", "X"]
  , "type Eq (v z) => ((u :: (* -> *) -> *) `Foo` v) z = (u, v, z)"   ==> ["Foo"]
  , "type Eq (v z) ⇒ ((u ∷ (* → *) → *) `Foo` v) z = (u, v, z)"       ==> ["Foo"]

  , "data (:*:) u v z = X"                                            ==> [":*:", "X"]
  , "data (Eq (u v), Ord (z)) => (:*:) u v z = X"                     ==> [":*:", "X"]
  , "data (u `W` v) z = X"                                            ==> ["W", "X"]
  , "data (Eq (u v), Ord (z)) => (u `W` v) z = X"                     ==> ["W", "X"]

  , "newtype X a = Z {\n\
    \ -- TODO blah\n\
    \ foo :: [a] }"
    ==>
    ["X", "Z", "foo"]
  , "newtype (u :*: v) z = X {\n\
    \ -- my insightful comment\n\
    \ extract :: (u (v z)) }"
    ==>
    [":*:", "X", "extract"]
  , "newtype (u :*: v) z = X {\n\
    \ -- my insightful comment\n\
    \ pattern :: (u (v z)) }"
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
  , "data Hadron a b = forall x y. Science { f :: [(Box x, Map x y)], h :: b }"
    ==>
    ["Hadron", "Science", "f", "h"]
  , "data Hadron a b = forall x y. Science { f ∷ [(Box x, Map x y)], h ∷ b }"
    ==>
    ["Hadron", "Science", "f", "h"]
  , "data Hadron a b = forall x y z. Science\n\
    \  { f :: x\n\
    \  , g :: [(Box x, Map x y, z)] \
    \  }"
    ==>
    ["Hadron", "Science", "f", "g"]
  , "data Hadron a b = forall x y z. Science\n\
    \  { f :: x\n\
    \  , g :: [(Box x, Map x y, z)] \
    \  , h :: b\n\
    \  }"
    ==>
    ["Hadron", "Science", "f", "g", "h"]
  , "data Hadron a b = Science { h :: b }"
    ==>
    ["Hadron", "Science", "h"]
  , "data Test a b =\n\
    \    Foo a\n\
    \  | Bar [(Maybe a, Map a b, b)]"
    ==>
    ["Bar", "Foo", "Test"]
  , "data Test a b =\n\
    \    Foo a\n\
    \  | [(Maybe b, Map b a, a)] `Bar` [(Maybe a, Map a b, b)]"
    ==>
    ["Bar", "Foo", "Test"]
  ]
  where
    (==>) = testTagNames filename Vanilla

testGADT :: TestTree
testGADT = testGroup "GADT"
  [ "data X where A :: X\n"              ==> ["A", "X"]
  , "data X where\n\tA :: X\n"           ==> ["A", "X"]
  , "data X where\n\tA :: X\n\tB :: X\n" ==> ["A", "B", "X"]
  , "data X where\n\tA, B :: X\n"        ==> ["A", "B", "X"]
  , "data X :: * -> * -> * where\n\
    \  A, B :: Int -> Int -> X\n"
    ==>
    ["A", "B", "X"]
  , "data X ∷ * → * → * where\n\
    \  A, B ∷ Int → Int → X\n"
    ==>
    ["A", "B", "X"]
  , "data Vec ix where\n\
    \  Nil   :: Int -> Foo Int\n\
    \  (:::) :: Int -> Vec Int -> Vec Int\n\
    \  (.+.) :: Int -> Int -> Vec Int -> Vec Int\n"
    ==>
    [".+.", ":::", "Nil", "Vec"]
  , "data Vec ix where\n\
    \  Nil   :: Int -> Foo Int\n\
    \  -- foo\n\
    \  (:::) :: Int -> Vec Int -> Vec Int\n\
    \-- bar\n\
    \  (.+.) :: Int     -> \n\
    \           -- ^ baz\n\
    \           Int     -> \n\
    \           Vec Int -> \n\
    \Vec Int\n"
    ==>
    [".+.", ":::", "Nil", "Vec"]
    , "data NatSing (n :: Nat) where\n\
      \  ZeroSing :: 'Zero\n\
      \  SuccSing :: NatSing n -> NatSing ('Succ n)\n"
    ==>
    ["NatSing", "SuccSing", "ZeroSing"]
  , "data Rec a where\n\
    \  C :: { foo :: Int } -> Rec a"
    ==>
    ["C", "Rec", "foo"]
  , "data Rec a where\n\
    \  C :: { foo :: Int, bar :: Int -> Int } -> Rec a"
    ==>
    ["C", "Rec", "bar", "foo"]
  , "data Rec a where\n\
    \  C :: { foo :: Int, bar :: Int -> Int } -> Rec a\n\
    \  D :: { baz :: (Int -> Int) -> Int, bar :: (((Int) -> (Int))) } -> Rec a"
    ==>
    ["C", "D", "Rec", "bar", "bar", "baz", "foo"]
  ]
  where
    (==>) = testTagNames filename Vanilla

testFamilies :: TestTree
testFamilies = testGroup "Families"
  [ "type family X :: *\n"                                           ==> ["X"]
  , "data family X :: * -> *\n"                                      ==> ["X"]
  , "data family a ** b"                                             ==> ["**"]

  , "type family a :<: b\n"                                          ==> [":<:"]
  , "type family (a :: Nat) :<: (b :: Nat) :: Nat\n"                 ==> [":<:"]
  , "type family (a :: Nat) `Family` (b :: Nat) :: Nat\n"            ==> ["Family"]
  , "type family (m :: Nat) <=? (n :: Nat) :: Bool"                  ==> ["<=?"]
  , "type family (m ∷ Nat) <=? (n ∷ Nat) ∷ Bool"                     ==> ["<=?"]

  , "data instance X a b = Y a | Z { unZ :: b }"                     ==> ["Y", "Z", "unZ"]
  , "data instance (Eq a, Eq b) => X a b = Y a | Z { unZ :: b }"     ==> ["Y", "Z", "unZ"]
  , "data instance (Eq a, Eq b) => X a b = Y a | Z { pattern :: b }" ==> ["Y", "Z", "pattern"]
  , "data instance XList Char = XCons !Char !(XList Char) | XNil"    ==> ["XCons", "XNil"]
  , "newtype instance Cxt x => T [x] = A (B x) deriving (Z,W)"       ==> ["A"]
  , "type instance Cxt x => T [x] = A (B x)"                         ==> []
  , "data instance G [a] b where\n\
    \   G1 :: c -> G [Int] b\n\
    \   G2 :: G [a] Bool"
    ==>
    ["G1", "G2"]
  , "class C where\n\ttype X y :: *\n" ==> ["C", "X"]
  , "class C where\n\tdata X y :: *\n" ==> ["C", "X"]
  , "class C where\n\ttype X y ∷ *\n"  ==> ["C", "X"]
  , "class C where\n\tdata X y ∷ *\n"  ==> ["C", "X"]
  ]
  where
    (==>) = testTagNames filename Vanilla

testFunctions :: TestTree
testFunctions = testGroup "Functions"
  [
  -- Multiple declarations.
    "a,b::X"          ==> ["a", "b"]
  -- With an operator.
  , "(+), a :: X"     ==> ["+", "a"]
  -- Don't get fooled by literals.
  , "1 :: Int"        ==> []

  -- plain functions and operators
  , "(.::) :: X -> Y" ==> [".::"]
  , "(:::) :: X -> Y" ==> [":::"]
  , "(->:) :: X -> Y" ==> ["->:"]
  , "(--+) :: X -> Y" ==> ["--+"]
  , "(=>>) :: X -> Y" ==> ["=>>"]

  , "_g :: X -> Y"    ==> ["_g"]
  , toplevelFunctionsWithoutSignatures
  ]
  where
    (==>) = testTagNames filename Vanilla

    toplevelFunctionsWithoutSignatures = testGroup "Toplevel functions without signatures"
      [ "infix 5 |+|"  ==> []
      , "infixl 5 |+|" ==> []
      , "infixr 5 |+|" ==> []
      , "f = g"        ==> ["f"]
        -- Relies on RepeatableTag.
      , "f :: a -> b -> a\n\
        \f x y = x"
        ==>
        ["f"]
      , "f x y = x"               ==> ["f"]
      , "f (x :+: y) z = x"       ==> ["f"]
      , "(x :+: y) `f` z = x"     ==> ["f"]
      , "(x :+: y) :*: z = x"     ==> [":*:"]
      , "((:+:) x y) :*: z = x"   ==> [":*:"]
      , "(:*:) (x :+: y) z = x"   ==> [":*:"]
      , "(:*:) ((:+:) x y) z = x" ==> [":*:"]
      , strictMatchTests
      , lazyMatchTests
      , atPatternsTests
      , "f x Nothing = x\n\
        \f x (Just y) = y"
        ==>
        ["f"]
      , "x `f` Nothing = x\n\
        \x `f` (Just y) = y"
        ==>
        ["f"]
      , "f x y = g x\n\
        \  where\n\
        \    g _ = y"
        ==>
        ["f"]
      , "x `f` y = x"   ==> ["f"]
      , "(|+|) x y = x" ==> ["|+|"]
      , "x |+| y = x"   ==> ["|+|"]
      , "(!) x y = x" ==> ["!"]
      , "--- my comment" ==> []
      , "foo :: Rec -> Bar\n\
        \foo Rec{..} = Bar (recField + 1)" ==> ["foo"]
      , "foo :: Rec -> Bar\n\
        \foo Rec { bar = Baz {..}} = Bar (recField + 1)" ==> ["foo"]
      -- Functions named "pattern"
      , "pattern :: Int -> String -> [Int]"           ==> ["pattern"]
      , "pattern, foo :: Int -> String -> [Int]"      ==> ["foo", "pattern"]
      , "foo, pattern :: Int -> String -> [Int]"      ==> ["foo", "pattern"]
      , "foo, pattern, bar :: Int -> String -> [Int]" ==> ["bar", "foo", "pattern"]
      , "pattern x = x "                              ==> ["pattern"]
      , "pattern x y = x + y"                         ==> ["pattern"]
      , "x `pattern` y = x + y"                       ==> ["pattern"]
      , "pattern x y z = x + y + z"                   ==> ["pattern"]
      -- Arguments named "forall
      , "f forall = forall + 1"                       ==> ["f"]
      ]
    strictMatchTests = testGroup "Strict match (!)"
      [ "f !x y = x"                  ==> ["f"]
      , "f x !y = x"                  ==> ["f"]
      , "f !x !y = x"                 ==> ["f"]
      , "f ! x y = x"                 ==> ["f"]
        -- this one is a bit controversial but it seems to be the way ghc
        -- parses it
      , "f ! x = x"                   ==> ["f"]
      , "(:*:) !(x :+: y) z = x"      ==> [":*:"]
      , "(:*:) !(!x :+: !y) !z = x"   ==> [":*:"]
      , "(:*:) !((:+:) x y) z = x"    ==> [":*:"]
      , "(:*:) !((:+:) !x !y) !z = x" ==> [":*:"]
      , "(!) :: a -> b -> a\n\
        \(!) x y = x"
        ==>
        ["!"]
      -- this is a degenerate case since even ghc treats ! here as
      -- a BangPatterns instead of operator
      , "x ! y = x" ==> ["x"]
      ]
    lazyMatchTests = testGroup "Lazy match (~)"
      [ "f ~x y = x"                  ==> ["f"]
      , "f x ~y = x"                  ==> ["f"]
      , "f ~x ~y = x"                 ==> ["f"]
      , "f ~ x y = x"                 ==> ["f"]
        -- this one is a bit controversial but it seems to be the way ghc
        -- parses it
      , "f ~ x = x"                   ==> ["f"]
      , "(:*:) ~(x :+: y) z = x"      ==> [":*:"]
      , "(:*:) ~(~x :+: ~y) ~z = x"   ==> [":*:"]
      , "(:*:) ~((:+:) x y) z = x"    ==> [":*:"]
      , "(:*:) ~((:+:) ~x ~y) ~z = x" ==> [":*:"]
      , "(~) :: a -> b -> a\n\
        \(~) x y = x"
        ==>
        ["~"]
      -- this is a degenerate case since even ghc treats ~ here as
      -- a BangPatterns instead of operator
      , "x ~ y = x" ==> ["x"]
      ]
    atPatternsTests = testGroup "At patterns (@)"
      [ "f z@x y    = z"                        ==> ["f"]
      , "f x   z'@y = z'"                       ==> ["f"]
      , "f z@x z'@y = z"                        ==> ["f"]
      , "f z@(Foo _) z'@y = z"                  ==> ["f"]
      , "f z@(Foo _) z'@(Bar _) = z"            ==> ["f"]
      , "f z @ x y  = z"                        ==> ["f"]
      , "f z @ (x : xs) = z: [x: xs]"           ==> ["f"]
      , "f z @ (x : zs @ xs) = z: [x: zs]"      ==> ["f"]
      , "f z @ (zz @x : zs @ xs) = z: [zz: zs]" ==> ["f"]

      , "(:*:) zzz@(x :+: y) z = x"             ==> [":*:"]
      , "(:*:) zzz@(zx@x :+: zy@y) zz@z = x"    ==> [":*:"]
      , "(:*:) zzz@((:+:) x y) z = x"           ==> [":*:"]
      , "(:*:) zzz@((:+:) zs@x zs@y) zz@z = x"  ==> [":*:"]

      , "f z@(!x) ~y = x"                       ==> ["f"]
      ]

testClass :: TestTree
testClass = testGroup "Class"
  [ "class (X x) => C a b where\n\tm :: a->b\n\tn :: c\n"          ==> ["C", "m", "n"]
  , "class (X x) ⇒ C a b where\n\tm ∷ a→b\n\tn ∷ c\n"              ==> ["C", "m", "n"]
  , "class (X x) => C a b | a -> b where\n\tm :: a->b\n\tn :: c\n" ==> ["C", "m", "n"]
  , "class (X x) ⇒ C a b | a → b where\n\tm ∷ a→b\n\tn ∷ c\n"      ==> ["C", "m", "n"]
  , "class A a where f :: X\n"                                     ==> ["A", "f"]
    -- indented inside where
  , "class X where\n\ta, (+) :: X\n"                               ==> ["+", "X", "a"]
  , "class X where\n\ta :: X\n\tb, c :: Y"                         ==> ["X", "a", "b", "c"]
  , "class X\n\twhere\n\ta :: X\n\tb, c :: Y"                      ==> ["X", "a", "b", "c"]
  , "class X\n\twhere\n\ta ::\n\t\tX\n\tb :: Y"                    ==> ["X", "a", "b"]

  , "class a :<: b where\n    f :: a -> b"                         ==> [":<:", "f"]
  , "class (:<:) a b where\n    f :: a -> b"                       ==> [":<:", "f"]
  , "class Eq a => a :<: b where\n    f :: a -> b"                 ==> [":<:", "f"]
  , "class a ~ 'Foo => a :<: b where\n    f :: a -> b"             ==> [":<:", "f"]
  , "class 'Foo ~ a => a :<: b where\n    f :: a -> b"             ==> [":<:", "f"]
  , "class (Eq a) => a :<: b where\n    f :: a -> b"               ==> [":<:", "f"]
  , "class (a ~ 'Foo) => a :<: b where\n    f :: a -> b"           ==> [":<:", "f"]
  , "class ('Foo ~ a) => a :<: b where\n    f :: a -> b"           ==> [":<:", "f"]
  , "class a :<<<: b => a :<: b where\n    f :: a -> b"
    ==>
    [":<:", "f"]
  , "class (a :<<<: b) => a :<: b where\n    f :: a -> b"   ==> [":<:", "f"]
  , "class (a :<<<: b) ⇒ a :<: b where\n    f ∷ a → b"      ==> [":<:", "f"]
  , "class (Eq a, Ord b) => a :<: b where\n    f :: a -> b" ==> [":<:", "f"]
  , "class (Eq a, Ord b) => (a :: (* -> *) -> *) :<: b where\n    f :: a -> b"
    ==>
    [":<:", "f"]
    -- this is bizzarre
  , "class (Eq (a), Ord (f a [a])) => f `Z` a" ==> ["Z"]

  , "class A f where\n  data F f :: *\n  g :: a -> f a\n  h :: f a -> a"
    ==>
    ["A", "F", "g", "h"]
  , "class A f where\n  data F f :: *\n  mkF :: f -> F f\n  getF :: F f -> f"
    ==>
    ["A", "F", "getF", "mkF"]
  , "class A f where\n\
    \  data F f :: * -- foo\n\
    \                -- bar\n\
    \                -- baz\n\
    \  mkF  :: f -> F f\n\
    \  getF :: F f -> f"
    ==>
    ["A", "F", "getF", "mkF"]
  -- Not confused by a class context on a method.
  , "class X a where\n\tfoo :: Eq a => a -> a\n" ==> ["X", "foo"]
  , "class Category cat where\n\
    \    -- | the identity morphism\n\
    \    id :: cat a a\n\
    \ \n\
    \    -- | morphism composition\n\
    \    (.) :: cat b c -> cat a b -> cat a c" ==> [".", "Category", "id"]
  , "class Match a b where\n\
    \    pattern :: Pattern a b\n" ==> ["Match", "pattern"]
  , "class a ~~ b => (a :: k) ~ (b :: k) | a -> b, b -> a"
    ==>
    ["~"]
  , "class a ~~ b => (a :: k) ! (b :: k) | a -> b, b -> a"
    ==>
    ["!"]
  ]
  where
    (==>) = testTagNames filename Vanilla

testInstance :: TestTree
testInstance = testGroup "Instance"
  [ "instance Foo Quux where\n\
    \  data Bar Quux a = QBar { frob :: a }\n\
    \                  | QBaz { fizz :: String }\n\
    \                  deriving (Show)"
    ==>
    ["QBar", "QBaz", "fizz", "frob"]
  , "instance Foo Quux where\n\
    \  data Bar Quux a = QBar a | QBaz String deriving (Show)"
    ==>
    ["QBar", "QBaz"]
  , "instance Foo Quux where\n\
    \  data Bar Quux a = QBar { frob :: a }\n\
    \                  | QBaz { fizz :: String }\n\
    \                  deriving (Show)\n\
    \  data IMRuunningOutOfNamesHere Quux = Whatever"
    ==>
    ["QBar", "QBaz", "Whatever", "fizz", "frob"]
    -- in this test foo function should not affect tags found
  , "instance Foo Quux where\n\
    \  data Bar Quux a = QBar { frob :: a }\n\
    \                  | QBaz { fizz :: String }\n\
    \                  deriving (Show)\n\
    \\n\
    \  foo _ = QBaz \"hey there\""
    ==>
    ["QBar", "QBaz", "fizz", "frob"]
  , "instance Foo Int where foo _ = 1"
    ==>
    []
  , "instance Foo Quux where\n\
    \  newtype Bar Quux a = QBar a \n\
    \                     deriving (Show)\n\
    \\n\
    \  foo _ = QBaz \"hey there\""
    ==>
    ["QBar"]
  , "instance Foo Quux where\n\
    \  newtype Bar Quux a = QBar { frob :: a }"
    ==>
    ["QBar", "frob"]
  , "instance (Monoid w,MonadBaseControl b m) => MonadBaseControl b (JournalT w m) where\n\
    \   newtype StM (JournalT w m) a =\n\
    \       StMJournal { unStMJournal :: ComposeSt (JournalT w) m a }\n\
    \   liftBaseWith = defaultLiftBaseWith StMJournal\n\
    \   restoreM     = defaultRestoreM   unStMJournal\n\
    \   {-# INLINE liftBaseWith #-}\n\
    \   {-# INLINE restoreM #-}\n\
    \"
    ==>
    ["StMJournal", "unStMJournal"]
  ]
  where
    (==>) = testTagNames filename Vanilla

testLiterate :: TestTree
testLiterate = testGroup "Literate"
  [ "> class (X x) => C a b where\n>\tm :: a->b\n>\tn :: c\n"
    ==>
    ["C", "m", "n"]
  , "Test\n\
    \\\begin{code}\n\
    \class (X x) => C a b where\n\
    \\tm :: a->b\n\
    \\tn :: c\n\
    \\\end{code}"
    ==>
    ["C", "m", "n"]
  ]
  where
    (==>) = makeTest f
    f = sort . map untag . fst . Tag.processTokens . tokenize' "fn.lhs" Literate

testPatterns :: TestTree
testPatterns = testGroup "Patterns"
  [ "pattern Arrow a b = ConsT \"->\" [a, b]"
    ==>
    ["Arrow"]
  , "pattern Arrow a b = ConsT \"->\" [a, b]\n\
    \pattern Pair a b = [a, b]"
    ==>
    ["Arrow", "Pair"]
  , "pattern Sub a b = Op '-' [a, b]\n\
    \pattern Pair a b = [a, b]"
    ==>
    ["Pair", "Sub"]
  , "pattern (:++) x y = [x, y]"
    ==>
    [":++"]
  , "pattern x :** y = [x, y]"
    ==>
    [":**"]
  , "pattern Nil :: Vec2 a\n\
    \pattern Nil = Vec2 []\n"
    ==>
    ["Nil", "Nil"]
  , "pattern (:>) x xs <- ((\\ys -> (head $ unvec2 ys,Vec2 . tail $ unvec2 ys)) -> (x,xs))\n\
    \where\n\
    \   (:>) x xs = Vec2 (x:unvec2 xs)"
    ==>
    [":>"]
  ]
  where
    (==>) = testTagNames filename Vanilla

testFFI :: TestTree
testFFI = testGroup "FFI"
  [ "foreign import ccall foo :: Double -> IO Double"            ==> ["foo"]
  , "foreign import unsafe java foo :: Double -> IO Double"      ==> ["foo"]
  , "foreign import safe stdcall foo :: Double -> IO Double"     ==> ["foo"]
  , "foreign import safe stdcall pattern :: Double -> IO Double" ==> ["pattern"]
  ]
  where
    (==>) = testTagNames filename Vanilla


testTokenizeCpp :: TestTree
testTokenizeCpp = testGroup "Tokenize with preprocessor"
  [ testTokenizeCppDefines
  , testTokenizeCppConditionals
  , testTokenizeCppDefinesWithinConditionals
  -- , testWithIncludes
  --     "Just two includes"
  --     "#include <foo.h>\n\
  --     \\n\
  --     \#include \"bar.h\"\n"
  --     [ Newline 0
  --     , Newline 0
  --     , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
  --     , T "foo", T "x", Equals, T "x", Newline 0
  --     , Newline 0
  --     , T "bar", DoubleColon, T "b", Arrow, T "b", Newline 0
  --     , T "bar", T "y", Equals, T "y", Newline 0
  --     , Newline 0
  --     , Newline 0
  --     , T "baz", DoubleColon, T "c", Arrow, T "c", Newline 0
  --     , T "baz", T "z", Equals, T "z", Newline 0
  --     ]
  ]

-- testWithIncludes :: Stirng ->

testTokenizeCppDefines :: TestTree
testTokenizeCppDefines = testGroup "#define"
  [ constants
  , functions
  , concatenation
  , functionsAndConstants
  ]
  where
    (==>) = makeAssertion f
    f = map valOf . tokenize' filename Vanilla

    constants :: TestTree
    constants = testGroup "Constants"
      [ testCase "Vanilla define" $
          "#define FOO foo\n\
          \FOO :: a -> a\n\
          \FOO x = x"
          ==>
          [ Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "foo", T "x", Equals, T "x", Newline 0
          ]
      , testCase "Haskell-style define names with primes and backticks" $
          "#define FOO'Bar` foo\n\
          \FOO'Bar` :: a -> a\n\
          \FOO'Bar` x = x"
          ==>
          [ Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "foo", T "x", Equals, T "x", Newline 0
          ]
      , testCase "Define with lots of continuation lines and indentation" $
         "#  \\\n\
          \  define \\\n\
          \    FOO      \\\n\
          \       foo\n\
          \FOO :: a -> a\n\
          \FOO x = x"
          ==>
          [ Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "foo", T "x", Equals, T "x", Newline 0
          ]
      , testCase "Define with lots of continuation lines, no indentation and some spaces" $
          "#  \\\n\
          \define \\\n\
          \FOO      \\\n\
          \foo\n\
          \FOO :: a -> a\n\
          \FOO x = x"
          ==>
          [ Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "foo", T "x", Equals, T "x", Newline 0
          ]
      , testCase "Define with lots of continuation lines, no indentation and minimum spaces" $
          "#\\\n\
          \define \\\n\
          \FOO \\\n\
          \foo\n\
          \FOO :: a -> a\n\
          \FOO x = x"
          ==>
          [ Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "foo", T "x", Equals, T "x", Newline 0
          ]
      , testCase "Try to use constant define as a macro" $
          "#define FOO 1\n\
          \quux = FOO(2)\n"
          ==>
          [ Newline 0
          , Newline 0
          , T "quux", Equals, T "1", LParen, T "2", RParen, Newline 0
          ]
      , testCase "Define constant that spans boundaries of a string token" $
          "#define BAZ \"foo bar\n\
          \\n\
          \bar = BAZ FOO quux\"\n\
          \\n\
          \foo ::  a -> b"
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , T "bar", Equals, String, Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "b", Newline 0
          ]
      , testCase "Define multiline constant that spans boundaries of a string token" $
          "#define BAZ2 \"foo \\\n\
          \  bar\n\
          \\n\
          \bar = BAZ2 FOO quux\"\n\
          \\n\
          \foo ::  a -> b"
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , T "bar", Equals, String, Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "b", Newline 0
          ]
      , testCase "Stripping of empty c-style comments" $
          "#define TEST foo/**/bar\n\
          \\n\
          \concatTest :: a -> a\n\
          \concatTest x =\n\
          \  x + TEST + \"foobar\"   "
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , T "concatTest", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "concatTest", T "x", Equals, Newline 2
          , T "x", T "+", T "foobar", T "+", String, Newline 0
          ]
      , testCase "Stripping of non-empty c-style comments" $
          "#define TEST foo/* hello world! */bar\n\
          \\n\
          \concatTest :: a -> a\n\
          \concatTest x =\n\
          \  x + TEST + \"foobar\"   "
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , T "concatTest", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "concatTest", T "x", Equals, Newline 2
          , T "x", T "+", T "foobar", T "+", String, Newline 0
          ]
      , testCase "Define after use" $
          "foo = X\n\
          \\n\
          \#define X Y\n\
          \\n\
          \bar = X"
          ==>
          [ Newline 0
          , T "foo", Equals, T "X", Newline 0
          , Newline 0
          , Newline 0
          , Newline 0
          , T "bar", Equals, T "Y", Newline 0
          ]
      , testCase "Redefine" $
          "#define X Y\n\
          \\n\
          \foo = X\n\
          \\n\
          \#define X Z\n\
          \\n\
          \bar = X"
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
          "#define FOO a + b\n\
          \foo :: Int -> Int -> Int\n\
          \foo a b c = FOO + c * (FOO)\n"
          ==>
          [ Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "Int", Arrow, T "Int", Arrow, T "Int", Newline 0
          , T "foo", T "a", T "b", T "c", Equals, T "a", T "+", T "b", T "+", T "c", T "*", LParen, T "a", T "+", T "b", RParen, Newline 0
          ]
      , testCase "Expand #define that expands to another #define'd name" $
          "#define FOO foo\n\
          \#define BAR FOO\n\
          \\n\
          \BAR :: a -> b\n\
          \BAR x = x"
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , Newline 0
          , T "foo", DoubleColon, T "a", Arrow, T "b", Newline 0
          , T "foo", T "x", Equals, T "x", Newline 0
          ]
      , testCase "Expand #define that expands to name #define'd later in the program" $
          "#define BAR FOO\n\
          \#define FOO foo\n\
          \\n\
          \BAR :: a -> b\n\
          \BAR x = x"
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
          "#define foo() bar\n\
          \test :: a -> a\n\
          \test x = foo()baz"
          ==>
          [ Newline 0
          , Newline 0
          , T "test", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "test", T "x", Equals, T "bar", T "baz", Newline 0
          ]
      , testCase "Function of 0 arguments with whitespace in argument list at definition site" $
          "#define foo(           ) bar\n\
          \test :: a -> a\n\
          \test x = foo()baz"
          ==>
          [ Newline 0
          , Newline 0
          , T "test", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "test", T "x", Equals, T "bar", T "baz", Newline 0
          ]
      , testCase "Function of 0 arguments and whitespace in argument list at call site" $
          "#define foo() bar\n\
          \test :: a -> a\n\
          \test x = foo(           )baz"
          ==>
          [ Newline 0
          , Newline 0
          , T "test", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "test", T "x", Equals, T "bar", T "baz", Newline 0
          ]
      , testCase "Function of 0 arguments not equivalent to macro name" $
          "#define foo() bar\n\
          \test :: a -> a\n\
          \test x = foo baz"
          ==>
          [ Newline 0
          , Newline 0
          , T "test", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "test", T "x", Equals, T "foo", T "baz", Newline 0
          ]
      , testCase "Function of 1 argument" $
          "#define MKLENS(x) x\n\
          \MKLENS(bar) :: a -> a\n\
          \MKLENS(bar) x = x"
          ==>
          [ Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "bar", T "x", Equals, T "x", Newline 0
          ]
      , testCase "Function of 1 arguments not expanded instead of constant" $
          "#define foo() bar\n\
          \test :: a -> a\n\
          \test x = foo baz"
          ==>
          [ Newline 0
          , Newline 0
          , T "test", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "test", T "x", Equals, T "foo", T "baz", Newline 0
          ]
      , testCase "Function of 2 vanilla arguments" $
          "#define TEST(x, y) x + y\n\
          \bar :: a -> a\n\
          \bar x = TEST(x, x)"
          ==>
          [ Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "bar", T "x", Equals, T "x", T "+",T "x", Newline 0
          ]
      , testCase "Function of 2 arguments when one argument contains comma within single quotes" $
          "#define TEST(x, y) x + y\n\
          \bar :: a -> a\n\
          \bar x = TEST(',', x)"
          ==>
          [ Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "bar", T "x", Equals, Character, T "+",T "x", Newline 0
          ]
      , testCase "Function of 2 arguments when one argument contains comma within double quotes" $
          "#define TEST(x, y) x + y\n\
          \bar :: a -> a\n\
          \bar x = TEST(\"x, x\", x)"
          ==>
          [ Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "bar", T "x", Equals, String, T "+",T "x", Newline 0
          ]
      , testCase "Function of 2 arguments when one argument contains comma within balanced parentheses" $
          "#define TEST(x, y) x + y\n\
          \bar :: a -> a\n\
          \bar x = TEST((x, x), x)"
          ==>
          [ Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "bar", T "x", Equals, LParen, T "x", Comma, T "x", RParen, T "+",T "x", Newline 0
          ]
      , testCase "Function of 2 arguments when one argument contains balanced parentheses surrounded by other text" $
          "#define TEST(x, y) x + y\n\
          \bar :: a -> a\n\
          \bar x = TEST(y (x, x) * z, x)"
          ==>
          [ Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "bar", T "x", Equals, T "y", LParen, T "x", Comma, T "x", RParen, T "*", T "z", T "+", T "x", Newline 0
          ]
      , testCase "Function of 2 arguments when one argument contains comma within many balanced parentheses" $
          "#define TEST(x, y) x + y\n\
          \bar :: a -> a\n\
          \bar x = TEST(((((((((((x, x)))))))))), x)"
          ==>
          ([ Newline 0
           , Newline 0
           , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
           , T "bar", T "x", Equals
           ] ++
           replicate 10 LParen ++ [T "x", Comma, T "x"] ++ replicate 10 RParen ++
           [T "+",T "x", Newline 0])
      , testCase "Function of 2 arguments - balanced brackets do not change semantics of comma" $
          "#define TEST(x, y) x + y\n\
          \bar :: a -> a\n\
          \bar x = TEST([x, x])"
          ==>
          [ Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "bar", T "x", Equals, LBracket, T "x", T "+", T "x", RBracket, Newline 0
          ]
      , testCase "Function of 4 arguments with some arguments empty" $
          "#define TEST(x, y, z, w) x + y + z + w\n\
          \bar :: a -> a\n\
          \bar x = TEST(x * x,   ,, 2 )"
          ==>
          [ Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "bar", T "x", Equals, T "x", T "*", T "x", T "+", T "+", T "+", T "2", Newline 0
          ]
      , testCase "Expand multiline function-like macro" $
          "#define TEST(name, tvar, var) \\\n\
          \  name :: tvar -> tvar \\\n\
          \  name var = var\n\
          \\n\
          \foo :: a -> a\n\
          \foo x = x\n\
          \TEST(bar, b, y)\n\
          \\n\
          \TEST(baz, c, z)"
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
          "#define FOO () foo()\n\
          \\n\
          \bar :: a -> ()\n\
          \bar = FOO() + 1"
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, LParen, RParen, Newline 0
          , T "bar", Equals, LParen, RParen, T "foo", LParen, RParen, LParen, RParen, T "+", T "1", Newline 0
          ]
      , testCase "Stringization with # without spaces" $
          "#define TEST(x) x (#x)\n\
          \bar :: a -> a\n\
          \bar x = TEST(x)"
          ==>
          [ Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "bar", T "x", Equals, T "x", LParen, String, RParen, Newline 0
          ]
      , testCase "Stringization with # surrounded with spaces" $
          "#define TEST(x) x (  #  x  )\n\
          \bar :: a -> a\n\
          \bar x = TEST(x)"
          ==>
          [ Newline 0
          , Newline 0
          , T "bar", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "bar", T "x", Equals, T "x", LParen, String, RParen, Newline 0
          ]
      , testCase "Redefine" $
          "#define X(a) Y\n\
          \\n\
          \foo = X(1)\n\
          \\n\
          \#define X(a) Z\n\
          \\n\
          \bar = X(1)"
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
          "#define BAR(x) 1\n\
          \#define BAZ(x) 2\n\
          \\n\
          \#define FOO(BAR, X) BAR(X)\n\
          \\n\
          \foo :: a -> a\n\
          \foo = FOO(BAZ, 3)"
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
          "#define CONCAT_TEST(name) name/**/Test\n\
          \\n\
          \concatTest :: a -> a\n\
          \concatTest x =\n\
          \  x + CONCAT_TEST(bar) + \"foobar\"   "
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
          "#define CONCAT_TEST(name) name##Test\n\
          \\n\
          \concatTest :: a -> a\n\
          \concatTest x =\n\
          \  x + CONCAT_TEST(bar) + \"foobar\"   "
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , T "concatTest", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "concatTest", T "x", Equals, Newline 2
          , T "x", T "+", T "barTest", T "+", String, Newline 0
          ]
      , testCase "Via ## with spaces" $
          "#define CONCAT_TEST(name) name ## Test\n\
          \\n\
          \concatTest :: a -> a\n\
          \concatTest x =\n\
          \  x + CONCAT_TEST(bar) + \"foobar\"   "
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
          "#define X Y\n\
          \\n\
          \foo = X\n\
          \\n\
          \#define X(a) Z\n\
          \\n\
          \bar = X(1)"
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
          "#define FOO(BAR) BAR\n\
          \\n\
          \#define BAR 15\n\
          \\n\
          \baz :: a -> a\n\
          \baz x = FOO(x) + 1"
          ==>
          [ Newline 0
          , Newline 0
          , Newline 0
          , Newline 0
          , T "baz", DoubleColon, T "a", Arrow, T "a", Newline 0
          , T "baz", T "X", Equals, T "x", T "+", T "1", Newline 0
          ]
      , testCase "Function macro passed as argument to another function macro but not aplied there" $
          "#define BAR(x) 1\n\
          \#define BAZ(x) 2\n\
          \\n\
          \#define FOO(BAR, X) BAR\n\
          \\n\
          \foo :: a -> a\n\
          \foo = FOO(BAZ, 3)"
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
          "#define BAR(x) 1\n\
          \#define BAZ 2\n\
          \\n\
          \#define FOO(BAR, X) BAR\n\
          \\n\
          \foo :: a -> a\n\
          \foo = FOO(BAZ, 3)"
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

testTokenizeCppConditionals :: TestTree
testTokenizeCppConditionals = testGroup "Conditionals"
  [ testCase "Expand #ifdef-#endif" $
      "#ifdef FOO\n\
      \foo :: a -> a\n\
      \foo x = x\n\
      \#endif\n\
      \bar :: b -> b\n\
      \bar y = y\n\
      \\n\
      \\n"
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
      "#ifdef FOO\n\
      \foo :: a -> a\n\
      \foo x = x\n\
      \#else\n\
      \bar :: b -> b\n\
      \bar y = y\n\
      \#endif\n"
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
      "#if defined(FOO)\n\
      \foo :: a -> a\n\
      \foo x = x\n\
      \#elif defined(BAR)\n\
      \bar :: b -> b\n\
      \bar y = y\n\
      \#else\n\
      \baz :: c -> c\n\
      \baz z = z\n\
      \#endif\n"
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
    f = map valOf . tokenize' filename Vanilla

testTokenizeCppDefinesWithinConditionals :: TestTree
testTokenizeCppDefinesWithinConditionals =
  testGroup "Defines within conditionals"
    [ testCase "Define same constant within conditional branches" $
        "#if defined(FOO)\n\
        \#define BAR x\n\
        \#else\n\
        \#define BAR y\n\
        \#endif\n\
        \\n\
        \BAR :: a -> b"
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
        "#if defined(FOO)\n\
        \#define BAR(a) x\n\
        \#else\n\
        \#define BAR(a) y\n\
        \#endif\n\
        \\n\
        \BAR(1) :: a -> b"
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
        "#if defined(FOO)\n\
        \#define BAR x\n\
        \#else\n\
        \#define BAR(a) y\n\
        \#endif\n\
        \\n\
        \BAR :: a -> b\n\
        \BAR(1) :: a -> b"
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
    f = map valOf . tokenize' filename Vanilla

testFullPipeline :: TestTree
testFullPipeline = testGroup "Full processing pipeline"
  [ ["data X", "module X"]
    ==>
    [ Pos (SrcPos "fn0" (Line 1) mempty) (TagVal "X" Type Nothing)
    , Pos (SrcPos "fn1" (Line 1) mempty) (TagVal "X" Module Nothing)
    ]
  -- Type goes ahead of Module.
  , [ "module X\n\
       \data X"
    ]
    ==>
    [ Pos (SrcPos "fn0" (Line 1) mempty) (TagVal "X" Module Nothing)
    , Pos (SrcPos "fn0" (Line 2) mempty) (TagVal "X" Type Nothing)
    ]
  , [ "module Z\n\
      \data X = Y\n"
    ]
    ==>
    [ Pos (SrcPos "fn0" (Line 1) mempty) (TagVal "Z" Module Nothing)
    , Pos (SrcPos "fn0" (Line 2) mempty) (TagVal "X" Type Nothing)
    , Pos (SrcPos "fn0" (Line 2) mempty) (TagVal "Y" Constructor (Just "X"))
    ]
  , [ "module Z\n\
      \data X a =\n\
      \  Y a\n"
    ]
    ==>
    [ Pos (SrcPos "fn0" (Line 1) mempty) (TagVal "Z" Module Nothing)
    , Pos (SrcPos "fn0" (Line 2) mempty) (TagVal "X" Type Nothing)
    , Pos (SrcPos "fn0" (Line 3) mempty) (TagVal "Y" Constructor (Just "X"))
    ]
  , [ "newtype A f a b = A\n\
      \  { unA :: f (a -> b) }"
    ]
    ==>
    [ Pos (SrcPos "fn0" (Line 1) mempty) (TagVal "A" Type Nothing)
    , Pos (SrcPos "fn0" (Line 1) mempty) (TagVal "A" Constructor (Just "A"))
    , Pos (SrcPos "fn0" (Line 2) mempty) (TagVal "unA" Function (Just "A"))
    ]
  ]
  where
    (==>) = makeTest f'
    f' = sort
       . concatMap (\(i, t) -> fst $ Tag.processTokens $ tokenize' ("fn" ++ show i) Vanilla t)
       . zip [0..]


filename :: FilePath
filename = "fn.hs"

testFullTagsWithoutPrefixes
  :: HasCallStack
  => FilePath -> LiterateMode -> Text -> [Pos TagVal] -> TestTree
testFullTagsWithoutPrefixes fn mode = \source tags ->
  makeTest (first sort . Tag.processTokens . tokenize' fn mode) source (tags, warnings)
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
    process = first (sort . map untag) . Tag.processTokens . tokenize' fn mode

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
