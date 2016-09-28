----------------------------------------------------------------------------
-- |
-- Module      :  Server.Tags.TypesTests
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Sunday, 18 September 2016
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Server.Tags.TypesTests (tests) where

import Control.Arrow (second)
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import Data.Symbols

tests :: TestTree
tests = testGroup "Type tests"
  [ splitQualifiedPartTests
  ]

splitQualifiedPartTests :: TestTree
splitQualifiedPartTests = testGroup "splitting of qualified part of symbol"
  [ mkQualifierTest
      "variable name, no qualification"
      "foo"
      (Nothing, mkSymbolName "foo")
  , mkQualifierTest
      "type name, no qualification"
      "Typ"
      (Nothing, mkSymbolName "Typ")
  , mkQualifierTest
      "operator, no qualification"
      "++"
      (Nothing, mkSymbolName "++")
  , mkQualifierTest
      "operator in paretheses, no qualification"
      "(++)"
      (Nothing, mkSymbolName "(++)")
  , mkQualifierTest
      "variable name, qualified"
      "Foo.Bar.foo"
      (fooBarQual, mkSymbolName "foo")
  , mkQualifierTest
      "type name, qualified"
      "Foo.Bar.Typ"
      (fooBarQual, mkSymbolName "Typ")
  , mkQualifierTest
      "operator, qualified"
      "Foo.Bar.++"
      (fooBarQual, mkSymbolName "++")
  , mkQualifierTest
      "operator in paretheses, qualified"
      "Foo.Bar.(++)"
      (fooBarQual, mkSymbolName "(++)")
  ]
  where
    fooBarQual :: Maybe ImportQualifier
    fooBarQual = Just $ mkImportQualifier (mkModuleName "Foo.Bar")

mkQualifierTest
  :: String
  -> Text
  -> (Maybe ImportQualifier, SymbolName)
  -> TestTree
mkQualifierTest name input expected =
  testCase name $ actual @?= expected
  where
    actual = second getUnqualifiedSymbolName (splitQualifiedPart (mkSymbolName input))
