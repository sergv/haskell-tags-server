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

import Control.Arrow (left)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Test.Tasty
import Test.Tasty.HUnit

import Server.Tags.Types
import Text.PrettyPrint.Leijen.Text.Utils (displayDoc)

tests :: TestTree
tests = testGroup "Type tests"
  [ splitQualifiedPartTests
  ]

splitQualifiedPartTests :: TestTree
splitQualifiedPartTests = testGroup "splitting of qualified part of symbol"
  [ mkQualifierTest
      "variable name, no qualification"
      "foo"
      (Right (Nothing, mkSymbolName "foo"))
  , mkQualifierTest
      "type name, no qualification"
      "Typ"
      (Right (Nothing, mkSymbolName "Typ"))
  , mkQualifierTest
      "operator, no qualification"
      "++"
      (Right (Nothing, mkSymbolName "++"))
  , mkQualifierTest
      "operator in paretheses, no qualification"
      "(++)"
      (Right (Nothing, mkSymbolName "(++)"))
  , mkQualifierTest
      "variable name, qualified"
      "Foo.Bar.foo"
      (Right (fooBarQual, mkSymbolName "foo"))
  , mkQualifierTest
      "type name, qualified"
      "Foo.Bar.Typ"
      (Right (fooBarQual, mkSymbolName "Typ"))
  , mkQualifierTest
      "operator, qualified"
      "Foo.Bar.++"
      (Right (fooBarQual, mkSymbolName "++"))
  , mkQualifierTest
      "operator in paretheses, qualified"
      "Foo.Bar.(++)"
      (Right (fooBarQual, mkSymbolName "(++)"))
  ]
  where
    fooBarQual :: Maybe ImportQualifier
    fooBarQual = Just $ mkImportQualifier (mkModuleName "Foo.Bar")

mkQualifierTest
  :: String
  -> Text
  -> Either TL.Text (Maybe ImportQualifier, SymbolName)
  -> TestTree
mkQualifierTest name input expected =
  testCase name $
    left displayDoc (splitQualifiedPart (mkSymbolName input)) @?= expected
