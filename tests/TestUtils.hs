----------------------------------------------------------------------------
-- |
-- Module      :  TestUtils
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Thursday, 13 October 2016
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

module TestUtils
  ( TestCase(..)
  , mkUnqualSymName
  , neSingleton
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe

import Data.Symbols
import qualified Data.Text as T

data TestCase i o = TestCase
  { testName       :: String
  , input          :: i
  , expectedResult :: o
  } deriving (Show, Eq, Ord)

mkUnqualSymName :: T.Text -> UnqualifiedSymbolName
mkUnqualSymName name =
  fromMaybe err $ mkUnqualifiedSymbolName $ mkSymbolName name
  where
    err = error $ "invalid unqualified symbol name: " ++ show name

neSingleton :: a -> NonEmpty a
neSingleton x = x :| []
