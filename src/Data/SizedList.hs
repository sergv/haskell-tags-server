-- |
-- Module:     Data.SizedList
-- Copyright:  (c) Sergey Vinokurov 2025
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Data.SizedList
  ( SizedList
  , empty
  , length
  , cons
  , fromList
  , toList
  ) where

import Prelude hiding (length)

import Data.List qualified as L

data SizedList a = SizedList
  { slContents :: [a]
  , slSize     :: !Int
  } deriving (Eq, Ord)

instance Semigroup (SizedList a) where
  SizedList xs xl <> SizedList ys yl = SizedList (xs ++ ys) (xl + yl)

instance Monoid (SizedList a) where
  mempty = empty

length :: SizedList a -> Int
length = slSize

empty :: SizedList a
empty = SizedList [] 0

cons :: a -> SizedList a -> SizedList a
cons x (SizedList xs xl) = SizedList (x : xs) (xl + 1)

fromList :: [a] -> SizedList a
fromList xs = SizedList xs (L.length xs)

toList :: SizedList a -> [a]
toList = slContents
