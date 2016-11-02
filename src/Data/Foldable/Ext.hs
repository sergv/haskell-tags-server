----------------------------------------------------------------------------
-- |
-- Module      :  Data.Foldable.Ext
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Sunday, 25 September 2016
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

module Data.Foldable.Ext
  ( foldMapA
  , foldForA
  , foldFor
  , module Data.Foldable
  ) where

import Data.Foldable

newtype MonoidalLift f a = MonoidalLift { unMonoidalLift :: f a }

instance (Applicative f, Monoid a) => Monoid (MonoidalLift f a) where
  mempty = MonoidalLift $ pure mempty
  mappend (MonoidalLift x) (MonoidalLift y) = MonoidalLift $ mappend <$> x <*> y

foldMapA :: (Applicative f, Monoid a, Foldable t) => (b -> f a) -> t b -> f a
foldMapA f = unMonoidalLift . foldMap (MonoidalLift . f)

{-# INLINE foldForA #-}
foldForA :: (Applicative f, Monoid a, Foldable t) => t b -> (b -> f a) -> f a
foldForA = flip foldMapA

{-# INLINE foldFor #-}
foldFor :: (Foldable f, Monoid b) => f a -> (a -> b) -> b
foldFor = flip foldMap
