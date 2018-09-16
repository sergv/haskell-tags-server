----------------------------------------------------------------------------
-- |
-- Module      :  Data.IgnoreEqOrdHash
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Data.IgnoreEqOrdHash
  ( IgnoreEqOrdHash(..)
  ) where

import Data.Coerce
import Data.Hashable
import Data.Text.Prettyprint.Doc.Ext

newtype IgnoreEqOrdHash a = IgnoreEqOrdHash { unIgnoreEqOrdHash :: a }
  deriving (Pretty)

instance Eq (IgnoreEqOrdHash a) where
  {-# INLINE (==) #-}
  (==) _ _ = True

instance Ord (IgnoreEqOrdHash a) where
  {-# INLINE compare #-}
  compare _ _ = EQ

instance Hashable (IgnoreEqOrdHash a) where
  {-# INLINE hashWithSalt #-}
  hashWithSalt = const

instance Show a => Show (IgnoreEqOrdHash a) where
  {-# INLINE showsPrec #-}
  showsPrec = coerce (showsPrec :: Int -> a -> ShowS)

instance {-# OVERLAPS #-} PPGenericOverride a => PPGenericOverride (IgnoreEqOrdHash a) where
  ppGenericOverride = ppGenericOverride . unIgnoreEqOrdHash

