----------------------------------------------------------------------------
-- |
-- Module      :  Data.KeyMap
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Monday, 19 September 2016
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Data.KeyMap
  ( KeyMap
  , unKeyMap
  , HasKey(..)
  , insert
  , lookup
  , member
  , notMember
  , fromList
  , toList
  ) where

import Control.Arrow
import Data.Coerce
import qualified Data.Map as M
import Prelude hiding (lookup)

newtype KeyMap a = KeyMap { unKeyMap :: M.Map (Key a) a }

deriving instance (Eq a, Eq (Key a))     => Eq (KeyMap a)
deriving instance (Ord a, Ord (Key a))   => Ord (KeyMap a)
deriving instance (Show a, Show (Key a)) => Show (KeyMap a)
deriving instance (Ord (Key a))          => Monoid (KeyMap a)

instance Foldable KeyMap where
  foldMap f = foldMap f . unKeyMap

class (Ord (Key a)) => HasKey a where
  type Key a :: *
  getKey :: a -> Key a

insert :: (HasKey a) => a -> KeyMap a -> KeyMap a
insert x = coerce $ M.insert (getKey x) x

lookup :: (HasKey a) => Key a -> KeyMap a -> Maybe a
lookup k = M.lookup k . unKeyMap

member :: (HasKey a) => Key a -> KeyMap a -> Bool
member k = M.member k . unKeyMap

notMember :: (HasKey a) => Key a -> KeyMap a -> Bool
notMember k = M.notMember k . unKeyMap

fromList :: (HasKey a) => [a] -> KeyMap a
fromList = KeyMap . M.fromList . map (getKey &&& id)

toList :: (HasKey a) => KeyMap a -> [(Key a, a)]
toList = M.toList . unKeyMap
