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
  , elems
  , intersectAgainst
  , keysSet
  ) where

import Control.Arrow
import Data.Coerce
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as M
import Data.Semigroup
import Data.Set (Set)
import Prelude hiding (lookup)

-- | Map than maintains sets of values that all share some key.
-- Every value must be a member of 'HasKey' typeclass.
newtype KeyMap a = KeyMap { unKeyMap :: M.Map (Key a) (NonEmpty a) }

deriving instance (Eq a, Eq (Key a))     => Eq (KeyMap a)
deriving instance (Ord a, Ord (Key a))   => Ord (KeyMap a)
deriving instance (Show a, Show (Key a)) => Show (KeyMap a)

instance Ord (Key a) => Semigroup (KeyMap a) where
  KeyMap m <> KeyMap m' = KeyMap $ M.unionWith (<>) m m'

instance Ord (Key a) => Monoid (KeyMap a) where
  mempty = KeyMap mempty
  mappend = (<>)

instance Foldable KeyMap where
  foldMap f = foldMap (foldMap f) . unKeyMap

class Ord (Key a) => HasKey a where
  type Key a :: *
  getKey :: a -> Key a

insert :: HasKey a => a -> KeyMap a -> KeyMap a
insert x = coerce $ M.insertWith (<>) (getKey x) (x :| [])

lookup :: HasKey a => Key a -> KeyMap a -> Maybe (NonEmpty a)
lookup k = M.lookup k . unKeyMap

member :: HasKey a => Key a -> KeyMap a -> Bool
member k = M.member k . unKeyMap

notMember :: HasKey a => Key a -> KeyMap a -> Bool
notMember k = M.notMember k . unKeyMap

fromList :: HasKey a => [a] -> KeyMap a
fromList = KeyMap . M.fromListWith (<>) . map (getKey &&& (:| []))

toList :: HasKey a => KeyMap a -> [(Key a, NonEmpty a)]
toList = M.toList . unKeyMap

elems :: KeyMap a -> [NonEmpty a]
elems = M.elems . unKeyMap

intersectAgainst :: HasKey a => KeyMap a -> Set (Key a) -> KeyMap a
intersectAgainst (KeyMap m) keys =
  KeyMap $ M.intersection m (M.fromSet (const ()) keys)

keysSet :: HasKey a => KeyMap a -> Set (Key a)
keysSet = M.keysSet . unKeyMap
