----------------------------------------------------------------------------
-- |
-- Module      :  Data.KeyMap
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Monday, 19 September 2016
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wredundant-constraints          #-}
{-# OPTIONS_GHC -Wsimplifiable-class-constraints #-}

module Data.KeyMap
  ( KeyMap
  , unKeyMap
  , HasKey(..)
  , insert
  , lookup
  , member
  , notMember
  , fromList
  , toMap
  , toList
  , elems
  , restrictKeys
  , keysSet
  , empty
  , null
  , intersectionWith
  , differenceWith
  ) where

import Control.Arrow
import Data.Coerce
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Semigroup
import Data.Set (Set)
import Prelude hiding (lookup, null)

import Data.Text.Prettyprint.Doc.Combinators

-- | Map than maintains sets of values that all share some key.
-- Every value must be a member of 'HasKey' typeclass.
newtype KeyMap a = KeyMap { unKeyMap :: Map (Key a) (NonEmpty a) }

deriving instance (Eq a, Eq (Key a))     => Eq (KeyMap a)
deriving instance (Ord a, Ord (Key a))   => Ord (KeyMap a)
deriving instance (Show a, Show (Key a)) => Show (KeyMap a)

instance (Pretty (Key a), Pretty a) => Pretty (KeyMap a) where
  pretty = ppAssocList . M.toList . unKeyMap

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

toMap :: KeyMap a -> Map (Key a) (NonEmpty a)
toMap = unKeyMap

toList :: KeyMap a -> [(Key a, NonEmpty a)]
toList = M.toList . toMap

elems :: KeyMap a -> [NonEmpty a]
elems = M.elems . unKeyMap

restrictKeys :: forall a. HasKey a => KeyMap a -> Set (Key a) -> KeyMap a
restrictKeys =
  coerce (M.restrictKeys :: Map (Key a) (NonEmpty a) -> Set (Key a) -> Map (Key a) (NonEmpty a))

keysSet :: KeyMap a -> Set (Key a)
keysSet = M.keysSet . unKeyMap

empty :: KeyMap a
empty = KeyMap M.empty

null :: KeyMap a -> Bool
null = M.null . unKeyMap

intersectionWith
  :: forall a. HasKey a
  => (NonEmpty a -> NonEmpty a -> NonEmpty a)
  -> KeyMap a
  -> KeyMap a
  -> KeyMap a
intersectionWith =
  coerce (M.intersectionWith :: (NonEmpty a -> NonEmpty a -> NonEmpty a) -> Map (Key a) (NonEmpty a) -> Map (Key a) (NonEmpty a) -> Map (Key a) (NonEmpty a))

differenceWith
  :: forall a. HasKey a
  => (NonEmpty a -> NonEmpty a -> Maybe (NonEmpty a))
  -> KeyMap a
  -> KeyMap a
  -> KeyMap a
differenceWith =
  coerce (M.differenceWith :: (NonEmpty a -> NonEmpty a -> Maybe (NonEmpty a)) -> Map (Key a) (NonEmpty a) -> Map (Key a) (NonEmpty a) -> Map (Key a) (NonEmpty a))
