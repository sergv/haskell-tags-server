----------------------------------------------------------------------------
-- |
-- Module      :  Data.KeyMap
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Monday, 19 September 2016
----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
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

import Prelude hiding (lookup, null)

import Control.Arrow
import Control.DeepSeq

import Data.Coerce
import Data.Map (Map)
import qualified Data.Map as M
import Data.Pointed
import Data.Set (Set)
import GHC.Generics

import Data.Text.Prettyprint.Doc.Combinators

-- | Map than maintains sets of values that all share some key.
-- Every value must be a member of 'HasKey' typeclass.
newtype KeyMap f a = KeyMap { unKeyMap :: Map (Key a) (f a) }
  deriving (Generic)

deriving instance (Eq   (f a), Eq   (Key a)) => Eq   (KeyMap f a)
deriving instance (Ord  (f a), Ord  (Key a)) => Ord  (KeyMap f a)
deriving instance (Show (f a), Show (Key a)) => Show (KeyMap f a)

instance (NFData (f a), NFData (Key a)) => NFData (KeyMap f a)

instance (Pretty (Key a), Pretty (f a)) => Pretty (KeyMap f a) where
  pretty = ppAssocList . M.toList . unKeyMap

instance (Ord (Key a), Semigroup (f a)) => Semigroup (KeyMap f a) where
  KeyMap m <> KeyMap m' = KeyMap $ M.unionWith (<>) m m'

instance (Ord (Key a), Semigroup (f a)) => Monoid (KeyMap f a) where
  mempty = KeyMap mempty
  mappend = (<>)

instance Foldable f => Foldable (KeyMap f) where
  foldMap f = foldMap (foldMap f) . unKeyMap

class Ord (Key a) => HasKey a where
  type Key a :: *
  getKey :: a -> Key a

insert
  :: forall a f. (HasKey a, Pointed f, Semigroup (f a))
  => a -> KeyMap f a -> KeyMap f a
insert x = coerce $ M.insertWith (<>) (getKey x) (point @f x)

lookup :: HasKey a => Key a -> KeyMap f a -> Maybe (f a)
lookup k = M.lookup k . unKeyMap

member :: HasKey a => Key a -> KeyMap f a -> Bool
member k = M.member k . unKeyMap

notMember :: HasKey a => Key a -> KeyMap f a -> Bool
notMember k = M.notMember k . unKeyMap

fromList :: (HasKey a, Pointed f, Semigroup (f a)) => [a] -> KeyMap f a
fromList = KeyMap . M.fromListWith (<>) . map (getKey &&& point)

toMap :: KeyMap f a -> Map (Key a) (f a)
toMap = unKeyMap

toList :: KeyMap f a -> [(Key a, f a)]
toList = M.toList . toMap

elems :: KeyMap f a -> [f a]
elems = M.elems . unKeyMap

restrictKeys :: forall a f. HasKey a => KeyMap f a -> Set (Key a) -> KeyMap f a
restrictKeys =
  coerce (M.restrictKeys :: Map (Key a) (f a) -> Set (Key a) -> Map (Key a) (f a))

keysSet :: KeyMap f a -> Set (Key a)
keysSet = M.keysSet . unKeyMap

empty :: KeyMap f a
empty = KeyMap M.empty

null :: KeyMap f a -> Bool
null = M.null . unKeyMap

intersectionWith
  :: forall a f. HasKey a
  => (f a -> f a -> f a)
  -> KeyMap f a
  -> KeyMap f a
  -> KeyMap f a
intersectionWith =
  coerce (M.intersectionWith :: (f a -> f a -> f a) -> Map (Key a) (f a) -> Map (Key a) (f a) -> Map (Key a) (f a))

differenceWith
  :: forall a f. HasKey a
  => (f a -> f a -> Maybe (f a))
  -> KeyMap f a
  -> KeyMap f a
  -> KeyMap f a
differenceWith =
  coerce (M.differenceWith :: (f a -> f a -> Maybe (f a)) -> Map (Key a) (f a) -> Map (Key a) (f a) -> Map (Key a) (f a))
