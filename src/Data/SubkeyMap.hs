----------------------------------------------------------------------------
-- |
-- Module      :  Data.SubkeyMap
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Saturday,  8 October 2016
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Data.SubkeyMap
  ( SubkeyMap
  , HasSubkey(..)
  , empty
  , null
  , insert
  , lookup
  , lookupSubkey
  , lookupSubkeyKeys
  , alter'
  , traverseWithKey
  , fromList
  , fromFoldable
  , toList
  , toSubkeyList
  , toSubkeyKeyList
  , keys
  ) where

import Control.Arrow
import Control.Monad.State.Strict
import Data.Foldable (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S
import Prelude hiding (lookup, null)

class (Ord k, Ord (Subkey k)) => HasSubkey k where
  type Subkey k :: *
  getSubkey :: k -> Subkey k

-- | Map which can index same set of values by two keys. One key is the
-- main one (the bigger one), the second key is the subkey which is a projection
-- of the main key. -- Since it is a projection, it may reference several
-- values.
--
-- Deletions are not provided for the time being in order to simplify
-- impementation.
--
-- Invariant: both keys are kept in sync with each other.
data SubkeyMap k v = SubkeyMap
  { smMainMap :: !(Map k v)
  , smSubMap  :: !(Map (Subkey k) (Set k))
  } deriving (Functor, Foldable, Traversable)

deriving instance (Show k, Show (Subkey k), Show v) => Show (SubkeyMap k v)
deriving instance (Eq k,   Eq (Subkey k),   Eq v)   => Eq (SubkeyMap k v)
deriving instance (Ord k,  Ord (Subkey k),  Ord v)  => Ord (SubkeyMap k v)

empty :: SubkeyMap k v
empty = SubkeyMap
  { smMainMap = M.empty
  , smSubMap  = M.empty
  }

null :: SubkeyMap k v -> Bool
null = M.null . smMainMap

insert :: (HasSubkey k) => k -> v -> SubkeyMap k v -> SubkeyMap k v
insert k v SubkeyMap{smMainMap, smSubMap} = SubkeyMap
  { smMainMap = M.insert k v smMainMap
  , smSubMap  = M.insertWith (<>) (getSubkey k) (S.singleton k) smSubMap
  }

lookup :: (Ord k) => k -> SubkeyMap k v -> Maybe v
lookup k = M.lookup k . smMainMap

lookupSubkey :: (HasSubkey k) => Subkey k -> SubkeyMap k v -> [v]
lookupSubkey k SubkeyMap{smMainMap, smSubMap} =
  case M.lookup k smSubMap of
    Nothing   -> []
    Just idxs -> smMainMap `indexBySet` idxs

lookupSubkeyKeys :: (HasSubkey k) => Subkey k -> SubkeyMap k v -> Maybe (Set k)
lookupSubkeyKeys k = M.lookup k . smSubMap

alter' :: (HasSubkey k) => (Maybe v -> v) -> k -> SubkeyMap k v -> SubkeyMap k v
alter' f k SubkeyMap{smMainMap, smSubMap} = SubkeyMap
  { smMainMap = M.alter (Just . f) k smMainMap
  , smSubMap  = M.insertWith (<>) (getSubkey k) (S.singleton k) smSubMap
  }

traverseWithKey :: (Applicative f) => (k -> v -> f v') -> SubkeyMap k v -> f (SubkeyMap k v')
traverseWithKey f sm@SubkeyMap{smMainMap} =
  (\smMainMap' -> sm { smMainMap = smMainMap' }) <$> M.traverseWithKey f smMainMap

fromList :: (HasSubkey k) => [(k, v)] -> SubkeyMap k v
fromList = fromFoldable

fromFoldable :: (Foldable f, HasSubkey k) => f (k, v) -> SubkeyMap k v
fromFoldable = foldl' (\acc (k, v) -> insert k v acc) empty

toList :: SubkeyMap k v -> [(k, v)]
toList = M.toList . smMainMap

toSubkeyList :: (Ord k) => SubkeyMap k v -> [(Subkey k, [v])]
toSubkeyList SubkeyMap{smMainMap, smSubMap} =
  map (second (smMainMap `indexBySet`)) $ M.toList smSubMap

toSubkeyKeyList :: (Ord k) => SubkeyMap k v -> [(Subkey k, Set k)]
toSubkeyKeyList = M.toList . smSubMap

keys :: SubkeyMap k v -> [k]
keys = M.keys . smMainMap

-- Utils

indexBySet :: (Ord k) => Map k v -> Set k -> [v]
indexBySet m ixs = M.elems $ m `M.intersection` M.fromSet (const ()) ixs
