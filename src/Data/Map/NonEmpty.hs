----------------------------------------------------------------------------
-- |
-- Module      :  Data.Map.NonEmpty
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Monday, 24 October 2016
----------------------------------------------------------------------------

{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

module Data.Map.NonEmpty
  ( NonEmptyMap
  , singleton
  , lookup
  , member
  , insert
  , insertWith
  , delete
  , fromNonEmpty
  , toNonEmpty
  , keysNE
  , elemsNE
  , union
  , unionWith
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Semigroup
import Prelude hiding (lookup)

-- | Map that always contains at least one key-value pair.
data NonEmptyMap k v =
  -- Invariant: map never contains root key stored in the constructor itself.
  NonEmptyMap k v (Map k v)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Ord k, Semigroup v) => Semigroup (NonEmptyMap k v) where
  (<>) = unionWith (<>)

singleton :: k -> v -> NonEmptyMap k v
singleton k v = NonEmptyMap k v M.empty

lookup :: Ord k => k -> NonEmptyMap k v -> Maybe v
lookup k (NonEmptyMap k' v' m)
  | k == k'   = Just v'
  | otherwise = M.lookup k m

member :: Ord k => k -> NonEmptyMap k v -> Bool
member k (NonEmptyMap k' _ m)
  | k == k'   = True
  | otherwise = M.member k m

insert :: Ord k => k -> v -> NonEmptyMap k v -> NonEmptyMap k v
insert = insertWith const

insertWith
  :: Ord k
  => (v -> v -> v) -> k -> v -> NonEmptyMap k v -> NonEmptyMap k v
insertWith f k v (NonEmptyMap k' v' m)
  | k == k'   = NonEmptyMap k (f v v') m
  | otherwise = NonEmptyMap k' v' $ M.insertWith f k v m

delete :: Ord k => k -> NonEmptyMap k v -> Maybe (NonEmptyMap k v)
delete k (NonEmptyMap k' v' m)
  | k == k'   =
    case M.minViewWithKey m of
      Nothing -> Nothing
      Just ((k'', v''), m') -> Just $ NonEmptyMap k'' v'' m'
  | otherwise = Just $ NonEmptyMap k' v' $ M.delete k m

fromNonEmpty :: Ord k => NonEmpty (k, v) -> NonEmptyMap k v
fromNonEmpty ((k, v) :| xs) = NonEmptyMap k v $ M.fromList xs

toNonEmpty :: NonEmptyMap k v -> NonEmpty (k, v)
toNonEmpty (NonEmptyMap k v m) = (k, v) :| M.toList m

keysNE :: NonEmptyMap k v -> NonEmpty k
keysNE (NonEmptyMap k _ m) = k :| M.keys m

elemsNE :: NonEmptyMap k v -> NonEmpty v
elemsNE (NonEmptyMap _ v m) = v :| M.elems m

union :: Ord k => NonEmptyMap k v -> NonEmptyMap k v -> NonEmptyMap k v
union = unionWith const

unionWith
  :: Ord k
  => (v -> v -> v)
  -> NonEmptyMap k v
  -> NonEmptyMap k v
  -> NonEmptyMap k v
unionWith f (NonEmptyMap k v m) (NonEmptyMap k' v' m') =
  insertWith f k' v' $
  case M.updateLookupWithKey (\_ _ -> Nothing) k m' of
    (Nothing,  m'') -> NonEmptyMap k v $ M.unionWith f m m''
    (Just v'', m'') -> NonEmptyMap k (v `f` v'') $ M.unionWith f m m''
