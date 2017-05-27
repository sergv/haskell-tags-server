----------------------------------------------------------------------------
-- |
-- Module      :  Data.SymbolMap
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Tuesday, 27 September 2016
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Data.SymbolMap
  ( SymbolMap
  , insert
  , registerChildren
  , lookup
  , lookupChildren
  , member
  , fromList
  , leaveNames
  , removeNames
  , intersectAgainst
  , keysSet
  ) where

import Control.Arrow ((&&&), second)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import Prelude hiding (lookup)

import Data.Symbols
import Text.PrettyPrint.Leijen.Text.Ext

data SymbolMap = SymbolMap
  { -- | Map from children entities to parents containing them. E.g.
    -- constructors are mapped to their corresponding datatypes, typeclass
    -- members - to typeclass names, etc.
    --
    -- Even though each child has one and only one parent, when taking union
    -- of two unrelated modules it may occur that child would be assigned to
    -- different parents. This should be prevented by careful analysis of
    -- module headers, but, unfortunately, cannot be ruled out entirely because
    -- we're not a Haskell compiler.
    smParentMap   :: Map UnqualifiedSymbolName (Set UnqualifiedSymbolName)
    -- | Map from parents to chidrens
  , smChildrenMap :: Map UnqualifiedSymbolName (Set UnqualifiedSymbolName)
  , smAllSymbols  :: Map UnqualifiedSymbolName (NonEmpty ResolvedSymbol)
  } deriving (Eq, Ord, Show)

instance Semigroup SymbolMap where
  SymbolMap x y z <> SymbolMap x' y' z' = SymbolMap
    { smParentMap   = M.unionWith (<>) x x'
    , smChildrenMap = M.unionWith (<>) y y'
    , smAllSymbols  = M.unionWith (<>) z z'
    }

instance Monoid SymbolMap where
  mempty  = SymbolMap mempty mempty mempty
  mappend = (<>)

instance Pretty SymbolMap where
  pretty SymbolMap{smParentMap, smChildrenMap, smAllSymbols} = ppDict "SymbolMap"
    [ "ParentMap"   :-> ppMap (ppSet <$> smParentMap)
    , "ChildrenMap" :-> ppMap (ppSet <$> smChildrenMap)
    , "AllSymbols"  :-> ppMap (ppNE <$> smAllSymbols)
    ]

insert :: ResolvedSymbol -> SymbolMap -> SymbolMap
insert sym m = SymbolMap
  { smParentMap   = parentMap
  , smChildrenMap = childrenMap
  , smAllSymbols  = M.alter (addToNE sym) name $ smAllSymbols m
  }
  where
    name :: UnqualifiedSymbolName
    name = resolvedSymbolName sym
    (parentMap, childrenMap) = case resolvedSymbolParent sym of
      Nothing -> (smParentMap m, smChildrenMap m)
      Just p  ->
        ( M.alter (addToSet p)    name $ smParentMap m
        , M.alter (addToSet name) p    $ smChildrenMap m
        )
    addToNE :: a -> Maybe (NonEmpty a) -> Maybe (NonEmpty a)
    addToNE x = \case
      Nothing -> Just $ x :| []
      Just xs -> Just $ NE.cons x xs
    addToSet :: (Ord a) => a -> Maybe (Set a) -> Maybe (Set a)
    addToSet x = \case
      Nothing -> Just $ S.singleton x
      Just xs -> Just $ S.insert x xs

registerChildren
  :: Map UnqualifiedSymbolName (Set UnqualifiedSymbolName) -- ^ Map from parents to children
  -> SymbolMap
  -> SymbolMap
registerChildren extraChildrenMap SymbolMap{smParentMap, smChildrenMap, smAllSymbols} = SymbolMap
  { smParentMap   = M.unionWith (<>) smParentMap extraParents
  , smChildrenMap = M.unionWith (<>) smChildrenMap extraChildrenMap'
  , smAllSymbols  = smAllSymbols
  }
  where
    extraChildrenMap' :: Map UnqualifiedSymbolName (Set UnqualifiedSymbolName)
    extraChildrenMap' =
      (`S.intersection` smAllSymbolsKeys) <$> (extraChildrenMap `M.intersection` smAllSymbols)
    extraParents :: Map UnqualifiedSymbolName (Set UnqualifiedSymbolName)
    extraParents
      = M.fromListWith (<>)
      $ concatMap (\(parent, children) -> map (\c -> (c, S.singleton parent)) $ toList children)
      $ M.toList extraChildrenMap'
    smAllSymbolsKeys :: Set UnqualifiedSymbolName
    smAllSymbolsKeys = M.keysSet smAllSymbols

lookup :: UnqualifiedSymbolName -> SymbolMap -> Maybe (NonEmpty ResolvedSymbol)
lookup sym = M.lookup sym . smAllSymbols

-- Noone uses it yet, so it's hidden here in case it will be needed later.
_lookupParent :: UnqualifiedSymbolName -> SymbolMap -> Maybe (Set UnqualifiedSymbolName)
_lookupParent sym = M.lookup sym . smParentMap

lookupChildren :: UnqualifiedSymbolName -> SymbolMap -> Maybe (Set UnqualifiedSymbolName)
lookupChildren sym = M.lookup sym . smChildrenMap

member :: UnqualifiedSymbolName -> SymbolMap -> Bool
member sym = M.member sym . smAllSymbols

fromList :: [ResolvedSymbol] -> SymbolMap
fromList syms = SymbolMap
  { smParentMap   = M.fromListWith (<>) $
      map (second S.singleton) symbolsWithParents
  , smChildrenMap = M.fromListWith (<>) $
      map (\(child, parent) -> (parent, S.singleton child)) symbolsWithParents
  , smAllSymbols  = M.fromListWith (<>) $
      map (resolvedSymbolName &&& (:| [])) syms
  }
  where
    symbolsWithParents :: [(UnqualifiedSymbolName, UnqualifiedSymbolName)]
    symbolsWithParents = mapMaybe (\sym -> (resolvedSymbolName sym,) <$> resolvedSymbolParent sym) syms

leaveNames :: SymbolMap -> Set UnqualifiedSymbolName -> SymbolMap
leaveNames SymbolMap{smParentMap, smChildrenMap, smAllSymbols} syms =
  SymbolMap
    { smParentMap   = (`S.intersection` syms) <$> (smParentMap `M.intersection` syms')
    , smChildrenMap = (`S.intersection` syms) <$> (smChildrenMap `M.intersection` syms')
    , smAllSymbols  = smAllSymbols `M.intersection` syms'
    }
  where
    syms' :: Map UnqualifiedSymbolName ()
    syms' = M.fromSet (const ()) syms

removeNames :: SymbolMap -> Set UnqualifiedSymbolName -> SymbolMap
removeNames SymbolMap{smParentMap, smChildrenMap, smAllSymbols} syms =
  SymbolMap
    { smParentMap   = (`S.difference` syms) <$> (smParentMap `M.difference` syms')
    , smChildrenMap = (`S.difference` syms) <$> (smChildrenMap `M.difference` syms')
    , smAllSymbols  = smAllSymbols `M.difference` syms'
    }
  where
    syms' :: Map UnqualifiedSymbolName ()
    syms' = M.fromSet (const ()) syms

intersectAgainst :: SymbolMap -> Set UnqualifiedSymbolName -> SymbolMap
intersectAgainst SymbolMap{smParentMap, smChildrenMap, smAllSymbols} names = SymbolMap
  { smParentMap   = (`S.difference` names) <$> (smParentMap `M.difference` namesMap)
  , smChildrenMap = (`S.difference` names) <$> (smChildrenMap `M.difference` namesMap)
  , smAllSymbols  = smAllSymbols `M.difference` namesMap
  }
  where
    namesMap :: Map UnqualifiedSymbolName ()
    namesMap = M.fromSet (const ()) names

keysSet :: SymbolMap -> Set UnqualifiedSymbolName
keysSet = M.keysSet . smAllSymbols
