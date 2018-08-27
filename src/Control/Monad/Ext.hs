----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Ext
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Wednesday, 12 October 2016
----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Ext
  ( partitionM
  ) where

import Data.Foldable
import Data.DList (DList)
import qualified Data.DList as DL

partitionM :: forall m a. (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM pred = go mempty mempty
  where
    go :: DList a -> DList a -> [a] -> m ([a], [a])
    go xs ys []       = pure (toList xs, toList ys)
    go xs ys (z : zs) = do
      res <- pred z
      if res
      then go (DL.snoc xs z) ys             zs
      else go xs             (DL.snoc ys z) zs
