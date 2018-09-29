----------------------------------------------------------------------------
-- |
-- Module      :  Control.Parallel.Strategies.Ext
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

module Control.Parallel.Strategies.Ext
  ( foldPar
  , module Control.Parallel.Strategies
  ) where

import Control.Parallel.Strategies

{-# INLINE foldPar #-}
-- | Reduce argument in a tree-like fashion.
--
-- NB only safe when used with commutative monoids.
foldPar :: Monoid a => [a] -> Eval a
foldPar = \case
  []     -> pure mempty
  x : xs -> go [] x xs
  where
    go []  x []         = pure x
    go acc x []         = go [] x acc
    go acc x [y1]       = do
      z <- rpar $ x <> y1
      go [] z acc
    go acc x (y1:y2:ys) = do
      z <- rpar $ x <> y1
      go (z : acc) y2 ys
