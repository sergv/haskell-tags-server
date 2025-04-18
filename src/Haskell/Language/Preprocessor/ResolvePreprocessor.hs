-- |
-- Module:     Haskell.Language.Preprocessor.ResolvePreprocessor
-- Copyright:  (c) Sergey Vinokurov 2025
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OrPatterns  #-}

module Haskell.Language.Preprocessor.ResolvePreprocessor
  ( preprocessorBlocks
  , Tree(..)
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Prettyprinter.Generics

import Haskell.Language.Lexer.CppTypes qualified as Cpp
import Haskell.Language.Lexer.Types (Pos(..), ServerToken(..))

data Tree a
  = Leaf a
  | Alt (NonEmpty (Tree a))
  | Seq (Tree a) (Tree a)
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
  deriving Pretty via PPGeneric (Tree a)

mkAlt :: NonEmpty (Tree a) -> Tree a
mkAlt (x :| xs) = case xs of
  [] -> x
  ys -> Alt $ x :| ys

preprocessorBlocks :: [Pos ServerToken] -> Tree [Pos ServerToken]
preprocessorBlocks = goTop []
  where
    goTop :: [Pos ServerToken] -> [Pos ServerToken] -> Tree [Pos ServerToken]
    goTop acc = \case

      Pos _ (Cpp t) : ts -> case t of
        (Cpp.Include _; Cpp.Define _; Cpp.Undef _; Cpp.Endif) ->
          goTop acc ts

        (Cpp.If _; Cpp.Ifdef _; Cpp.Ifndef _; Cpp.Elif; Cpp.Else) ->
          Seq
            (Leaf (reverse acc))
            (Seq
              cppBlock
              (goTop [] rest))
          where
            (cppBlock, rest) = goNest ts

      t : ts -> goTop (t : acc) ts
      []     -> Leaf $ reverse acc

    goNest :: [Pos ServerToken] -> (Tree [Pos ServerToken], [Pos ServerToken])
    goNest = go [] id []
      where
        go
          :: [Tree [Pos ServerToken]]
          -> (Tree [Pos ServerToken] -> Tree [Pos ServerToken])
          -> [Pos ServerToken]
          -> [Pos ServerToken]
          -> (Tree [Pos ServerToken], [Pos ServerToken])
        go alts f acc []       = (mkAlt $ f (Leaf (reverse acc)) :| alts, [])
        go alts f acc (t : ts) = case t of

          Pos _ (Cpp t') -> case t' of
            (Cpp.Include _; Cpp.Define _; Cpp.Undef _) ->
              continue

            (Cpp.If _; Cpp.Ifdef _; Cpp.Ifndef _)      ->
              go alts (f . Seq (Leaf (reverse acc)) . Seq cppBlock) [] ts'
              where
                (cppBlock, ts') = goNest ts

            (Cpp.Elif; Cpp.Else)                       ->
              go (f (Leaf (reverse acc)) : alts) id [] ts

            Cpp.Endif                                  ->
              (mkAlt $ f (Leaf (reverse acc)) :| alts, ts)

          _ -> continue
          where
            continue = go alts f (t : acc) ts
