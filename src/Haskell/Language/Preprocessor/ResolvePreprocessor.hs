-- |
-- Module:     Haskell.Language.Preprocessor.ResolvePreprocessor
-- Copyright:  (c) Sergey Vinokurov 2025
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OrPatterns        #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskell.Language.Preprocessor.ResolvePreprocessor
  ( preprocessorBlocks
  , resolveAlternativesLinearly
  , Tree(..)
  ) where

import Data.Foldable1 qualified as Foldable1
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Ord (comparing)
import Data.SizedList (SizedList)
import Data.SizedList qualified as SL
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

mkSeq :: Tree [a] -> Tree [a] -> Tree [a]
mkSeq (Leaf []) ys        = ys
mkSeq xs        (Leaf []) = xs
mkSeq xs        ys        = Seq xs ys

mkSeq' :: Maybe (Tree [a]) -> Tree [a] -> Tree [a]
mkSeq' Nothing   ys = ys
mkSeq' (Just xs) ys = mkSeq xs ys

preprocessorBlocks :: [Pos ServerToken] -> Tree [Pos ServerToken]
preprocessorBlocks = goTop []
  where
    goTop :: [Pos ServerToken] -> [Pos ServerToken] -> Tree [Pos ServerToken]
    goTop = go
      where
        go acc = \case
          t'@(Pos _ (Cpp t)) : ts -> case t of
            Cpp.Define{} ->
              go (t' : acc) ts

            (Cpp.Include _; Cpp.Undef _; Cpp.Endif) ->
              go acc ts

            (Cpp.If "0" ; Cpp.Elif "0") ->
              mkSeq
                (Leaf (reverse acc))
                (mkSeq'
                  cppBlock
                  (go [] rest))
              where
                (cppBlock, rest) = goNest True ts

            (Cpp.If _; Cpp.Ifdef _; Cpp.Ifndef _; Cpp.Elif _; Cpp.Else) ->
              mkSeq
                (Leaf (reverse acc))
                (mkSeq'
                  cppBlock
                  (go [] rest))
              where
                (cppBlock, rest) = goNest False ts

          t : ts -> go (t : acc) ts
          []     -> Leaf $ reverse acc

    goNest :: Bool -> [Pos ServerToken] -> (Maybe (Tree [Pos ServerToken]), [Pos ServerToken])
    goNest dropAlt = go [] id []
      where
        produceAlt :: (Tree [a] -> Tree b) -> [a] -> Maybe (Tree b)
        produceAlt f acc =
          if dropAlt then Nothing else Just $ f (Leaf (reverse acc))

        produceResult
          :: [Tree [Pos ServerToken]]
          -> (Tree [Pos ServerToken] -> Tree [Pos ServerToken])
          -> [Pos ServerToken]
          -> [Pos ServerToken]
          -> (Maybe (Tree [Pos ServerToken]), [Pos ServerToken])
        produceResult alts f acc ts = case (produceAlt f acc, alts) of
          (x,       [])     -> (x, ts)
          (Nothing, a : as) -> (Just $ mkAlt $ a :| as, ts)
          (Just x,  as)     -> (Just $ mkAlt $ x :| as, ts)

        go
          :: [Tree [Pos ServerToken]]
          -> (Tree [Pos ServerToken] -> Tree [Pos ServerToken])
          -> [Pos ServerToken]
          -> [Pos ServerToken]
          -> (Maybe (Tree [Pos ServerToken]), [Pos ServerToken])
        go alts f acc ts@[]    = produceResult alts f acc ts
        go alts f acc (t : ts) = case t of

          Pos _ (Cpp t') -> case t' of
            (Cpp.Include _; Cpp.Define _; Cpp.Undef _) ->
              continue

            (Cpp.If "0" ; Cpp.Elif "0") ->
              go alts (f . mkSeq (Leaf (reverse acc)) . mkSeq' cppBlock) [] ts'
              where
                (cppBlock, ts') = goNest True ts

            (Cpp.If _; Cpp.Ifdef _; Cpp.Ifndef _)      ->
              go alts (f . mkSeq (Leaf (reverse acc)) . mkSeq' cppBlock) [] ts'
              where
                (cppBlock, ts') = goNest False ts

            (Cpp.Elif _; Cpp.Else)                     ->
              go (prepend (produceAlt f acc) alts) id [] ts

            Cpp.Endif                                  ->
              produceResult alts f acc ts

          _ -> continue
          where
            continue = go alts f (t : acc) ts

prepend :: Maybe a -> [a] -> [a]
prepend Nothing  xs = xs
prepend (Just x) xs = x : xs

resolveAlternativesLinearly :: Tree [Pos ServerToken] -> NonEmpty [Pos ServerToken]
resolveAlternativesLinearly = fmap SL.toList . go . fmap SL.fromList
  where
    go :: Tree (SizedList (Pos ServerToken)) -> NonEmpty (SizedList (Pos ServerToken))
    go = \case
      Leaf x  -> NE.singleton x
      Alt xs  -> diag $ go <$> xs
      Seq x y -> zipAlts (<>) (go x) (go y)

diag :: forall a. NonEmpty (NonEmpty (SizedList a)) -> NonEmpty (SizedList a)
diag (xs :| xss) =
  NE.head xs :|
    case L.unsnoc xss of
      Nothing -> []
      Just (yss, ys) -> map NE.head yss ++ [NE.last ys]

-- Zip first alternative with all but last in bs. Last in bs gets zipped with last in as.
zipAlts :: forall a b c. (SizedList a -> b -> c) -> NonEmpty (SizedList a) -> NonEmpty b -> NonEmpty c
zipAlts f (x :| []) (y :| []) = f x y :| []
zipAlts f xs        (y :| []) = (`f` y) <$> xs
zipAlts f (x :| []) ys        = f x <$> ys
zipAlts f xs        ys'       = go ys'
  where
    largestX :: SizedList a
    largestX = Foldable1.maximumBy (comparing SL.length) xs

    go :: NonEmpty b -> NonEmpty c
    go (y :| [])      = f (NE.last xs) y :| []
    go (y :| y' : ys) = NE.cons (f largestX y) $ go (y' :| ys)

