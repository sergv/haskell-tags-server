----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer.InputStack
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :   2 June 2017
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}

module Haskell.Language.Lexer.InputStack
  ( InputStack(..)
  , InputType(..)
  , take
  , uncons
  ) where

import Control.Arrow (second)
import Data.Text (Text)
import qualified Data.Text as T

import Prelude hiding (take)

import Data.Symbols.MacroName (MacroName)

data InputType = OriginalSource | Macro

data InputStack =
    OriginalSourceStack Text
  | MacroStack
      !MacroName          -- ^ Macro name
      !Text               -- ^ Macro body
      {-# UNPACK #-} !Int -- ^ Length of macro text
      InputStack          -- ^ Rest of the stack
  deriving (Eq, Ord, Show)

take :: Int -> InputStack -> Text
take = go []
  where
    go :: [Text] -> Int -> InputStack -> Text
    go acc !n = \case
      OriginalSourceStack txt ->
        T.concat $ T.take n txt : reverse acc
      MacroStack _ txt k rest
        | n < k     -> T.concat $ T.take n txt : reverse acc
        | otherwise -> go (txt : acc) (n - k) rest

uncons :: InputStack -> Maybe (InputType, Char, InputStack)
uncons = \case
  OriginalSourceStack txt    ->
    addInputType OriginalSource . second OriginalSourceStack <$> T.uncons txt
  MacroStack _    _   0 rest -> uncons rest
  MacroStack name txt n rest ->
    addInputType Macro . second (\txt' -> MacroStack name txt' (n - 1) rest) <$> T.uncons txt
  where
    addInputType :: t -> (a,  b) -> (t, a, b)
    addInputType t (a, b) = (t, a, b)

