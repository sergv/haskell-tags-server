----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer.RulePredicate
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  19 June 2017
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Haskell.Language.Lexer.RulePredicate
  ( RulePredM
  , runRulePredM
  , AlexPred
  , matchedNameInPredicate
  , isLiterate
  , isInBirdEnv
  , isInLatexCodeEnv
  , isNameDefinedAsConstant
  , isNameDefinedAsFunction
  , (.&&&.)
  ) where

import Control.Monad.Reader
-- import Data.HasLens
import Data.Profunctor
import Data.Semigroup
import Data.Text (Text)

import qualified Data.KeyMap as KM
import Data.Symbols.MacroName (MacroName)
import Haskell.Language.Lexer.Env
import Haskell.Language.Lexer.Input
import Haskell.Language.Lexer.Preprocessor (isConstant, isFunction)
import Haskell.Language.Lexer.State
import Haskell.Language.Lexer.Types

newtype RulePredM r a = RulePredM
  { runRulePredM
      :: r         -- ^ Predicate state.
      -> AlexInput -- ^ Input stream before the token.
      -> Int       -- ^ Length of the token.
      -> AlexInput -- ^ Input stream after the token.
      -> a
  } deriving (Functor)

type AlexPred a = RulePredM a Bool

instance Applicative (RulePredM r) where
  pure x = RulePredM $ \_ _ _ _ -> x
  RulePredM gf <*> RulePredM gx =
    RulePredM $ \a b c d -> gf a b c d $ gx a b c d

instance Monad (RulePredM r) where
  return = pure
  RulePredM gf >>= k =
    RulePredM $ \a b c d -> runRulePredM (k (gf a b c d)) a b c d

instance MonadReader r (RulePredM r) where
  ask = RulePredM $ \x _ _ _ -> x
  local f (RulePredM g) = RulePredM $ \a -> g (f a)

instance Profunctor RulePredM where
  dimap f g (RulePredM action) =
    RulePredM $ \a b c d -> g $ action (f a) b c d

matchedNameInPredicate :: RulePredM r Text
matchedNameInPredicate =
  RulePredM $ \_ input len _ -> retrieveToken input len

isLiterate :: AlexPred AlexEnv
isLiterate = do
  env <- ask
  pure $ case aeLiterateMode env of
    Literate -> True
    Vanilla  -> False

isInBirdEnv :: AlexPred AlexState
isInBirdEnv = do
  style <- asks asLiterateStyle
  pure $ case style of
    Nothing    -> False
    Just Latex -> False
    Just Bird  -> True

isInLatexCodeEnv :: AlexPred AlexState
isInLatexCodeEnv = do
  style <- asks asLiterateStyle
  pure $ case style of
    Nothing    -> False
    Just Latex -> True
    Just Bird  -> False

-- | Check whether given name is a cpp define.
isNameDefinedAsConstant :: MacroName -> AlexPred AlexState
isNameDefinedAsConstant name = -- do
    asks
  $ getAny
  . foldMap (foldMap (Any . isConstant))
  . KM.lookup name
  . asDefines
  -- defines <- asks asDefines
  -- pure $ if KM.null defines
  --   then False
  --   else getAny $ foldMap (foldMap (Any . isConstant)) $ KM.lookup name $ defines

-- | Check whether given name is a cpp define of a macro with arguments.
isNameDefinedAsFunction :: MacroName -> AlexPred AlexState
isNameDefinedAsFunction name = -- do
    asks
  $ getAny
  . foldMap (foldMap (Any . isFunction))
  . KM.lookup name
  . asDefines
  -- defines <- asks asDefines
  -- pure $ if KM.null defines
  --   then False
  --   else getAny $ foldMap (foldMap (Any . isFunction)) $ KM.lookup name $ defines

(.&&&.) :: AlexPred a -> AlexPred b -> AlexPred (a, b)
(.&&&.) x y = (&&) <$> lmap fst x <*> lmap snd y
