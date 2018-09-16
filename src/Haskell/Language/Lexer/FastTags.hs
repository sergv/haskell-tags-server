----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer.FastTags
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  20 June 2017
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Haskell.Language.Lexer.FastTags
  ( PragmaType(..)
  , ServerToken(..)
  , tokToName
  , stripNewlines
  , processTokens
  , stripServerTokens
  , removeDuplicatePatterns
  , module FastTags.Token
  , module FastTags.Tag
  ) where

import Control.Arrow (second)
import Control.DeepSeq

import Data.Either
import Data.Hashable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Ext
import Data.Void (Void)
import GHC.Generics (Generic)

import Data.IgnoreEqOrdHash

import FastTags.Tag
  ( TagVal(..)
  , Pos(..)
  , Type(..)
  , breakBlocks
  , whereBlock
  , UnstrippedTokens(..)
  , unstrippedTokensOf
  )
import FastTags.Token (Line(..), SrcPos(..), TokenVal(..), increaseLine, posFile, posLine, unLine)

import qualified FastTags.Tag (processTokens)
-- import FastTags.Tag (tokToName, processTokens)
-- import FastTags.Token (PragmaType(..))

data PragmaType = SourcePragma
  deriving (Show, Eq, Ord, Generic)

instance Hashable PragmaType
instance NFData   PragmaType

instance Pretty PragmaType where
  pretty = ppGeneric

data ServerToken =
    Pragma !PragmaType
  | HSC2HS  -- Any HSC2HS directive
  | LBanana -- Arrows: (|
  | RBanana -- Arrows: |)
  | Tok !TokenVal
  | Error (IgnoreEqOrdHash (Doc Void))
  deriving (Eq, Ord, Show, Generic)

instance Hashable ServerToken

instance NFData ServerToken where
  rnf = \case
    Pragma x -> rnf x
    HSC2HS   -> ()
    LBanana  -> ()
    RBanana  -> ()
    Tok x    -> rnf x
    Error x  -> x `seq` ()

instance Pretty ServerToken where
  pretty = ppGeneric

deriving instance Generic TokenVal
instance Hashable TokenVal
instance NFData   TokenVal

instance Pretty TokenVal where
  pretty = ppGeneric

deriving instance Generic Line
instance Hashable Line

deriving instance Generic SrcPos
instance Hashable SrcPos

instance Pretty SrcPos where
  pretty SrcPos{posFile, posLine} = pretty posFile <> ":" <> pretty (unLine posLine)

deriving instance Generic (Pos a)
instance Hashable a => Hashable (Pos a)

instance Pretty a => Pretty (Pos a) where
  pretty = ppGeneric

deriving instance Generic Type
instance Hashable Type

instance Pretty Type where
  pretty = ppGeneric

deriving instance Generic TagVal
instance Hashable TagVal

tokToName :: ServerToken -> Maybe Text
tokToName (Tok ExclamationMark) = Just "!"
tokToName (Tok Tilde)           = Just "~"
tokToName (Tok Dot)             = Just "."
tokToName (Tok (T "_"))         = Nothing
tokToName (Tok (T name))        = Just name
tokToName _                     = Nothing

stripNewlines :: [Pos ServerToken] -> [Pos ServerToken]
stripNewlines = filter isNonNewline
  where
    isNonNewline (Pos _ (Tok (Newline _))) = False
    isNonNewline _                         = True

stripServerTokens :: [Pos ServerToken] -> ([Pos TokenVal], [Doc Void])
stripServerTokens = partitionEithers . mapMaybe f
  where
    f :: Pos ServerToken -> Maybe (Either (Pos TokenVal) (Doc Void))
    f = \case
      Pos p (Tok x)     -> Just $ Left $ Pos p x
      Pos _ (Error msg) -> Just $ Right $ unIgnoreEqOrdHash msg
      _                 -> Nothing

processTokens :: [Pos ServerToken] -> ([Pos TagVal], [Doc Void])
processTokens toks
  = second ((errs ++) . map docFromString)
  $ FastTags.Tag.processTokens toks'
  where
    (toks', errs) = stripServerTokens toks

-- | Keep only one Pattern tag for each unique name.
removeDuplicatePatterns :: [Pos TagVal] -> [Pos TagVal]
removeDuplicatePatterns = go mempty
  where
    go :: Map Text SrcPos -> [Pos TagVal] -> [Pos TagVal]
    go !acc []     =
      map (\(name, pos) -> Pos pos (TagVal name Pattern Nothing)) $ M.toList acc
    go !acc (t:ts) =
      case t of
        Pos pos TagVal{tvName, tvType = Pattern, tvParent = Nothing} ->
          go (M.insertWith minPos tvName pos acc) ts
        t' -> t' : go acc ts

minPos :: SrcPos -> SrcPos -> SrcPos
minPos p1@SrcPos{posLine = l1} p2@SrcPos{posLine = l2}
  | l1 < l2   = p1
  | otherwise = p2
