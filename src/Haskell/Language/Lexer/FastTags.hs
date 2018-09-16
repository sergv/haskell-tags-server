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

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Ext
import Data.Void (Void)
import GHC.Generics (Generic)

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

instance Pretty PragmaType where
  pretty = ppGeneric

data ServerToken =
    Pragma !PragmaType
  | HSC2HS  -- Any HSC2HS directive
  | LBanana -- Arrows: (|
  | RBanana -- Arrows: |)
  | Tok !TokenVal
  -- | Error
  deriving (Eq, Ord, Show, Generic)

instance Pretty ServerToken where
  pretty = ppGeneric

deriving instance Generic TokenVal

instance Pretty TokenVal where
  pretty = ppGeneric

instance Pretty SrcPos where
  pretty SrcPos{posFile, posLine} = pretty posFile <> ":" <> pretty (unLine posLine)

deriving instance Generic (Pos a)

instance Pretty a => Pretty (Pos a) where
  pretty = ppGeneric

instance Pretty Type where
  pretty = ppShow

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

stripServerTokens :: [Pos ServerToken] -> [Pos TokenVal]
stripServerTokens xs = [Pos p x | Pos p (Tok x) <- xs]

processTokens :: [Pos ServerToken] -> ([Pos TagVal], [Doc Void])
processTokens =
  second (map docFromString) . FastTags.Tag.processTokens . stripServerTokens

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
