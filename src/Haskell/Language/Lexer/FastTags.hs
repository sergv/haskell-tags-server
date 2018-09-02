----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer.FastTags
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  20 June 2017
----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric      #-}
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
  , module FastTags.Token
  , module FastTags.Tag
  ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Ext
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
  | HSC2HS -- Any HSC2HS directive
  | Tok !TokenVal
  -- | Error
  deriving (Eq, Ord, Show, Generic)

instance Pretty ServerToken where
  pretty = ppGeneric

deriving instance Generic TokenVal

instance Pretty TokenVal where
  pretty = ppGeneric

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
stripServerTokens xs = [ Pos p x | Pos p (Tok x) <- xs ]

processTokens :: [Pos ServerToken] -> ([Pos TagVal], [String])
processTokens = FastTags.Tag.processTokens . stripServerTokens
