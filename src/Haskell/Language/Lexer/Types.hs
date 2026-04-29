---------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer.Types
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Thursday,  3 November 2016
----------------------------------------------------------------------------

{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Haskell.Language.Lexer.Types
  ( mkSrcPos
  , Context(..)
  , AlexCode(..)
  , LitStyle(..)
  , LitMode(..)
  , isLiterateEnabled
  , isLiterateBirdInside
  , isLiterateLatexInside

  , PragmaType(..)
  , Token(..)
  , tokToName
  , tokToNameExcludingBangPatSyms
  , removeDuplicatePatterns

  , forallServerToken
  , patternServerToken

  , PPTokens(..)
  , ppTokens
  ) where

import Control.DeepSeq
import Data.IgnoreEqOrdHashNFData
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Void (Void)
import Prettyprinter qualified as PP
import Prettyprinter.Ext

import Haskell.Language.Lexer.CppTypes qualified as Cpp
import Haskell.Language.Tags.Types

{-# INLINE mkSrcPos #-}
mkSrcPos :: Line -> SrcPos
mkSrcPos line = SrcPos
  { posLine   = line
  , posPrefix = mempty
  , posOffset = Offset 0
  , posSuffix = mempty
  }

data Context
  = CtxHaskell
  | CtxQuasiquoter
  deriving (Eq, Ord, Show)

-- | Abstract wrapper around alex automata states.
newtype AlexCode = AlexCode { unAlexCode :: Int }
  deriving (Eq, Ord, Show, Pretty, Enum, Num, Real, Integral)

data LitStyle = Bird | Latex
  deriving (Eq, Ord, Show, Enum, Bounded)

data LitMode a
  = LitInside !a -- ^ Inside literal code block
  | LitOutside -- ^ Outside literal code block
  | LitVanilla -- ^ Processing regular file without literate parts
  deriving (Eq, Ord, Show, Functor, Generic)
  deriving Pretty via PPGeneric (LitMode a)

{-# INLINE isLiterateEnabled #-}
isLiterateEnabled :: LitMode a -> Bool
isLiterateEnabled = \case
  LitInside _ -> True
  LitOutside  -> True
  LitVanilla  -> False

{-# INLINE isLiterateBirdInside #-}
isLiterateBirdInside :: LitMode LitStyle -> Bool
isLiterateBirdInside = \case
  LitInside Bird  -> True
  LitInside Latex -> False
  LitOutside      -> True
  LitVanilla      -> False

{-# INLINE isLiterateLatexInside #-}
isLiterateLatexInside :: LitMode LitStyle -> Bool
isLiterateLatexInside = \case
  LitInside Bird  -> False
  LitInside Latex -> True
  LitOutside      -> False
  LitVanilla      -> False


data PragmaType = SourcePragma
  deriving (Show, Eq, Ord, Generic)
  deriving Pretty via PPGeneric (PragmaType)

instance NFData PragmaType

data Token
  = KWCase
  | KWClass
  | KWData
  | KWDefault
  | KWDeriving
  | KWDo
  | KWElse
  | KWFamily
  | KWForeign
  | KWIf
  | KWImport
  | KWIn
  | KWInfix
  | KWInfixl
  | KWInfixr
  | KWInstance
  | KWLet
  | KWModule
  | KWNewtype
  | KWOf
  | KWThen
  | KWType
  | KWWhere
  | Arrow
  | At
  | Backtick
  | Comma
  | Dot
  | DoubleColon
  | Equals
  | ExclamationMark
  | Implies
  | LBrace
  | LBracket
  | LParen
  | Pipe
  | RBrace
  | RBracket
  | RParen
  | Tilde
  | Semicolon
  | T {-# UNPACK #-} !Text
  -- | Special token, not part of Haskell spec. Stores indentation.
  | Newline {-# UNPACK #-} !Int
  -- | String contents is not tracked since it's irrelevant.
  | String
  -- | Actual character not tracked since it's irrelevant.
  | Character
  -- | Actual value not tracked since it's irrelevant.
  | Number
  | QuasiquoterStart
  | QuasiquoterEnd
  | SpliceStart -- \$(, ends with RParen
  | ToplevelSplice -- e.g. \$foo
  | LambdaBackslash -- \
  | Cpp !Cpp.Directive
  | HSCEnum      -- #{enum...}
  | HSCDirective -- e.g. #define foo bar...
  | HSCDirectiveBraced -- ^ e.g. #{define foo...\nbar}, #{\ndefine foo...\nbar}, ends with RBrace
  | LBanana      -- Arrows: (|
  | RBanana      -- Arrows: |)
  | Error (IgnoreEqOrdHashNFData (Doc Void))
  | DQuote -- '"' when not part of string in Alex or Happy

  | Pragma !PragmaType -- Actual new addition compared to fast-tags

  | EOF
  deriving (Eq, Ord, Show, Generic)
  deriving Pretty via PPGeneric Token


tokToNameExcludingBangPatSyms :: Token -> Maybe Text
tokToNameExcludingBangPatSyms (T "_")  = Nothing
tokToNameExcludingBangPatSyms (T name) = Just name
tokToNameExcludingBangPatSyms Dot      = Just "."
tokToNameExcludingBangPatSyms _        = Nothing

tokToName :: Token -> Maybe Text
tokToName ExclamationMark = Just "!"
tokToName Tilde           = Just "~"
tokToName x               = tokToNameExcludingBangPatSyms x

-- | Keep only one Pattern tag for each unique name.
removeDuplicatePatterns :: [Pos TagVal] -> [Pos TagVal]
removeDuplicatePatterns = go mempty
  where
    go :: Map Text SrcPos -> [Pos TagVal] -> [Pos TagVal]
    go !acc []     =
      map (\(name, pos) -> Pos pos (TagVal name Pattern Nothing)) $ M.toList acc
    go  acc (t:ts) =
      case t of
        Pos pos TagVal{tvName, tvType = Pattern, tvParent = Nothing} ->
          go (M.insertWith minPos tvName pos acc) ts
        t' -> t' : go acc ts

minPos :: SrcPos -> SrcPos -> SrcPos
minPos p1@SrcPos{posLine = l1} p2@SrcPos{posLine = l2}
  | l1 < l2   = p1
  | otherwise = p2

forallServerToken :: Token
forallServerToken = T "forall"

patternServerToken :: Token
patternServerToken = T "pattern"

newtype PPTokens = PPTokens [Pos Token]

instance Pretty PPTokens where
  pretty (PPTokens ts) =
    ppDictHeader "Tokens"
      [ "tokens" :-> ppListWith ppTokenVal ts
      ]
    where
      ppTokenVal :: Pos Token -> Doc ann
      ppTokenVal (Pos SrcPos{posLine} tok) =
        pretty (unLine posLine) <> PP.colon <> pretty tok

ppTokens :: [Pos Token] -> Doc ann
ppTokens = pretty . PPTokens . take 16



