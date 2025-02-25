----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer.FastTags
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  20 June 2017
----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Haskell.Language.Lexer.FastTags
  ( PragmaType(..)
  , ServerToken(..)
  , tokToName
  , stripNewlines
  , processTokens
  , stripServerTokens
  , embedServerToken
  , removeDuplicatePatterns

  , forallServerToken
  , patternServerToken

  , FastTags.TokenVal
  , module FastTags.Token
  , module FastTags.Tag
  ) where

import Control.Arrow (second)
import Control.DeepSeq

import Data.Either
import Data.Hashable
import Data.IgnoreEqOrdHashNFData
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Store (Store)
import Data.Text (Text)
import Data.Void (Void)
import Prettyprinter.Ext
import System.FilePath (takeExtension)

import FastTags.Tag qualified as FastTags
import FastTags.Token qualified as FastTags
import FastTags.Tag
  ( Pos(..)
  , TagVal(..)
  , ParentTag(..)
  , Type(..)
  , breakBlocks
  , whereBlock
  , UnstrippedTokens(..)
  , unstrippedTokensOf
  )
import FastTags.Token (Line(..), Offset(..), SrcPos(..), increaseLine, posLine, unLine)

data PragmaType = SourcePragma
  deriving (Show, Eq, Ord, Generic)

instance Hashable PragmaType
instance NFData   PragmaType

instance Pretty PragmaType where
  pretty = ppGeneric


data ServerToken
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
  | T {-# UNPACK  #-} !Text
  -- | Special token, not part of Haskell spec. Stores indentation.
  | Newline {-# UNPACK  #-} !Int
  -- | String contents is not tracked since it's irrelevant.
  | String
  -- | Actual character not tracked since it's irrelevant.
  | Character
  -- | Actual value not tracked since it's irrelevant.
  | Number
  | QuasiquoterStart
  | QuasiquoterEnd
  | SpliceStart -- \$(
  | ToplevelSplice -- e.g. \$foo
  | LambdaBackslash -- \

  | CppDefine {-# UNPACK #-} !Text
  | HSCEnum      -- #{enum...}
  | HSCDirective -- e.g. #define foo bar...
  | HSCDirectiveBraced
    -- ^ e.g. #{define foo...\nbar}, #{\ndefine foo...\nbar}, ends with RBrace
  | LBanana -- Arrows: (|
  | RBanana -- Arrows: |)
  | Error (IgnoreEqOrdHashNFData (Doc Void))
  | DQuote -- '"' when not part of string in Alex or Happy

  | Pragma !PragmaType -- Actual new addition compared to fast-tags

  | EOF
  deriving (Eq, Ord, Show, Generic)

instance Hashable ServerToken

instance NFData ServerToken where

instance Pretty ServerToken where
  pretty = ppGeneric

instance Hashable FastTags.TokenVal

instance Pretty FastTags.TokenVal where
  pretty = ppGeneric

deriving instance Generic  Line
deriving instance Hashable Line
deriving instance Pretty   Line
deriving instance Store    Line

instance Pretty SrcPos where
  pretty SrcPos{posLine} = pretty (unLine posLine)

deriving instance Generic Type
instance Hashable Type
instance Store    Type

instance Pretty Type where
  pretty = ppGeneric

deriving instance Generic FastTags.TagVal
instance Hashable FastTags.TagVal

deriving instance Generic FastTags.ParentTag
instance Hashable FastTags.ParentTag

tokToName :: ServerToken -> Maybe Text
tokToName ExclamationMark = Just "!"
tokToName Tilde           = Just "~"
tokToName Dot             = Just "."
tokToName (T "_")         = Nothing
tokToName (T name)        = Just name
tokToName _               = Nothing

stripNewlines :: [Pos ServerToken] -> [Pos ServerToken]
stripNewlines = filter isNonNewline
  where
    isNonNewline (Pos _ (Newline _)) = False
    isNonNewline _                   = True

stripServerTokens :: [Pos ServerToken] -> ([Pos FastTags.TokenVal], [Doc Void])
stripServerTokens = second catMaybes . partitionEithers . map f
  where
    f :: Pos ServerToken -> Either (Pos FastTags.TokenVal) (Maybe (Doc Void))
    f (Pos p t) = case t of
      KWCase             -> Left $ Pos p FastTags.KWCase
      KWClass            -> Left $ Pos p FastTags.KWClass
      KWData             -> Left $ Pos p FastTags.KWData
      KWDefault          -> Left $ Pos p FastTags.KWDefault
      KWDeriving         -> Left $ Pos p FastTags.KWDeriving
      KWDo               -> Left $ Pos p FastTags.KWDo
      KWElse             -> Left $ Pos p FastTags.KWElse
      KWFamily           -> Left $ Pos p FastTags.KWFamily
      KWForeign          -> Left $ Pos p FastTags.KWForeign
      KWIf               -> Left $ Pos p FastTags.KWIf
      KWImport           -> Left $ Pos p FastTags.KWImport
      KWIn               -> Left $ Pos p FastTags.KWIn
      KWInfix            -> Left $ Pos p FastTags.KWInfix
      KWInfixl           -> Left $ Pos p FastTags.KWInfixl
      KWInfixr           -> Left $ Pos p FastTags.KWInfixr
      KWInstance         -> Left $ Pos p FastTags.KWInstance
      KWLet              -> Left $ Pos p FastTags.KWLet
      KWModule           -> Left $ Pos p FastTags.KWModule
      KWNewtype          -> Left $ Pos p FastTags.KWNewtype
      KWOf               -> Left $ Pos p FastTags.KWOf
      KWThen             -> Left $ Pos p FastTags.KWThen
      KWType             -> Left $ Pos p FastTags.KWType
      KWWhere            -> Left $ Pos p FastTags.KWWhere
      Arrow              -> Left $ Pos p FastTags.Arrow
      At                 -> Left $ Pos p FastTags.At
      Backtick           -> Left $ Pos p FastTags.Backtick
      Comma              -> Left $ Pos p FastTags.Comma
      Dot                -> Left $ Pos p FastTags.Dot
      DoubleColon        -> Left $ Pos p FastTags.DoubleColon
      Equals             -> Left $ Pos p FastTags.Equals
      ExclamationMark    -> Left $ Pos p FastTags.ExclamationMark
      Implies            -> Left $ Pos p FastTags.Implies
      LBrace             -> Left $ Pos p FastTags.LBrace
      LBracket           -> Left $ Pos p FastTags.LBracket
      LParen             -> Left $ Pos p FastTags.LParen
      Pipe               -> Left $ Pos p FastTags.Pipe
      RBrace             -> Left $ Pos p FastTags.RBrace
      RBracket           -> Left $ Pos p FastTags.RBracket
      RParen             -> Left $ Pos p FastTags.RParen
      Tilde              -> Left $ Pos p FastTags.Tilde
      Semicolon          -> Left $ Pos p FastTags.Semicolon
      T x                -> Left $ Pos p $ FastTags.T x
      Newline n          -> Left $ Pos p $ FastTags.Newline n
      String             -> Left $ Pos p FastTags.String
      Character          -> Left $ Pos p FastTags.Character
      Number             -> Left $ Pos p FastTags.Number
      QuasiquoterStart   -> Left $ Pos p FastTags.QuasiquoterStart
      QuasiquoterEnd     -> Left $ Pos p FastTags.QuasiquoterEnd
      SpliceStart        -> Left $ Pos p FastTags.SpliceStart
      ToplevelSplice     -> Left $ Pos p FastTags.ToplevelSplice
      LambdaBackslash    -> Left $ Pos p FastTags.LambdaBackslash

      CppDefine x        -> Left $ Pos p $ FastTags.CppDefine x
      HSCEnum            -> Left $ Pos p $ FastTags.HSCEnum
      HSCDirective       -> Left $ Pos p $ FastTags.HSCDirective
      HSCDirectiveBraced -> Left $ Pos p $ FastTags.HSCDirectiveBraced
      LBanana            -> Left $ Pos p FastTags.LBanana
      RBanana            -> Left $ Pos p FastTags.RBanana
      Error msg          -> Right $ Just $ unIgnoreEqOrdHashNFData msg
      DQuote             -> Left $ Pos p FastTags.DQuote

      Pragma _           -> Right Nothing

      EOF                -> Left $ Pos p FastTags.EOF

embedServerToken :: FastTags.TokenVal -> Maybe ServerToken
embedServerToken = \case
  FastTags.KWCase             -> Just KWCase
  FastTags.KWClass            -> Just KWClass
  FastTags.KWData             -> Just KWData
  FastTags.KWDefault          -> Just KWDefault
  FastTags.KWDeriving         -> Just KWDeriving
  FastTags.KWDo               -> Just KWDo
  FastTags.KWElse             -> Just KWElse
  FastTags.KWFamily           -> Just KWFamily
  FastTags.KWForeign          -> Just KWForeign
  FastTags.KWIf               -> Just KWIf
  FastTags.KWImport           -> Just KWImport
  FastTags.KWIn               -> Just KWIn
  FastTags.KWInfix            -> Just KWInfix
  FastTags.KWInfixl           -> Just KWInfixl
  FastTags.KWInfixr           -> Just KWInfixr
  FastTags.KWInstance         -> Just KWInstance
  FastTags.KWLet              -> Just KWLet
  FastTags.KWModule           -> Just KWModule
  FastTags.KWNewtype          -> Just KWNewtype
  FastTags.KWOf               -> Just KWOf
  FastTags.KWThen             -> Just KWThen
  FastTags.KWType             -> Just KWType
  FastTags.KWWhere            -> Just KWWhere
  FastTags.Arrow              -> Just Arrow
  FastTags.At                 -> Just At
  FastTags.Backtick           -> Just Backtick
  FastTags.Comma              -> Just Comma
  FastTags.Dot                -> Just Dot
  FastTags.DoubleColon        -> Just DoubleColon
  FastTags.Equals             -> Just Equals
  FastTags.ExclamationMark    -> Just ExclamationMark
  FastTags.Implies            -> Just Implies
  FastTags.LBrace             -> Just LBrace
  FastTags.LBracket           -> Just LBracket
  FastTags.LParen             -> Just LParen
  FastTags.Pipe               -> Just Pipe
  FastTags.RBrace             -> Just RBrace
  FastTags.RBracket           -> Just RBracket
  FastTags.RParen             -> Just RParen
  FastTags.Tilde              -> Just Tilde
  FastTags.Semicolon          -> Just Semicolon
  FastTags.T x                -> Just $ T x
  FastTags.Newline n          -> Just $ Newline n
  FastTags.String             -> Just String
  FastTags.Character          -> Just Character
  FastTags.Number             -> Just Number
  FastTags.QuasiquoterStart   -> Just QuasiquoterStart
  FastTags.QuasiquoterEnd     -> Just QuasiquoterEnd
  FastTags.SpliceStart        -> Just SpliceStart
  FastTags.LambdaBackslash    -> Just LambdaBackslash
  FastTags.EOF                -> Just EOF

  FastTags.ToplevelSplice     -> Just ToplevelSplice
  FastTags.CppDefine x        -> Just $ CppDefine x
  FastTags.HSCEnum            -> Just HSCEnum
  FastTags.HSCDirective       -> Just HSCDirective

  FastTags.HSCDirectiveBraced -> Just HSCDirectiveBraced
  FastTags.LBanana            -> Just LBanana
  FastTags.RBanana            -> Just RBanana
  FastTags.Error msg          -> Just $ Error $ IgnoreEqOrdHashNFData $ pretty msg

  FastTags.DQuote             -> Just DQuote

processTokens :: FilePath -> [Pos ServerToken] -> ([Pos FastTags.TagVal], [Doc Void])
processTokens filename toks
  = second ((errs ++) . map docFromString)
  $ FastTags.processTokens mode toks'
  where
    (toks', errs) = stripServerTokens toks
    mode :: FastTags.ProcessMode
    mode
      | takeExtension filename `elem` [".x", ".lx", ".y", ".ly"]
      = FastTags.ProcessAlexHappy
      | otherwise
      = FastTags.ProcessVanilla

-- | Keep only one Pattern tag for each unique name.
removeDuplicatePatterns :: [Pos FastTags.TagVal] -> [Pos FastTags.TagVal]
removeDuplicatePatterns = go mempty
  where
    go :: Map Text SrcPos -> [Pos FastTags.TagVal] -> [Pos FastTags.TagVal]
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

forallServerToken :: ServerToken
forallServerToken = T "forall"

patternServerToken :: ServerToken
patternServerToken = T "pattern"

