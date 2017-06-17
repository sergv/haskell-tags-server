----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer.Preprocessor
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  27 May 2017
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Haskell.Language.Lexer.Preprocessor
  ( PreprocessorMacro(..)
  , isConstant
  , isFunction
  , parsePreprocessorDefine
  , parsePreprocessorUndef
  ) where

import Control.Monad.Except.Ext
import Data.Attoparsec.Text
import Data.Char (isAlphaNum, isAlpha)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.PrettyPrint.Leijen.Text as PP
import Text.PrettyPrint.Leijen.Text.Ext (Pretty(..))
import qualified Text.PrettyPrint.Leijen.Text.Ext as PP

import Data.ErrorMessage
import Data.KeyMap (HasKey)
import qualified Data.KeyMap as KM
import Data.Symbols.MacroName

data PreprocessorMacro =
    PreprocessorConstant
      !MacroName -- ^ Name
      !Text      -- ^ Value
  | PreprocessorFunction
      !MacroName -- ^ Name
      ![Text]    -- ^ Arguments
      !Text      -- ^ Body
  deriving (Eq, Ord, Show)

macroName :: PreprocessorMacro -> MacroName
macroName = \case
  PreprocessorConstant name _   -> name
  PreprocessorFunction name _ _ -> name

instance HasKey PreprocessorMacro where
  type Key PreprocessorMacro = MacroName
  getKey = macroName

instance Pretty PreprocessorMacro where
  pretty = \case
    PreprocessorConstant name value     ->
      PP.hsep ["#define", pretty name, pretty value]
    PreprocessorFunction name args body -> PP.hsep
      [ "#define"
      , pretty name
      , PP.parens $ PP.hsep $ map ((<> ",") . pretty) args
      , pretty body
      ]

isConstant :: PreprocessorMacro -> Bool
isConstant = \case
  PreprocessorConstant{} -> True
  PreprocessorFunction{} -> False

isFunction :: PreprocessorMacro -> Bool
isFunction = \case
  PreprocessorConstant{} -> False
  PreprocessorFunction{} -> True

-- | Parse "#define ..." directive
parsePreprocessorDefine
  :: (HasCallStack, MonadError ErrorMessage m)
  => Text
  -> m PreprocessorMacro
parsePreprocessorDefine =
  either (throwErrorWithCallStack . PP.docFromString) pure . parseOnly (pDefine <* endOfInput)

-- | Parse "#undef ..." directive
parsePreprocessorUndef
  :: (HasCallStack, MonadError ErrorMessage m)
  => Text
  -> m MacroName
parsePreprocessorUndef =
  either (throwErrorWithCallStack . PP.docFromString) pure . parseOnly (pUndef <* endOfInput)

-- | Parse preprocessor directive start - hash, followed by optional whitespace,
-- and literal directive name.
pDirectiveStart :: Text -> Parser ()
pDirectiveStart directive = do
  _ <- char '#'    <?> "hash"
  skipMany pCppWS  <?> "optional whitespace after hash"
  void (string directive) <?> T.unpack directive

pDefine :: Parser PreprocessorMacro
pDefine = do
  pDirectiveStart "define"
  skipMany1 pCppWS       <?> "mandatory whitespace after define"
  name <- pCppIdentifier <?> "defined name"
  args <- option Nothing (Just <$> pArguments <?> "arguments")
  skipMany1 pCppWS       <?> "space before macro body"
  body <- pCppBody       <?> "macro body"
  let name' = mkMacroName name
      body' = case args of
        Nothing    -> PreprocessorConstant name' body
        Just args' -> PreprocessorFunction name' args' body
  pure body'

pUndef :: Parser MacroName
pUndef = do
  pDirectiveStart "undef"
  skipMany1 pCppWS        <?> "mandatory whitespace after define"
  ident <- pCppIdentifier <?> "name"
  pure $ mkMacroName ident

pCppIdentifier :: Parser Text
pCppIdentifier =
  T.cons
    <$> ((satisfy isLeadingCPPIdentifierChar <?> "first cpp identifier char")
        <* skipMany pContinuationLine)
    <*> (T.concat <$> (takeWhile1 isCPPIdentifierChar `sepBy` pContinuationLine <?> "rest of cpp identifier"))
  where
    pContinuationLine = char '\\' *> skipNewline <?> "continuation line"

-- Characters that can occur at first position of Cpp identifier.
isLeadingCPPIdentifierChar :: Char -> Bool
isLeadingCPPIdentifierChar c = case c of
 '_' -> True
 c   -> isAlpha c

isCPPIdentifierChar :: Char -> Bool
isCPPIdentifierChar c = case c of
  '_' -> True
  c   -> isAlphaNum c

pArguments :: Parser [Text]
pArguments = do
  _    <- char '('
  skipMany pCppWS
  args <- (pCppIdentifier <* skipMany pCppWS) `sepBy` (char ',' *> skipMany pCppWS)
  skipMany pCppWS
  _    <- char ')'
  pure args

pCppBody :: Parser Text
pCppBody = stripTrailingNewline . removeComments . removeContinuationMarkers <$> takeText
  where
    removeContinuationMarkers =
      T.replace "\\\r" "" . T.replace "\\\n" "" . T.replace "\\\r\n" ""
    -- TODO: improve stripping to handle non-empty comments.
    removeComments xs =
      case T.splitOn "/*" xs of
        []   -> T.empty -- no occurrences of /*
        [y]  -> y       -- no occurrences of /*
        y:ys -> T.concat $ y : map (T.drop 2 . snd . T.breakOn "*/") ys
      -- T.replace "/**/" ""
    stripTrailingNewline = T.dropWhileEnd isAsciiSpaceOrNewline

-- pCppBody = T.concat <$> sepBy1 takeMany
-- pCppBody = takeText
--   c <- anyChar
--   case c of
--     '\\' -> undefined
--     '\r' -> undefined
--     '\n' -> undefined

pCppWS :: Parser ()
pCppWS = do
  c <- anyChar
  case c of
    '\\'                 -> skipNewline
    c' | isAsciiSpace c' -> pure ()
    _                    -> fail "pCppWS"
  -- char '\\' *> skipNewline <|> skip isAsciiSpace

skipNewline :: Parser ()
skipNewline = do
  c <- anyChar
  case c of
    '\r' -> skip isNewline
    '\n' -> pure ()
    _    -> fail "skipNewline"
  where
    isNewline :: Char -> Bool
    isNewline = \case
      '\n' -> True
      _    -> False

isAsciiSpace :: Char -> Bool
isAsciiSpace = \case
  ' '  -> True
  '\t' -> True
  '\r' -> True
  _    -> False

isAsciiSpaceOrNewline :: Char -> Bool
isAsciiSpaceOrNewline = \case
  ' '  -> True
  '\t' -> True
  '\r' -> True
  '\n' -> True
  '\f' -> True
  '\v' -> True
  _    -> False

