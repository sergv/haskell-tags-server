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
import Data.Char (isAlphaNum)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.PrettyPrint.Leijen.Text as PP
import Text.PrettyPrint.Leijen.Text.Ext (Pretty(..))
import qualified Text.PrettyPrint.Leijen.Text.Ext as PP

import Data.ErrorMessage
import Data.KeyMap (HasKey)
import qualified Data.KeyMap as KM

data PreprocessorMacro =
    PreprocessorConstant
      !Text -- ^ Name
      !Text -- ^ Value
  | PreprocessorFunction
      !Text -- ^ Name
      ![Text] -- ^ Arguments
      !Text   -- ^ Body
  deriving (Eq, Ord, Show)

macroName :: PreprocessorMacro -> Text
macroName = \case
  PreprocessorConstant name _   -> name
  PreprocessorFunction name _ _ -> name

instance HasKey PreprocessorMacro where
  type Key PreprocessorMacro = Text
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
  -> m Text
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
  let body' = case args of
        Nothing    -> PreprocessorConstant name body
        Just args' -> PreprocessorFunction name args' body
  pure body'

pUndef :: Parser Text
pUndef = do
  pDirectiveStart "undef"
  skipMany1 pCppWS <?> "mandatory whitespace after define"
  pCppIdentifier   <?> "name"

pCppIdentifier :: Parser Text
pCppIdentifier =
  T.concat <$> (takeWhile1 isCPPIdentifierChar `sepBy1` (char '\\' *> skipNewline))

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
pCppBody = stripTrailingNewline . removeContinuationMarkers <$> takeText
  where
    removeContinuationMarkers =
      T.replace "\\\r" "" . T.replace "\\\n" "" . T.replace "\\\r\n" ""
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

