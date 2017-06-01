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

module Haskell.Language.Lexer.Preprocessor
  ( PreprocessorMacro(..)
  , parsePreprocessorDefine
  ) where

import Control.Monad.Except
import Data.Attoparsec.Text
import Data.Char (isAlphaNum)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import qualified Data.Text as T
import Text.PrettyPrint.Leijen.Text.Ext (Doc, docFromString)

data PreprocessorMacro =
    PreprocessorConstant !Text -- ^ Value
  | PreprocessorFunction
      !(NonEmpty Text) -- ^ Arguments
      !Text            -- ^ Body
  deriving (Eq, Ord, Show)

parsePreprocessorDefine
  :: MonadError Doc m
  => Text
  -> m (Text, PreprocessorMacro)
parsePreprocessorDefine =
  either (throwError . docFromString) pure . parseOnly (pDefine <* endOfInput)

pDefine :: Parser (Text, PreprocessorMacro)
pDefine = do
  _ <- char '#'          <?> "hash"
  skipMany pCppWS        <?> "optional whitespace after hash"
  _ <- string "define"   <?> "define"
  skipMany1 pCppWS       <?> "mandatory whitespace after define"
  name <- pCppIdentifier <?> "defined name"
  args <- option Nothing (Just <$> pArguments <?> "arguments")
  skipMany1 pCppWS       <?> "space before macro body"
  body <- pCppBody       <?> "macro body"
  let body' = case args of
        Nothing    -> PreprocessorConstant body
        Just args' -> PreprocessorFunction args' body
  pure (name, body')

pCppIdentifier :: Parser Text
pCppIdentifier =
  T.concat <$> (takeWhile1 isCPPIdentifierChar `sepBy1` (char '\\' *> skipNewline))

isCPPIdentifierChar :: Char -> Bool
isCPPIdentifierChar c = case c of
  '_' -> True
  c   -> isAlphaNum c

pArguments :: Parser (NonEmpty Text)
pArguments = do
  _    <- char '('
  skipMany pCppWS
  args <- (pCppIdentifier <* skipMany pCppWS) `sepBy1` (char ',' *> skipMany pCppWS)
  _    <- char ')'
  case args of
    []   -> fail "Empty list of arguments"
    x:xs -> pure $ x :| xs

pCppBody :: Parser Text
pCppBody = removeContinuationMarkers <$> takeText
  where
    removeContinuationMarkers =
      T.replace "\\\r" "" . T.replace "\\\n" "" . T.replace "\\\r\n" ""

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
