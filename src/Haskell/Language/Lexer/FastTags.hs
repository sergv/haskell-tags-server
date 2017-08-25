----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer.FastTags
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  20 June 2017
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

module Haskell.Language.Lexer.FastTags
  ( module FastTags.Token
  , module FastTags.Tag
  ) where

import FastTags.Tag (TagVal(..), Pos(..), UnstrippedTokens(..), Type(..), breakBlocks, unstrippedTokensOf, processTokens, stripNewlines, tokToName)
import FastTags.Token (Line(..), SrcPos(..), TokenVal(..), increaseLine, PragmaType(..), Token, posFile, posLine, unLine)