----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer.FastTags.TagVal
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE PatternSynonyms #-}

module Haskell.Language.Lexer.FastTags.TagValPatterns
  ( pattern KWCase
  , pattern KWClass
  , pattern KWData
  , pattern KWDefault
  , pattern KWDeriving
  , pattern KWDo
  , pattern KWElse
  , pattern KWFamily
  , pattern KWForeign
  , pattern KWIf
  , pattern KWImport
  , pattern KWIn
  , pattern KWInfix
  , pattern KWInfixl
  , pattern KWInfixr
  , pattern KWInstance
  , pattern KWLet
  , pattern KWModule
  , pattern KWNewtype
  , pattern KWOf
  , pattern KWThen
  , pattern KWType
  , pattern KWWhere
  , pattern Arrow
  , pattern At
  , pattern Backtick
  , pattern Comma
  , pattern Dot
  , pattern DoubleColon
  , pattern Equals
  , pattern ExclamationMark
  , pattern Implies
  , pattern LBrace
  , pattern LBracket
  , pattern LParen
  , pattern Pipe
  , pattern RBrace
  , pattern RBracket
  , pattern RParen
  , pattern Tilde
  , pattern Semicolon
  , pattern T
  , pattern Newline
  , pattern String
  , pattern Character
  , pattern Number
  , pattern QuasiquoterStart
  , pattern QuasiquoterEnd
  , pattern SpliceStart
  , pattern LambdaBackslash
  , pattern EOF
  ) where

import Data.Text (Text)

import qualified FastTags.Token as FT (TokenVal(..))
import Haskell.Language.Lexer.FastTags (ServerToken(..))

pattern KWCase           :: ServerToken
pattern KWCase           = Tok FT.KWCase
pattern KWClass          :: ServerToken
pattern KWClass          = Tok FT.KWClass
pattern KWData           :: ServerToken
pattern KWData           = Tok FT.KWData
pattern KWDefault        :: ServerToken
pattern KWDefault        = Tok FT.KWDefault
pattern KWDeriving       :: ServerToken
pattern KWDeriving       = Tok FT.KWDeriving
pattern KWDo             :: ServerToken
pattern KWDo             = Tok FT.KWDo
pattern KWElse           :: ServerToken
pattern KWElse           = Tok FT.KWElse
pattern KWFamily         :: ServerToken
pattern KWFamily         = Tok FT.KWFamily
pattern KWForeign        :: ServerToken
pattern KWForeign        = Tok FT.KWForeign
pattern KWIf             :: ServerToken
pattern KWIf             = Tok FT.KWIf
pattern KWImport         :: ServerToken
pattern KWImport         = Tok FT.KWImport
pattern KWIn             :: ServerToken
pattern KWIn             = Tok FT.KWIn
pattern KWInfix          :: ServerToken
pattern KWInfix          = Tok FT.KWInfix
pattern KWInfixl         :: ServerToken
pattern KWInfixl         = Tok FT.KWInfixl
pattern KWInfixr         :: ServerToken
pattern KWInfixr         = Tok FT.KWInfixr
pattern KWInstance       :: ServerToken
pattern KWInstance       = Tok FT.KWInstance
pattern KWLet            :: ServerToken
pattern KWLet            = Tok FT.KWLet
pattern KWModule         :: ServerToken
pattern KWModule         = Tok FT.KWModule
pattern KWNewtype        :: ServerToken
pattern KWNewtype        = Tok FT.KWNewtype
pattern KWOf             :: ServerToken
pattern KWOf             = Tok FT.KWOf
pattern KWThen           :: ServerToken
pattern KWThen           = Tok FT.KWThen
pattern KWType           :: ServerToken
pattern KWType           = Tok FT.KWType
pattern KWWhere          :: ServerToken
pattern KWWhere          = Tok FT.KWWhere
pattern Arrow            :: ServerToken
pattern Arrow            = Tok FT.Arrow
pattern At               :: ServerToken
pattern At               = Tok FT.At
pattern Backtick         :: ServerToken
pattern Backtick         = Tok FT.Backtick
pattern Comma            :: ServerToken
pattern Comma            = Tok FT.Comma
pattern Dot              :: ServerToken
pattern Dot              = Tok FT.Dot
pattern DoubleColon      :: ServerToken
pattern DoubleColon      = Tok FT.DoubleColon
pattern Equals           :: ServerToken
pattern Equals           = Tok FT.Equals
pattern ExclamationMark  :: ServerToken
pattern ExclamationMark  = Tok FT.ExclamationMark
pattern Implies          :: ServerToken
pattern Implies          = Tok FT.Implies
pattern LBrace           :: ServerToken
pattern LBrace           = Tok FT.LBrace
pattern LBracket         :: ServerToken
pattern LBracket         = Tok FT.LBracket
pattern LParen           :: ServerToken
pattern LParen           = Tok FT.LParen
pattern Pipe             :: ServerToken
pattern Pipe             = Tok FT.Pipe
pattern RBrace           :: ServerToken
pattern RBrace           = Tok FT.RBrace
pattern RBracket         :: ServerToken
pattern RBracket         = Tok FT.RBracket
pattern RParen           :: ServerToken
pattern RParen           = Tok FT.RParen
pattern Tilde            :: ServerToken
pattern Tilde            = Tok FT.Tilde
pattern Semicolon        :: ServerToken
pattern Semicolon        = Tok FT.Semicolon
pattern T                :: Text -> ServerToken
pattern T x              = Tok (FT.T x)
pattern Newline          :: Int -> ServerToken
pattern Newline x        = Tok (FT.Newline x)
pattern String           :: ServerToken
pattern String           = Tok FT.String
pattern Character        :: ServerToken
pattern Character        = Tok FT.Character
pattern Number           :: ServerToken
pattern Number           = Tok FT.Number
pattern QuasiquoterStart :: ServerToken
pattern QuasiquoterStart = Tok FT.QuasiquoterStart
pattern QuasiquoterEnd   :: ServerToken
pattern QuasiquoterEnd   = Tok FT.QuasiquoterEnd
pattern SpliceStart      :: ServerToken
pattern SpliceStart      = Tok FT.SpliceStart
pattern LambdaBackslash  :: ServerToken
pattern LambdaBackslash  = Tok FT.LambdaBackslash
pattern EOF              :: ServerToken
pattern EOF              = Tok FT.EOF
