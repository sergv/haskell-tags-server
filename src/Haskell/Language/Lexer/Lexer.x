{
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskell.Language.Lexer.Lexer (tokenize) where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.PrettyPrint.Leijen.Text.Utils (Doc, Pretty(..), (<+>))

import Haskell.Language.Lexer.LexerTypes
import Token

}

$ascspace  = [\ \t\r]
$unispace  = \x01
$space     = [$ascspace $unispace]
$nl        = [\n]
$ws        = [$space\f\v] # $nl

$dot       = [\.]

$asclarge  = [A-Z]
$unilarge  = \x02
$large     = [$asclarge $unilarge]

$ascsmall  = [a-z]
$unismall  = \x03
$small     = [$ascsmall $unismall]

$special_sym  = [\(\)\,\;\[\]\`\{\}]
$ascsymbol    = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:]
$unisymbol    = \x04
$symbol       = [$ascsymbol $unisymbol] # [$special_sym \_\'\"]

$ascident  = [$ascsmall $asclarge]
$uniident  = [$unismall $unilarge]
$ascdigit  = [0-9]
$unidigit  = \x05
$digit     = [$ascdigit $unidigit]
$unisuffix = \x06
$ident_nonsym = [$ascident $uniident $unisuffix $digit] # [$symbol]
$ident_syms   = [\'\_\#]
$ident     = [$ident_nonsym $ident_syms]

-- $reserved_op = [→ ∷ ⇒ ∀]

@qualificationPrefix = ( $large $ident* $dot )*

@arrow       = ( "->" | "→" )
@doublecolon = ( "::" | "∷" )
@implies     = ( "=>" | "⇒" )

$charesc    = [a b f n r t v \\ \" \' \&]
$octdigit   = [0-7]
$hexdigit   = [0-9a-fA-F]
@charescape = [\\] ( $charesc | $asclarge+ | "o" $octdigit+ | "x" $hexdigit+ )

@source_pragma = [Ss][Oo][Uu][Rr][Cc][Ee]

:-

-- Can skip whitespace everywhere since it does not affect meaning in any
-- state.
<0, comment, qq, literate> $ws+ ;

-- Literate stuff
<literate> ^ > $ws*             / { isLiterate }           { \_ _   -> startNonLiterateBird }
<literate> ^ "\begin{code}" $nl / { isLiterate }           { \_ _   -> startNonLiterateLatex }
<literate> (. | $nl)                                       ;


<0> $nl > $space*  / { isLiterate }                        { \_ len -> pure $! Newline $! len - 2 }
<0> $nl [^>]       / { isLiterate .&&&. isInBirdEnv }      { \_ _   -> endNonLiterate }
<0> ^ "\end{code}" / { isLiterate .&&&. isInLatexCodeEnv } { \_ _   -> endNonLiterate }

<0> {

$nl $space*             { \_ len -> pure $! Newline $! len - 1 }

[\-][\-]+ ~[$symbol $nl] .* ;
[\-][\-]+ / $nl         ;

-- Pragmas
"{-#" $ws* @source_pragma $ws* "#-}" { kw $ Pragma SourcePragma }

}


-- Comments
<0, comment> "{-"       { \_ _ -> startComment }
<comment> "-}"          { \_ _ -> endComment }
<comment> (. | $nl)     ;
<0> "-}"                { \_ _ -> errorAtLine "Unmatched -}" }

-- Strings
<0> [\"]                { \_ _ -> startString }
<string> [\"]           { \_ _ -> endString }
<string> [\\] $nl ( $ws+ [\\] )? ;
<string> ( $ws+ | [^\"\\$nl] | [\\] . )+ ;

-- Strings
<0> [\"]                { \_ _ -> startString }
<string> [\\] [\"\\]    ;
<string> [\\] $nl ($ws+ [\\])? ;
<string> [\"]           { \_ _ -> endString }
<string> (. | $nl)      ;

-- Characters
<0> [\'] ( [^\'\\] | @charescape ) [\'] { kw Character }

-- Template Haskell quasiquoters

<0> "[" $ident* "|"     { \_ _ -> startQuasiquoter }
<qq> "$("               { \_ _ -> startSplice CtxQuasiquoter }
<qq> "|]"               { \_ _ -> endQuasiquoter }
<qq> (. | $nl)          ;

<0> "$("                { \_ _ -> startSplice CtxHaskell }

-- Vanilla tokens
<0> {

"case"                  { kw KWCase }
"class"                 { kw KWClass }
"data"                  { kw KWData }
"default"               { kw KWDefault }
"deriving"              { kw KWDeriving }
"do"                    { kw KWDo }
"else"                  { kw KWElse }
"family"                { kw KWFamily }
"forall"                { \_ _ -> pure $ T "forall" }
"∀"                     { kw KWForall }
"foreign"               { kw KWForeign }
"if"                    { kw KWIf }
"import"                { kw KWImport }
"in"                    { kw KWIn }
"infix"                 { kw KWInfix }
"infixl"                { kw KWInfixl }
"infixr"                { kw KWInfixr }
"instance"              { kw KWInstance }
"let"                   { kw KWLet }
"module"                { kw KWModule }
"newtype"               { kw KWNewtype }
"of"                    { kw KWOf }
"pattern"               { \_ _ -> return $ T "pattern" }
"then"                  { kw KWThen }
"type"                  { kw KWType }
"where"                 { kw KWWhere }
@arrow                  { kw Arrow }
"@"                     { kw At }
"`"                     { kw Backtick }
","                     { kw Comma }
"."                     { kw Dot }
@doublecolon            { kw DoubleColon }
"="                     { kw Equals }
"!"                     { kw ExclamationMark }
@implies                { kw Implies }
"{"                     { kw LBrace }
"["                     { kw LBracket }
"("                     { pushLParen }
"|"                     { kw Pipe }
"}"                     { kw RBrace }
"]"                     { kw RBracket }
")"                     { popRParen }
"~"                     { kw Tilde }
-- semicolons are not used
";"                     ;

@qualificationPrefix ( $ident+ | $symbol+ )
                        { \input len -> return $ T $ retrieveToken input len }

}

{

kw :: Applicative m => TokenVal -> AlexAction m
kw tok = \_ _ -> pure tok

tokenize :: FilePath -> LiterateMode -> Text -> Either Doc [Token]
tokenize filename mode input = runAlexM filename mode code input scanTokens
  where
    code = case mode of
      Vanilla  -> startCode
      Literate -> literateStartCode

scanTokens :: AlexM [Token]
scanTokens = do
  tok <- alexMonadScan
  case valOf tok of
    EOF -> return []
    _   -> (tok :) <$> scanTokens

alexMonadScan :: AlexM Token
alexMonadScan = do
  filename <- asks aeFilename
  line     <- gets (aiLine . asInput)
  tokVal   <- alexScanTokenVal
  pure $ Pos (mkSrcPos filename line) tokVal

alexScanTokenVal :: AlexM TokenVal
alexScanTokenVal = do
  env                              <- ask
  state@AlexState{asInput, asCode} <- get
  case alexScanUser (env, state) asInput (unAlexCode asCode) of
    AlexEOF                              ->
      pure EOF
    AlexError AlexInput{aiLine, aiInput} -> do
      AlexState{asCode} <- get
      throwError $ "lexical error while in state" <+> pretty asCode <+> "at line" <+>
        pretty (unLine aiLine) <> ":" <+> pretty (TL.fromStrict (T.take 40 aiInput))
    AlexSkip input _                     -> do
      alexSetInput input
      alexScanTokenVal
    AlexToken input tokLen action        -> do
      alexSetInput input
      action asInput tokLen

startComment :: AlexM TokenVal
startComment = do
  void $ modifyCommentDepth (+1)
  alexSetCode commentCode
  alexScanTokenVal

endComment :: AlexM TokenVal
endComment = do
  newDepth <- modifyCommentDepth (\x -> x - 1)
  when (newDepth == 0) $
    alexSetCode startCode
  alexScanTokenVal

startString :: AlexM TokenVal
startString = do
  alexSetCode stringCode
  alexScanTokenVal

endString :: AlexM TokenVal
endString = do
  alexSetCode startCode
  pure String

startQuasiquoter :: AlexM TokenVal
startQuasiquoter = do
  alexSetCode qqCode
  pure QuasiquoterStart

startSplice :: Context -> AlexM TokenVal
startSplice ctx = do
  alexSetCode startCode
  pushContext ctx
  pure SpliceStart

endQuasiquoter :: AlexM TokenVal
endQuasiquoter = do
  alexSetCode startCode
  pure QuasiquoterEnd

pushLParen :: AlexAction AlexM
pushLParen _ _ = do
  pushContext CtxHaskell
  pure LParen

popRParen :: AlexAction AlexM
popRParen _ _ = (tryRestoringContext *> pure RParen) <|> pure RParen

tryRestoringContext :: AlexM ()
tryRestoringContext = do
  ctx <- popContext
  alexSetCode $ case ctx of
    CtxHaskell     -> startCode
    CtxQuasiquoter -> qqCode

errorAtLine :: (MonadError Doc m, MonadState AlexState m) => Doc -> m a
errorAtLine msg = do
  line <- gets (unLine . aiLine . asInput)
  throwError $ pretty line <> ":" <+> msg

startNonLiterateBird :: AlexM TokenVal
startNonLiterateBird = do
  alexSetCode startCode
  alexEnterBirdLiterateEnv
  alexScanTokenVal

startNonLiterateLatex :: AlexM TokenVal
startNonLiterateLatex = do
  alexSetCode startCode
  alexEnterLatexCodeEnv
  alexScanTokenVal

endNonLiterate :: AlexM TokenVal
endNonLiterate = do
  alexSetCode literateCode
  alexExitLiterateEnv
  alexScanTokenVal

-- Alex codes

startCode :: AlexCode
startCode = AlexCode 0

literateStartCode :: AlexCode
literateStartCode = literateCode

literateCode :: AlexCode
literateCode = AlexCode literate

commentCode :: AlexCode
commentCode = AlexCode comment

qqCode :: AlexCode
qqCode = AlexCode qq

stringCode :: AlexCode
stringCode = AlexCode string

}
