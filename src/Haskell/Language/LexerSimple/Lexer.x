{
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskell.Language.LexerSimple.Lexer (tokenize) where

import Control.Monad
import Control.Monad.Except.Ext
import Control.Monad.State.Strict
import qualified Data.IntSet as IS
import Data.Text (Text)
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Ext (Pretty(..), Doc, (<+>), (##))
import qualified Data.Text as T
import qualified Data.Text.Unsafe as T
import Data.Void (Void, absurd)

import Data.ErrorMessage
import Haskell.Language.LexerSimple.Types
import Haskell.Language.Lexer.FastTags
import Haskell.Language.Lexer.Types (LiterateStyle(..), Context(..), mkSrcPos, AlexCode(..))

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

-- These symbols can be part of operators but are reserved when occur by
-- themselves.
$symbols_reserved_as_standalone = [ \→ \∷ \⇒ \∀ ]

$special_sym  = [\(\)\,\;\[\]\`\{\}]
$ascsymbol    = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:]
$unisymbol    = \x04
$symbol       = [$ascsymbol $unisymbol $symbols_reserved_as_standalone] # [$special_sym \_\'\"]

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

@nl = ( [\r]? $nl )

@qualificationPrefix = ( $large $ident* $dot )*

@arrow       = ( "->" | "→" )
@doublecolon = ( "::" | "∷" )
@implies     = ( "=>" | "⇒" )

@lbanana     = ( "(|" | ⦇ )
@rbanana     = ( "|)" | ⦈ )

$charesc    = [a b f n r t v \\ \" \' \&]
$octdigit   = [0-7]
$hexdigit   = [0-9a-fA-F]
@charescape = [\\] ( $charesc | $asclarge+ | "o" $octdigit+ | "x" $hexdigit+ )

@float_number =  ( [\+\-]? ( $digit+ ( "." $digit+ )? | $digit* "." $digit+ ) ( [eE] [\+\-]? $digit* )? )

@number = ( [\+\-]? $digit+ | 0 ( [oO] $octdigit+ | [xX] $hexdigit ) | @float_number )

@source_pragma = [Ss][Oo][Uu][Rr][Cc][Ee]

@cpp_ws          = ( $ascspace | [\\] @nl )
@cpp_opt_ws      = @cpp_ws*
@cpp_nonempty_ws = ( $ascspace @cpp_ws* | @cpp_ws* $ascspace )
@define_body     = ( [^ \\ $nl]+ | [\\] ( @nl | . ) )+ @nl

:-

-- Can skip whitespace everywhere since it does not affect meaning in any
-- state.
<0, comment, qq, literate> $ws+ ;

-- Literate Haskell support. 'literate' code handles all text except actuall
-- Haskell program text. It aims to strip all non-Haskell text.
<literate> {
^ ">" $ws*
  -- / { isLiterateEnabled' }
  { \_ len -> (Tok $! Newline $! len - 1)  <$ startLiterateBird }
^ "\begin{code}" $nl
  -- / { isLiterateEnabled' }
  { \_ len -> (Tok $! Newline $! len - 12) <$ startLiterateLatex }
(. | $nl) ;
}

-- Analyse "#if 0" constructs used in e.g. GHC.Base
<0> {
"#" @cpp_opt_ws "if" @cpp_opt_ws "0" (@cpp_nonempty_ws .*)? { \_ _ -> startPreprocessorStripping }

-- Strip preprocessor
"#" @cpp_opt_ws "define" @define_body
  { kw (Newline 0) }

"#" @cpp_opt_ws ( "if" | "ifdef" | "endif" | "elif" | "else" | "undef" | "line" | "error" | "include" ) .* ;
}

<stripCpp> {
"#" @cpp_opt_ws ( "ifdef" | "if" ) .*  { \_ _ -> startPreprocessorStripping }
"#" @cpp_opt_ws "endif" .*             { \_ _ -> endPreprocessorStripping   }
"#" @cpp_opt_ws ( "elif" | "else" | "define" | "undef" | "line" | "error" | "include" ) .* ;
(. | $nl) ;
}

<0> {

$nl ">" $space*
  / { isLiterateEnabled' }
  { \_ len -> pure $! Tok $! Newline $! len - 2 }
$nl [^>]
  / { isLiterateBirdOrOutside }
  { \_ _   -> endLiterate }
^ "\end{code}"
  / { isLiterateLatexOrOutside }
  { \_ _   -> endLiterate }


[\\]? @nl $space* "{-"  { \input len -> startIndentationCounting (countInputSpace input len) }
[\\]? @nl $space*       { \input len -> pure $! Tok $! Newline $! (len - 1) - (case T.unsafeHead (aiInput input) of { '\\' -> 1; _ -> 0 }) }
[\-][\-]+ ~[$symbol $nl] .* ;
[\-][\-]+ / @nl         ;

}

-- Pragmas
<0> "{-#" $ws* @source_pragma $ws* "#-}" { \_ _ -> pure $ Pragma SourcePragma }


-- Comments
<0, comment>
  "{-"                  { \_ _ -> startComment }
<comment> "-}"          { \_ _ -> endComment startCode }
<comment> (. | $nl)     ;
<0> "-}"                { \_ _ -> errorAtLine "Unmatched -}" }

<indentComment>
  "{-"                  { \_ _ -> startIndentComment }
<indentComment> "-}"    { \_ _ -> endComment indentCountCode }
<indentComment> (. | $nl) ;

<indentCount> {
$space* "{-"            { \input len -> addIndentationSize (countInputSpace input len) *> startIndentComment }
$space*                 { \_ len -> endIndentationCounting len }
}

-- Strings
<0> [\"]                { \_ _ -> startString }
<string> ( [\\] @nl $ws* [\\] )? [\"]
                        { \_ _ -> endString startCode }
<string> [\\] @nl ( $ws* [\\] )? ;
<string> ( $ws | [^ \" \\ $nl] )+ ;
<string> ( . | $nl | [\\] . )     ;

-- Characters
<0> [\'] ( [^\'\\] | @charescape ) [\'] { kw Character }

-- Template Haskell quasiquoters

<0> "[" [\$\(]* @qualificationPrefix $ident* [\)]*  "|"
                        { \input len -> startQuasiquoter input len }
<0> "⟦"                 { \_ _ -> startUnconditionalQuasiQuoter }
<qq> "$("               { \_ _ -> startSplice CtxQuasiquoter }
<qq> ("|]" | "⟧")       { \_ _ -> endQuasiquoter startCode }
<qq> (. | $nl)          ;

<0> "$("                { \_ _ -> startSplice CtxHaskell }

-- Vanilla tokens
<0> {

"#{" [^\}]+ "}"         { \_ _ -> pure HSC2HS }

"case"                  { kw KWCase }
"class"                 { kw KWClass }
"data"                  { kw KWData }
"default"               { kw KWDefault }
"deriving"              { kw KWDeriving }
"do"                    { kw KWDo }
"else"                  { kw KWElse }
"family"                { kw KWFamily }
"forall"                { \_ _ -> pure $! Tok $! T "forall" }
"∀"                     { \_ _ -> pure $! Tok $! T "forall" }
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
"pattern"               { \_ _ -> pure $! Tok $! T "pattern" }
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
";"                     { kw Semicolon }

[\\]                    { kw LambdaBackslash }

-- Not interested in numbers, but it takes time to extract their text so
-- it's quicker to just ignore them.
@number                 { kw Number }

[\']* @qualificationPrefix ($ident | $large)+
                        { \input len -> pure $! Tok $! T $! retrieveToken input len }
@qualificationPrefix $symbol+
                        { \input len -> pure $! Tok $! T $! retrieveToken input len }

@lbanana / ~[$symbol]   { \_ _ -> pure LBanana }
@rbanana                { \_ _ -> pure RBanana }

}

{

type AlexAction m = AlexInput -> Int -> m ServerToken
type AlexPred a = a -> AlexInput -> Int -> AlexInput -> Bool

{-# INLINE kw #-}
kw :: Applicative m => TokenVal -> AlexAction m
kw tok = \_ _ -> pure $ Tok tok


isLiterateEnabled'
  :: AlexPred (LiterateLocation a)
isLiterateEnabled' litLoc _inputBefore _len _inputAfter =
  isLiterateEnabled litLoc

isLiterateBirdOrOutside
  :: AlexPred (LiterateLocation LiterateStyle)
isLiterateBirdOrOutside litLoc _inputBefore _len _inputAfter =
  case litLoc of
    LiterateInside Bird  -> True
    LiterateInside Latex -> False
    LiterateOutside      -> True
    Vanilla              -> False

isLiterateLatexOrOutside
  :: AlexPred (LiterateLocation LiterateStyle)
isLiterateLatexOrOutside litLoc _inputBefore _len _inputAfter =
  case litLoc of
    LiterateInside Bird  -> False
    LiterateInside Latex -> True
    LiterateOutside      -> True
    Vanilla              -> False

tokenize
  :: HasCallStack
  => FilePath -> LiterateLocation Void -> Text -> Either ErrorMessage [Pos ServerToken]
tokenize filename litLoc input =
  runAlexM litLoc startCode' input $ scanTokens filename
  where
    startCode' = case litLoc of
      Vanilla          -> startCode
      LiterateOutside  -> literateCode
      LiterateInside x -> absurd x

-- TODO: add unsafe interleave here for producing tokens
scanTokens :: HasCallStack => FilePath -> AlexM [Pos ServerToken]
scanTokens filename = go []
  where
    go acc = do
      nextTok <- continueScanning
      case nextTok of
        Tok EOF -> return $ reverse acc
        _       -> do
          -- Use input after reading token to get proper prefix that includes
          -- token we currently read.
          input <- gets asInput
          let !tok = Pos (mkSrcPos filename $! aiLine input) nextTok
          go (tok : acc)

continueScanning :: HasCallStack => AlexM ServerToken
continueScanning = do
  AlexState{asInput, asCode, asLiterateLoc} <- get
  go asCode asLiterateLoc asInput
  where
    go :: AlexCode -> LiterateLocation LiterateStyle -> AlexInput -> AlexM ServerToken
    go code litLoc = go'
      where
        go' input =
          case alexScanUser litLoc input (unAlexCode code) :: AlexReturn (AlexAction AlexM) of
            AlexEOF                              ->
              pure $ Tok EOF
            AlexError AlexInput{aiLine, aiInput} -> do
              code <- gets asCode
              throwErrorWithCallStack $ "Lexical error while in state" <+> pretty code
                <+> "at line" <+>
                pretty (unLine aiLine) <> ":" ## PP.squotes (pretty (T.take 40 aiInput))
            AlexSkip input' _                    ->
              go' input'
            AlexToken input' tokLen action       ->
              alexSetInput input' *> action input tokLen

startIndentationCounting :: HasCallStack => Int -> AlexM ServerToken
startIndentationCounting !n = do
  modify (\s -> s { asIndentationSize = n, asCommentDepth = 1 })
  alexSetNextCode indentCommentCode
  continueScanning

endIndentationCounting :: Int -> AlexM ServerToken
endIndentationCounting !n = do
  addIndentationSize n
  alexSetNextCode startCode
  Tok . Newline <$> gets asIndentationSize

startIndentComment :: HasCallStack => AlexM ServerToken
startIndentComment = do
  void $ modifyCommentDepth (+1)
  alexSetNextCode indentCommentCode
  continueScanning

startPreprocessorStripping :: HasCallStack => AlexM ServerToken
startPreprocessorStripping = do
  void $ modifyPreprocessorDepth (+1)
  alexSetNextCode stripCppCode
  continueScanning

endPreprocessorStripping :: HasCallStack => AlexM ServerToken
endPreprocessorStripping = do
  newDepth <- modifyPreprocessorDepth (\x -> x - 1)
  when (newDepth == 0) $
    alexSetNextCode startCode
  continueScanning

startComment :: HasCallStack => AlexM ServerToken
startComment = do
  void $ modifyCommentDepth (+1)
  alexSetNextCode commentCode
  continueScanning

endComment :: HasCallStack => AlexCode -> AlexM ServerToken
endComment nextCode = do
  newDepth <- modifyCommentDepth (\x -> x - 1)
  when (newDepth == 0) $
    alexSetNextCode nextCode
  continueScanning

startString :: HasCallStack => AlexM ServerToken
startString =
  alexSetNextCode stringCode *> continueScanning

endString :: AlexCode -> AlexM ServerToken
endString nextCode =
  Tok String <$ alexSetNextCode nextCode

startQuasiquoter :: AlexInput -> Int -> AlexM ServerToken
startQuasiquoter _ n
  | n == 2 = startUnconditionalQuasiQuoter
startQuasiquoter AlexInput{aiInput, aiAbsPos} _ = do
  ends   <- gets asPositionsOfQuasiQuoteEnds
  qqEnds <- case ends of
    Nothing    -> do
      let ends' = calculateQuasiQuoteEnds aiAbsPos aiInput
      modify $ \s -> s { asPositionsOfQuasiQuoteEnds = Just ends' }
      pure ends'
    Just ends' -> pure ends'
  case IS.lookupGT aiAbsPos qqEnds of
    -- No chance of quasi-quote closing till the end of current file.
    -- Assume that file ought to be well-formed and treat currently
    -- matched input
    Nothing -> pure $ Tok LBracket
    Just _  -> startUnconditionalQuasiQuoter

startUnconditionalQuasiQuoter :: AlexM ServerToken
startUnconditionalQuasiQuoter =
  Tok QuasiquoterStart <$ alexSetNextCode qqCode

startSplice :: Context -> AlexM ServerToken
startSplice ctx = do
  alexSetNextCode startCode
  pushContext ctx
  pure $ Tok SpliceStart

endQuasiquoter :: AlexCode -> AlexM ServerToken
endQuasiquoter nextCode =
  Tok QuasiquoterEnd <$ alexSetNextCode nextCode

pushLParen :: AlexAction AlexM
pushLParen _ _ =
  Tok LParen <$ pushContext CtxHaskell

popRParen :: AlexAction AlexM
popRParen _ _ = do
  cs <- gets asContextStack
  case cs of
    [] -> pure ()
    c : cs' -> do
      modify $ \s -> s { asContextStack = cs' }
      alexSetNextCode $ case c of
        CtxHaskell     -> startCode
        CtxQuasiquoter -> qqCode
  pure $ Tok RParen

{-# INLINE errorAtLine #-}
errorAtLine
  :: (MonadError ErrorMessage m, MonadState AlexState m)
  => Doc Void -> m a
errorAtLine msg = do
  line <- gets (unLine . aiLine . asInput)
  throwErrorWithCallStack $ "Error at line" <+> pretty line <> ":" <+> msg

startLiterateBird :: AlexM ()
startLiterateBird = do
  alexSetNextCode startCode
  alexEnterBirdLiterateEnv

startLiterateLatex :: AlexM ()
startLiterateLatex = do
  alexSetNextCode startCode
  alexEnterLiterateLatexEnv

endLiterate :: AlexM ServerToken
endLiterate = do
  alexSetNextCode literateCode
  alexExitLiterateEnv
  continueScanning

-- Known codes

{-# INLINE startCode         #-}
{-# INLINE qqCode            #-}
{-# INLINE stringCode        #-}
{-# INLINE commentCode       #-}
{-# INLINE indentCommentCode #-}
{-# INLINE indentCountCode   #-}
{-# INLINE literateCode      #-}
{-# INLINE stripCppCode      #-}
startCode, qqCode, stringCode, commentCode, indentCommentCode, indentCountCode, literateCode, stripCppCode :: AlexCode
startCode         = AlexCode 0
qqCode            = AlexCode qq
stringCode        = AlexCode string
commentCode       = AlexCode comment
indentCommentCode = AlexCode indentComment
indentCountCode   = AlexCode indentCount
literateCode      = AlexCode literate
stripCppCode      = AlexCode stripCpp

}
