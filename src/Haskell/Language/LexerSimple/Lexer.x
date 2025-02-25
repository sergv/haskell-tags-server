{
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskell.Language.LexerSimple.Lexer (tokenize) where

import Control.Monad
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict

import qualified Data.ByteString as BS
import Data.Void (Void, absurd)
import Data.Word
import Foreign.Ptr (plusPtr)
import GHC.Stack.Ext (WithCallStack)
import qualified Prettyprinter as PP
import Prettyprinter.Ext (Pretty(..), Doc, (<+>), (##))
import qualified Prettyprinter.Ext as PP

import Data.IgnoreEqOrdHashNFData
import Haskell.Language.Lexer.FastTags
import Haskell.Language.Lexer.Types (LiterateStyle(..), AlexCode(..), mkSrcPos)
import Haskell.Language.LexerSimple.LensBlaze
import Haskell.Language.LexerSimple.Types

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

-- Stands for "→", "∷", "⇒", "⦇", "⦈", "∀", "⟦", "⟧"
$reserved_symbol = \x07

-- Random characters for which there's no category.
$other = \x08

-- Some rules assume that there's an optional \r before \n so don't change this definition.
@nl = ( [\r]? $nl )

@qualificationPrefix = ( $large $ident* $dot )*

@arrow       =  "->"
@doublecolon =  "::"
@implies     =  "=>"

@lbanana     =  "(|"
@rbanana     =  "|)"

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
@cpp_dir_start   = "#"+ @cpp_opt_ws
@define_name     = [$ascident $ascdigit _ ']+
@define_body     = ( [^ \\ $nl]+ | [\\] ( @nl | . ) )+ @nl

-- Except "define"
@cppdirective = ( "if" | "ifdef" | "ifndef" | "endif" | "elif" | "else" | "undef" | "line" | "error" | "warning" | "include" )

-- Except cpp directives, "let", and "enum".
@hscdirective = ( "def" | "const" | "const_str" | "type" | "peek" | "poke" | "ptr" | "offset" | "size" | "alignment" )

-- Except "enum"
@all_cpp_and_hsc_directives = ( "define" | "let" | @cppdirective | @hscdirective )

:-

-- Literate Haskell support. 'literate' code handles all text except actual
-- Haskell program text. It aims to strip all non-Haskell text.
<literate> {
@nl ">" $ws*
  { \input len -> (Newline $! countInputSpace input len) <$ startLiterateBird }
@nl "\begin{code}" @nl $space*
  { \input len -> (Newline $! countInputSpace input len) <$ startLiterateLatex }
@nl ;
.
  { \_ _ -> dropUntilNL' }
}

-- Drop shebang
<0, literate> $nl "#!" .* ;

-- Can skip whitespace everywhere since it does not affect meaning in any
-- state.
<0, comment, qq, literate> $ws+ ;

<0> {

-- Analyse "#if 0" constructs used in e.g. GHC.Base
^ @cpp_dir_start "if" @cpp_opt_ws "0" ( @cpp_nonempty_ws .* )?
  { \_ _ -> startPreprocessorStripping }

-- Named defines, implicitly drops: '( @cpp_ws+ | "(" ) @define_body'
^ @cpp_dir_start ("define" | "let") @cpp_ws+ @define_name
  { \input len -> do
    modify $ \s -> s { asInput = dropUntilUnescapedNL $ asInput s }
    pure $! CppDefine $! extractDefineOrLetName input len
  }

^ @cpp_dir_start @cppdirective .* ( [\\] @nl .* )* ;

^ @cpp_dir_start ("{" (@cpp_ws | @nl)*)? "enum"
  { \_ _ -> pure HSCEnum }
^ @cpp_dir_start @all_cpp_and_hsc_directives
  { \_ _ -> pure HSCDirective }
^ @cpp_dir_start "{" (@cpp_ws | @nl)* @all_cpp_and_hsc_directives
  { \_ _ -> pure HSCDirectiveBraced }

-- Drop everything else that starts with #, e.g.
-- '# 17 "/usr/include/stdc-predef.h" 3 4'
-- #{get_area "Queue.T"}

^ "#" $ascspace* "{"? @define_name+
  { \_ _ -> dropUntilNL' }

}

<stripCpp> {
@cpp_dir_start @cpp_opt_ws ( "ifdef" | "if" ) .*
  { \_ _ -> startPreprocessorStripping }
@cpp_dir_start @cpp_opt_ws ("else" | "elif" | "endif") .*
  { \_ _ -> endPreprocessorStripping }
@cpp_dir_start @cpp_opt_ws ( "define" | "undef" | "line" | "error" | "warning" | "include"  | "let" )
  { \_ _ -> dropUntilNL' }
$nl ;
.
  { \_ _ -> dropUntilNLOr' 35 -- '#'
  }
}

-- Newlines and comments.
<0> {

@nl ">" $space*
  / { isLiterateEnabled' }
  { \input len -> pure $! Newline $! countInputSpace input len }
@nl
  / { shouldEndLiterateBird }
  { \_ _   -> Newline 0 <$ endLiterate }
@nl "\end{code}"
  / { shouldEndLiterateLatex }
  { \_ _   -> endLiterate' }

[\\]? @nl $space* "{-"  { \input len -> startIndentationCounting (countInputSpace input len) }
[\\]? @nl $space*       { \input len -> pure $! Newline $! len - countBackslashCR input - 1 }
[\-][\-]+ ~[$symbol $nl] { \_ _ -> dropUntilNL' }
[\-][\-]+ / @nl         ;

}

-- Pragmas
<0> "{-#" $ws* @source_pragma $ws* "#-}" { \_ _ -> pure $ Pragma SourcePragma }


-- Nested comments
<0, comment>
  "{-"                  { \_ _ -> startComment }
<comment> "-}"          { \_ _ -> endComment startCode }
-- 45  - '-'
-- 123 - '{'
<comment> @nl ;
<comment> ($other | .)  { \_ _ -> dropUntilNLOrEither' 45 123 }
<0> "-}"                { \_ _ -> errorAtLine "Unmatched -}" }

<indentComment>
  "{-"                  { \_ _ -> startIndentComment }
<indentComment> "-}"    { \_ _ -> endComment indentCountCode }
<indentComment> (. | @nl) ;

<indentCount> {
$space* "{-"            { \input len -> addIndentationSize (fromIntegral (countInputSpace input len)) *> startIndentComment }
$space*                 { \_ len -> endIndentationCounting len }
}

<0> {
-- Strings
[\"] ( [^\" \\ \r \n] | [\\] . | [\\] @nl ( $ws* [\\] )? )* [\"]
                        { \_ _ -> pure String }

[\"]                    { \_ _ -> pure DQuote }

-- Characters.
[\'] ( [^ \' \\ \n \r] | @charescape ) [\']
                        { kw Character }
}

-- Template Haskell quasiquoters

<0> {
"[|"                    { \_ _ -> startUnconditionalQuasiQuoter }
"[" [\$\(]* @qualificationPrefix $ident+ [\)]*  "|"
                        { \input _ -> startQuasiquoter input }
"$("                    { \_ _ -> startSplice CtxHaskell }
^ "$" [\']* @qualificationPrefix $ident+
                        { \_ _ -> pure ToplevelSplice }

}

<qq> {
"$("                    { \_ _ -> startSplice CtxQuasiquoter }
"|]"                    { \_ _ -> endQuasiquoter }
$reserved_symbol        { \input _len -> reservedSymbolQQ (unsafeTextHead (aiPtr input)) }
(. | @nl)               ;
}

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
"forall"                { \_ _ -> pure forallServerToken }
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
"pattern"               { \_ _ -> pure patternServerToken }
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
                        { \input len -> pure $! T $! takeText input len }
@qualificationPrefix $symbol+
                        { \input len -> pure $! T $! takeText input len }

$reserved_symbol        { \input _len -> reservedSymbol (unsafeTextHead (aiPtr input)) }

@lbanana / ~[$symbol]   { \_ _ -> pure LBanana }
@rbanana                { \_ _ -> pure RBanana }

}


{

type AlexAction m = AlexInput -> Int -> m ServerToken
type AlexPred a = a -> AlexInput -> Int -> AlexInput -> Bool

{-# INLINE kw #-}
kw :: Applicative m => ServerToken -> AlexAction m
kw tok = \_ _ -> pure tok


isLiterateEnabled'
  :: AlexPred (LiterateLocation a)
isLiterateEnabled' litLoc _inputBefore _len _inputAfter =
  isLiterateEnabled litLoc

shouldEndLiterateBird
  :: AlexPred (LiterateLocation LiterateStyle)
shouldEndLiterateBird litLoc inputBefore _len _inputAfter =
  case unsafeTextHeadAscii $ (`plusPtr` 1) $ aiPtr inputBefore of
    -- 62 = '>'
    62 -> False
    _  -> isLiterateBirdOrOutside litLoc

shouldEndLiterateLatex
  :: AlexPred (LiterateLocation LiterateStyle)
shouldEndLiterateLatex litLoc _inputBefore _len _inputAfter =
  isLiterateLatexOrOutside litLoc

tokenize
  :: WithCallStack
  => LiterateLocation Void -> BS.ByteString -> [Pos ServerToken]
tokenize litLoc input =
  runAlexM litLoc startCode' input scanTokens
  where
    startCode' = case litLoc of
      Vanilla          -> startCode
      LiterateOutside  -> literateCode
      LiterateInside x -> absurd x

scanTokens :: WithCallStack => AlexM ()
scanTokens = go
  where
    go = do
      nextTok <- continueScanning
      case nextTok of
        EOF -> pure ()
        _   -> do
          -- Use input after reading token to get proper prefix that includes
          -- token we currently read.
          AlexState{asInput} <- get
          let !tok = Pos (mkSrcPos (aiLine asInput)) nextTok
          tell [tok]
          go

continueScanning :: WithCallStack => AlexM ServerToken
continueScanning = do
  s@AlexState{asInput} <- get
  go (view asCodeL s) (view asLiterateLocL s) asInput
  where
    go :: AlexCode -> LiterateLocation LiterateStyle -> AlexInput -> AlexM ServerToken
    go code !litLoc = go'
      where
        go' input =
          case alexScanUser litLoc input (unAlexCode code) :: AlexReturn (AlexAction AlexM) of
            AlexEOF                              ->
              pure EOF
            AlexError input@AlexInput{aiPtr} -> do
              code <- gets (view asCodeL)
              pure $ Error $ IgnoreEqOrdHashNFData $ "Lexical error while in state" <+> pretty code
                <+> "at line" <+>
                pretty (unLine (view aiLineL input)) <> ":" ## PP.squotes (PP.ppByteString (utf8BS 40 aiPtr))
            AlexSkip input' _                    ->
              go' input'
            AlexToken input' tokLen action       ->
              alexSetInput input' *> action input tokLen

dropUntilNL' :: AlexM ServerToken
dropUntilNL' = do
  modify $ \s -> s { asInput = dropUntilNL $ asInput s }
  continueScanning

dropUntilNLOr' :: Word8 -> AlexM ServerToken
dropUntilNLOr' w = do
  modify $ \s -> s { asInput = dropUntilNLOr w $ asInput s }
  continueScanning

dropUntilNLOrEither' :: Word8 -> Word8 -> AlexM ServerToken
dropUntilNLOrEither' w1 w2 = do
    modify $ \s -> s { asInput = dropUntilNLOrEither w1 w2 $ asInput s }
    continueScanning

dropUntil' :: Word8 -> AlexM ServerToken
dropUntil' w = do
  modify $ \s -> s { asInput = dropUntil w $ asInput s }
  continueScanning

dropUntil2' :: Word8 -> Word8 -> AlexM ServerToken
dropUntil2' w1 w2 = do
  modify $ \s -> s { asInput = dropUntil2 w1 w2 $ asInput s }
  continueScanning

startIndentationCounting :: WithCallStack => Int -> AlexM ServerToken
startIndentationCounting !n = do
  modify (\s -> set asCommentDepthL 1 $ set asIndentationSizeL (fromIntegral n) s)
  alexSetNextCode indentCommentCode
  continueScanning

endIndentationCounting :: Int -> AlexM ServerToken
endIndentationCounting !n = do
  alexSetNextCode startCode
  Newline . (+ n) . fromIntegral <$> gets (view asIndentationSizeL)

startIndentComment :: WithCallStack => AlexM ServerToken
startIndentComment = do
  void $ modifyCommentDepth (+1)
  alexSetNextCode indentCommentCode
  continueScanning

startPreprocessorStripping :: WithCallStack => AlexM ServerToken
startPreprocessorStripping = do
  void $ modifyPreprocessorDepth (+1)
  alexSetNextCode stripCppCode
  continueScanning

endPreprocessorStripping :: WithCallStack => AlexM ServerToken
endPreprocessorStripping = do
  newDepth <- modifyPreprocessorDepth (\x -> x - 1)
  when (newDepth == 0) $
    alexSetNextCode startCode
  continueScanning

startComment :: WithCallStack => AlexM ServerToken
startComment = do
  void $ modifyCommentDepth (+1)
  alexSetNextCode commentCode
  continueScanning

endComment :: WithCallStack => AlexCode -> AlexM ServerToken
endComment nextCode = do
  newDepth <- modifyCommentDepth (\x -> x - 1)
  when (newDepth == 0) $
    alexSetNextCode nextCode
  continueScanning

endString :: AlexCode -> AlexM ServerToken
endString nextCode =
  String <$ alexSetNextCode nextCode

startQuasiquoter :: AlexInput -> AlexM ServerToken
startQuasiquoter AlexInput{aiPtr} = do
  !haveEnd     <- gets (view asHaveQQEndL)
  isEndPresent <- case haveEnd of
    Nothing    -> do
      let haveEnd' = checkQuasiQuoteEndPresent aiPtr
      modify $ set asHaveQQEndL (Just haveEnd')
      pure haveEnd'
    Just ends' -> pure ends'
  case isEndPresent of
    -- No chance of quasi-quote closing till the end of current file.
    -- Assume that file ought to be well-formed and treat currently
    -- matched input (and throw away the pipe character).
    False -> pure LBracket
    True  -> startUnconditionalQuasiQuoter

startUnconditionalQuasiQuoter :: AlexM ServerToken
startUnconditionalQuasiQuoter =
  QuasiquoterStart <$ alexSetNextCode qqCode

startSplice :: Context -> AlexM ServerToken
startSplice ctx = do
  alexSetNextCode startCode
  pushContext ctx
  pure SpliceStart

endQuasiquoter :: AlexM ServerToken
endQuasiquoter =
  QuasiquoterEnd <$ alexSetNextCode startCode

pushLParen :: AlexAction AlexM
pushLParen _ _ =
  LParen <$ pushContext CtxHaskell

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
  pure RParen

{-# INLINE errorAtLine #-}
errorAtLine
  :: MonadState AlexState m
  => Doc Void -> m ServerToken
errorAtLine msg = do
  line <- gets (unLine . view aiLineL . asInput)
  pure $ Error $ IgnoreEqOrdHashNFData $ "Error at line" <+> pretty line <> ":" <+> msg

startLiterateBird :: AlexM ()
startLiterateBird = do
  alexSetNextCode startCode
  alexEnterBirdLiterateEnv

startLiterateLatex :: AlexM ()
startLiterateLatex = do
  alexSetNextCode startCode
  alexEnterLiterateLatexEnv

endLiterate :: AlexM ()
endLiterate = do
  alexSetNextCode literateCode
  alexExitLiterateEnv

endLiterate' :: AlexM ServerToken
endLiterate' = endLiterate *> continueScanning

reservedSymbol :: WithCallStack => Char -> AlexM ServerToken
reservedSymbol = \case
  '→' -> pure Arrow
  '∷' -> pure DoubleColon
  '⇒' -> pure Implies
  '∀' -> pure $! T $! "forall"
  '⦇' -> pure LBanana
  '⦈' -> pure RBanana
  '⟦' -> startUnconditionalQuasiQuoter
  '⟧' -> endQuasiquoter
  c   -> error $ PP.renderString $ "Unexpected reserved symbol:" <+> pretty c

reservedSymbolQQ :: WithCallStack => Char -> AlexM ServerToken
reservedSymbolQQ = \case
  '⟧' -> endQuasiquoter
  _   -> continueScanning

-- Known codes

{-# INLINE startCode         #-}
{-# INLINE qqCode            #-}
{-# INLINE commentCode       #-}
{-# INLINE indentCommentCode #-}
{-# INLINE indentCountCode   #-}
{-# INLINE literateCode      #-}
{-# INLINE stripCppCode      #-}
startCode, qqCode, commentCode, indentCommentCode, indentCountCode, literateCode, stripCppCode :: AlexCode
startCode         = AlexCode 0
qqCode            = AlexCode qq
commentCode       = AlexCode comment
indentCommentCode = AlexCode indentComment
indentCountCode   = AlexCode indentCount
literateCode      = AlexCode literate
stripCppCode      = AlexCode stripCpp

}
