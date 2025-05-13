{
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples     #-}

-- Very important to have this one as it enables GHC to infer proper type of
-- Alex 3.2.1 actions.
--
-- The basic type is (Monad m => AlexInput -> Int -> AlexT m ServerToken), but
-- monomorphism restriction breaks its inference.
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}

module Haskell.Language.LexerSimple.Lexer (tokenize) where

import Control.Applicative as A
import Control.Monad
import Control.Monad.State
import Data.ByteString qualified as BS
import Data.Char
import Data.ErrorMessage
import Data.IgnoreEqOrdHashNFData
import Data.Void (Void)
import Data.Word
import Foreign.Ptr (plusPtr)
import GHC.Stack.Ext
import Prettyprinter hiding (line)

import Haskell.Language.Lexer.CppTypes qualified as Cpp
import Haskell.Language.Lexer.Types
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

@filename        = [^$nl]+
@include_name    = ("<" @filename ">" | [\"] @filename [\"])

-- -- Except "define"
-- @cppdirective = ( "if" | "ifdef" | "ifndef" | "endif" | "elif" | "else" | "undef" | "line" | "error" | "warning" | "include" )

-- Except cpp directives, "let", and "enum".
@hscdirective = ( "def" | "const" | "const_str" | "type" | "peek" | "poke" | "ptr" | "offset" | "size" | "alignment" )

-- Except "enum"
@all_hsc_directives = ( "define" | "let" | @hscdirective )

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

^ @cpp_dir_start "include" @cpp_ws+ @include_name
  { \input len -> do
    _ <- dropUntilCppDirectiveEnd
    pure $! Cpp $! Cpp.Include $! extractIncludeName input len }

-- Named defines, implicitly drops: '( @cpp_ws+ | "(" ) @define_body
^ @cpp_dir_start ("define" | "let") @cpp_ws+ @define_name
  { \input len -> do
    _ <- dropUntilCppDirectiveEnd
    pure $! Cpp $! Cpp.Define $! extractDefineOrLetName input len
  }

^ @cpp_dir_start "undef" @cpp_ws+ @define_name
  { \input len -> do
    _ <- dropUntilCppDirectiveEnd
    pure $! Cpp $! Cpp.Undef $! extractDefineOrLetName input len }

^ @cpp_dir_start "ifdef" @cpp_ws+ @define_name
  { \input len -> do
    _ <- dropUntilCppDirectiveEnd
    pure $! Cpp $! Cpp.Ifdef $! extractDefineOrLetName input len }

^ @cpp_dir_start "ifndef" @cpp_ws+ @define_name
  { \input len -> do
    _ <- dropUntilCppDirectiveEnd
    pure $! Cpp $! Cpp.Ifndef $! extractDefineOrLetName input len }


-- Implicitly drops: @define_body
^ @cpp_dir_start "if"
  { \_ _ -> Cpp . Cpp.If   <$> dropUntilCppDirectiveEnd }
^ @cpp_dir_start "elif"
  { \_ _ -> Cpp . Cpp.Elif <$> dropUntilCppDirectiveEnd }
^ @cpp_dir_start "else"
  { \_ _ -> Cpp Cpp.Else   <$  dropUntilCppDirectiveEnd }
^ @cpp_dir_start "endif"
  { \_ _ -> Cpp Cpp.Endif  <$  dropUntilCppDirectiveEnd }

^ @cpp_dir_start ("line" | "error" | "warning") .* ( [\\] @nl .* )* ;

-- ^ @cpp_dir_start @cppdirective .* ( [\\] @nl .* )* ;

^ @cpp_dir_start ("{" (@cpp_ws | @nl)*)? "enum"
  { \_ _ -> pure HSCEnum }
^ @cpp_dir_start @all_hsc_directives
  { \_ _ -> pure HSCDirective }
@cpp_dir_start "{" (@cpp_ws | @nl)* @all_hsc_directives
  { \_ _ -> pure HSCDirectiveBraced }

-- Drop everything else that starts with #, e.g.
-- '# 17 "/usr/include/stdc-predef.h" 3 4'
-- #{get_area "Queue.T"}

^ "#" $ascspace* "{"? @define_name+
  { \_ _ -> dropUntilNL' }

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
<0, comment> "{-"       { \_ _ -> startComment }
<comment>    "-}"       { \_ _ -> endComment startCode }
-- 45  - '-'
-- 123 - '{'
<comment> @nl ;
<comment> ($other | .)  { \_ _ -> dropUntilNLOrEither' 45 123 }
<0> "-}"                { \_ _ -> errorAtLine "Unmatched -}" }

<indentComment> {
"{-"    { \_ _ -> startIndentComment }
"-}"    { \_ _ -> endComment indentCountCode }
(. | @nl) ;
}

<indentCount> {
$space* "{-"            { \input len -> addIndentationSize (fromIntegral (countInputSpace input len)) *> startIndentComment }
$space*                 { \_ len -> endIndentationCounting len }
}

<0> {
-- Strings
[\"] ( [^\" \\ \r \n] | [\\] . | [\\] @nl ( $ws* [\\] )? )* [\"]
                        { \_ _ -> pure String }

[\"]                    { \_ _ -> pure DQuote }

-- Character.
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
-- ([^\|] | @nl)+          ;
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

[']* @qualificationPrefix ($ident | $large)+
                        { \input len -> pure $! T $! takeText input len }
@qualificationPrefix $symbol+
                        { \input len -> pure $! T $! takeText input len }

$reserved_symbol        { \input _len -> reservedSymbol (unsafeTextHead (aiPtr input)) }

@lbanana / ~[$symbol]   { \_ _ -> pure LBanana }
@rbanana                { \_ _ -> pure RBanana }

}

{

type AlexAction = AlexInput -> Int -> AlexM ServerToken
type AlexPred a = a -> AlexInput -> Int -> AlexInput -> Bool

{-# INLINE kw #-}
kw :: ServerToken -> AlexAction
kw tok = \_ _ -> pure tok

isLiterateEnabled'
  :: AlexPred (LitMode a)
isLiterateEnabled' litLoc _inputBefore _len _inputAfter =
  isLiterateEnabled litLoc

shouldEndLiterateBird
  :: AlexPred (LitMode LitStyle)
shouldEndLiterateBird litLoc inputBefore _len _inputAfter =
  case unsafeTextHeadAscii $ (`plusPtr` 1) $ aiPtr inputBefore of
    -- 62 = '>'
    62 -> False
    _  -> isLiterateBirdInside litLoc

shouldEndLiterateLatex
  :: AlexPred (LitMode LitStyle)
shouldEndLiterateLatex litLoc _inputBefore _len _inputAfter =
  isLiterateLatexInside litLoc

tokenize
  :: WithCallStack => LitMode Void -> BS.ByteString -> Either ErrorMessage [Pos ServerToken]
tokenize litLoc input =
  case runAlexM litLoc code input scanTokens of
    (Nothing, xs) -> Right xs
    (Just err, _) -> Left err
  where
    code = case litLoc of
      LitVanilla -> startCode
      LitOutside -> literateCode

scanTokens :: WithCallStack => AlexM (Maybe ErrorMessage, [(AlexInput, ServerToken)])
scanTokens = go []
  where
    go acc = do
      !nextTok <- continueScanning
      case nextTok of
        EOF       -> pure (Nothing, reverse acc)
        Error err -> pure (Just $ ErrorMessage (unIgnoreEqOrdHashNFData err) callStack, reverse acc)
        _         -> do
          -- Use input after reading token to get proper prefix that includes
          -- token we currently read.
          AlexState{asInput} <- get
          go ((asInput, nextTok) : acc)

-- {-# INLINE continueScanning #-}
continueScanning :: AlexM ServerToken
continueScanning = do
  !s@AlexState{asInput} <- get
  go (view asCodeL s) (view asLiterateLocL s) asInput
  where
    go :: AlexCode -> LitMode LitStyle -> AlexInput -> AlexM ServerToken
    go !code !litLoc = go'
      where
        go' :: AlexInput -> AlexM ServerToken
        go' !input =
          case alexScanUser' litLoc input (unAlexCode code) :: AlexReturn AlexAction of
            AlexEOF                        ->
              pure EOF
            AlexError input'               -> do
              code' <- gets (view asCodeL)
              pure $ Error $ IgnoreEqOrdHashNFData $ "Lexical error while in state" <+> pretty (show code') <+>
                "at line" <+> pretty (view aiLineL input') <> ":" <+> squotes (pretty (takeText input' 40))
            AlexSkip input' _              -> go' input'
            AlexToken input' tokLen action -> alexSetInput input' *> action input tokLen
              -- runState (alexSetInput input' *> action input tokLen) s

alexScanUser' :: LitMode LitStyle -> AlexInput -> Int -> AlexReturn (AlexInput -> Int -> AlexM ServerToken)
alexScanUser' user__ !input__ !(I# sc) =
  case alex_scan_tkn' user__ input__ 0# input__ sc AlexNone of
    (AlexNone, !input__') ->
      case alexGetByte input__ of
        Nothing -> AlexEOF
        Just _  -> AlexError input__'

    (AlexLastSkip input__'' len, _) ->
      AlexSkip input__'' len

    (AlexLastAcc k input__''' len, _) ->
      AlexToken input__''' len ({-# SCC "alexScanUser/quickIndex" #-} alex_actions `quickIndex` k)

-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn' :: LitMode LitStyle -> AlexInput -> Int# -> AlexInput -> Int# -> AlexLastAcc -> (AlexLastAcc, AlexInput)
alex_scan_tkn' !user__ !orig_input = go
  where
    go len !input__ s !last_acc =
      let !new_acc = check_accs (alex_accept `quickIndex` (I# s)) in
      case alexGetByte input__ of
         Nothing             -> (new_acc, input__)
         Just (c, new_input) ->
           case fromIntegral c of
             I# ord_c ->
               let base   = alexIndexInt32OffAddr alex_base s
                   offset = base +# ord_c
                   new_s  = if isTrue# (offset >=# 0#) && isTrue# (alexIndexInt16OffAddr alex_check offset ==# ord_c)
                            then alexIndexInt16OffAddr alex_table offset
                            else alexIndexInt16OffAddr alex_deflt s
               in
                 case new_s of
                   -1# -> (new_acc, input__)
                       -- on an error, we want to keep the input *before* the
                       -- character that failed, not after.
                   _   ->
                     alex_scan_tkn'
                       user__
                       orig_input
                       (if c < 0x80 || c >= 0xC0 then len +# 1# else len)
                       -- note that the length is increased ONLY if this is the 1st byte in a char encoding)
                       new_input
                       new_s
                       new_acc
      where
        check_accs (AlexAccNone) = last_acc
        check_accs (AlexAcc a  ) = AlexLastAcc a input__ (I# len)
        check_accs (AlexAccSkip) = AlexLastSkip  input__ (I# len)

-- #ifndef ALEX_NOPRED
        check_accs (AlexAccPred a predx rest)
           | predx user__ orig_input (I# len) input__
           = AlexLastAcc a input__ (I# len)
           | otherwise
           = check_accs rest
        check_accs (AlexAccSkipPred predx rest)
           | predx user__ orig_input (I# len) input__
           = AlexLastSkip input__ (I# len)
           | otherwise
           = check_accs rest
-- #endif

dropUntilNL_ :: AlexM ()
dropUntilNL_ =
  modify $ \s -> s { asInput = dropUntilNL $ asInput s }

dropUntilNL' :: AlexM ServerToken
dropUntilNL' = dropUntilNL_ *> continueScanning

dropUntilNLOr' :: Word8 -> AlexM ServerToken
dropUntilNLOr' w = do
  modify $ \s -> s { asInput = dropUntilNLOr w $ asInput s }
  continueScanning

dropUntilNLOrEither' :: Word8 -> Word8 -> AlexM ServerToken
dropUntilNLOrEither' w1 w2 = do
  modify $ \s -> s { asInput = dropUntilNLOrEither w1 w2 $ asInput s }
  continueScanning

startIndentationCounting :: Int -> AlexM ServerToken
startIndentationCounting !n = do
  modify (\s -> set asCommentDepthL 1 $ set asIndentationSizeL (fromIntegral n) s)
  alexSetNextCode indentCommentCode
  continueScanning

endIndentationCounting :: Int -> AlexM ServerToken
endIndentationCounting !n = do
  alexSetNextCode startCode
  Newline . (+ n) . fromIntegral <$> gets (view asIndentationSizeL)

startIndentComment :: AlexM ServerToken
startIndentComment = do
  void $ modifyCommentDepth (+ 1)
  alexSetNextCode indentCommentCode
  continueScanning

startComment :: AlexM ServerToken
startComment = do
  void $ modifyCommentDepth (+ 1)
  alexSetNextCode commentCode
  continueScanning

endComment :: AlexCode -> AlexM ServerToken
endComment nextCode = do
  newDepth <- modifyCommentDepth (\x -> x - 1)
  when (newDepth == 0) $
    alexSetNextCode nextCode
  continueScanning

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

pushLParen :: AlexAction
pushLParen _ _ =
  LParen <$ pushContext CtxHaskell

popRParen :: AlexAction
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
  pure $ Error $ IgnoreEqOrdHashNFData $
    "Error at line" <+> pretty line <> ":" <+> msg

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
endLiterate' = do
  alexSetNextCode literateCode
  alexExitLiterateEnv
  continueScanning

reservedSymbol :: Char -> AlexM ServerToken
reservedSymbol = \case
  '→' -> pure Arrow
  '∷' -> pure DoubleColon
  '⇒' -> pure Implies
  '∀' -> pure forallServerToken
  '⦇' -> pure LBanana
  '⦈' -> A.pure RBanana
  '⟦' -> startUnconditionalQuasiQuoter
  '⟧' -> endQuasiquoter
  c   -> error $ "Unexpected reserved symbol: " ++ show c

reservedSymbolQQ :: Char -> AlexM ServerToken
reservedSymbolQQ c = case ord c of
  0x27e7 -> endQuasiquoter -- '\⟧'
  _      -> continueScanning

{-# INLINE countBackslashCR #-}
countBackslashCR :: AlexInput -> Int
countBackslashCR AlexInput{aiPtr} = case unsafeTextHeadAscii aiPtr of
  -- '\\'
  92 -> case unsafeTextHeadOfTailAscii aiPtr of
    -- '\r'
    13 -> 2
    _  -> 1
  -- '\r'
  13 -> 1
  _  -> 0

-- Known codes

{-# INLINE startCode         #-}
{-# INLINE qqCode            #-}
{-# INLINE commentCode       #-}
{-# INLINE indentCommentCode #-}
{-# INLINE indentCountCode   #-}
{-# INLINE literateCode      #-}
startCode, qqCode, commentCode, indentCommentCode, indentCountCode, literateCode :: AlexCode
startCode          = AlexCode 0
qqCode             = AlexCode qq
commentCode        = AlexCode comment
indentCommentCode  = AlexCode indentComment
indentCountCode    = AlexCode indentCount
literateCode       = AlexCode literate

}
