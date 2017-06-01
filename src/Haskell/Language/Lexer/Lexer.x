{
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}

-- Very important to have this one as it enables GHC to infer proper type of
-- Alex 3.2.1 actions.
--
-- The basic type is (Monad m => AlexInput -> Int -> AlexT m TokenVal), but
-- monomorphism restriction breaks its inference.
{-# LANGUAGE NoMonomorphismRestriction #-}

module Haskell.Language.Lexer.Lexer (tokenizeM) where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Profunctor (lmap)
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.PrettyPrint.Leijen.Text.Ext (Doc, Pretty(..), (<+>))

import Haskell.Language.Lexer.LexerTypes
import Haskell.Language.Lexer.Preprocessor
import FastTags.Token

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

@nl = ( [\r]? $nl )

$cpp_ident       =  [ $ascsmall $asclarge $ascdigit \_ ]
@cpp_ident_split = ( $cpp_ident | [\\] @nl )
@cpp_ws          = ( $ascspace | [\\] @nl )
@cpp_opt_ws      = @cpp_ws*
@cpp_nonempty_ws = ( $ascspace @cpp_ws* | @cpp_ws* $ascspace )
@define_body       = ( [^\\$nl] | [\\] @nl )+ @nl

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
<literate> ^ > $ws*
  / { runAlexPredM (lmap fst isLiterate) }
  { \_ len -> startLiterateBird  *> pure (Newline (len - 1)) }
<literate> ^ "\begin{code}" @nl
  / { runAlexPredM (lmap fst isLiterate) }
  { \_ len -> startLiterateLatex *> pure (Newline (len - 12)) }
<literate> (. | @nl)
  ;


<0> @nl > $space*
  / { runAlexPredM (lmap fst isLiterate) }
  { \_ len -> pure $! Newline $! len - 2 }
<0> @nl [^>]
  / { runAlexPredM (isLiterate .&&&. isInBirdEnv) }
  { \_ _   -> endLiterate }
<0> ^ "\end{code}"
  / { runAlexPredM (isLiterate .&&&. isInLatexCodeEnv) }
  { \_ _   -> endLiterate }

<0> {

$nl $space*             { \_ len -> pure $! Newline $! len - 1 }

[\-][\-]+ ~[$symbol $nl] .* ;
[\-][\-]+ / $nl         ;

-- Pragmas
"{-#" $ws* @source_pragma $ws* "#-}" { kw $ Pragma SourcePragma }

}

-- Preprocessor

<0> "#" @cpp_opt_ws
    "define" @cpp_nonempty_ws
    @cpp_ident_split+
    ( "(" @cpp_opt_ws @cpp_ident_split+ ( @cpp_opt_ws "," @cpp_opt_ws @cpp_ident_split+ )* @cpp_opt_ws ")" )?
    @cpp_nonempty_ws @define_body
  { \input len -> do
      (name, macro) <- parsePreprocessorDefine $! retrieveToken input len
      addMacroDef name macro
      pure $ Newline 0
  }

<0> "#" @cpp_opt_ws
    "undef" @cpp_nonempty_ws
    @cpp_ident_split+
  { \input len -> do
      name <- parsePreprocessorUndef $! retrieveToken input len
      removeMacroDef name
      pure $ Newline 0
  }

-- Comments
<0, comment> "{-"       { \_ _ -> startComment }
<comment> "-}"          { \_ _ -> endComment }
<comment> (. | @nl)     ;
<0> "-}"                { \_ _ -> errorAtLine "Unmatched -}" }

-- Strings
<0> [\"]                { \_ _ -> startString }
<string> [\"]           { \_ _ -> endString }
<string> [\\] @nl ( $ws+ [\\] )? ;
<string> ( $ws+ | [^\"\\$nl] | [\\] . )+ ;

-- Strings
<0> [\"]                { \_ _ -> startString }
<string> [\\] [\"\\]    ;
<string> [\\] @nl ($ws+ [\\])? ;
<string> [\"]           { \_ _ -> endString }
<string> (. | @nl)      ;

-- Characters
<0> [\'] ( [^\'\\] | @charescape ) [\'] { kw Character }

-- Template Haskell quasiquoters

<0> "[" $ident* "|"     { \_ _ -> startQuasiquoter }
<qq> "$("               { \_ _ -> startSplice CtxQuasiquoter }
<qq> "|]"               { \_ _ -> endQuasiquoter }
<qq> (. | @nl)          ;

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
"∀"                     { \_ _ -> pure $ T "forall" }
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
                        { \input len -> pure $ T $ retrieveToken input len }

}

{

kw :: Applicative m => TokenVal -> AlexAction m
kw tok = \_ _ -> pure tok

tokenizeM :: Monad m => FilePath -> LiterateMode -> Text -> m (Either Doc [Token])
tokenizeM filename mode input = runAlexT filename mode code input scanTokens
  where
    code = case mode of
      Vanilla  -> startCode
      Literate -> literateStartCode

scanTokens :: Monad m => AlexT m [Token]
scanTokens = do
  tok <- alexMonadScan
  case valOf tok of
    EOF -> return []
    _   -> (tok :) <$> scanTokens

alexMonadScan :: Monad m => AlexT m Token
alexMonadScan = do
  filename <- asks aeFilename
  line     <- gets (aiLine . asInput)
  Pos (mkSrcPos filename line) <$> alexScanTokenVal

alexScanTokenVal :: Monad m => AlexT m TokenVal
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

startComment :: Monad m => AlexT m TokenVal
startComment = do
  void $ modifyCommentDepth (+1)
  alexSetCode commentCode
  alexScanTokenVal

endComment :: Monad m => AlexT m TokenVal
endComment = do
  newDepth <- modifyCommentDepth (\x -> x - 1)
  when (newDepth == 0) $
    alexSetCode startCode
  alexScanTokenVal

startString :: Monad m => AlexT m TokenVal
startString = do
  alexSetCode stringCode
  alexScanTokenVal

endString :: Monad m => AlexT m TokenVal
endString = do
  alexSetCode startCode
  pure String

startQuasiquoter :: Monad m => AlexT m TokenVal
startQuasiquoter = do
  alexSetCode qqCode
  pure QuasiquoterStart

startSplice :: Monad m => Context -> AlexT m TokenVal
startSplice ctx = do
  alexSetCode startCode
  pushContext ctx
  pure SpliceStart

endQuasiquoter :: Monad m => AlexT m TokenVal
endQuasiquoter = do
  alexSetCode startCode
  pure QuasiquoterEnd

pushLParen :: Monad m => AlexAction (AlexT m)
pushLParen _ _ = do
  pushContext CtxHaskell
  pure LParen

popRParen :: Monad m => AlexAction (AlexT m)
popRParen _ _ = (tryRestoringContext *> pure RParen) <|> pure RParen

tryRestoringContext :: Monad m => AlexT m ()
tryRestoringContext = do
  ctx <- popContext
  alexSetCode $ case ctx of
    CtxHaskell     -> startCode
    CtxQuasiquoter -> qqCode

errorAtLine :: (MonadError Doc m, MonadState AlexState m) => Doc -> m a
errorAtLine msg = do
  line <- gets (unLine . aiLine . asInput)
  throwError $ pretty line <> ":" <+> msg

startLiterateBird :: Monad m => AlexT m ()
startLiterateBird = do
  alexSetCode startCode
  alexEnterBirdLiterateEnv

startLiterateLatex :: Monad m => AlexT m ()
startLiterateLatex = do
  alexSetCode startCode
  alexEnterLatexCodeEnv

endLiterate :: Monad m => AlexT m TokenVal
endLiterate = do
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

-- Types for Alex actions

-- alex_actions :: Monad m => Array Int (AlexAction (AlexT m))
-- alex_action_1 :: Monad m => AlexAction (AlexT m)
-- alex_action_2 :: Monad m => AlexAction (AlexT m)
-- alex_action_4 :: Monad m => AlexAction (AlexT m)
-- alex_action_5 :: Monad m => AlexAction (AlexT m)
-- alex_action_6 :: Monad m => AlexAction (AlexT m)
-- alex_action_7 :: Monad m => AlexAction (AlexT m)
-- alex_action_10 :: Monad m => AlexAction (AlexT m)
-- alex_action_11 :: Monad m => AlexAction (AlexT m)
-- alex_action_12 :: Monad m => AlexAction (AlexT m)
-- alex_action_14 :: Monad m => AlexAction (AlexT m)
-- alex_action_15 :: Monad m => AlexAction (AlexT m)
-- alex_action_16 :: Monad m => AlexAction (AlexT m)
-- alex_action_19 :: Monad m => AlexAction (AlexT m)
-- alex_action_22 :: Monad m => AlexAction (AlexT m)
-- alex_action_24 :: Monad m => AlexAction (AlexT m)
-- alex_action_25 :: Monad m => AlexAction (AlexT m)
-- alex_action_26 :: Monad m => AlexAction (AlexT m)
-- alex_action_27 :: Monad m => AlexAction (AlexT m)
-- alex_action_29 :: Monad m => AlexAction (AlexT m)
-- alex_action_30 :: Monad m => AlexAction (AlexT m)
-- alex_action_31 :: Monad m => AlexAction (AlexT m)
-- alex_action_32 :: Monad m => AlexAction (AlexT m)
-- alex_action_33 :: Monad m => AlexAction (AlexT m)
-- alex_action_34 :: Monad m => AlexAction (AlexT m)
-- alex_action_35 :: Monad m => AlexAction (AlexT m)
-- alex_action_36 :: Monad m => AlexAction (AlexT m)
-- alex_action_37 :: Monad m => AlexAction (AlexT m)
-- alex_action_38 :: Monad m => AlexAction (AlexT m)
-- alex_action_39 :: Monad m => AlexAction (AlexT m)
-- alex_action_40 :: Monad m => AlexAction (AlexT m)
-- alex_action_41 :: Monad m => AlexAction (AlexT m)
-- alex_action_42 :: Monad m => AlexAction (AlexT m)
-- alex_action_43 :: Monad m => AlexAction (AlexT m)
-- alex_action_44 :: Monad m => AlexAction (AlexT m)
-- alex_action_45 :: Monad m => AlexAction (AlexT m)
-- alex_action_46 :: Monad m => AlexAction (AlexT m)
-- alex_action_47 :: Monad m => AlexAction (AlexT m)
-- alex_action_48 :: Monad m => AlexAction (AlexT m)
-- alex_action_49 :: Monad m => AlexAction (AlexT m)
-- alex_action_50 :: Monad m => AlexAction (AlexT m)
-- alex_action_51 :: Monad m => AlexAction (AlexT m)
-- alex_action_52 :: Monad m => AlexAction (AlexT m)
-- alex_action_53 :: Monad m => AlexAction (AlexT m)
-- alex_action_54 :: Monad m => AlexAction (AlexT m)
-- alex_action_55 :: Monad m => AlexAction (AlexT m)
-- alex_action_56 :: Monad m => AlexAction (AlexT m)
-- alex_action_57 :: Monad m => AlexAction (AlexT m)
-- alex_action_58 :: Monad m => AlexAction (AlexT m)
-- alex_action_59 :: Monad m => AlexAction (AlexT m)
-- alex_action_60 :: Monad m => AlexAction (AlexT m)
-- alex_action_61 :: Monad m => AlexAction (AlexT m)
-- alex_action_62 :: Monad m => AlexAction (AlexT m)
-- alex_action_63 :: Monad m => AlexAction (AlexT m)
-- alex_action_64 :: Monad m => AlexAction (AlexT m)
-- alex_action_65 :: Monad m => AlexAction (AlexT m)
-- alex_action_66 :: Monad m => AlexAction (AlexT m)
-- alex_action_67 :: Monad m => AlexAction (AlexT m)
-- alex_action_68 :: Monad m => AlexAction (AlexT m)
-- alex_action_69 :: Monad m => AlexAction (AlexT m)
-- alex_action_70 :: Monad m => AlexAction (AlexT m)
-- alex_action_71 :: Monad m => AlexAction (AlexT m)
-- alex_action_72 :: Monad m => AlexAction (AlexT m)
-- alex_action_74 :: Monad m => AlexAction (AlexT m)

}
