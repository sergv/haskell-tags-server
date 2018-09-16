{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskell.Language.LexerSimple.Types
  ( advanceLine
  , countInputSpace
  , AlexInput(..)
  , mkAlexInput
  , Context(..)
  , LiterateLocation(..)
  , isLiterateEnabled
  , AlexState(..)
  , mkAlexState
  , alexEnterBirdLiterateEnv
  , alexEnterLiterateLatexEnv
  , alexExitLiterateEnv
  , pushContext
  , modifyCommentDepth
  , modifyQuasiquoterDepth
  , modifyPreprocessorDepth
  , retrieveToken
  , addIndentationSize
  , calculateQuasiQuoteEnds
  , AlexM
  , runAlexM
  , alexSetInput
  , alexSetNextCode
  , alexInputPrevChar
  , alexGetByte
  ) where

import Codec.Binary.UTF8.String (encodeChar)
import Control.Applicative
import Control.Monad.State
import Data.Char
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void, vacuous)
import Data.Word (Word8)

import Haskell.Language.Lexer.FastTags
import Haskell.Language.Lexer.Types (LiterateStyle(..), Context(..), AlexCode(..))

{-# INLINE advanceLine #-}
advanceLine :: Char -> Line -> Line
advanceLine '\n' = increaseLine
advanceLine _    = id

countInputSpace :: AlexInput -> Int -> Int
countInputSpace input len =
  countSpace $ Text.take len $ aiInput input
  where
    countSpace :: Text -> Int
    countSpace = Text.foldl' inc 0
      where
        inc acc ' '    = acc + 1
        inc acc '\t'   = acc + 8
        inc acc '\x01' = acc + 1
        inc acc _      = acc

data AlexInput = AlexInput
  { aiInput         :: Text
  , aiPrevChar      :: {-# UNPACK #-} !Char
  , aiBytes         :: [Word8]
  , aiLine          :: {-# UNPACK #-} !Line
  , aiAbsPos        :: {-# UNPACK #-} !Int
  } deriving (Show, Eq, Ord)

mkAlexInput :: Text -> AlexInput
mkAlexInput s = AlexInput
  { aiInput         = s'
  , aiPrevChar      = '\n'
  , aiBytes         = []
  , aiLine          = initLine
  , aiAbsPos        = initAbsPos
  }
  where
    -- Line numbering starts from 0 because we're adding additional newline
    -- at the beginning to simplify processing. Thus, line numbers in the
    -- result are 1-based.
    initLine = Line 0
    -- Same reasoning applies to the initial absolute position.
    initAbsPos = -1

    s' = Text.cons '\n' $ Text.snoc (stripBOM s) '\n'
    stripBOM :: Text -> Text
    stripBOM xs =
      fromMaybe xs $
      Text.stripPrefix utf8BOM xs <|> Text.stripPrefix utf8BOM' xs
    -- utf8BOM = "\xEF\xBB\xBF"
    utf8BOM = "\xFFEF"
    utf8BOM' = "\xFEFF"

data LiterateLocation a = LiterateInside a | LiterateOutside | Vanilla
  deriving (Eq, Ord, Show, Functor)

isLiterateEnabled :: LiterateLocation a -> Bool
isLiterateEnabled = \case
  LiterateInside _ -> True
  LiterateOutside  -> True
  Vanilla          -> False

data AlexState = AlexState
  { asInput                     :: AlexInput
    -- | Current Alex state the lexer is in. E.g. comments, string, TH quasiquoter
    -- or vanilla toplevel mode.
  , asCode                      :: {-# UNPACK #-} !AlexCode
  , asCommentDepth              :: {-# UNPACK #-} !Int
  , asQuasiquoterDepth          :: {-# UNPACK #-} !Int
  , asIndentationSize           :: {-# UNPACK #-} !Int
    -- | Whether we're in bird-style or latex-style literate environment
  , asLiterateLoc               :: !(LiterateLocation LiterateStyle)
  , asContextStack              :: [Context]
  , asPositionsOfQuasiQuoteEnds :: !(Maybe IntSet)
    -- | How many directives deep are we.
  , asPreprocessorDepth         :: {-# UNPACK #-} !Int
  } deriving (Show, Eq, Ord)

mkAlexState :: LiterateLocation Void -> AlexCode -> AlexInput -> AlexState
mkAlexState litLoc startCode input = AlexState
  { asInput                     = input
  , asCode                      = startCode
  , asCommentDepth              = 0
  , asQuasiquoterDepth          = 0
  , asIndentationSize           = 0
  , asLiterateLoc               = vacuous litLoc
  , asContextStack              = []
  , asPositionsOfQuasiQuoteEnds = Nothing
  , asPreprocessorDepth         = 0
  }

{-# INLINE alexEnterBirdLiterateEnv #-}
alexEnterBirdLiterateEnv :: MonadState AlexState m => m ()
alexEnterBirdLiterateEnv =
  modify $ \s -> s { asLiterateLoc = LiterateInside Bird }

{-# INLINE alexEnterLiterateLatexEnv #-}
alexEnterLiterateLatexEnv :: MonadState AlexState m => m ()
alexEnterLiterateLatexEnv =
  modify $ \s -> s { asLiterateLoc = LiterateInside Latex }

{-# INLINE alexExitLiterateEnv #-}
alexExitLiterateEnv :: MonadState AlexState m => m ()
alexExitLiterateEnv =
  modify $ \s -> s { asLiterateLoc = LiterateOutside }

{-# INLINE pushContext #-}
pushContext :: MonadState AlexState m => Context -> m ()
pushContext ctx = modify (\s -> s { asContextStack = ctx : asContextStack s })

{-# INLINE modifyCommentDepth #-}
modifyCommentDepth :: MonadState AlexState m => (Int -> Int) -> m Int
modifyCommentDepth f = do
  depth <- gets asCommentDepth
  let depth' = f depth
  modify $ \s -> s { asCommentDepth = depth' }
  return depth'

{-# INLINE modifyQuasiquoterDepth #-}
modifyQuasiquoterDepth :: MonadState AlexState m => (Int -> Int) -> m Int
modifyQuasiquoterDepth f = do
  depth <- gets asQuasiquoterDepth
  let depth' = f depth
  modify $ \s -> s { asQuasiquoterDepth = depth' }
  return depth'

{-# INLINE modifyPreprocessorDepth #-}
modifyPreprocessorDepth :: MonadState AlexState m => (Int -> Int) -> m Int
modifyPreprocessorDepth f = do
  depth <- gets asPreprocessorDepth
  let depth' = f depth
  modify $ \s -> s { asPreprocessorDepth = depth' }
  return depth'

{-# INLINE retrieveToken #-}
retrieveToken :: AlexInput -> Int -> Text
retrieveToken AlexInput{aiInput} len = Text.take len aiInput

{-# INLINE addIndentationSize #-}
addIndentationSize :: MonadState AlexState m => Int -> m ()
addIndentationSize x =
  modify (\s -> s { asIndentationSize = x + asIndentationSize s })

data QQEndsState = QQEndsState
  { qqessPos      :: {-# UNPACK #-} !Int
  , qqessMap      :: !IntSet
  , qqessPrevChar :: {-# UNPACK #-} !Char
  }

calculateQuasiQuoteEnds :: Int -> Text -> IntSet
calculateQuasiQuoteEnds startPos =
  qqessMap . Text.foldl' combine (QQEndsState startPos mempty '\n')
  where
    combine :: QQEndsState -> Char -> QQEndsState
    combine QQEndsState{qqessPos, qqessMap, qqessPrevChar} c = QQEndsState
      { qqessPos      = qqessPos + 1
      , qqessMap      =
        case (qqessPrevChar, c) of
          ('|', ']') -> IS.insert qqessPos qqessMap
          (_,   '⟧') -> IS.insert qqessPos qqessMap
          _          -> qqessMap
      , qqessPrevChar = c
      }

type AlexM = State AlexState

runAlexM
  :: LiterateLocation Void
  -> AlexCode
  -> Text
  -> AlexM a
  -> a
runAlexM litLoc startCode input action =
  evalState action s
  where
    s = mkAlexState litLoc startCode (mkAlexInput input)

{-# INLINE alexSetInput #-}
alexSetInput :: MonadState AlexState m => AlexInput -> m ()
alexSetInput input = modify $ \s -> s { asInput = input }

{-# INLINE alexSetNextCode #-}
alexSetNextCode :: MonadState AlexState m => AlexCode -> m ()
alexSetNextCode code = modify $ \s -> s { asCode = code }

-- Alex interface
{-# INLINE alexInputPrevChar #-}
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = aiPrevChar

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input@AlexInput{aiInput, aiBytes, aiLine, aiAbsPos} =
  case aiBytes of
    b:bs -> Just (b, input { aiBytes = bs })
    []   -> nextChar
  where
    nextChar = case Text.uncons aiInput of
      Nothing      -> Nothing
      Just (c, cs) -> encode (fromMaybe c $ fixChar c) cs
    encode c cs =
      case encodeChar c of
        b:bs -> Just (b, input')
          where
            input' = input
              { aiInput    = cs
              , aiBytes    = bs
              , aiPrevChar = c
              , aiLine     = advanceLine c aiLine
              , aiAbsPos   = aiAbsPos + 1
              }
        []   -> error
          "alexGetByte: should not happen - utf8 encoding of a character is empty"

-- Translate unicode character into special symbol we teached Alex to recognize.
fixChar :: Char -> Maybe Char
-- These should not be translated since Alex knows about them
fixChar '→' = Nothing
fixChar '∷' = Nothing
fixChar '⇒' = Nothing
fixChar '∀' = Nothing
fixChar c
  | c <= '\x7f' = Nothing -- Plain ascii needs no fixing.
  | otherwise
  = case generalCategory c of
      UppercaseLetter      -> Just upper
      LowercaseLetter      -> Just lower
      TitlecaseLetter      -> Just upper
      ModifierLetter       -> Just suffix
      OtherLetter          -> Just lower
      DecimalNumber        -> Just digit
      OtherNumber          -> Just digit
      Space                -> Just space
      ConnectorPunctuation -> Just symbol
      DashPunctuation      -> Just symbol
      OtherPunctuation     -> Just symbol
      MathSymbol           -> Just symbol
      CurrencySymbol       -> Just symbol
      ModifierSymbol       -> Just symbol
      OtherSymbol          -> Just symbol
      _                    -> Nothing
  where
    space  = '\x01'
    upper  = '\x02'
    lower  = '\x03'
    symbol = '\x04'
    digit  = '\x05'
    suffix = '\x06'
