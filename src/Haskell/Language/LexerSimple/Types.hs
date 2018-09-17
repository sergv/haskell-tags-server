{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskell.Language.LexerSimple.Types
  ( countInputSpace
  , AlexInput(..)
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
  , unsafeTextHead
  ) where

import Control.Monad.State
import Data.Char
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import qualified Data.Text as T
import qualified Data.Text.Internal.Lazy as TIL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Unsafe as T
import Data.Void (Void, vacuous)
import Data.Word (Word8)

import Haskell.Language.Lexer.FastTags
import Haskell.Language.Lexer.Types (LiterateStyle(..), Context(..), AlexCode(..))

{-# INLINE advanceLine #-}
advanceLine :: Word8 -> Line -> Line
-- advanceLine '\n' = increaseLine
advanceLine 10 = increaseLine -- 10 = '\n'
advanceLine _    = id

countInputSpace :: AlexInput -> Int -> Int
countInputSpace input len =
  countSpace $ TL.take (fromIntegral len) $ aiInput input
  where
    countSpace :: TL.Text -> Int
    countSpace = TL.foldl' inc 0
      where
        inc acc ' '    = acc + 1
        inc acc '\t'   = acc + 8
        inc acc '\x01' = acc + 1
        inc acc _      = acc

data AlexInput = AlexInput
  { aiInput  :: TL.Text
  , aiLine   :: {-# UNPACK #-} !Line
  , aiAbsPos :: {-# UNPACK #-} !Int
  } deriving (Show, Eq, Ord)

mkAlexInput :: TL.Text -> AlexInput
mkAlexInput s = AlexInput
  { aiInput  = s'
  , aiLine   = initLine
  , aiAbsPos = initAbsPos
  }
  where
    -- Line numbering starts from 0 because we're adding additional newline
    -- at the beginning to simplify processing. Thus, line numbers in the
    -- result are 1-based.
    initLine = Line 0
    -- Same reasoning applies to the initial absolute position.
    initAbsPos = -1

    s' = TL.cons '\n' $ TL.snoc (stripBOM s) '\n'
    stripBOM :: TL.Text -> TL.Text
    stripBOM xs = case TL.uncons xs of
      Just ('\xFF', xs') -> TL.tail xs'
      Just ('\xFE', xs') -> TL.tail xs'
      _                  -> xs

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
retrieveToken :: AlexInput -> Int -> T.Text
retrieveToken AlexInput{aiInput} len =
  TL.toStrict $ TL.take (fromIntegral len) aiInput

{-# INLINE addIndentationSize #-}
addIndentationSize :: MonadState AlexState m => Int -> m ()
addIndentationSize x =
  modify (\s -> s { asIndentationSize = x + asIndentationSize s })

data QQEndsState = QQEndsState
  { qqessPos      :: {-# UNPACK #-} !Int
  , qqessMap      :: !IntSet
  , qqessPrevChar :: {-# UNPACK #-} !Char
  }

calculateQuasiQuoteEnds :: Int -> TL.Text -> IntSet
calculateQuasiQuoteEnds startPos =
  qqessMap . TL.foldl' combine (QQEndsState startPos mempty '\n')
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
  -> TL.Text
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
alexInputPrevChar = const '\0'

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input@AlexInput{aiInput, aiLine, aiAbsPos} =
  case TL.uncons aiInput of
    Nothing      -> Nothing
    Just (c, cs) -> Just $! encode (fixChar c) cs
  where
    encode :: Word8 -> TL.Text -> (Word8, AlexInput)
    encode b cs = (b, input')
      where
        input' = input
          { aiInput  = cs
          , aiLine   = advanceLine b aiLine
          , aiAbsPos = aiAbsPos + 1
          }

-- Translate unicode character into special symbol we teached Alex to recognize.
fixChar :: Char -> Word8
fixChar c =
  case c of
  -- These should not be translated since Alex knows about them
  '→' -> reservedSym
  '∷' -> reservedSym
  '⇒' -> reservedSym
  '∀' -> reservedSym
  '⦇' -> reservedSym
  '⦈' -> reservedSym
  '⟦' -> reservedSym
  '⟧' -> reservedSym
  _ | c <= '\x7f' -> if  c <= '\x07' then other else fromIntegral (ord c) -- Plain ascii needs no fixing.
    | otherwise   ->
    case generalCategory c of
      UppercaseLetter      -> upper
      LowercaseLetter      -> lower
      TitlecaseLetter      -> upper
      ModifierLetter       -> suffix
      NonSpacingMark       -> suffix
      OtherLetter          -> lower
      DecimalNumber        -> digit
      OtherNumber          -> digit
      Space                -> space
      ConnectorPunctuation -> symbol
      DashPunctuation      -> symbol
      OtherPunctuation     -> symbol
      MathSymbol           -> symbol
      CurrencySymbol       -> symbol
      ModifierSymbol       -> symbol
      OtherSymbol          -> symbol
      _                    -> other
  where
    other       = 0x00 -- Don't care about these
    space       = 0x01
    upper       = 0x02
    lower       = 0x03
    symbol      = 0x04
    digit       = 0x05
    suffix      = 0x06
    reservedSym = 0x07

-- unsafeTextHead :: T.Text -> Char
-- unsafeTextHead = T.unsafeHead

unsafeTextHead :: TL.Text -> Char
unsafeTextHead = \case
  TIL.Chunk x _ -> T.unsafeHead x
  TIL.Empty -> error "unsafeTextHead called on empty lazy text"
