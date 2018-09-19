{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}

{-# OPTIONS_GHC -O2 #-}

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
  , unsafeTextHeadAscii
  , unsafeTextHead
  ) where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Char
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Void (Void, vacuous)
import Data.Word (Word8)

import Haskell.Language.Lexer.FastTags
import Haskell.Language.Lexer.Types (LiterateStyle(..), Context(..), AlexCode(..))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU

import Foreign.ForeignPtr
import GHC.Base
import GHC.IO (IO(..))
import GHC.Ptr
import GHC.Word

{-# INLINE advanceLine #-}
advanceLine :: Word8 -> Line -> Line
advanceLine 10 = increaseLine -- 10 = '\n'
advanceLine _  = id

countInputSpace :: AlexInput -> Int -> Int
countInputSpace input len =
  countSpace $ utf8Take len $ aiInput input
  where
    countSpace :: BS.ByteString -> Int
    countSpace = utf8Foldl' inc 0
      where
        inc acc ' '#    = acc + 1
        inc acc '\t'#   = acc + 8
        inc acc c#      = case fixChar c# of 1## -> acc + 1; _ -> acc

data AlexInput = AlexInput
  { aiInput  :: C8.ByteString
  , aiLine   :: {-# UNPACK #-} !Line
  , aiAbsPos :: {-# UNPACK #-} !Int
  } deriving (Show, Eq, Ord)

mkAlexInput :: C8.ByteString -> AlexInput
mkAlexInput s = AlexInput { aiInput = s', aiLine = initLine, aiAbsPos = initAbsPos }
 where
    -- Line numbering starts from 0 because we're adding additional newline
    -- at the beginning to simplify processing. Thus, line numbers in the
    -- result are 1-based.
  initLine   = Line 0
  -- Same reasoning applies to the initial absolute position.
  initAbsPos = -1

  s'         = C8.cons '\n' $ C8.snoc (stripBOM s) '\n'
  stripBOM :: C8.ByteString -> C8.ByteString
  stripBOM xs = case C8.uncons xs of
    Just ('\xFF', xs') -> C8.tail xs'
    Just ('\xFE', xs') -> C8.tail xs'
    _                  -> xs

data LiterateLocation a = LiterateInside a | LiterateOutside | Vanilla
  deriving (Eq, Ord, Show, Functor)

{-# INLINE isLiterateEnabled #-}
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
  pure depth'

{-# INLINE modifyQuasiquoterDepth #-}
modifyQuasiquoterDepth :: MonadState AlexState m => (Int -> Int) -> m Int
modifyQuasiquoterDepth f = do
  depth <- gets asQuasiquoterDepth
  let depth' = f depth
  modify $ \s -> s { asQuasiquoterDepth = depth' }
  pure depth'

{-# INLINE modifyPreprocessorDepth #-}
modifyPreprocessorDepth :: MonadState AlexState m => (Int -> Int) -> m Int
modifyPreprocessorDepth f = do
  depth <- gets asPreprocessorDepth
  let depth' = f depth
  modify $ \s -> s { asPreprocessorDepth = depth' }
  pure depth'

{-# INLINE retrieveToken #-}
retrieveToken :: AlexInput -> Int -> T.Text
retrieveToken AlexInput{aiInput} len =
  TE.decodeUtf8 $ utf8Take len aiInput

{-# INLINE addIndentationSize #-}
addIndentationSize :: MonadState AlexState m => Int -> m ()
addIndentationSize x =
  modify (\s -> s { asIndentationSize = x + asIndentationSize s })

data QQEndsState = QQEndsState
  { qqessPos      :: {-# UNPACK #-} !Int
  , qqessMap      :: !IntSet
  , qqessPrevChar :: !Char#
  }

calculateQuasiQuoteEnds :: Int -> C8.ByteString -> IntSet
calculateQuasiQuoteEnds startPos =
  qqessMap . utf8Foldl' combine (QQEndsState startPos mempty '\n'#)
  where
    combine :: QQEndsState -> Char# -> QQEndsState
    combine QQEndsState{qqessPos, qqessMap, qqessPrevChar} c# = QQEndsState
      { qqessPos      = qqessPos + 1
      , qqessMap      =
        case (# qqessPrevChar, c# #) of
          (# '|'#, ']'# #) -> IS.insert qqessPos qqessMap
          (# _,    '⟧'# #) -> IS.insert qqessPos qqessMap
          _                -> qqessMap
      , qqessPrevChar = c#
      }

type AlexM = WriterT [Pos ServerToken] (State AlexState)

runAlexM
  :: LiterateLocation Void
  -> AlexCode
  -> C8.ByteString
  -> AlexM ()
  -> [Pos ServerToken]
runAlexM litLoc startCode input action =
  evalState (execWriterT action) s
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

{-# INLINE alexGetByte #-}
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input@AlexInput{aiInput, aiLine, aiAbsPos}
  | C8.null aiInput = Nothing
  | otherwise       = Just $! (b, input')
  where
    !(C# c#, cs) = nextChar aiInput
    !b           = W8# (fixChar c#)
    input'       = input
      { aiInput  = cs
      , aiLine   = advanceLine b aiLine
      , aiAbsPos = aiAbsPos + 1
      }

-- Translate unicode character into special symbol we teached Alex to recognize.
{-# INLINE fixChar #-}
fixChar :: Char# -> Word#
fixChar = \case
  -- These should not be translated since Alex knows about them
  '→'# -> reservedSym
  '∷'# -> reservedSym
  '⇒'# -> reservedSym
  '∀'# -> reservedSym
  '⦇'# -> reservedSym
  '⦈'# -> reservedSym
  '⟦'# -> reservedSym
  '⟧'# -> reservedSym
  c# -> case ord# c# of
    c2# | isTrue# (c2# <=# 0x7f#) ->
          if isTrue# (c2# <=# 0x07#)
          then other
          else int2Word# c2# -- Plain ascii needs no fixing.
        | otherwise   ->
          case generalCategory (C# c#) of
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
    other, space, upper, lower, symbol, digit, suffix, reservedSym :: Word#
    other       = 0x00## -- Don't care about these
    space       = 0x01##
    upper       = 0x02##
    lower       = 0x03##
    symbol      = 0x04##
    digit       = 0x05##
    suffix      = 0x06##
    reservedSym = 0x07##

unsafeTextHeadAscii :: C8.ByteString -> Word8
unsafeTextHeadAscii = BSU.unsafeHead

unsafeTextHead :: C8.ByteString -> Char
unsafeTextHead = fst . nextChar

{-# INLINE nextChar #-}
nextChar :: BS.ByteString -> (Char, BS.ByteString)
nextChar (BSI.PS payload (I# offset#) (I# len#)) =
  inlinePerformIO $ withForeignPtr payload $ \(Ptr ptr#) ->
    case utf8DecodeChar# (ptr# `plusAddr#` offset#) of
      (# c#, nBytes# #) ->
        let offset' = I# (offset# +# nBytes#)
        in pure (C# c#, BSI.PS payload offset' (I# (len# -# nBytes#)))

{-# INLINE utf8Foldl' #-}
utf8Foldl' :: forall a. (a -> Char# -> a) -> a -> BS.ByteString -> a
utf8Foldl' f x0 (BSI.PS payload (I# offset#) (I# len#)) =
  inlinePerformIO $ withForeignPtr payload $ \(Ptr ptr#) ->
    pure $! go len# x0 (ptr# `plusAddr#` offset#)
  where
    go :: Int# -> a -> Addr# -> a
    go 0# !acc _     = acc
    go n# !acc addr# =
      case utf8DecodeChar# addr# of
        (# c#, nBytes# #) -> go (n# -# nBytes#) (acc `f` c#) (addr# `plusAddr#` nBytes#)

{-# INLINE utf8Take #-}
utf8Take :: Int -> BS.ByteString -> BS.ByteString
utf8Take (I# n#) (BSI.PS payload (I# offset#) (I# len#)) =
  inlinePerformIO $ withForeignPtr payload $ \(Ptr ptr#) ->
    let start# = (ptr# `plusAddr#` offset#)

        go :: Int# -> Int# -> Int# -> Int#
        go 0#  _     m# = m#
        go _   0#    m# = m#
        go n'# len'# m# =
          case utf8SizeChar# (start# `plusAddr#` m#) of
            nBytes# -> go (n'# -# 1#) (len'# -# nBytes#) (m# +# nBytes#)

    in pure $! BSI.PS payload (I# offset#) (I# (go n# len# 0#))

{-# INLINE inlinePerformIO #-}
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #)   -> r

{-# INLINE utf8DecodeChar# #-}
utf8DecodeChar# :: Addr# -> (# Char#, Int# #)
utf8DecodeChar# a# =
  let !ch0 = word2Int# (indexWord8OffAddr# a# 0#) in
  case () of
    () | isTrue# (ch0 <=# 0x7F#) -> (# chr# ch0, 1# #)

      | isTrue# ((ch0 >=# 0xC0#) `andI#` (ch0 <=# 0xDF#)) ->
        let !ch1 = word2Int# (indexWord8OffAddr# a# 1#) in
        if isTrue# ((ch1 <# 0x80#) `orI#` (ch1 >=# 0xC0#)) then err 1# else
        (# chr# (((ch0 -# 0xC0#) `uncheckedIShiftL#` 6#) +#
                  (ch1 -# 0x80#)),
           2# #)

      | isTrue# ((ch0 >=# 0xE0#) `andI#` (ch0 <=# 0xEF#)) ->
        let !ch1 = word2Int# (indexWord8OffAddr# a# 1#) in
        if isTrue# ((ch1 <# 0x80#) `orI#` (ch1 >=# 0xC0#)) then err 1# else
        let !ch2 = word2Int# (indexWord8OffAddr# a# 2#) in
        if isTrue# ((ch2 <# 0x80#) `orI#` (ch2 >=# 0xC0#)) then err 2# else
        (# chr# (((ch0 -# 0xE0#) `uncheckedIShiftL#` 12#) +#
                 ((ch1 -# 0x80#) `uncheckedIShiftL#` 6#)  +#
                  (ch2 -# 0x80#)),
           3# #)

     | isTrue# ((ch0 >=# 0xF0#) `andI#` (ch0 <=# 0xF8#)) ->
        let !ch1 = word2Int# (indexWord8OffAddr# a# 1#) in
        if isTrue# ((ch1 <# 0x80#) `orI#` (ch1 >=# 0xC0#)) then err 1# else
        let !ch2 = word2Int# (indexWord8OffAddr# a# 2#) in
        if isTrue# ((ch2 <# 0x80#) `orI#` (ch2 >=# 0xC0#)) then err 2# else
        let !ch3 = word2Int# (indexWord8OffAddr# a# 3#) in
        if isTrue# ((ch3 <# 0x80#) `orI#` (ch3 >=# 0xC0#)) then err 3# else
        (# chr# (((ch0 -# 0xF0#) `uncheckedIShiftL#` 18#) +#
                 ((ch1 -# 0x80#) `uncheckedIShiftL#` 12#) +#
                 ((ch2 -# 0x80#) `uncheckedIShiftL#` 6#)  +#
                  (ch3 -# 0x80#)),
           4# #)

      | otherwise -> err 1#
  where
    -- all invalid sequences end up here:
    err :: Int# -> (# Char#, Int# #)
    err nBytes# = (# '\0'#, nBytes# #)
    -- TODO: check whether following note from ghc applies to server's lexer:
    -- '\xFFFD' would be the usual replacement character, but
    -- that's a valid symbol in Haskell, so will result in a
    -- confusing parse error later on.  Instead we use '\0' which
    -- will signal a lexer error immediately.

{-# INLINE utf8SizeChar# #-}
utf8SizeChar# :: Addr# -> Int#
utf8SizeChar# a# =
  let !ch0 = word2Int# (indexWord8OffAddr# a# 0#) in
  case () of
    _ | isTrue# (ch0 <=# 0x7F#) -> 1#

      | isTrue# ((ch0 >=# 0xC0#) `andI#` (ch0 <=# 0xDF#)) ->
        let !ch1 = word2Int# (indexWord8OffAddr# a# 1#) in
        if isTrue# ((ch1 <# 0x80#) `orI#` (ch1 >=# 0xC0#)) then 1# else
        2#

      | isTrue# ((ch0 >=# 0xE0#) `andI#` (ch0 <=# 0xEF#)) ->
        let !ch1 = word2Int# (indexWord8OffAddr# a# 1#) in
        if isTrue# ((ch1 <# 0x80#) `orI#` (ch1 >=# 0xC0#)) then 1# else
        let !ch2 = word2Int# (indexWord8OffAddr# a# 2#) in
        if isTrue# ((ch2 <# 0x80#) `orI#` (ch2 >=# 0xC0#)) then 2# else
        3#

     | isTrue# ((ch0 >=# 0xF0#) `andI#` (ch0 <=# 0xF8#)) ->
        let !ch1 = word2Int# (indexWord8OffAddr# a# 1#) in
        if isTrue# ((ch1 <# 0x80#) `orI#` (ch1 >=# 0xC0#)) then 1# else
        let !ch2 = word2Int# (indexWord8OffAddr# a# 2#) in
        if isTrue# ((ch2 <# 0x80#) `orI#` (ch2 >=# 0xC0#)) then 2# else
        let !ch3 = word2Int# (indexWord8OffAddr# a# 3#) in
        if isTrue# ((ch3 <# 0x80#) `orI#` (ch3 >=# 0xC0#)) then 3# else
        4#

      | otherwise -> 1#
