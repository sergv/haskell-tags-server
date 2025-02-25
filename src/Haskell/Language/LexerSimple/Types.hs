{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE ExtendedLiterals    #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}

{-# OPTIONS_GHC -O2 #-}

module Haskell.Language.LexerSimple.Types
  ( AlexInput(..)
  , aiLineL
  , byteStringPos
  , Context(..)
  , LiterateLocation(..)
  , isLiterateEnabled
  , isLiterateBirdOrOutside
  , isLiterateLatexOrOutside
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
  , checkQuasiQuoteEndPresent
  , AlexM
  , runAlexM
  , alexSetInput
  , alexSetNextCode
  , alexInputPrevChar
  , extractDefineOrLetName
  , dropUntilNL
  , dropUntilUnescapedNL
  , dropUntil
  , dropUntilNLOr
  , dropUntilNLOrEither
  , dropUntil2
  , alexGetByte
  , unsafeTextHeadAscii
  , unsafeTextHeadOfTailAscii
  , unsafeTextHead
  , utf8BS
  , takeText
  , countInputSpace

  , asCodeL
  , asCommentDepthL
  , asQuasiquoterDepthL
  , asIndentationSizeL
  , asPreprocessorDepthL
  , asLiterateLocL
  , asHaveQQEndL

  , countBackslashCR
  ) where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Internal as BSI
import Data.Char
import Data.Int
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Foreign.ForeignPtr
import GHC.Base
import GHC.Ptr
import GHC.Word

import Haskell.Language.Lexer.FastTags
import Haskell.Language.Lexer.Types (LiterateStyle(..), AlexCode(..), Context(..))
import Haskell.Language.LexerSimple.LensBlaze

data AlexInput = AlexInput
  { aiPtr             :: {-# UNPACK #-} !(Ptr Word8)
  -- , aiIntStore      :: {-# UNPACK #-} !Word64

  , aiLine            :: {-# UNPACK #-} !Line
  , aiAbsPos          :: {-# UNPACK #-} !Offset -- in number of characters
  , aiLineLength      :: {-# UNPACK #-} !Int -- in bytes
  } deriving (Show, Eq, Ord)

{-# INLINE aiLineL #-}
aiLineL :: Lens' AlexInput Line
aiLineL = lens aiLine (\b s -> s { aiLine = b })

{-# INLINE byteStringPos #-}
byteStringPos :: C8.ByteString -> Int
byteStringPos (BSI.PS _payload offset _len) = offset

{-# INLINE withAlexInput #-}
withAlexInput :: C8.ByteString -> (AlexInput -> a) -> a
withAlexInput s f =
  case s' of
    BSI.PS ptr offset _len ->
      inlinePerformIO $ withForeignPtr ptr $ \ptr' -> do
        let !input = set aiLineL initLine AlexInput
              { aiPtr        = ptr' `plusPtr` offset
              , aiLine       = Line 0
              , aiAbsPos     = initAbsPos
              , aiLineLength = 0
              }
            !res = f input
        touchForeignPtr ptr
        pure res
  where
    -- Line numbering starts from 0 because we're adding additional newline
    -- at the beginning to simplify processing. Thus, line numbers in the
    -- result are 1-based.
    initLine = Line 0
    -- Same reasoning applies to the initial absolute position.
    initAbsPos = Offset (-1)

    -- Add '\0' at the end so that we'll find the end of stream (just
    -- as in the old C days...)
    s' = C8.cons '\n' $ C8.snoc (C8.snoc (stripBOM s) '\n') '\0'
    stripBOM :: C8.ByteString -> C8.ByteString
    stripBOM xs = fromMaybe xs $ C8.stripPrefix "\xEF\xBB\xBF" xs

data LiterateLocation a = LiterateInside a | LiterateOutside | Vanilla
  deriving (Eq, Ord, Show, Functor)

{-# INLINE litLocToInt #-}
litLocToInt :: LiterateLocation LiterateStyle -> Int
litLocToInt = \case
  Vanilla              -> 0
  LiterateOutside      -> 1
  LiterateInside Bird  -> 2
  LiterateInside Latex -> 3

{-# INLINE intToLitLoc #-}
intToLitLoc :: Int -> LiterateLocation LiterateStyle
intToLitLoc = \case
  0 -> Vanilla
  1 -> LiterateOutside
  2 -> LiterateInside Bird
  3 -> LiterateInside Latex
  x -> error $ "Invalid literate location representation: " ++ show x

{-# INLINE isLiterateEnabled #-}
isLiterateEnabled :: LiterateLocation a -> Bool
isLiterateEnabled = \case
  LiterateInside _ -> True
  LiterateOutside  -> True
  Vanilla          -> False

{-# INLINE isLiterateBirdOrOutside #-}
isLiterateBirdOrOutside :: LiterateLocation LiterateStyle -> Bool
isLiterateBirdOrOutside = \case
  LiterateInside Bird  -> True
  LiterateInside Latex -> False
  LiterateOutside      -> True
  Vanilla              -> False

{-# INLINE isLiterateLatexOrOutside #-}
isLiterateLatexOrOutside :: LiterateLocation LiterateStyle -> Bool
isLiterateLatexOrOutside = \case
  LiterateInside Bird  -> False
  LiterateInside Latex -> True
  LiterateOutside      -> True
  Vanilla              -> False

data AlexState = AlexState
  { asInput        :: {-# UNPACK #-} !AlexInput
  , asIntStore     :: {-# UNPACK #-} !Word64
  , asContextStack :: [Context]
  } deriving (Show, Eq, Ord)

{-# INLINE asIntStoreL #-}
asIntStoreL :: Lens' AlexState Word64
asIntStoreL = lens asIntStore (\b s -> s { asIntStore = b })

{-# INLINE maybeBoolToInt #-}
maybeBoolToInt :: Maybe Bool -> Int
maybeBoolToInt = \case
  Nothing    -> 0
  Just False -> 1
  Just True  -> 2

{-# INLINE intToMaybeBool #-}
intToMaybeBool :: Int -> Maybe Bool
intToMaybeBool = \case
  0 -> Nothing
  1 -> Just False
  2 -> Just True
  x -> error $ "Invalid integer representation of 'Maybe Bool': " ++ show x

{-# INLINE asCodeL              #-}
{-# INLINE asCommentDepthL      #-}
{-# INLINE asQuasiquoterDepthL  #-}
{-# INLINE asIndentationSizeL   #-}
{-# INLINE asPreprocessorDepthL #-}
{-# INLINE asLiterateLocL       #-}
{-# INLINE asHaveQQEndL         #-}
-- | Current Alex state the lexer is in. E.g. comments, string, TH quasiquoter
-- or vanilla toplevel mode.
asCodeL        :: Lens' AlexState AlexCode
asCommentDepthL, asQuasiquoterDepthL, asIndentationSizeL :: Lens' AlexState Int16
-- | How many directives deep are we.
asPreprocessorDepthL :: Lens' AlexState Int16
-- | Whether we're in bird-style or latex-style literate environment
asLiterateLocL :: Lens' AlexState (LiterateLocation LiterateStyle)
asHaveQQEndL   :: Lens' AlexState (Maybe Bool)
asCodeL              = asIntStoreL . int16L' 0  0x000f
asCommentDepthL      = asIntStoreL . int16L' 4  0x03ff
asQuasiquoterDepthL  = asIntStoreL . int16L' 14 0x03ff
asIndentationSizeL   = asIntStoreL . int16L  24
asPreprocessorDepthL = asIntStoreL . int16L  40
asLiterateLocL       = \f -> asIntStoreL (int16L' 56 0x0003 (fmap litLocToInt    . f . intToLitLoc))
asHaveQQEndL         = \f -> asIntStoreL (int16L' 58 0x0003 (fmap maybeBoolToInt . f . intToMaybeBool))

mkAlexState :: LiterateLocation Void -> AlexCode -> AlexInput -> AlexState
mkAlexState litLoc startCode input =
  set asCodeL startCode $
  set asLiterateLocL (vacuous litLoc) AlexState
    { asInput        = input
    , asIntStore     = 0
    , asContextStack = []
    }

{-# INLINE alexEnterBirdLiterateEnv #-}
alexEnterBirdLiterateEnv :: MonadState AlexState m => m ()
alexEnterBirdLiterateEnv =
  modify $ set asLiterateLocL (LiterateInside Bird)

{-# INLINE alexEnterLiterateLatexEnv #-}
alexEnterLiterateLatexEnv :: MonadState AlexState m => m ()
alexEnterLiterateLatexEnv =
  modify $ set asLiterateLocL (LiterateInside Latex)

{-# INLINE alexExitLiterateEnv #-}
alexExitLiterateEnv :: MonadState AlexState m => m ()
alexExitLiterateEnv =
  modify $ set asLiterateLocL LiterateOutside

{-# INLINE pushContext #-}
pushContext :: MonadState AlexState m => Context -> m ()
pushContext ctx = modify (\s -> s { asContextStack = ctx : asContextStack s })

{-# INLINE modifyCommentDepth #-}
modifyCommentDepth :: MonadState AlexState m => (Int16 -> Int16) -> m Int16
modifyCommentDepth f = do
  depth <- gets (view asCommentDepthL)
  let depth' = f depth
  modify $ \s -> set asCommentDepthL depth' s
  pure depth'

{-# INLINE modifyQuasiquoterDepth #-}
modifyQuasiquoterDepth :: MonadState AlexState m => (Int16 -> Int16) -> m Int16
modifyQuasiquoterDepth f = do
  depth <- gets (view asQuasiquoterDepthL)
  let depth' = f depth
  modify $ \s -> set asQuasiquoterDepthL depth' s
  pure depth'

{-# INLINE modifyPreprocessorDepth #-}
modifyPreprocessorDepth :: MonadState AlexState m => (Int16 -> Int16) -> m Int16
modifyPreprocessorDepth f = do
  depth <- gets (view asPreprocessorDepthL)
  let depth' = f depth
  modify $ \s -> set asPreprocessorDepthL depth' s
  pure depth'

{-# INLINE retrieveToken #-}
retrieveToken :: AlexInput -> Int -> T.Text
retrieveToken AlexInput{aiPtr} len =
  TE.decodeUtf8 $ utf8BS len aiPtr

{-# INLINE addIndentationSize #-}
addIndentationSize :: MonadState AlexState m => Int16 -> m ()
addIndentationSize x =
  modify (over asIndentationSizeL (+ x))

data QQEndsState = QQEndsState
  { qqessPresent  :: !Int#
  , qqessPrevChar :: !Char#
  }

checkQuasiQuoteEndPresent :: Ptr Word8 -> Bool
checkQuasiQuoteEndPresent
  = (\x -> isTrue# (qqessPresent x))
  . utf8Foldl' combine (QQEndsState 0# '\n'#)
  where
    combine :: QQEndsState -> Char# -> QQEndsState
    combine QQEndsState{qqessPresent, qqessPrevChar} c# = QQEndsState
      { qqessPresent      =
          qqessPresent `orI#`
            case (# qqessPrevChar, c# #) of
              (# '|'#, ']'# #) -> 1#
              (# _,    '⟧'# #) -> 1#
              _                -> 0#
      , qqessPrevChar = c#
      }

type AlexM = WriterT [Pos ServerToken] (State AlexState)

{-# INLINE runAlexM #-}
runAlexM
  :: LiterateLocation Void
  -> AlexCode
  -> C8.ByteString
  -> AlexM ()
  -> [Pos ServerToken]
runAlexM litLoc startCode input action =
  withAlexInput input $ \input' ->
    evalState (execWriterT action) $ mkAlexState litLoc startCode input'

{-# INLINE alexSetInput #-}
alexSetInput :: MonadState AlexState m => AlexInput -> m ()
alexSetInput input = modify $ \s -> s { asInput = input }

{-# INLINE alexSetNextCode #-}
alexSetNextCode :: MonadState AlexState m => AlexCode -> m ()
alexSetNextCode code = modify $ set asCodeL code

-- Alex interface
{-# INLINE alexInputPrevChar #-}
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = const '\0'

{-# INLINE extractDefineOrLetName #-}
extractDefineOrLetName :: AlexInput -> Int -> T.Text
extractDefineOrLetName AlexInput{aiPtr} n =
    TE.decodeUtf8 $ regionToUtf8BS (Ptr start#) end
    where
    !end        = aiPtr `plusPtr` n
    !(Ptr end#) = end
    start#      = (goBack# (end# `plusAddr#` -1#)) `plusAddr#` 1#

    goBack# :: Addr# -> Addr#
    goBack# ptr# = case indexWord8OffAddr# ptr# 0# of
        0#Word8  -> ptr#
        9#Word8  -> ptr# -- '\n'
        10#Word8 -> ptr# -- '\n'
        13#Word8 -> ptr# -- '\r'
        32#Word8 -> ptr# -- ' '
        92#Word8 -> ptr# -- '\\'
        _        -> goBack# (ptr# `plusAddr#` -1#)

{-# INLINE dropUntilNL #-}
dropUntilNL :: AlexInput -> AlexInput
dropUntilNL input@AlexInput{aiPtr} =
  input { aiPtr = dropUntilNL# aiPtr }

{-# INLINE dropUntilUnescapedNL #-}
dropUntilUnescapedNL :: AlexInput -> AlexInput
dropUntilUnescapedNL input@AlexInput{aiPtr = start} =
  case dropUntilUnescapedNL# start of
    (# seenNewlines, end #) ->
      over aiLineL (\(Line n) -> Line (n + seenNewlines)) $
      input { aiPtr = end }

{-# INLINE dropUntil #-}
dropUntil :: Word8 -> AlexInput -> AlexInput
dropUntil w input@AlexInput{aiPtr} =
  input { aiPtr = dropUntil# w aiPtr }

{-# INLINE dropUntil2 #-}
dropUntil2 :: Word8 -> Word8 -> AlexInput -> AlexInput
dropUntil2 w1 w2 input@AlexInput{aiPtr} =
  input { aiPtr = dropUntil2# w1 w2 aiPtr }

{-# INLINE dropUntilNLOr #-}
dropUntilNLOr :: Word8 -> AlexInput -> AlexInput
dropUntilNLOr w input@AlexInput{aiPtr} =
    input { aiPtr = dropUntilNLOr# w aiPtr }

{-# INLINE dropUntilNLOrEither #-}
-- | Drop until either of two bytes.
dropUntilNLOrEither :: Word8 -> Word8 -> AlexInput -> AlexInput
dropUntilNLOrEither w1 w2 input@AlexInput{aiPtr} =
    input { aiPtr = dropUntilNLOrEither# w1 w2 aiPtr }

{-# INLINE alexGetByte #-}
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input@AlexInput{aiPtr} =
  case nextChar aiPtr of
    (# '\0'#, _, _  #) -> Nothing
    (# c#,    n, cs #) -> Just (b, input')
      where
        !b     = W8# (fixChar c#)
        input' = case c# of
          '\n'# ->
            over aiLineL increaseLine $
            input { aiPtr = cs, aiLineLength = 0, aiAbsPos = aiAbsPos input + 1 }
          _     ->
            input { aiPtr = cs, aiLineLength = aiLineLength input + I# n, aiAbsPos = aiAbsPos input + 1 }

-- Translate unicode character into special symbol we teached Alex to recognize.
{-# INLINE fixChar #-}
fixChar :: Char# -> Word8#
fixChar = \case
  -- These should not be translated since Alex knows about them
  '→'#    -> reservedSym
  '∷'#    -> reservedSym
  '⇒'#    -> reservedSym
  '∀'#    -> reservedSym
  '⦇'#    -> reservedSym
  '⦈'#    -> reservedSym
  '⟦'#    -> reservedSym
  '⟧'#    -> reservedSym
  '\x01'# -> fullStop
  '\x02'# -> fullStop
  '\x03'# -> fullStop
  '\x04'# -> fullStop
  '\x05'# -> fullStop
  '\x06'# -> fullStop
  '\x07'# -> fullStop
  '\x08'# -> other
  c# -> case ord# c# of
    c2# | isTrue# (c2# <=# 0x7f#) ->
      wordToWord8# (int2Word# c2#) -- Plain ascii needs no fixing.
        | otherwise   ->
                  case generalCategory (C# c#) of
                    UppercaseLetter      -> upper
                    LowercaseLetter      -> lower
                    TitlecaseLetter      -> upper
                    ModifierLetter       -> suffix
                    OtherLetter          -> lower
                    NonSpacingMark       -> suffix
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

                    SpacingCombiningMark -> space
                    EnclosingMark        -> other
                    LetterNumber         -> symbol
                    OpenPunctuation      -> symbol
                    ClosePunctuation     -> symbol
                    InitialQuote         -> symbol
                    FinalQuote           -> symbol
                    LineSeparator        -> space
                    ParagraphSeparator   -> space
                    Control              -> other
                    Format               -> other
                    Surrogate            -> other
                    PrivateUse           -> other
                    NotAssigned          -> other
  where
    fullStop, space, upper, lower, symbol, digit, suffix, reservedSym, other :: Word8#
    fullStop    = 0x00#Word8 -- Don't care about these
    space       = 0x01#Word8
    upper       = 0x02#Word8
    lower       = 0x03#Word8
    symbol      = 0x04#Word8
    digit       = 0x05#Word8
    suffix      = 0x06#Word8
    reservedSym = 0x07#Word8
    other       = 0x08#Word8

{-# INLINE unsafeTextHeadAscii #-}
unsafeTextHeadAscii :: Ptr Word8 -> Word8
unsafeTextHeadAscii (Ptr ptr#) = W8# (indexWord8OffAddr# ptr# 0#)

{-# INLINE unsafeTextHeadOfTailAscii #-}
unsafeTextHeadOfTailAscii :: Ptr Word8 -> Word8
unsafeTextHeadOfTailAscii (Ptr ptr#) = W8# (indexWord8OffAddr# ptr# 1#)

{-# INLINE unsafeTextHead #-}
unsafeTextHead :: Ptr Word8 -> Char
unsafeTextHead x =
  case nextChar x of
    (# c#, _, _ #) -> C# c#

{-# INLINE nextChar #-}
nextChar :: Ptr Word8 -> (# Char#, Int#, Ptr Word8 #)
nextChar (Ptr ptr#) =
  case utf8DecodeChar# ptr# of
    (# c#, nBytes# #) -> (# c#, nBytes#, Ptr (ptr# `plusAddr#` nBytes#) #)

{-# INLINE dropUntilNL# #-}
dropUntilNL# :: Ptr Word8 -> Ptr Word8
dropUntilNL# (Ptr start#) = Ptr (go start#)
  where
    go :: Addr# -> Addr#
    go ptr# = case indexWord8OffAddr# ptr# 0# of
      0#Word8  -> ptr#
      10#Word8 -> ptr# -- '\n'
      _        -> go (ptr# `plusAddr#` 1#)

{-# INLINE dropUntilUnescapedNL# #-}
dropUntilUnescapedNL# :: Ptr Word8 -> (# Int, Ptr Word8 #)
dropUntilUnescapedNL# (Ptr start#) = go 0 start#
    where
    go :: Int -> Addr# -> (# Int, Ptr Word8 #)
    go !n ptr# = case indexWord8OffAddr# ptr# 0# of
        0#Word8  -> (# n, Ptr ptr# #)
        -- '\n'
        10#Word8 -> (# n, Ptr ptr# #)
        -- '\\'
        92#Word8 ->
            case indexWord8OffAddr# ptr# 1# of
                0#Word8  -> (# n, Ptr (ptr# `plusAddr#` 1#) #)
                -- '\n'
                10#Word8 -> go (n + 1) (ptr# `plusAddr#` 2#)
                _        -> go n (ptr# `plusAddr#` 2#)
        _        -> go n (ptr# `plusAddr#` 1#)

{-# INLINE dropUntilNLOr# #-}
dropUntilNLOr# :: Word8 -> Ptr Word8 -> Ptr Word8
dropUntilNLOr# (W8# w#) (Ptr start#) = Ptr (go start#)
    where
    go :: Addr# -> Addr#
    go ptr# = case word8ToWord# (indexWord8OffAddr# ptr# 0#) of
        0##  -> ptr#
        -- '\n'
        10## -> ptr#
        c# | isTrue# (c# `eqWord#` word8ToWord# w#) -> ptr#
           | otherwise                 -> go (ptr# `plusAddr#` 1#)

{-# INLINE dropUntilNLOrEither# #-}
dropUntilNLOrEither# :: Word8 -> Word8 -> Ptr Word8 -> Ptr Word8
dropUntilNLOrEither# (W8# w1#) (W8# w2#) (Ptr start#) = Ptr (go start#)
    where
    go :: Addr# -> Addr#
    go ptr# = case word8ToWord# (indexWord8OffAddr# ptr# 0#) of
        0##  -> ptr#
        -- '\n'
        10## -> ptr#
        c# | isTrue# ((c# `eqWord#` word8ToWord# w1#) `orI#` (c# `eqWord#` word8ToWord# w2#))
           -> ptr#
           | otherwise
           -> go (ptr# `plusAddr#` 1#)

{-# INLINE dropUntil# #-}
dropUntil# :: Word8 -> Ptr Word8 -> Ptr Word8
dropUntil# (W8# w#) (Ptr start#) = Ptr (go start#)
  where
    go :: Addr# -> Addr#
    go ptr# = case indexWord8OffAddr# ptr# 0# of
      0#Word8 -> ptr#
      1#Word8 -> ptr# -- '\n'
      c# | isTrue# (c# `eqWord8#` w#) -> ptr#
         | otherwise                  -> go (ptr# `plusAddr#` 1#)

{-# INLINE dropUntil2# #-}
dropUntil2# :: Word8 -> Word8 -> Ptr Word8 -> Ptr Word8
dropUntil2# (W8# w1#) (W8# w2#) (Ptr start#) = Ptr (go start#)
  where
    go :: Addr# -> Addr#
    go ptr# = case indexWord8OffAddr# ptr# 0# of
      0#Word8  -> ptr#
      10#Word8 -> ptr# -- '\n'
      c# | isTrue# ((c# `eqWord8#` w1#) `orI#` (c# `eqWord8#` w2#)) -> ptr#
         | otherwise                                                -> go (ptr# `plusAddr#` 1#)

{-# INLINE utf8Foldl' #-}
utf8Foldl' :: forall a. (a -> Char# -> a) -> a -> Ptr Word8 -> a
utf8Foldl' f x0 (Ptr ptr#) =
  go x0 ptr#
  where
    go :: a -> Addr# -> a
    go !acc addr# =
      case utf8DecodeChar# addr# of
        (# _,  0#      #) -> acc
        (# c#, nBytes# #) -> go (acc `f` c#) (addr# `plusAddr#` nBytes#)

{-# INLINE utf8FoldlBounded #-}
utf8FoldlBounded :: forall a. Int -> (a -> Char# -> a) -> a -> Ptr Word8 -> a
utf8FoldlBounded (I# len#) f x0 (Ptr ptr#) =
  go len# x0 ptr#
  where
    go :: Int#-> a -> Addr# -> a
    go 0# !acc _ = acc
    go n# !acc addr# =
      case utf8DecodeChar# addr# of
        (# _,  0#      #) -> acc
        (# c#, nBytes# #) -> go (n# -# 1#) (acc `f` c#) (addr# `plusAddr#` nBytes#)

{-# INLINE utf8BS #-}
utf8BS :: Int -> Ptr Word8 -> BS.ByteString
utf8BS (I# n#) (Ptr start#) =
  BSI.PS (inlinePerformIO (newForeignPtr_ (Ptr start#))) 0 (I# (go n# start# 0#))
  where
    go :: Int# -> Addr# -> Int# -> Int#
    go 0# _    m# = m#
    go k# ptr# m# =
      case utf8SizeChar# ptr# of
        0#      -> m#
        nBytes# -> go (k# -# 1#) (ptr# `plusAddr#` nBytes#) (m# +# nBytes#)

{-# INLINE takeText #-}
takeText :: AlexInput -> Int -> T.Text
takeText AlexInput{aiPtr} len =
    TE.decodeUtf8 $! utf8BS len aiPtr

countInputSpace :: AlexInput -> Int -> Int
countInputSpace AlexInput{aiPtr} len =
  utf8FoldlBounded len inc 0 aiPtr
  where
    inc acc ' '#  = acc + 1
    inc acc '\t'# = acc + 8
    inc acc c#    = case word8ToWord# (fixChar c#) of
      1## -> acc + 1
      _   -> acc

{-# INLINE inlinePerformIO #-}
inlinePerformIO :: IO a -> a
inlinePerformIO = BSI.accursedUnutterablePerformIO

{-# INLINE regionToUtf8BS #-}
regionToUtf8BS :: Ptr Word8 -> Ptr Word8 -> BS.ByteString
regionToUtf8BS start end =
    BSI.PS (inlinePerformIO (newForeignPtr_ start)) 0 (minusPtr end start)

{-# INLINE utf8DecodeChar# #-}
utf8DecodeChar# :: Addr# -> (# Char#, Int# #)
utf8DecodeChar# a# =
  case indexWord8OffAddr# a# 0# of
    0#Word8 -> (# '\0'#, 0# #)
    !x#     ->
      let !ch0 = word2Int# (word8ToWord# x#) in
      case () of
        () | isTrue# (ch0 <=# 0x7F#) -> (# chr# ch0, 1# #)

          | isTrue# ((ch0 >=# 0xC0#) `andI#` (ch0 <=# 0xDF#)) ->
            let !ch1 = word2Int# (word8ToWord# (indexWord8OffAddr# a# 1#)) in
            if isTrue# ((ch1 <# 0x80#) `orI#` (ch1 >=# 0xC0#)) then err 1# else
            (# chr# (((ch0 -# 0xC0#) `uncheckedIShiftL#` 6#) +#
                      (ch1 -# 0x80#)),
               2# #)

          | isTrue# ((ch0 >=# 0xE0#) `andI#` (ch0 <=# 0xEF#)) ->
            let !ch1 = word2Int# (word8ToWord# (indexWord8OffAddr# a# 1#)) in
            if isTrue# ((ch1 <# 0x80#) `orI#` (ch1 >=# 0xC0#)) then err 1# else
            let !ch2 = word2Int# (word8ToWord# (indexWord8OffAddr# a# 2#)) in
            if isTrue# ((ch2 <# 0x80#) `orI#` (ch2 >=# 0xC0#)) then err 2# else
            (# chr# (((ch0 -# 0xE0#) `uncheckedIShiftL#` 12#) +#
                     ((ch1 -# 0x80#) `uncheckedIShiftL#` 6#)  +#
                      (ch2 -# 0x80#)),
               3# #)

         | isTrue# ((ch0 >=# 0xF0#) `andI#` (ch0 <=# 0xF8#)) ->
            let !ch1 = word2Int# (word8ToWord# (indexWord8OffAddr# a# 1#)) in
            if isTrue# ((ch1 <# 0x80#) `orI#` (ch1 >=# 0xC0#)) then err 1# else
            let !ch2 = word2Int# (word8ToWord# (indexWord8OffAddr# a# 2#)) in
            if isTrue# ((ch2 <# 0x80#) `orI#` (ch2 >=# 0xC0#)) then err 2# else
            let !ch3 = word2Int# (word8ToWord# (indexWord8OffAddr# a# 3#)) in
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
        err nBytes# = (# '\8'#, nBytes# #)
        -- TODO: check whether following note from ghc applies to server's lexer:
        -- '\xFFFD' would be the usual replacement character, but
        -- that's a valid symbol in Haskell, so will result in a
        -- confusing parse error later on.  Instead we use '\0' which
        -- will signal a lexer error immediately.

{-# INLINE utf8SizeChar# #-}
utf8SizeChar# :: Addr# -> Int#
utf8SizeChar# a# =
  case indexWord8OffAddr# a# 0# of
    0#Word8 -> 0#
    !x#     ->
      let !ch0 = word2Int# (word8ToWord# x#) in
      case () of
        _ | isTrue# (ch0 <=# 0x7F#) -> 1#

          | isTrue# ((ch0 >=# 0xC0#) `andI#` (ch0 <=# 0xDF#)) ->
            let !ch1 = word2Int# (word8ToWord# (indexWord8OffAddr# a# 1#)) in
            if isTrue# ((ch1 <# 0x80#) `orI#` (ch1 >=# 0xC0#)) then 1# else
            2#

          | isTrue# ((ch0 >=# 0xE0#) `andI#` (ch0 <=# 0xEF#)) ->
            let !ch1 = word2Int# (word8ToWord# (indexWord8OffAddr# a# 1#)) in
            if isTrue# ((ch1 <# 0x80#) `orI#` (ch1 >=# 0xC0#)) then 1# else
            let !ch2 = word2Int# (word8ToWord# (indexWord8OffAddr# a# 2#)) in
            if isTrue# ((ch2 <# 0x80#) `orI#` (ch2 >=# 0xC0#)) then 2# else
            3#

         | isTrue# ((ch0 >=# 0xF0#) `andI#` (ch0 <=# 0xF8#)) ->
            let !ch1 = word2Int# (word8ToWord# (indexWord8OffAddr# a# 1#)) in
            if isTrue# ((ch1 <# 0x80#) `orI#` (ch1 >=# 0xC0#)) then 1# else
            let !ch2 = word2Int# (word8ToWord# (indexWord8OffAddr# a# 2#)) in
            if isTrue# ((ch2 <# 0x80#) `orI#` (ch2 >=# 0xC0#)) then 2# else
            let !ch3 = word2Int# (word8ToWord# (indexWord8OffAddr# a# 3#)) in
            if isTrue# ((ch3 <# 0x80#) `orI#` (ch3 >=# 0xC0#)) then 3# else
            4#

          | otherwise -> 1#

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
