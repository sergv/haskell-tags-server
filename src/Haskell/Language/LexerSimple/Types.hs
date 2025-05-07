{-# LANGUAGE ExtendedLiterals  #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples     #-}

{-# OPTIONS_GHC -O2 #-}

module Haskell.Language.LexerSimple.Types
  ( AlexState(..)
  , mkAlexState
  , alexEnterBirdLiterateEnv
  , alexEnterLiterateLatexEnv
  , alexExitLiterateEnv
  , pushContext
  , modifyCommentDepth
  , modifyQuasiquoterDepth
  , modifyPreprocessorDepth
  , addIndentationSize
  , checkQuasiQuoteEndPresent

  , AlexM
  , unAlexM
  , runAlexM
  , alexSetInput
  , alexSetNextCode

  , AlexInput(..)
  , aiLineL
  , takeText
  , countInputSpace
  , extractIncludeName
  , extractDefineOrLetName
  , dropUntilNL
  , dropUntilCppDirectiveEnd
  , dropUntilUnescapedNL
  , dropUntilNLOr
  , dropUntilNLOrEither
  , unsafeTextHeadAscii
  , unsafeTextHeadOfTailAscii
  , unsafeTextHead
  , utf8BS

  , asCodeL
  , asCommentDepthL
  , asQuasiquoterDepthL
  , asIndentationSizeL
  , asPreprocessorDepthL
  , asLiterateLocL
  , asHaveQQEndL

    -- * Alex interface
  , alexInputPrevChar
  , alexGetByte
  ) where

import Control.Exception
import Control.Monad.State
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Internal qualified as BSI
import Data.Char
import Data.Int
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Foreign.ForeignPtr
import Foreign.Ptr
import GHC.Base
import GHC.Ptr
import GHC.Word
import Text.Printf

import Haskell.Language.Lexer.Types (Context(..), AlexCode(..), LitMode(..), LitStyle(..), SrcPos(..), ServerToken(..), Pos(..), Line(..), increaseLine, Offset(..))
import Haskell.Language.LexerSimple.LensBlaze

data AlexState = AlexState
  { asInput        :: {-# UNPACK #-} !AlexInput
  , asIntStore     :: {-# UNPACK #-} !Word64
    -- ^ Integer field that stores all the other useful fields for lexing.
  , asContextStack :: [Context]
  } deriving (Show, Eq, Ord)

{-# INLINE asIntStoreL #-}
asIntStoreL :: Lens' AlexState Word64
asIntStoreL = lens asIntStore (\b s -> s { asIntStore = b })

{-# INLINE maybeBoolToInt #-}
-- | Encode 'Maybe Bool' as bit mask to store it within integer store.
maybeBoolToInt :: Maybe Bool -> Int
maybeBoolToInt = \case
  Nothing    -> 0
  Just False -> 1
  Just True  -> 2

{-# INLINE intToMaybeBool #-}
-- | Decode 'Maybe Bool' from bit mask stored within integer store.
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
asCodeL :: Lens' AlexState AlexCode
asCommentDepthL, asQuasiquoterDepthL, asIndentationSizeL :: Lens' AlexState Int16
-- | How many directives deep are we.
asPreprocessorDepthL :: Lens' AlexState Int16
-- | Whether we're in bird-style or latex-style literate environment
asLiterateLocL :: Lens' AlexState (LitMode LitStyle)
asHaveQQEndL   :: Lens' AlexState (Maybe Bool)
asCodeL              = asIntStoreL . intL 0  0x000f
asCommentDepthL      = asIntStoreL . intL 4  0x03ff
asQuasiquoterDepthL  = asIntStoreL . intL 14 0x03ff
asIndentationSizeL   = asIntStoreL . int16L  24
asPreprocessorDepthL = asIntStoreL . int16L  40
asLiterateLocL       = \f -> asIntStoreL (intL 56 0x0003 (fmap litLocToInt    . f . intToLitLoc))
asHaveQQEndL         = \f -> asIntStoreL (intL 58 0x0003 (fmap maybeBoolToInt . f . intToMaybeBool))

{-# INLINE litLocToInt #-}
litLocToInt :: LitMode LitStyle -> Int
litLocToInt = \case
  LitVanilla      -> 0
  LitOutside      -> 1
  LitInside Bird  -> 2
  LitInside Latex -> 3

{-# INLINE intToLitLoc #-}
intToLitLoc :: Int -> LitMode LitStyle
intToLitLoc = \case
  0 -> LitVanilla
  1 -> LitOutside
  2 -> LitInside Bird
  3 -> LitInside Latex
  x -> error $ "Invalid literate location representation: " ++ show x

mkAlexState :: LitMode Void -> AlexCode -> AlexInput -> AlexState
mkAlexState !litLoc !startCode !input =
  set asCodeL startCode $
    set asLiterateLocL (vacuous litLoc) AlexState
      { asInput        = input
      , asIntStore     = 0
      , asContextStack = []
      }

{-# INLINE alexEnterBirdLiterateEnv #-}
alexEnterBirdLiterateEnv :: MonadState AlexState m => m ()
alexEnterBirdLiterateEnv =
  modify $ set asLiterateLocL (LitInside Bird)

{-# INLINE alexEnterLiterateLatexEnv #-}
alexEnterLiterateLatexEnv :: MonadState AlexState m => m ()
alexEnterLiterateLatexEnv =
  modify $ set asLiterateLocL (LitInside Latex)

{-# INLINE alexExitLiterateEnv #-}
alexExitLiterateEnv :: MonadState AlexState m => m ()
alexExitLiterateEnv =
  modify $ set asLiterateLocL LitOutside

{-# INLINE pushContext #-}
pushContext :: MonadState AlexState m => Context -> m ()
pushContext ctx =
  modify (\s -> s { asContextStack = ctx : asContextStack s })

{-# INLINE modifyCommentDepth #-}
modifyCommentDepth :: MonadState AlexState m => (Int16 -> Int16) -> m Int16
modifyCommentDepth f = do
  depth <- gets (view asCommentDepthL)
  let !depth' = f depth
  modify $ \s -> set asCommentDepthL depth' s
  return depth'

{-# INLINE modifyQuasiquoterDepth #-}
modifyQuasiquoterDepth :: MonadState AlexState m => (Int16 -> Int16) -> m Int16
modifyQuasiquoterDepth f = do
  depth <- gets (view asQuasiquoterDepthL)
  let !depth' = f depth
  modify $ \s -> set asQuasiquoterDepthL depth' s
  return depth'

{-# INLINE modifyPreprocessorDepth #-}
modifyPreprocessorDepth :: MonadState AlexState m => (Int16 -> Int16) -> m Int16
modifyPreprocessorDepth f = do
  depth <- gets (view asPreprocessorDepthL)
  let !depth' = f depth
  modify $ \s -> set asPreprocessorDepthL depth' s
  return depth'

{-# INLINE alexSetInput #-}
alexSetInput :: MonadState AlexState m => AlexInput -> m ()
alexSetInput !input = modify $ \s -> s { asInput = input }

{-# INLINE alexSetNextCode #-}
alexSetNextCode :: MonadState AlexState m => AlexCode -> m ()
alexSetNextCode !code = modify $ set asCodeL code

{-# INLINE addIndentationSize #-}
addIndentationSize :: MonadState AlexState m => Int16 -> m ()
addIndentationSize !x =
  modify (over asIndentationSizeL (+ x))

data QQEndsState = QQEndsState
  { qqessPresent  :: Int#
  , qqessPrevChar :: Char#
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

newtype AlexM a = AlexM { unAlexM :: AlexState -> (# a, AlexState #) }
  deriving (Functor)

instance Applicative AlexM where
  pure x = AlexM $ \ !s -> (# x, s #)
  AlexM f <*> AlexM x = AlexM $ \ !s1 ->
    let (# f', s2 #) = f s1
        (# x', s3 #) = x s2
    in (# f' x', s3 #)

instance Monad AlexM where
  AlexM x >>= f = AlexM $ \ !s1 ->
    let (# x', s2 #) = x s1
    in unAlexM (f x') s2

instance MonadState AlexState AlexM where
  -- {-# INLINE get #-}
  -- {-# INLINE put #-}
  get = AlexM $ \x -> (# x, x #)
  put x = AlexM $ \_ -> (# (), x #)

-- type AlexM = State AlexState

{-# INLINE runAlexM #-}
runAlexM
  :: forall a. LitMode Void
  -> AlexCode
  -> C8.ByteString
  -> AlexM (a, [(AlexInput, ServerToken)])
  -> (a, [Pos ServerToken])
runAlexM litLoc startCode input action =
  performIO $
    withAlexInput input $ \input' _ -> do
      let a  :: a
          xs :: [(AlexInput, ServerToken)]
          (# (a, xs), _ #) = unAlexM action
                           $ mkAlexState litLoc startCode input'
      -- Contents of 'xs' has been seq'ed so TokenVals in there should
      -- have been forced and thus should not contain any references to the
      -- original input bytestring. However, in GHC 9.0 it seems that
      -- GHC does some transformation which results in some entries within 'xs'
      -- being not fully evaluated and thus lead to an error since they get
      -- forced outside of 'withForeignPtr' bounds. The call to 'evaluate' below
      -- is intended to prevent such transformation from occuring.
      _ <- evaluate xs
      pure (a, map (\(x, y) -> Pos (mkSrcPosNoPrefix x) y) xs)

mkSrcPosNoPrefix :: AlexInput -> SrcPos
mkSrcPosNoPrefix input =
  SrcPos { posLine   = view aiLineL input
         , posOffset = Offset 0
         , posPrefix = mempty
         , posSuffix = mempty
         }

data AlexInput = AlexInput
  { aiPtr      :: {-# UNPACK #-} !(Ptr Word8)
  , aiIntStore :: {-# UNPACK #-} !Word64
    -- ^ Integer field that stores all the other useful fields for lexing.
  } deriving (Eq, Ord)

instance Show AlexInput where
  show AlexInput{aiPtr, aiIntStore} =
    printf "AlexInput 0x%08x 0x%08x" ptr aiIntStore
    where
      ptr :: Word
      ptr = fromIntegral $ ptrToWordPtr aiPtr

{-# INLINE aiIntStoreL #-}
aiIntStoreL :: Lens' AlexInput Word64
aiIntStoreL = lens aiIntStore (\b s -> s { aiIntStore = b })

lineInt32L :: Lens' Int32 Line
lineInt32L = lens (Line . fromIntegral) (\(Line x) _ -> fromIntegral x)

int2Int32L :: Lens' Int32 Int
int2Int32L = lens fromIntegral (\x _ -> fromIntegral x)

{-# INLINE aiLineL       #-}
{-# INLINE aiLineLengthL #-}
-- | Current line in input stream.
aiLineL       :: Lens' AlexInput Line
-- | Length of current line.
aiLineLengthL :: Lens' AlexInput Int

aiLineL       = aiIntStoreL . int32L 0  . lineInt32L
aiLineLengthL = aiIntStoreL . int32L 32 . int2Int32L

{-# INLINE takeText #-}
takeText :: AlexInput -> Int -> Text
takeText AlexInput{aiPtr} len =
  TE.decodeUtf8 $! utf8BS len aiPtr

countInputSpace :: AlexInput -> Int -> Int
countInputSpace AlexInput{aiPtr} len =
  utf8FoldlBounded len inc 0 aiPtr
  where
    inc acc ' '#  = acc + 1
    inc acc '\t'# = acc + 8
    inc acc c#    = case fixChar c# of
      1## -> acc + 1
      _   -> acc

{-# INLINE performIO #-}
performIO :: IO a -> a
performIO = BSI.accursedUnutterablePerformIO

{-# INLINE withAlexInput #-}
withAlexInput :: C8.ByteString -> (AlexInput -> Int -> IO a) -> IO a
withAlexInput !s f =
  case s' of
    BSI.PS ptr offset len ->
      withForeignPtr ptr $ \ptr' -> do
        let !input =
             set aiLineL initLine $
               AlexInput
                 { aiPtr      = ptr' `plusPtr` offset
                 , aiIntStore = 0
                 }
        f input $! len - offset
  where
    -- Line numbering starts from 0 because we're adding additional newline
    -- at the beginning to simplify processing. Thus, line numbers in the
    -- result are 1-based.
    initLine = Line 0

    -- Add '\0' at the end so that we'll find the end of stream (just
    -- as in the old C days...)
    !s' = C8.cons '\n' $ C8.snoc (C8.snoc (stripBOM s) '\n') '\0'
    stripBOM :: C8.ByteString -> C8.ByteString
    stripBOM xs
        | "\xEF\xBB\xBF" `C8.isPrefixOf` xs
        = C8.drop 3 xs
        | otherwise
        = xs

{-# INLINE extractIncludeName #-}
extractIncludeName :: AlexInput -> Int -> Text
extractIncludeName !AlexInput{aiPtr} !n =
  textFromUtf8Region (Ptr nameStart#) (Ptr nameEnd#)
  where
    !(Ptr inputEnd#) = aiPtr `plusPtr` (n - 1)

    nameStart#, nameEnd# :: Addr#
    !(# nameStart#, nameEnd# #) = goBack# inputEnd#

    goBack# :: Addr# -> (# Addr#, Addr# #)
    goBack# ptr# = case indexWord8OffAddr# ptr# 0# of
      0#Word8  -> (# ptr#, ptr# #)
      34#Word8 -> takeBack# 34#Word8 ptr# -- '"'
      62#Word8 -> takeBack# 60#Word8 ptr# -- 60 - '<', 62 - '>'
      _        -> goBack# (ptr# `plusAddr#` -1#)

    takeBack# :: Word8# -> Addr# -> (# Addr#, Addr# #)
    takeBack# w' start# = go (start# `plusAddr#` -1#)
      where
        go :: Addr# -> (# Addr#, Addr# #)
        go ptr# = case indexWord8OffAddr# ptr# 0# of
          0#Word8  -> (# ptr# `plusAddr#` 1#, start# #)
          w
            | isTrue# (w `eqWord8#` w')
            -> (# ptr# `plusAddr#` 1#, start# #)
            | otherwise
            -> go (ptr# `plusAddr#` -1#)

{-# INLINE extractDefineOrLetName #-}
extractDefineOrLetName :: AlexInput -> Int -> Text
extractDefineOrLetName !AlexInput{aiPtr} !n =
  textFromUtf8Region (Ptr start#) end
  where
    end :: Ptr b
    !end = aiPtr `plusPtr` n

    end#, start# :: Addr#
    !(Ptr end#) = end
    start#      = (goBack# (end# `plusAddr#` -1#)) `plusAddr#` 1#

    goBack# :: Addr# -> Addr#
    goBack# ptr# = case indexWord8OffAddr# ptr# 0# of
      0#Word8  -> ptr#
      9#Word8  -> ptr# -- '\t'
      10#Word8 -> ptr# -- '\n'
      13#Word8 -> ptr# -- '\r'
      32#Word8 -> ptr# -- ' '
      92#Word8 -> ptr# -- '\\'
      _        -> goBack# (ptr# `plusAddr#` -1#)

{-# INLINE dropUntilNL #-}
dropUntilNL :: AlexInput -> AlexInput
dropUntilNL !input@AlexInput{aiPtr} =
  input { aiPtr = dropUntilNL# aiPtr }

{-# INLINE dropUntilCppDirectiveEnd #-}
dropUntilCppDirectiveEnd :: AlexM Text
dropUntilCppDirectiveEnd = do
  old@AlexState{asInput} <- get
  let input' = dropUntilUnescapedNL asInput
  put $ old { asInput = input' }
  pure $ T.strip $ T.replace "\\\n" " " $ T.replace "\r" "" $ textFromUtf8Region (aiPtr asInput) (aiPtr input')

{-# INLINE dropUntilUnescapedNL #-}
dropUntilUnescapedNL :: AlexInput -> AlexInput
dropUntilUnescapedNL !input@AlexInput{aiPtr = start} =
  case dropUntilUnescapedNL# start of
    (# seenEscapedNewlines, end #) ->
      over aiLineL (\(Line n) -> Line (n + seenEscapedNewlines)) $
        input { aiPtr = end }

{-# INLINE dropUntilNLOr #-}
dropUntilNLOr :: Word8 -> AlexInput -> AlexInput
dropUntilNLOr !w !input@AlexInput{aiPtr} =
  input { aiPtr = dropUntilNLOr# w aiPtr }

{-# INLINE dropUntilNLOrEither #-}
-- | Drop until either of two bytes.
dropUntilNLOrEither :: Word8 -> Word8 -> AlexInput -> AlexInput
dropUntilNLOrEither !w1 !w2 !input@AlexInput{aiPtr} =
  input { aiPtr = dropUntilNLOrEither# w1 w2 aiPtr }

-- Alex interface

{-# INLINE alexInputPrevChar #-}
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar !AlexInput{ aiPtr = Ptr ptr# } =
  case base# `minusAddr#` start# of
    0# -> C# (chr# ch0)
    1# -> let !(# x, _ #) = readChar1# start# ch0 in C# x
    2# -> let !(# x, _ #) = readChar2# start# ch0 in C# x
    3# -> let !(# x, _ #) = readChar3# start# ch0 in C# x
    _  -> '\0' -- Invalid!
  where
    ch0 :: Int#
    !ch0 = word2Int# (word8ToWord# (indexWord8OffAddr# start# 0#))

    base# = findCharStart ptr# `plusAddr#` -1#

    start# = findCharStart base#

    findCharStart :: Addr# -> Addr#
    findCharStart p#
        | startsWith10# w#
        = findCharStart (p# `plusAddr#` -1#)
        | otherwise
        = p#
      where
        w# = word2Int# (word8ToWord# (indexWord8OffAddr# p# 0#))

{-# INLINE alexGetByte #-}
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte !input@AlexInput{aiPtr} =
  case nextChar aiPtr of
    (# c#, n, cs #) ->
      case fixChar c# of
        0##  -> Nothing -- Abort on an unknown character
        -- '\n'
        10## -> Just (10, input')
          where
            !input' =
              over aiLineL increaseLine $
                set aiLineLengthL 0 $
                  input { aiPtr = cs }
        c    -> Just (b, input')
          where
            !b     = W8# (wordToWord8# c)
            !input' =
              over aiLineLengthL (+ I# n) $
                input { aiPtr = cs }

-- Translate unicode character into special symbol we teached Alex to recognize.
{-# INLINE fixChar #-}
fixChar :: Char# -> Word#
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
  '\x00'# -> fullStop
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
          int2Word# c2# -- Plain ascii needs no fixing.
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
    fullStop, space, upper, lower, symbol :: Word#
    digit, suffix, reservedSym, other :: Word#
    fullStop    = 0x00## -- Don't care about these
    space       = 0x01##
    upper       = 0x02##
    lower       = 0x03##
    symbol      = 0x04##
    digit       = 0x05##
    suffix      = 0x06##
    reservedSym = 0x07##
    other       = 0x08##

{-# INLINE unsafeTextHeadAscii #-}
unsafeTextHeadAscii :: Ptr Word8 -> Word8
unsafeTextHeadAscii !(Ptr ptr#) = W8# (indexWord8OffAddr# ptr# 0#)

{-# INLINE unsafeTextHeadOfTailAscii #-}
unsafeTextHeadOfTailAscii :: Ptr Word8 -> Word8
unsafeTextHeadOfTailAscii !(Ptr ptr#) = W8# (indexWord8OffAddr# ptr# 1#)

{-# INLINE unsafeTextHead #-}
unsafeTextHead :: Ptr Word8 -> Char
unsafeTextHead !x =
  case nextChar x of
    (# c#, _, _ #) -> C# c#

{-# INLINE nextChar #-}
nextChar :: Ptr Word8 -> (# Char#, Int#, Ptr Word8 #)
nextChar !(Ptr ptr#) =
  case utf8DecodeChar# ptr# of
    (# c#, nBytes# #) -> (# c#, nBytes#, Ptr (ptr# `plusAddr#` nBytes#) #)

{-# INLINE dropUntilNL# #-}
dropUntilNL# :: Ptr Word8 -> Ptr Word8
dropUntilNL# !(Ptr start#) = Ptr (go start#)
  where
    go :: Addr# -> Addr#
    go ptr# = case indexWord8OffAddr# ptr# 0# of
      0#Word8  -> ptr#
      10#Word8 -> ptr# -- '\n'
      _        -> go (ptr# `plusAddr#` 1#)

{-# INLINE dropUntilUnescapedNL# #-}
dropUntilUnescapedNL# :: Ptr Word8 -> (# Int, Ptr Word8 #)
dropUntilUnescapedNL# !(Ptr start#) = go 0 start#
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
dropUntilNLOr# !(W8# w#) !(Ptr start#) = Ptr (go start#)
  where
    go :: Addr# -> Addr#
    go ptr# = case indexWord8OffAddr# ptr# 0# of
      0#Word8  -> ptr#
      -- '\n'
      10#Word8 -> ptr#
      c#
        | isTrue# (c# `eqWord8#` w#) -> ptr#
        | otherwise                  -> go (ptr# `plusAddr#` 1#)

{-# INLINE dropUntilNLOrEither# #-}
dropUntilNLOrEither# :: Word8 -> Word8 -> Ptr Word8 -> Ptr Word8
dropUntilNLOrEither# !(W8# w1#) !(W8# w2#) !(Ptr start#) = Ptr (go start#)
  where
    go :: Addr# -> Addr#
    go ptr# = case indexWord8OffAddr# ptr# 0# of
      0#Word8  -> ptr#
      -- '\n'
      10#Word8 -> ptr#
      c#
        | isTrue# ((c# `eqWord8#` w1#) `orI#` (c# `eqWord8#` w2#))
        -> ptr#
        | otherwise
        -> go (ptr# `plusAddr#` 1#)

{-# INLINE utf8Foldl' #-}
utf8Foldl' :: forall a. (a -> Char# -> a) -> a -> Ptr Word8 -> a
utf8Foldl' f !x0 !(Ptr ptr#) =
  go x0 ptr#
  where
    go :: a -> Addr# -> a
    go !acc addr# =
      case utf8DecodeChar# addr# of
        (# _,  0#      #) -> acc
        (# c#, nBytes# #) -> go (acc `f` c#) (addr# `plusAddr#` nBytes#)

{-# INLINE utf8FoldlBounded #-}
utf8FoldlBounded :: forall a. Int -> (a -> Char# -> a) -> a -> Ptr Word8 -> a
utf8FoldlBounded !(I# len#) f !x0 !(Ptr ptr#) =
  go len# x0 ptr#
  where
    go :: Int#-> a -> Addr# -> a
    go 0# !acc _     = acc
    go n# !acc addr# =
      case utf8DecodeChar# addr# of
        (# _,  0#      #) -> acc
        (# c#, nBytes# #) ->
          go (n# -# 1#) (acc `f` c#) (addr# `plusAddr#` nBytes#)

{-# INLINE utf8BS #-}
utf8BS :: Int -> Ptr Word8 -> BS.ByteString
utf8BS !(I# nChars#) !(Ptr start#) =
  BSI.PS (performIO (newForeignPtr_ (Ptr start#))) 0 (I# (go nChars# 0#))
  where
    go :: Int# -> Int# -> Int#
    go 0# bytes# = bytes#
    go k# bytes# =
      case utf8SizeChar# (start# `plusAddr#` bytes#)  of
        0#      -> bytes#
        nBytes# -> go (k# -# 1#) (bytes# +# nBytes#)

{-# INLINE textFromUtf8Region #-}
textFromUtf8Region :: Ptr Word8 -> Ptr Word8 -> Text
textFromUtf8Region !start !end =
  TE.decodeUtf8 $ BSI.PS (performIO (newForeignPtr_ start)) 0 (minusPtr end start)

{-# INLINE utf8DecodeChar# #-}
utf8DecodeChar# :: Addr# -> (# Char#, Int# #)
utf8DecodeChar# a# =
  case indexWord8OffAddr# a# 0# of
    0#Word8 -> (# '\0'#, 0# #)
    x#      ->
      let !ch0 = word2Int# (word8ToWord# x#) in
      if  | startsWith0# ch0     -> (# chr# ch0, 1# #)
          | startsWith110# ch0   -> readChar1# a# ch0
          | startsWith1110# ch0  -> readChar2# a# ch0
          | startsWith11110# ch0 -> readChar3# a# ch0
          | otherwise            -> invalid# 1#

-- all invalid# sequences end up here:
{-# INLINE invalid# #-}
invalid# :: Int# -> (# Char#, Int# #)
invalid# nBytes# = (# '\8'#, nBytes# #)
-- TODO: check whether following note from ghc applies to server's lexer:
-- '\xFFFD' would be the usual replacement character, but
-- that's a valid symbol in Haskell, so will result in a
-- confusing parse error later on.  Instead we use '\0' which
-- will signal a lexer error immediately.

{-# INLINE readChar1# #-}
readChar1# :: Addr# -> Int# -> (# Char#, Int# #)
readChar1# a# ch0 =
  let !ch1 = word2Int# (word8ToWord# (indexWord8OffAddr# a# 1#)) in
  if noValidUtf8Cont# ch1 then invalid# 1# else
    (# chr# (((ch0 `andI#` 0x3F#) `uncheckedIShiftL#` 6#) `orI#`
              (ch1 `andI#` 0x7F#)),
      2# #)

{-# INLINE readChar2# #-}
readChar2# :: Addr# -> Int# -> (# Char#, Int# #)
readChar2# a# ch0 =
  let !ch1 = word2Int# (word8ToWord# (indexWord8OffAddr# a# 1#)) in
  if noValidUtf8Cont# ch1 then invalid# 1# else
    let !ch2 = word2Int# (word8ToWord# (indexWord8OffAddr# a# 2#)) in
    if noValidUtf8Cont# ch2 then invalid# 2# else
      (# chr# (((ch0 `andI#` 0x1F#) `uncheckedIShiftL#` 12#) `orI#`
                ((ch1 `andI#` 0x7F#) `uncheckedIShiftL#` 6#)  `orI#`
                  (ch2 `andI#` 0x7F#)),
        3# #)

{-# INLINE readChar3# #-}
readChar3# :: Addr# -> Int# -> (# Char#, Int# #)
readChar3# a# ch0 =
  let !ch1 = word2Int# (word8ToWord# (indexWord8OffAddr# a# 1#)) in
  if noValidUtf8Cont# ch1 then invalid# 1# else
    let !ch2 = word2Int# (word8ToWord# (indexWord8OffAddr# a# 2#)) in
    if noValidUtf8Cont# ch2 then invalid# 2# else
      let !ch3 = word2Int# (word8ToWord# (indexWord8OffAddr# a# 3#)) in
      if noValidUtf8Cont# ch3 then invalid# 3# else
        (# chr# (((ch0 `andI#` 0x0F#) `uncheckedIShiftL#` 18#) `orI#`
                  ((ch1 `andI#` 0x7F#) `uncheckedIShiftL#` 12#) `orI#`
                    ((ch2 `andI#` 0x7F#) `uncheckedIShiftL#` 6#)  `orI#`
                      (ch3 `andI#` 0x7F#)),
          4# #)

{-# INLINE noValidUtf8Cont# #-}
noValidUtf8Cont# :: Int# -> Bool
noValidUtf8Cont# x = isTrue# ((x <# 0x80#) `orI#` (x ># 0xBF#))

{-# INLINE startsWith0# #-}
startsWith0# :: Int# -> Bool
startsWith0# x = isTrue# ((x `andI#` 0x80#) ==# 0#)

{-# INLINE startsWith10# #-}
startsWith10# :: Int# -> Bool
startsWith10# x = isTrue# ((x `andI#` 0xC0#) ==# 0x80#)

{-# INLINE startsWith110# #-}
startsWith110# :: Int# -> Bool
startsWith110# x = isTrue# ((x `andI#` 0xE0#) ==# 0xC0#)

{-# INLINE startsWith1110# #-}
startsWith1110# :: Int# -> Bool
startsWith1110# x = isTrue# ((x `andI#` 0xF0#) ==# 0xE0#)

{-# INLINE startsWith11110# #-}
startsWith11110# :: Int# -> Bool
startsWith11110# x = isTrue# ((x `andI#` 0xF8#) ==# 0xF0#)

{-# INLINE utf8SizeChar# #-}
utf8SizeChar# :: Addr# -> Int#
utf8SizeChar# a# =
  case indexWord8OffAddr# a# 0# of
    0#Word8 -> 0#
    x#      ->
      let !ch0 = word2Int# (word8ToWord# x#) in
      if  | startsWith0# ch0     -> 1#
          | startsWith110# ch0   -> 2#
          | startsWith1110# ch0  -> 3#
          | startsWith11110# ch0 -> 4#
          | otherwise            -> 1#
