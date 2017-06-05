---------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer.LexerTypes
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Thursday,  3 November 2016
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}

module Haskell.Language.Lexer.LexerTypes
  ( AlexInput(..)
  , mkAlexInput
  , mkSrcPos
  , Context(..)
  , LiterateMode(..)
  , AlexEnv(..)
  , mkAlexEnv
  , AlexCode(..)
  , AlexState(..)
  , mkAlexState
  , alexEnterBirdLiterateEnv
  , alexEnterLatexCodeEnv
  , alexExitLiterateEnv
  , pushContext
  , popContext
  , modifyCommentDepth
  , modifyQuasiquoterDepth
  , addMacroDef
  , removeMacroDef
  , enterConstantMacroDef
  , retrieveToken
    -- * Alex monad
  , AlexT
  , runAlexT
  , alexSetInput
  , alexSetCode
  , alexChangeToplevelCode
  , alexToplevelCode
  , AlexAction
    -- * Predicates
  , AlexPredM
  , runAlexPredM
  , AlexPred
  , matchedNameInPredicate
  , isLiterate
  , isInBirdEnv
  , isInLatexCodeEnv
  , isNameDefinedAsConstant
  , isNameDefinedAsFunction
  , (.&&&.)
    -- * Interface for alex
  , alexInputPrevChar
  , alexGetByte
  ) where

import Codec.Binary.UTF8.String (encodeChar)
import Control.Applicative
import Control.Monad.Except.Ext
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import Data.Foldable
import Data.Maybe
import Data.Profunctor
import Data.Semigroup (Any(..))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Lens.Micro (Lens', lens, over)
import qualified Text.PrettyPrint.Leijen.Text as PP
import Text.PrettyPrint.Leijen.Text.Ext (Pretty(..))
import qualified Text.PrettyPrint.Leijen.Text.Ext as PP

import Control.Monad.EitherK
import Data.ErrorMessage
import Data.KeyMap (KeyMap)
import qualified Data.KeyMap as KM
import Data.Symbols.MacroName (MacroName)
import FastTags.Token
import Haskell.Language.Lexer.InputStack (InputStack(..), InputType(..))
import qualified Haskell.Language.Lexer.InputStack as InputStack
import Haskell.Language.Lexer.Preprocessor (PreprocessorMacro(..), isConstant, isFunction)

advanceLine :: Char -> Line -> Line
advanceLine '\n' = increaseLine
advanceLine _    = id

data AlexInput = AlexInput
  { aiInput    :: InputStack
  , aiPrevChar :: {-# UNPACK #-} !Char
  , aiBytes    :: [Word8]
  , aiLine     :: {-# UNPACK #-} !Line
  } deriving (Eq, Ord, Show)

{-# INLINE aiInputL #-}
aiInputL :: Lens' AlexInput InputStack
aiInputL = lens aiInput (\s a -> s { aiInput = a })

mkAlexInput :: Text -> AlexInput
mkAlexInput s = AlexInput
  { aiInput    = OriginalSourceStack s'
  , aiPrevChar = '\n'
  , aiBytes    = []
  , aiLine     = initLine
  }
  where
    -- Line numbering starts from 0 because we're adding additional newline
    -- at the beginning to simplify processing. Thus, line numbers in the
    -- result are 1-based.
    initLine = Line 0

    s' = addLeadingNewline $ addTrailingNewline $ stripBOM s
    addLeadingNewline :: Text -> Text
    addLeadingNewline = T.cons '\n'
    addTrailingNewline :: Text -> Text
    addTrailingNewline str
      | T.null str         = "\n"
      | T.last str == '\n' = str
      | otherwise          = T.snoc str '\n'
    stripBOM :: Text -> Text
    stripBOM xs =
      fromMaybe xs $
      T.stripPrefix utf8BOM xs <|> T.stripPrefix utf8BOM' xs
    -- utf8BOM = "\xEF\xBB\xBF"
    utf8BOM = "\xFFEF"
    utf8BOM' = "\xFEFF"

mkSrcPos :: FilePath -> Line -> SrcPos
mkSrcPos filename line = SrcPos
  { posFile   = filename
  , posLine   = line
  , posPrefix = mempty
  }

data Context
  = CtxHaskell
  | CtxQuasiquoter
  deriving (Eq, Ord, Show)

data LiterateMode = Literate | Vanilla
  deriving (Eq, Ord, Show)

data AlexEnv = AlexEnv
  { aeFilename     :: FilePath
  , aeLiterateMode :: LiterateMode
  } deriving (Eq, Ord, Show)

mkAlexEnv :: FilePath -> LiterateMode -> AlexEnv
mkAlexEnv filename mode = AlexEnv
  { aeFilename     = filename
  , aeLiterateMode = mode
  }

-- | Abstract wrapper around alex automata states.
newtype AlexCode = AlexCode { unAlexCode :: Int }
  deriving (Eq, Ord, Show, Pretty)

data LiterateStyle = Bird | Latex
  deriving (Eq, Ord, Show, Enum, Bounded)

data AlexState = AlexState
  { asInput            :: AlexInput
  -- | Current Alex state the lexer is in. E.g. comments, string, TH quasiquoter
  -- or vanilla toplevel mode.
  , asCode             :: {-# UNPACK #-} !AlexCode
    -- | Code used to parse toplevel definitions.
  , asToplevelCode     :: {-# UNPACK #-} !AlexCode
  , asCommentDepth     :: {-# UNPACK #-} !Int
  , asQuasiquoterDepth :: {-# UNPACK #-} !Int
  -- | Whether we're in bird-style or latex-style literate environment
  , asLiterateStyle    :: !(Maybe LiterateStyle)
  , asContextStack     :: [Context]
  , asDefines          :: !(KeyMap PreprocessorMacro)
  , asUndefinedMacro   :: !(Set MacroName)
  } deriving (Eq, Ord, Show)

{-# INLINE asInputL #-}
asInputL :: Lens' AlexState AlexInput
asInputL = lens asInput (\s a -> s { asInput = a })

mkAlexState :: AlexInput -> AlexCode -> AlexCode -> AlexState
mkAlexState input startCode toplevelCode = AlexState
  { asInput            = input
  , asCode             = startCode
  , asToplevelCode     = toplevelCode
  , asCommentDepth     = 0
  , asQuasiquoterDepth = 0
  , asLiterateStyle    = Nothing
  , asContextStack     = []
  , asDefines          = KM.empty
  , asUndefinedMacro   = S.empty
  }

alexEnterBirdLiterateEnv :: MonadState AlexState m => m ()
alexEnterBirdLiterateEnv = modify $ \s -> s { asLiterateStyle = Just Bird }

alexEnterLatexCodeEnv :: MonadState AlexState m => m ()
alexEnterLatexCodeEnv = modify $ \s -> s { asLiterateStyle = Just Latex }

alexExitLiterateEnv :: MonadState AlexState m => m ()
alexExitLiterateEnv = modify $ \s -> s { asLiterateStyle = Nothing }

pushContext :: MonadState AlexState m => Context -> m ()
pushContext ctx = modify (\s -> s { asContextStack = ctx : asContextStack s })

popContext
  :: (HasCallStack, MonadState AlexState m, MonadError ErrorMessage m)
  => m Context
popContext = do
  cs <- gets asContextStack
  case cs of
    []      -> throwErrorWithCallStack "Popping empty context stack"
    c : cs' -> do
      modify $ \s -> s { asContextStack = cs' }
      pure c

modifyCommentDepth :: MonadState AlexState m => (Int -> Int) -> m Int
modifyCommentDepth f = do
  depth <- gets asCommentDepth
  let depth' = f depth
  modify $ \s -> s { asCommentDepth = depth' }
  pure depth'

modifyQuasiquoterDepth :: MonadState AlexState m => (Int -> Int) -> m Int
modifyQuasiquoterDepth f = do
  depth <- gets asQuasiquoterDepth
  let depth' = f depth
  modify $ \s -> s { asQuasiquoterDepth = depth' }
  pure depth'

addMacroDef :: MonadState AlexState m => PreprocessorMacro -> m ()
addMacroDef macro =
  modify $ \s -> s { asDefines = KM.insert macro $ asDefines s }

removeMacroDef :: MonadState AlexState m => MacroName -> m ()
removeMacroDef name =
  -- Mark name as undefined for later checks.
  modify $ \s -> s { asUndefinedMacro = S.insert name $ asUndefinedMacro s }

enterConstantMacroDef
  :: (HasCallStack, MonadState AlexState m, MonadError ErrorMessage m)
  => MacroName -- ^ Macro name, must be already defined.
  -> m ()
enterConstantMacroDef name = do
  macroDef <- gets $ KM.lookup name . asDefines
  case macroDef of
    Nothing  -> throwErrorWithCallStack $ PP.hsep
      [ "Macro name is not actually defined,"
      , "but must be at this point in program:"
      , pretty name
      ]
    Just defs ->
      case mapMaybe
             (\case
               PreprocessorConstant name body -> Just (name, body)
               PreprocessorFunction{}         -> Nothing)
             $ toList defs of
        [(macroName, body)] ->
          modify $ over (asInputL . aiInputL) (MacroStack macroName body (T.length body))
        []  -> throwErrorWithCallStack $ PP.hsep
          [ "The macro name"
          , PP.squotes $ pretty name
          , "does not define any macros constants"
          ]
        -- Not really an error, but quite embarassing to deal with...
        xs@(_:_:_) -> throwErrorWithCallStack $ PP.hsep
          [ "The macro name"
          , PP.squotes $ pretty name
          , "defines multiple constant items:"
          , PP.ppList $ map pretty xs
          ]

retrieveToken :: AlexInput -> Int -> Text
retrieveToken AlexInput{aiInput} len = InputStack.take len aiInput

newtype AlexT m a = AlexT (EitherKT ErrorMessage (ReaderT AlexEnv (StateT AlexState m)) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , Alternative
    , MonadError ErrorMessage
    , MonadReader AlexEnv
    , MonadState AlexState
    )

runAlexT
  :: Monad m
  => FilePath
  -> LiterateMode
  -> AlexCode
  -> AlexCode
  -> Text
  -> AlexT m a
  -> m (Either ErrorMessage a)
runAlexT filename mode code toplevelCode input (AlexT action) =
  flip evalStateT s $
  flip runReaderT env $
  runEitherKT action (pure . Left) (pure . Right)
  where
    s :: AlexState
    s   = mkAlexState (mkAlexInput input) code toplevelCode
    env :: AlexEnv
    env = mkAlexEnv filename mode

alexSetInput :: MonadState AlexState m => AlexInput -> m ()
alexSetInput input = modify $ \s -> s { asInput = input }

alexSetCode :: MonadState AlexState m => AlexCode -> m ()
alexSetCode code = modify $ \s -> s { asCode = code }

alexChangeToplevelCode :: MonadState AlexState m => AlexCode -> m ()
alexChangeToplevelCode code = modify $ \s -> s { asToplevelCode = code }

alexToplevelCode :: MonadState AlexState m => m ()
alexToplevelCode = alexSetCode =<< gets asToplevelCode

type AlexAction m = AlexInput -> Int -> m TokenVal


newtype AlexPredM r a = AlexPredM
  { runAlexPredM
      :: r         -- ^ Predicate state.
      -> AlexInput -- ^ Input stream before the token.
      -> Int       -- ^ Length of the token.
      -> AlexInput -- ^ Input stream after the token.
      -> a
  } deriving (Functor)

type AlexPred a = AlexPredM a Bool

instance Applicative (AlexPredM r) where
  pure x = AlexPredM $ \_ _ _ _ -> x
  AlexPredM gf <*> AlexPredM gx =
    AlexPredM $ \a b c d -> gf a b c d $ gx a b c d

instance Monad (AlexPredM r) where
  return = pure
  AlexPredM gf >>= k =
    AlexPredM $ \a b c d -> runAlexPredM (k (gf a b c d)) a b c d

instance MonadReader r (AlexPredM r) where
  ask = AlexPredM $ \x _ _ _ -> x
  local f (AlexPredM g) = AlexPredM $ \a -> g (f a)

instance Profunctor AlexPredM where
  dimap f g (AlexPredM action) = AlexPredM $ \a b c d -> g $ action (f a) b c d

matchedNameInPredicate :: AlexPredM r Text
matchedNameInPredicate = AlexPredM $ \_ input len _ -> retrieveToken input len

isLiterate :: AlexPred AlexEnv
isLiterate = do
  env <- ask
  pure $ case aeLiterateMode env of
    Literate -> True
    Vanilla  -> False

isInBirdEnv :: AlexPred AlexState
isInBirdEnv = do
  style <- asks asLiterateStyle
  pure $ case style of
    Nothing    -> False
    Just Latex -> False
    Just Bird  -> True

isInLatexCodeEnv :: AlexPred AlexState
isInLatexCodeEnv = do
  style <- asks asLiterateStyle
  pure $ case style of
    Nothing    -> False
    Just Latex -> True
    Just Bird  -> False

-- | Check whether given name is a cpp define.
isNameDefinedAsConstant :: MacroName -> AlexPred AlexState
isNameDefinedAsConstant name = -- do
    asks
  $ getAny
  . foldMap (foldMap (Any . isConstant))
  . KM.lookup name
  . asDefines
  -- defines <- asks asDefines
  -- pure $ if KM.null defines
  --   then False
  --   else getAny $ foldMap (foldMap (Any . isConstant)) $ KM.lookup name $ defines

-- | Check whether given name is a cpp define of a macro with arguments.
isNameDefinedAsFunction :: MacroName -> AlexPred AlexState
isNameDefinedAsFunction name = -- do
    asks
  $ getAny
  . foldMap (foldMap (Any . isFunction))
  . KM.lookup name
  . asDefines
  -- defines <- asks asDefines
  -- pure $ if KM.null defines
  --   then False
  --   else getAny $ foldMap (foldMap (Any . isFunction)) $ KM.lookup name $ defines

(.&&&.) :: AlexPred a -> AlexPred b -> AlexPred (a, b)
(.&&&.) x y = (&&) <$> lmap fst x <*> lmap snd y

-- Alex interface
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = aiPrevChar

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input@AlexInput{aiInput, aiBytes, aiLine} =
  case aiBytes of
    b:bs -> Just (b, input { aiBytes = bs })
    []   -> nextChar
  where
    nextChar :: Maybe (Word8, AlexInput)
    nextChar = case InputStack.uncons aiInput of
      Nothing           -> Nothing
      Just (typ, c, cs) -> Just $ encode typ (fromMaybe c $ fixChar c) cs
    encode :: InputType -> Char -> InputStack -> (Word8, AlexInput)
    encode typ c cs =
      case encodeChar c of
        b:bs -> (b, input')
          where
            input' = input
              { aiInput    = cs
              , aiBytes    = bs
              , aiPrevChar = c
              , aiLine     =
                  case typ of
                    OriginalSource -> advanceLine c aiLine
                    Macro          -> aiLine
              }
        []   -> error
          "alexGetByte: should not happen - utf8 encoding of a character is empty"

-- Translate unicode character into special symbol we teached Alex to recognize.
fixChar :: Char -> Maybe Char
-- These should not be translated since Alex known about them
fixChar '→' = Nothing
fixChar '∷' = Nothing
fixChar '⇒' = Nothing
fixChar '∀' = Nothing
fixChar c
  | c <= '\x7f' = Nothing -- Plain ascii needs no fixing.
  | otherwise
  = case generalCategory c of
      UppercaseLetter       -> Just upper
      LowercaseLetter       -> Just lower
      TitlecaseLetter       -> Just upper
      ModifierLetter        -> Just suffix
      OtherLetter           -> Just lower
      DecimalNumber         -> Just digit
      OtherNumber           -> Just digit
      ConnectorPunctuation  -> Just symbol
      DashPunctuation       -> Just symbol
      OtherPunctuation      -> Just symbol
      MathSymbol            -> Just symbol
      CurrencySymbol        -> Just symbol
      ModifierSymbol        -> Just symbol
      OtherSymbol           -> Just symbol
      Space                 -> Just space
      _other                -> Nothing
  where
    space  = '\x01'
    upper  = '\x02'
    lower  = '\x03'
    symbol = '\x04'
    digit  = '\x05'
    suffix = '\x06'
