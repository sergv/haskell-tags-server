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

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , retrieveToken
  , AlexT
  , runAlexT
  , alexSetInput
  , alexSetCode
  , AlexAction
  , AlexPred
  , isLiterate
  , isInBirdEnv
  , isInLatexCodeEnv
  , (.&&&.)
    -- * Interface for alex
  , alexInputPrevChar
  , alexGetByte
  ) where

import Codec.Binary.UTF8.String (encodeChar)
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Data.Char
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Text.PrettyPrint.Leijen.Text (Pretty, Doc)

import Control.Monad.EitherK
import FastTags.Token

advanceLine :: Char -> Line -> Line
advanceLine '\n' = increaseLine
advanceLine _    = id

data AlexInput = AlexInput
  { aiInput         :: Text -- ^ TODO: try out lazy text
  , aiPrevChar      :: {-# UNPACK #-} !Char
  , aiBytes         :: [Word8]
  , aiLine          :: {-# UNPACK #-} !Line
  } deriving (Show, Eq, Ord)

mkAlexInput :: Text -> AlexInput
mkAlexInput s = AlexInput
  { aiInput    = s'
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
  deriving (Show, Eq, Ord)

data LiterateMode = Literate | Vanilla
  deriving (Show, Eq, Ord)

data AlexEnv = AlexEnv
  { aeFilename     :: FilePath
  , aeLiterateMode :: LiterateMode
  } deriving (Show, Eq, Ord)

mkAlexEnv :: FilePath -> LiterateMode -> AlexEnv
mkAlexEnv filename mode = AlexEnv
  { aeFilename     = filename
  , aeLiterateMode = mode
  }

newtype AlexCode = AlexCode { unAlexCode :: Int }
  deriving (Show, Eq, Ord, Pretty)

data LiterateStyle = Bird | Latex
  deriving (Show, Eq, Ord, Enum, Bounded)

data AlexState = AlexState
  { asInput            :: AlexInput
  -- | Current Alex state the lexer is in. E.g. comments, string, TH quasiquoter
  -- or vanilla toplevel mode.
  , asCode             :: {-# UNPACK #-} !AlexCode
  , asCommentDepth     :: {-# UNPACK #-} !Int
  , asQuasiquoterDepth :: {-# UNPACK #-} !Int
  -- | Whether we're in bird-style or latex-style literate environment
  , asLiterateStyle    :: !(Maybe LiterateStyle)
  , asContextStack     :: [Context]
  } deriving (Show, Eq, Ord)

mkAlexState :: AlexInput -> AlexCode -> AlexState
mkAlexState input code = AlexState
  { asInput            = input
  , asCode             = code
  , asCommentDepth     = 0
  , asQuasiquoterDepth = 0
  , asLiterateStyle    = Nothing
  , asContextStack     = []
  }

alexEnterBirdLiterateEnv :: MonadState AlexState m => m ()
alexEnterBirdLiterateEnv = modify $ \s -> s { asLiterateStyle = Just Bird }

alexEnterLatexCodeEnv :: MonadState AlexState m => m ()
alexEnterLatexCodeEnv = modify $ \s -> s { asLiterateStyle = Just Latex }

alexExitLiterateEnv :: MonadState AlexState m => m ()
alexExitLiterateEnv = modify $ \s -> s { asLiterateStyle = Nothing }

pushContext :: MonadState AlexState m => Context -> m ()
pushContext ctx = modify (\s -> s { asContextStack = ctx : asContextStack s })

popContext :: (MonadState AlexState m, MonadError Doc m) => m Context
popContext = do
  cs <- gets asContextStack
  case cs of
    []      -> throwError "Popping empty context stack"
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

retrieveToken :: AlexInput -> Int -> Text
retrieveToken AlexInput{aiInput} len = T.take len aiInput

newtype AlexT m a = AlexT (EitherKT Doc (ReaderT AlexEnv (StateT AlexState m)) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , Alternative
    , MonadError Doc
    , MonadReader AlexEnv
    , MonadState AlexState
    )

runAlexT
  :: Monad m
  => FilePath
  -> LiterateMode
  -> AlexCode
  -> Text
  -> AlexT m a
  -> m (Either Doc a)
runAlexT filename mode code input (AlexT action) =
  flip evalStateT s $
  flip runReaderT env $
  runEitherKT action (pure . Left) (pure . Right)
  where
    s :: AlexState
    s   = mkAlexState (mkAlexInput input) code
    env :: AlexEnv
    env = mkAlexEnv filename mode

alexSetInput :: MonadState AlexState m => AlexInput -> m ()
alexSetInput input = modify $ \s -> s { asInput = input }

alexSetCode :: MonadState AlexState m => AlexCode -> m ()
alexSetCode code = modify $ \s -> s { asCode = code }

type AlexAction m = AlexInput -> Int -> m TokenVal
type AlexPred a = a -> AlexInput -> Int -> AlexInput -> Bool

isLiterate :: AlexPred (AlexEnv, b)
isLiterate (env, _) _ _ _ = case aeLiterateMode env of
  Literate -> True
  Vanilla  -> False

isInBirdEnv :: AlexPred (a, AlexState)
isInBirdEnv (_, state) _ _ _ = case asLiterateStyle state of
  Nothing    -> False
  Just Latex -> False
  Just Bird  -> True

isInLatexCodeEnv :: AlexPred (a, AlexState)
isInLatexCodeEnv (_, state) _ _ _ = case asLiterateStyle state of
  Nothing    -> False
  Just Latex -> True
  Just Bird  -> False

(.&&&.) :: AlexPred a -> AlexPred a -> AlexPred a
(.&&&.) f g x y z w = f x y z w && g x y z w

-- Alex interface
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = aiPrevChar

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte input@AlexInput{aiInput, aiBytes, aiLine} =
  case aiBytes of
    b:bs -> Just (b, input { aiBytes = bs })
    []   -> nextChar
  where
    nextChar = case T.uncons aiInput of
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
