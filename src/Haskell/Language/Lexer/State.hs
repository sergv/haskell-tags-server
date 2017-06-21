----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Lexer.State
-- Copyright   :  (c) Sergey Vinokurov 2017
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  20 June 2017
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskell.Language.Lexer.State
  ( AlexState
  , asInput
  , asCode
  , asToplevelCode
  , asCommentDepth
  , asQuasiquoterDepth
  , asLiterateStyle
  , asContextStack
  , asDefines
  , asUndefinedMacro
  , asInputL
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
  , alexSetInput
  , alexSetCode
  , alexChangeToplevelCode
  , alexSetToplevelCode
  ) where

import Control.Monad.Except.Ext
import Control.Monad.State
import Data.Foldable
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import Lens.Micro
import qualified Text.PrettyPrint.Leijen.Text as PP
import Text.PrettyPrint.Leijen.Text.Ext (Pretty(..))
import qualified Text.PrettyPrint.Leijen.Text.Ext as PP

import Data.ErrorMessage
import Data.KeyMap (KeyMap)
import qualified Data.KeyMap as KM
import Data.Symbols.MacroName (MacroName)
import Haskell.Language.Lexer.Input (AlexInput, aiInputL)
import Haskell.Language.Lexer.InputStack
import Haskell.Language.Lexer.Preprocessor (PreprocessorMacro(..))
import Haskell.Language.Lexer.Types

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

alexSetInput :: MonadState AlexState m => AlexInput -> m ()
alexSetInput input = modify $ \s -> s { asInput = input }

alexSetCode :: MonadState AlexState m => AlexCode -> m ()
alexSetCode code = modify $ \s -> s { asCode = code }

alexChangeToplevelCode :: MonadState AlexState m => AlexCode -> m ()
alexChangeToplevelCode code = modify $ \s -> s { asToplevelCode = code }

alexSetToplevelCode :: MonadState AlexState m => m ()
alexSetToplevelCode = alexSetCode =<< gets asToplevelCode

