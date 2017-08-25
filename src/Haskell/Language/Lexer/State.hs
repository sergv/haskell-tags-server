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
{-# LANGUAGE NamedFieldPuns    #-}
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
  , asCodeBeforeParsingMacroArgs
  , asFunctionMacroDef
  , asMacroArgsParenDepth
  , asMacroArgs
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
  , enterFunctionMacroDef
  , addToCurrentMacroArg
  , addNewMacroArg
  , alexSetInput
  , alexSetCode
  , alexChangeToplevelCode
  , alexSetToplevelCode
  ) where

import Control.Monad.Except.Ext
import Control.Monad.State
import qualified Data.Map as M
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import Lens.Micro
import Text.PrettyPrint.Leijen.Text.Ext (Pretty(..), ppDict, MapEntry(..))

import Data.ErrorMessage
import Data.KeyMap (KeyMap)
import qualified Data.KeyMap as KM
import Data.Symbols.MacroName (MacroName)
import Haskell.Language.Lexer.Input (AlexInput, aiInputL)
import qualified Haskell.Language.Lexer.InputStack as InputStack
import Haskell.Language.Lexer.Preprocessor
import Haskell.Language.Lexer.Types

data AlexState = AlexState
  { asInput               :: AlexInput
  -- | Current Alex state the lexer is in. E.g. comments, string, TH quasiquoter
  -- or vanilla toplevel mode.
  , asCode                :: {-# UNPACK #-} !AlexCode
    -- | Code used to parse toplevel definitions.
  , asToplevelCode        :: {-# UNPACK #-} !AlexCode
  , asCommentDepth        :: {-# UNPACK #-} !Int
  , asQuasiquoterDepth    :: {-# UNPACK #-} !Int
  -- | Whether we're in bird-style or latex-style literate environment
  , asLiterateStyle       :: !(Maybe LiterateStyle)
  , asContextStack        :: [Context]
  , asDefines             :: !(KeyMap PreprocessorMacro)
  , asUndefinedMacro      :: !(Set MacroName)

  , asCodeBeforeParsingMacroArgs :: {-# UNPACK #-} !AlexCode
  , -- | Definition of function macro that were're parsing arguments for.
    asFunctionMacroDef    :: FunctionMacroDef
    --
  , asMacroArgsParenDepth :: {-# UNPACK #-} !Int
    -- Actual textual arguments stored in reverse order. That is, in
    -- order to get correct actual arguments reverse the list itself.
  , asMacroArgs           :: [T.Text]
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

  , asCodeBeforeParsingMacroArgs = startCode
  , asFunctionMacroDef    = defaultFunctionMacroDef
  , asMacroArgsParenDepth = 0
    -- Actual textual arguments stored in reverse order. That is, in
    -- order to get correct actual arguments reverse the list itself.
  , asMacroArgs           = []
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
  => m (Maybe Context)
popContext = do
  cs <- gets asContextStack
  case cs of
    []      -> pure Nothing
    c : cs' -> do
      modify $ \s -> s { asContextStack = cs' }
      pure $ Just c

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
  => ConstantMacroDef
  -> m ()
enterConstantMacroDef ConstantMacroDef{cmdName, cmdBody} =
  modify $
    asInputL . aiInputL %~
      InputStack.ExpandingConstant cmdName cmdBody (T.length cmdBody)

enterFunctionMacroDef
  :: (HasCallStack, MonadState AlexState m, MonadError ErrorMessage m)
  => FunctionMacroDef
  -> [T.Text]
  -> m ()
enterFunctionMacroDef FunctionMacroDef{fmdName, fmdArgs, fmdBody} realArgs = do
  unless (sameLength fmdArgs realArgs) $
    throwErrorWithCallStack $ ppDict
      "Macro definition and macro application have different number of arguments"
      [ "defined arguments" :-> pretty (show fmdArgs)
      , "real arguments"    :-> pretty (show realArgs)
      , "defined arguments length" :-> pretty (length fmdArgs)
      , "real arguments length"    :-> pretty (length realArgs)
      ]
  let args = M.fromList $ zip fmdArgs realArgs
  modify $
    asInputL . aiInputL %~
      InputStack.ExpandingFunction fmdName args fmdBody (T.length fmdBody)

sameLength :: [a] -> [b] -> Bool
sameLength []     []     = True
sameLength []     _      = False
sameLength _      []     = False
sameLength (_:xs) (_:ys) = sameLength xs ys

addToCurrentMacroArg :: MonadState AlexState m => T.Text -> m ()
addToCurrentMacroArg argPart =
  modify $ \s -> s
    { asMacroArgs = case asMacroArgs s of
        []     -> [argPart]
        a : as -> a <> argPart : as
    }

addNewMacroArg :: MonadState AlexState m => m ()
addNewMacroArg =
  modify $ \s -> s { asMacroArgs = T.empty : asMacroArgs s }

alexSetInput :: MonadState AlexState m => AlexInput -> m ()
alexSetInput input = modify $ \s -> s { asInput = input }

alexSetCode :: MonadState AlexState m => AlexCode -> m ()
alexSetCode code = modify $ \s -> s { asCode = code }

alexChangeToplevelCode :: MonadState AlexState m => AlexCode -> m ()
alexChangeToplevelCode code = modify $ \s -> s { asToplevelCode = code }

alexSetToplevelCode :: MonadState AlexState m => m ()
alexSetToplevelCode = alexSetCode =<< gets asToplevelCode
