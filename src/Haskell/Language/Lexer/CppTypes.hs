-- |
-- Module:     Haskell.Language.Lexer.CppTypes
-- Copyright:  (c) Sergey Vinokurov 2025
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE DerivingVia #-}

module Haskell.Language.Lexer.CppTypes
  ( Directive(..)
  ) where

import Control.DeepSeq
import Data.Hashable
import Data.Text (Text)
import Prettyprinter.Ext

data Directive
  = Include {-# UNPACK #-} !Text
  | Define  {-# UNPACK #-} !Text
  | Undef   {-# UNPACK #-} !Text
  | Ifdef   {-# UNPACK #-} !Text
  | Ifndef  {-# UNPACK #-} !Text
  | If      {-# UNPACK #-} !Text
  | Elif    {-# UNPACK #-} !Text
  | Else
  | Endif
  -- | Line    -- {-# UNPACK #-} !Text
  -- | Error   -- {-# UNPACK #-} !Text
  -- | Warning -- {-# UNPACK #-} !Text
  deriving (Eq, Ord, Show, Generic)
  deriving Pretty via PPGeneric Directive

instance Hashable Directive
instance NFData Directive
