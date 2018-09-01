----------------------------------------------------------------------------
-- |
-- Module      :  Types
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Haskell.Language.Server.Tags.Types
  ( -- * API types
    Request(..)
  , Response(..)
  , RequestHandler
    -- * Tag server types
  , TagsServerConf(..)
  , defaultTagsServerConf
  , TagsServerState(..)
  , emptyTagsServerState
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text.Prettyprint.Doc.Ext
import GHC.Generics (Generic)

import Control.Monad.Filesystem.FileSearch (SearchCfg(..), versionControlDirs)
import Data.CompiledRegex
import Data.ErrorMessage
import Data.Map.NonEmpty (NonEmptyMap)
import Data.Path (FullPath, Extension)
import Data.Promise (Promise)
import Data.SubkeyMap (SubkeyMap)
import Data.Symbols
import Haskell.Language.Server.Tags.Types.Imports
import Haskell.Language.Server.Tags.Types.Modules

-- | Types of user requests that can be handled.
data Request =
    -- | Request to find vanilla name in module identified by file path.
    FindSymbol FullPath SymbolName
    -- | Request to find all names that match a gived regexp, starting with module
    -- identified by file path.
  | FindSymbolByRegexp FullPath CompiledRegex
  deriving (Eq, Ord, Show, Generic)

instance Pretty Request where
  pretty = ppGeneric

data Response =
    Found (NonEmpty ResolvedSymbol)
  | NotFound SymbolName
  deriving (Eq, Ord, Show, Generic)

instance Pretty Response where
  pretty = ppGeneric

type RequestHandler = Request -> IO (Promise (Either ErrorMessage Response))

data TagsServerConf = TagsServerConf
  { tsconfSearchDirs        :: SearchCfg
  , tsconfVanillaExtensions :: Set Extension
  , tsconfHsBootExtensions  :: Set Extension
    -- | Whether to read and compute tags lazily or read them all at once when
    -- server starts.
  , tsconfEagerTagging      :: Bool
  } deriving (Eq, Ord, Show)

defaultTagsServerConf :: TagsServerConf
defaultTagsServerConf = TagsServerConf
  { tsconfSearchDirs        = mempty { ignoredDirs = versionControlDirs }
  , tsconfVanillaExtensions = S.fromList [".hs", ".lhs", ".hsc", ".chs"]
  , tsconfHsBootExtensions  = S.fromList [".hs-boot", ".lhs-boot"]
  , tsconfEagerTagging      = False
  }

-- | Server state that may change while processing request.
data TagsServerState = TagsServerState
  { -- | Single module name can refer to multiple modules.
    tssLoadedModules   :: !(SubkeyMap ImportKey (NonEmpty ResolvedModule))
    -- | Set of modules we started loading. Used mainly for detecting import
    -- cycles.
  , tssLoadsInProgress :: !(Map ImportKey (NonEmptyMap FullPath UnresolvedModule))
  } deriving (Eq, Ord, Show)

emptyTagsServerState :: TagsServerState
emptyTagsServerState = TagsServerState mempty mempty
