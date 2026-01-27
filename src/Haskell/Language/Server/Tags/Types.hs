----------------------------------------------------------------------------
-- |
-- Module      :  Types
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Haskell.Language.Server.Tags.Types
  ( -- * API types
    SomeRequest(..)
  , Request(..)
  , UserRequest(..)
  , QueryRequest(..)
  , Namespace(..)
  , isPathWithinNamespace
  , FSNotifyEvent(..)
  , QueryResponse(..)
  , RequestHandler
  , NameResolutionScope(..)
    -- * Tag server types
  , NameResolutionStrictness(..)
  , TagsServerConf(..)
  , defaultTagsServerConf
  , TagsServerState(..)
  , emptyTagsServerState
  , LoadState(..)
  , emptyLoadState
  ) where

import Data.Kind
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Store (Store)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generically(..))
import Prettyprinter.Ext

import Data.CompiledRegex
import Data.ErrorMessage
import Data.Map.NonEmpty (NonEmptyMap)
import Data.Path
import Data.Promise (Promise)
import Data.Symbols
import Haskell.Language.Server.Tags.Types.Imports
import Haskell.Language.Server.Tags.Types.Modules

data SomeRequest = forall resp. SomeRequest !(Request resp) !resp

-- | Types of user requests that can be handled.
data Request (resp :: Type) where
  -- | Request to find a name in module identified by file path.
  UserReq     :: !(UserRequest resp) -> Request (Promise (Either ErrorMessage resp))
  FSNotifyReq :: !FSNotifyEvent      -> Request ()

deriving instance Eq   (Request resp)
deriving instance Ord  (Request resp)
deriving instance Show (Request resp)

instance Pretty (Request resp) where
  pretty = \case
    UserReq req       -> ppDictHeader "UserReq"
      [ "req" --> req
      ]
    FSNotifyReq event -> ppDictHeader "FSNotifyReq"
      [ "event" --> event
      ]

-- | Types of user requests that can be handled.
data UserRequest (resp :: Type) where
    -- | Request to find a name in module identified by file path.
  QueryReq
    :: !(FullPath 'File)
    -> !QueryRequest
    -> !Namespace
    -> UserRequest QueryResponse
  -- | Stop accepting new requests and exit. Possibly serialise current
  -- state for quicker future loads.
  FinishReq
    :: UserRequest ()

deriving instance Eq   (UserRequest resp)
deriving instance Ord  (UserRequest resp)
deriving instance Show (UserRequest resp)

instance Pretty (UserRequest resp) where
  pretty = \case
    QueryReq file req ns -> ppDictHeader "QueryReq"
      [ "file"      --> file
      , "req"       --> req
      , "namespace" --> ns
      ]
    FinishReq -> "FinishReq"

-- | Set of directories
data Namespace = Namespace
  { -- | Set of extra watched shallow directories. Subdirectories will not be watched.
    nsShallowDirs   :: !(Set (FullPath 'Dir))
    -- | Set extra of watched recursive directories.
    -- The directory and all its subdirectories will be watched.
  , nsRecursiveDirs :: !(Set (FullPath 'Dir))
  , nsIgnoredGlobs  :: !(Set Text)
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (Semigroup, Monoid) via Generically Namespace
  deriving Pretty via PPGeneric Namespace

instance Store Namespace

-- | Test whether a path lies within directories specified by a namespace.
-- Ignored globs will not be tested - it's assumed that they were taken
-- into account when paths were produced.
isPathWithinNamespace :: Namespace -> FullPath a -> Bool
isPathWithinNamespace Namespace{nsShallowDirs, nsRecursiveDirs} path =
  S.member (takeDirectory path) nsShallowDirs ||
  any ((`T.isPrefixOf` path') . unFullPath) nsRecursiveDirs
  where
    path' = unFullPath path

data NameResolutionScope
  = ScopeCurrentModule
  | ScopeAllModules
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving Pretty via PPGeneric NameResolutionScope

-- | Query server for some information.
data QueryRequest
  = -- | Request to find vanilla name in current module and all its imports.
    FindSymbol !NameResolutionScope !SymbolName
    -- | Request to find all names that match a gived regex starting from
    -- current module.
  | FindSymbolByRegex !NameResolutionScope !CompiledRegex
  deriving (Eq, Ord, Show, Generic)
  deriving Pretty via PPGeneric QueryRequest

data FSNotifyEvent
  = FSAdded    !(FullPath 'File)
  | FSRemoved  !(FullPath 'File)
  | FSModified !(FullPath 'File)
  deriving (Eq, Ord, Show, Generic)
  deriving Pretty via PPGeneric FSNotifyEvent

data QueryResponse
  = Found !(NonEmpty ResolvedSymbol)
  | NotFound
  deriving (Eq, Ord, Show, Generic)
  deriving Pretty via PPGeneric QueryResponse

type RequestHandler = forall resp. UserRequest resp -> IO (Promise (Either ErrorMessage resp))

-- | Whether to ignore some issues when resolving names.
data NameResolutionStrictness
  = -- | Default: use for day-to-day lookups, ignores some errors but
    -- this allows to analyse more modules e.g. the ones that define
    -- names with preprocessor/Tempate Haskell.
    NameResolutionLax
  | -- | Do not ignore following events:
    -- 1. A module exports a name with children but no definition of children can be found.
    -- 2. Imported module is missing during eager tagging - default to no names in lax mode.
    -- 3. A module file is not found - default to no names in lax mode.
    NameResolutionStrict
  deriving (Eq, Ord, Show, Bounded, Enum, Generic)
  deriving Pretty via PPGeneric NameResolutionStrictness

data TagsServerConf = TagsServerConf
  { tsconfVanillaExtensions :: !(Set Extension)
  , tsconfHsBootExtensions  :: !(Set Extension)
    -- | Whether to read and compute tags lazily or read them all at once when
    -- server starts.
  , tsconfEagerTagging      :: !Bool
  , tsconfNameResolution    :: !NameResolutionStrictness
  , tsconfSerialisedState   :: !(Maybe FilePath)
  } deriving (Eq, Ord, Show)

defaultTagsServerConf :: TagsServerConf
defaultTagsServerConf = TagsServerConf
  { tsconfVanillaExtensions = S.fromList [".hs", ".lhs", ".hsc", ".chs", ".x", ".y", ".lx", ".ly"]
  , tsconfHsBootExtensions  = S.fromList [".hs-boot", ".lhs-boot"]
  , tsconfEagerTagging      = False
  , tsconfNameResolution    = NameResolutionLax
  , tsconfSerialisedState   = Nothing
  }

data LoadState = LoadState
  { -- | Single module name can refer to multiple modules.
    lsLoadedModules   :: !(Map ImportKey (NonEmpty ResolvedModule))
    -- | Set of modules we started loading. Mainly used for detecting
    -- import cycles.
  , lsLoadsInProgress :: !(Map ImportKey (NonEmptyMap (FullPath 'File) UnresolvedModule))
  , lsUnloadedFiles   :: !(Map ImportKey (NonEmpty UnresolvedModule))
  }
  deriving (Eq, Ord, Show, Generic)
  deriving Pretty via PPGeneric LoadState

instance Store LoadState

emptyLoadState :: LoadState
emptyLoadState = LoadState mempty mempty mempty

-- | Server state that may change while a request is processed.
data TagsServerState = TagsServerState
  { tssLoadState       :: !LoadState
  , tssKnownFiles      :: !(Map (FullPath 'File) ImportKey)
    -- Namespace currently loaded
  , tssNamespace       :: !Namespace
  } deriving (Eq, Ord, Show, Generic)

instance Store TagsServerState

emptyTagsServerState :: TagsServerState
emptyTagsServerState = TagsServerState emptyLoadState mempty mempty
