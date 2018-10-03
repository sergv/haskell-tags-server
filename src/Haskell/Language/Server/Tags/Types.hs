----------------------------------------------------------------------------
-- |
-- Module      :  Types
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Haskell.Language.Server.Tags.Types
  ( -- * API types
    SomeRequest(..)
  , Request(..)
  , UserRequest(..)
  , QueryRequest(..)
  , DirRequest(..)
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
  ) where

import Data.Kind
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Ext
import GHC.Generics (Generic)

import Data.CompiledRegex
import Data.ErrorMessage
import Data.Map.NonEmpty (NonEmptyMap)
import Data.Path (FileType(..), FullPath, Extension)
import Data.Promise (Promise)
import Data.SubkeyMap (SubkeyMap)
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
  QueryReq :: !(FullPath 'File) -> !QueryRequest -> UserRequest QueryResponse
  DirReq   :: !DirRequest       -> UserRequest ()

deriving instance Eq   (UserRequest resp)
deriving instance Ord  (UserRequest resp)
deriving instance Show (UserRequest resp)

instance Pretty (UserRequest resp) where
  pretty = \case
    QueryReq file req -> ppDictHeader "QueryReq"
      [ "file" --> file
      , "req"  --> req
      ]
    DirReq req -> ppDictHeader "DirReq"
      [ "req" --> req
      ]

-- | Query server for some information.
data DirRequest =
    AddShallowRecursiveIgnored
      !(Set (FullPath 'Dir)) -- ^ Set of extra watched shallow directories. Subdirectories will not be watched.
      !(Set (FullPath 'Dir)) -- ^ Set extra of watched recursive directories. -- The directory and all its subdirectories will be watched.
      !(Set Text)
  deriving (Eq, Ord, Show, Generic)

instance Pretty DirRequest where
  pretty = ppGeneric

data NameResolutionScope =
    ScopeCurrentModule
  | ScopeAllModules
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance Pretty NameResolutionScope where
  pretty = ppGeneric

-- | Query server for some information.
data QueryRequest =
    -- | Request to find vanilla name in current module and all its imports.
    FindSymbol !SymbolName
    -- | Request to find all names that match a gived regexp starting from
    -- current module.
  | FindSymbolByRegexp !NameResolutionScope !CompiledRegex
  deriving (Eq, Ord, Show, Generic)

instance Pretty QueryRequest where
  pretty = ppGeneric

data FSNotifyEvent =
    FSAdded    !(FullPath 'File)
  | FSRemoved  !(FullPath 'File)
  | FSModified !(FullPath 'File)
  deriving (Eq, Ord, Show, Generic)

instance Pretty FSNotifyEvent where
  pretty = ppGeneric

data QueryResponse =
    Found !(NonEmpty ResolvedSymbol)
  | NotFound !SymbolName
  deriving (Eq, Ord, Show, Generic)

instance Pretty QueryResponse where
  pretty = ppGeneric

type RequestHandler = forall resp. UserRequest resp -> IO (Promise (Either ErrorMessage resp))

-- | Whether to ignore some issues when resolving names.
data NameResolutionStrictness =
    -- | Default: use for day-to-day lookups, ignores some errors but
    -- this allows to analyse more modules e.g. the ones that define
    -- names with preprocessor/Tempate Haskell.
    NameResolutionLax
  | -- | Do ignore following events:
    -- 1. A module exports a name with children but no definition of children can be found.
    -- 2. Imported module is missing during eager tagging - default to no names.
    -- 3. A module file is not found - default to no names.
    NameResolutionStrict
  deriving (Eq, Ord, Show, Bounded, Enum, Generic)

instance Pretty NameResolutionStrictness where
  pretty = ppGeneric

data TagsServerConf = TagsServerConf
  { tsconfVanillaExtensions :: !(Set Extension)
  , tsconfHsBootExtensions  :: !(Set Extension)
    -- | Whether to read and compute tags lazily or read them all at once when
    -- server starts.
  , tsconfEagerTagging      :: !Bool
  , tsconfNameResolution    :: !NameResolutionStrictness
  } deriving (Eq, Ord, Show)

defaultTagsServerConf :: TagsServerConf
defaultTagsServerConf = TagsServerConf
  { tsconfVanillaExtensions = S.fromList [".hs", ".lhs", ".hsc", ".chs"]
  , tsconfHsBootExtensions  = S.fromList [".hs-boot", ".lhs-boot"]
  , tsconfEagerTagging      = False
  , tsconfNameResolution    = NameResolutionLax
  }

-- | Server state that may change while a request is processed.
data TagsServerState = TagsServerState
  { -- | Single module name can refer to multiple modules.
    tssLoadedModules   :: !(SubkeyMap ImportKey (NonEmpty ResolvedModule))
    -- | Set of modules we started loading. Mainly used for detecting
    -- import cycles.
  , tssLoadsInProgress :: !(Map ImportKey (NonEmptyMap (FullPath 'File) UnresolvedModule))
  , tssUnloadedFiles   :: !(Map ImportKey (NonEmpty UnresolvedModule))
  } deriving (Eq, Ord, Show)

emptyTagsServerState :: TagsServerState
emptyTagsServerState = TagsServerState mempty mempty mempty
