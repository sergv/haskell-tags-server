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
    Namespace(..)
  , isPathWithinNamespace
  , NameResolutionScope(..)
    -- * Tag server types
  , NameResolutionStrictness(..)
  , TagsServerConf(..)
  , defaultTagsServerConf
  , LoadState(..)
  , emptyLoadState
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generically(..))
import Prettyprinter.Ext

import Data.Map.NonEmpty (NonEmptyMap)
import Data.Path
import Haskell.Language.Server.Tags.Types.Imports
import Haskell.Language.Server.Tags.Types.Modules

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
  {
    -- | Whether to read and compute tags lazily or read them all at once when
    -- server starts.
    tsconfEagerTagging      :: !Bool
  , tsconfNameResolution    :: !NameResolutionStrictness
  , tsconfSerialisedState   :: !(Maybe FilePath)
  } deriving (Eq, Ord, Show)

defaultTagsServerConf :: TagsServerConf
defaultTagsServerConf = TagsServerConf
  { tsconfEagerTagging      = False
  , tsconfNameResolution    = NameResolutionLax
  , tsconfSerialisedState   = Nothing
  }

data LoadState = LoadState
  { -- | Single module name can refer to multiple modules.
    lsLoadedModules   :: !(Map ImportKey (NonEmpty ResolvedModule))
    -- | Set of modules we started loading. Mainly used for detecting
    -- import cycles.
  , lsLoadsInProgress :: !(Map ImportKey (NonEmptyMap (FullPath 'File) UnresolvedModule))
    -- | Modules that have not been loaded yet.
  , lsUnloadedFiles   :: !(Map ImportKey (NonEmpty UnresolvedModule))
  }
  deriving (Eq, Ord, Show, Generic)
  deriving Pretty via PPGeneric LoadState

emptyLoadState :: LoadState
emptyLoadState = LoadState mempty mempty mempty
