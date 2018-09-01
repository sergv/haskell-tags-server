----------------------------------------------------------------------------
-- |
-- Module      :  Types
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
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

    -- * Types for representing Haskell modules
  , isModuleNameConstituentChar
  , Module(..)
  , UnresolvedModule
  , ResolvedModule
  , moduleNeedsReloading
  , ModuleHeader(..)
  , UnresolvedModuleHeader
  , ResolvedModuleHeader
  , resolveQualifier
  , ModuleExports(..)
  ) where

import Prelude hiding (mod)

import Control.Monad.Except.Ext
import Data.Char
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text.Prettyprint.Doc.Ext
import Data.Time.Clock (UTCTime(..))
import Data.Traversable (for)
import GHC.Generics (Generic)

import Control.Monad.Filesystem (MonadFS)
import qualified Control.Monad.Filesystem as MonadFS
import Control.Monad.Filesystem.FileSearch (SearchCfg(..), versionControlDirs)
import Data.CompiledRegex
import Data.ErrorMessage
import Data.KeyMap (KeyMap)
import Data.Map.NonEmpty (NonEmptyMap)
import Data.Path (FullPath, Extension)
import Data.Promise (Promise)
import Data.SubkeyMap (SubkeyMap)
import qualified Data.SubkeyMap as SubkeyMap
import Data.SymbolMap (SymbolMap)
import Data.Symbols
import Haskell.Language.Server.Tags.Types.Imports

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

isModuleNameConstituentChar :: Char -> Bool
isModuleNameConstituentChar '\'' = True
isModuleNameConstituentChar '_'  = True
isModuleNameConstituentChar c    = isAlphaNum c

data Module a = Module
  { modHeader           :: !(ModuleHeader a)
    -- | All symbols defined in this module. SymbolMap tracks all children-parent
    -- relationships.
  , modAllSymbols       :: !SymbolMap
    -- | File the module was loaded from.
  , modFile             :: !FullPath
    -- | Time as reported by getModificationTime.
  , modLastModified     :: !UTCTime
    -- | All names that this module brings into scope
  , modAllExportedNames :: !a
    -- | Whether some imports of this module were updated and thus revolved
    -- module exports are no longer valid.
  , modIsDirty          :: !Bool
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type UnresolvedModule = Module ()
type ResolvedModule   = Module SymbolMap

-- TODO: if module is dirty it is only needed to recompute its @modAllExportedName@
-- field. There's no need
moduleNeedsReloading :: MonadFS m => Module a -> m (Bool, UTCTime)
moduleNeedsReloading m = do
  modifTime <- MonadFS.getModificationTime $ modFile m
  pure (modLastModified m /= modifTime || modIsDirty m, modifTime)

instance Pretty a => Pretty (Module a) where
  pretty mod =
    ppDictHeader "Module"
      [ "Name"          --> mhModName $ modHeader mod
      , "File"          --> modFile mod
      , "Last modified" :-> ppShow $ modLastModified mod
      , "Header"        --> modHeader mod
      , "AllSymbols"    --> modAllSymbols mod
      ]

-- | Result of analysing module's export list and any subsequent imports. E.g.
--
-- > module Foo (...) where
-- > import Bar
-- > import qualified Baz hiding (frob)
data ModuleHeader a = ModuleHeader
  { mhModName          :: !ModuleName
    -- | Exports of a module. Nothing - everything is exported
  , mhExports          :: !(Maybe ModuleExports)
    -- | Mapping from qualifiers to original module names. Single qualifier
    -- may be used for several modules.
  , mhImportQualifiers :: !(Map ImportQualifier (NonEmpty ModuleName))
    -- | All imports of a given module, including qualified ones.
    -- NB same module name may be present several times with different qualifications
    -- because it may be imported several times.
  , mhImports          :: !(SubkeyMap ImportKey (NonEmpty (ImportSpec a)))
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type UnresolvedModuleHeader = ModuleHeader ()
type ResolvedModuleHeader   = ModuleHeader SymbolMap

instance Pretty a => Pretty (ModuleHeader a) where
  pretty ModuleHeader{mhModName, mhExports, mhImportQualifiers, mhImports} =
    ppDictHeader "Module" $
      [ "Name"             :-> pretty mhModName
      ] ++
      [ "Exports"          :-> pretty exports
      | Just exports <- [mhExports]
      ] ++
      [ "ImportQualifiers" :-> ppMapWith pretty ppNE $ mhImportQualifiers
      | not $ M.null mhImportQualifiers
      ] ++
      [ "Imports"          :-> ppSubkeyMapWith pretty pretty ppNE $ mhImports
      | not $ SubkeyMap.null mhImports
      ]

-- | Find out which modules a given @ImportQualifier@ refers to.
resolveQualifier
  :: (HasCallStack, MonadError ErrorMessage m)
  => ImportQualifier
  -> ModuleHeader a
  -> m (Maybe (NonEmpty (ImportSpec a)))
resolveQualifier qual ModuleHeader{mhImports, mhImportQualifiers} =
  case SubkeyMap.lookupSubkey qualifiedModName mhImports of
    []     ->
      case M.lookup qual mhImportQualifiers of
        Nothing    -> pure Nothing
        Just names ->
          fmap (Just . foldr1 (<>)) $ for names $ \modName ->
            case SubkeyMap.lookupSubkey modName mhImports of
              []    ->
                -- If module's not found then this is a violation of internal
                -- invariant of ModuleHeader module header datastructure.
                throwErrorWithCallStack $
                  "Internal error: module" <+> pretty modName <+>
                  "for qualifier" <+> pretty qual <+> "not found in the imports map"
              s:ss -> pure $ sconcat $ s :| ss
    s : ss -> pure $ Just $ sconcat $ s :| ss
  where
    qualifiedModName = getImportQualifier qual

data ModuleExports = ModuleExports
  { -- | Toplevel names exported from this particular module as specified in
    -- the header.
    meExportedEntries    :: KeyMap (EntryWithChildren SymbolName)
    -- | Module name here refer to real modules only.
  , meReexports          :: Set ModuleName
    -- | Whether this module exports some entities that export all children.
  , meHasWildcardExports :: Bool
  } deriving (Eq, Ord, Show, Generic)

instance Pretty ModuleExports where
  pretty = ppGeneric

instance Semigroup ModuleExports where
  (<>) (ModuleExports x y z) (ModuleExports x' y' z') =
    ModuleExports (x <> x') (y <> y') (z || z')

instance Monoid ModuleExports where
  mempty = ModuleExports mempty mempty False
  mappend = (<>)
