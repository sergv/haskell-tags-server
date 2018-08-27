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
{-# LANGUAGE LambdaCase        #-}
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
  , ImportKey(..)
  , ModuleHeader(..)
  , UnresolvedModuleHeader
  , ResolvedModuleHeader
  , resolveQualifier
  , ImportTarget(..)
  , ImportSpec(..)
  , UnresolvedImportSpec
  , ResolvedImportSpec
  , importBringsUnqualifiedNames
  , importBringQualifiedNames
  , importBringsNamesQualifiedWith
  , ImportQualification(..)
  , hasQualifier
  , getQualifier
  , ImportType(..)
  , ImportList(..)
  , ModuleExports(..)
  , EntryWithChildren(..)
  , mkEntryWithoutChildren
  , ChildrenVisibility(..)
  ) where

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
import Data.KeyMap (KeyMap, HasKey(..))
import Data.Map.NonEmpty (NonEmptyMap)
import Data.Path (FullPath, Extension)
import Data.Promise (Promise)
import Data.SubkeyMap (SubkeyMap, HasSubkey(..))
import qualified Data.SubkeyMap as SubkeyMap
import Data.SymbolMap (SymbolMap)
import Data.Symbols

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

-- | Handle for when particular module enters another module's scope.
data ImportKey = ImportKey
  { -- | Whether this import statement is annotated with {-# SOURCE #-} pragma.
    -- This means that it refers to .hs-boot file, rather than vanilla .hs file.
    ikImportTarget :: ImportTarget
    -- | Name of imported module
  , ikModuleName   :: ModuleName
  } deriving (Eq, Ord, Show, Generic)

instance HasSubkey ImportKey where
  type Subkey ImportKey = ModuleName
  getSubkey = ikModuleName

instance Pretty ImportKey where
  pretty ik =
    "ImportKey" <+> pretty (ikImportTarget ik) <+> pretty (ikModuleName ik)

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

data ImportTarget = VanillaModule | HsBootModule
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance Pretty ImportTarget where
  pretty = ppGeneric

-- | Information about import statement
data ImportSpec a = ImportSpec
  { ispecImportKey     :: ImportKey
  , ispecQualification :: ImportQualification
  , ispecImportList    :: Maybe ImportList
  , ispecImportedNames :: a
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type UnresolvedImportSpec = ImportSpec ()
type ResolvedImportSpec   = ImportSpec SymbolMap

instance Pretty a => Pretty (ImportSpec a) where
  pretty spec = ppDictHeader "ImportSpec" $
    [ "ModuleName"    --> ikModuleName $ ispecImportKey spec
    , "Qualification" --> ispecQualification spec
    ] ++
    [ "ImportList"    --> importList
    | Just importList <- [ispecImportList spec]
    ]

importBringsUnqualifiedNames :: ImportSpec a -> Bool
importBringsUnqualifiedNames ImportSpec{ispecQualification} =
  case ispecQualification of
    Qualified _                   -> False
    Unqualified                   -> True
    BothQualifiedAndUnqualified _ -> True

importBringQualifiedNames :: ImportSpec a -> Bool
importBringQualifiedNames ImportSpec{ispecQualification} =
  case ispecQualification of
    Qualified _                   -> True
    Unqualified                   -> False
    BothQualifiedAndUnqualified _ -> True

importBringsNamesQualifiedWith :: ImportSpec a -> ImportQualifier -> Bool
importBringsNamesQualifiedWith ImportSpec{ispecQualification} q =
  getQualifier ispecQualification == Just q

data ImportQualification =
    -- | Qualified import, e.g.
    --
    -- import qualified X as Y
    --
    -- The ModuleName field would store "Y" in this case.
    --
    -- import qualified X - field would store "X"
    Qualified ImportQualifier
    -- | Vanilla import, e.g.
    --
    -- import X
  | Unqualified
    -- | Vanilla import with explicit alias, e.g.
    --
    -- import X as Y
  | BothQualifiedAndUnqualified ImportQualifier
  deriving (Eq, Ord, Show, Generic)

instance Pretty ImportQualification where
  pretty = ppGeneric

hasQualifier :: ImportQualifier -> ImportQualification -> Bool
hasQualifier qual = maybe False (== qual) . getQualifier

getQualifier :: ImportQualification -> Maybe ImportQualifier
getQualifier (Qualified q)                   = Just q
getQualifier Unqualified                     = Nothing
getQualifier (BothQualifiedAndUnqualified q) = Just q

data ImportType =
    -- | Explicit import list of an import statement, e.g.
    --
    -- import Foo (x, y, z(Baz))
    -- import Bar ()
    Imported
  | -- | Hiding import list
    --
    -- import Foo hiding (x, y(Bar), z)
    -- import Foo hiding ()
    Hidden
  deriving (Eq, Ord, Show, Generic)

instance Pretty ImportType where
  pretty = ppGeneric

-- | User-provided import/hiding list.
data ImportList = ImportList
  { ilEntries    :: KeyMap (EntryWithChildren UnqualifiedSymbolName)
  , ilImportType :: ImportType
  } deriving (Eq, Ord, Show)

instance Pretty ImportList where
  pretty ImportList{ilImportType, ilEntries} =
    ppFoldableHeader ("Import list[" <> pretty ilImportType <> "]") ilEntries

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

data EntryWithChildren name = EntryWithChildren
  { entryName               :: name
  , entryChildrenVisibility :: Maybe ChildrenVisibility
  } deriving (Eq, Ord, Show, Generic)

instance Pretty name => Pretty (EntryWithChildren name) where
  pretty = ppGeneric

mkEntryWithoutChildren :: a -> EntryWithChildren a
mkEntryWithoutChildren name = EntryWithChildren name Nothing

instance Ord a => HasKey (EntryWithChildren a) where
  type Key (EntryWithChildren a) = a
  getKey (EntryWithChildren name _) = name

data ChildrenVisibility =
    -- | Wildcard import/export, e.g. Foo(..)
    VisibleAllChildren
    -- | Import/export with explicit list of children, e.g. Foo(Bar, Baz), Quux(foo, bar).
    -- Set is always non-empty.
  | VisibleSpecificChildren (Set UnqualifiedSymbolName)
    -- | Wildcard export with some things added in, so they'll be visible on
    -- wildcard import, e.g.
    -- ErrorCall(..,ErrorCall)
  | VisibleAllChildrenPlusSome (Set UnqualifiedSymbolName)
  deriving (Eq, Ord, Show, Generic)

instance Pretty ChildrenVisibility where
  pretty = ppGeneric
