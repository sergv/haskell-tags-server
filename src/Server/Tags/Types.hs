----------------------------------------------------------------------------
-- |
-- Module      :  Types
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Server.Tags.Types
  ( -- * API types
    Request(..)
  , Response(..)
  , RequestHandler
    -- * Tag server types
  , TagsServerConf(..)
  , emptyTagsServerConf
  , canonicalizeConfPaths
  , addRecursiveRootsToConf
  , TagsServerState(..)
  , emptyTagsServerState

    -- * Types for representing Haskell modules
  , isModuleNameConstituentChar
  , Module(..)
  , moduleNeedsReloading
  , ModuleHeader(..)
  , resolveQualifier
  , ImportSpec(..)
  , importBringsUnqualifiedNames
  , importBringQualifiedNames
  , importBringsNamesQualifiedWith
  , ImportQualification(..)
  , hasQualifier
  , getQualifier
  , ImportList(..)
  , ModuleExports(..)
  , EntryWithChildren(..)
  , mkEntryWithoutChildren
  , ChildrenVisibility(..)
  , isChildExported
  ) where

import Control.Monad
import Control.Monad.Base
import Control.Monad.Except
import Data.Char
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Semigroup as Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time.Clock (UTCTime(..))
import Data.Traversable (for)
import System.Directory
import System.FilePath.Find (FileType(Directory), find, fileType, (==?), always)
import Text.PrettyPrint.Leijen.Text.Utils


import Control.Monad.Filesystem (MonadFS)
import qualified Control.Monad.Filesystem as MonadFS
import Data.CompiledRegex
import Data.KeyMap (KeyMap, HasKey(..))
import Data.Promise (Promise)
import Data.SymbolMap (SymbolMap)
import Data.Symbols

-- | Types of user requests that can be handled.
data Request =
    -- | Request to find vanilla name in module identified by file path.
    FindSymbol FilePath SymbolName
    -- | Request to find all names that match a gived regexp, starting with module
    -- identified by file path.
  | FindSymbolByRegexp FilePath CompiledRegex
  deriving (Show, Eq, Ord)

data Response =
    Found (NonEmpty ResolvedSymbol)
  | NotFound SymbolName
  deriving (Show, Eq, Ord)

type RequestHandler = Request -> IO (Promise (Either Doc Response))

data TagsServerConf = TagsServerConf
  { tsconfSourceDirectories :: Set FilePath -- ^ directories with haskell files to index
  , tsconfCabalDirectories  :: Set FilePath -- ^ directories with cabal packages to index
  , tsconfLazyTagging       :: Bool         -- ^ whether to read and compute tags lazily
                                            -- or read them all at once on server start
  } deriving (Show, Eq, Ord)

emptyTagsServerConf :: TagsServerConf
emptyTagsServerConf = TagsServerConf
  { tsconfSourceDirectories = mempty
  , tsconfCabalDirectories  = mempty
  , tsconfLazyTagging       = False
  }

forSet :: (Ord a, Ord b, Applicative f) => Set a -> (a -> f b) -> f (Set b)
forSet xs f = S.fromList <$> traverse f (S.toList xs)

canonicalizeConfPaths :: (MonadBase IO m) => TagsServerConf -> m TagsServerConf
canonicalizeConfPaths conf = liftBase $ do
  tsconfSourceDirectories' <- forSet (tsconfSourceDirectories conf) canonicalizePath
  tsconfCabalDirectories'  <- forSet (tsconfCabalDirectories conf) canonicalizePath
  pure conf
    { tsconfSourceDirectories = tsconfSourceDirectories'
    , tsconfCabalDirectories  = tsconfCabalDirectories'
    }

addRecursiveRootsToConf :: (MonadBase IO m) => Set FilePath -> TagsServerConf -> m TagsServerConf
addRecursiveRootsToConf dirTrees conf = liftBase $ do
  dirs <- forSet dirTrees $
            find always (fileType ==? Directory) <=< canonicalizePath
  pure conf
    { tsconfSourceDirectories =
        foldMap S.fromList dirs <> tsconfSourceDirectories conf
    }

-- | Server state that may change while processing request.
data TagsServerState = TagsServerState
  { -- | Single module name can refer to multiple modules.
    tssLoadedModules :: Map ModuleName (NonEmpty Module)
  } deriving (Show, Eq, Ord)

emptyTagsServerState :: TagsServerState
emptyTagsServerState = TagsServerState mempty

isModuleNameConstituentChar :: Char -> Bool
isModuleNameConstituentChar '\'' = True
isModuleNameConstituentChar '_'  = True
isModuleNameConstituentChar c    = isAlphaNum c

data Module = Module
  { modHeader           :: ModuleHeader
    -- -- | Map for tags that can influence other tags when exporting, e.g.
    -- -- keys are data types and values are their constructors and fields. Thus
    -- -- export list construction Foo(..) will export all constructors and fields
    -- -- for datatype Foo, and Foo will be a key in this map.
  -- , modChildrenMap      :: Map SymbolName (NonEmpty SymbolName)

    -- | All symbols defined in this module.
  , modAllSymbols       :: SymbolMap
    -- | File the module was loaded from.
  , modFile             :: FilePath
    -- | Time as reported by getModificationTime.
  , modLastModified     :: UTCTime
    -- | All names that this module brings into scope
  , meAllExportedNames  :: SymbolMap
    -- | Whether some imports of this module were updated and thus revolved
    -- module exports are no longer valid.
  , meIsDirty           :: Bool
  } deriving (Show, Eq, Ord)

moduleNeedsReloading :: (MonadFS m) => Module -> m (Bool, UTCTime)
moduleNeedsReloading m = do
  modifTime <- MonadFS.getModificationTime $ modFile m
  pure (modLastModified m /= modifTime || meIsDirty m, modifTime)

instance Pretty Module where
  pretty mod =
    ppDict "Module"
      [ "Name"          :-> pretty (mhModName (modHeader mod))
      , "File"          :-> pretty (modFile mod)
      , "Last modified" :-> showDoc (modLastModified mod)
      , "Header"        :-> pretty (modHeader mod)
      , "Names"         :-> pretty (modAllSymbols mod)
      ]

data ModuleHeader = ModuleHeader
  { mhModName          :: ModuleName
    -- | Exports of a module. Nothing - everything is exported
  , mhExports          :: Maybe ModuleExports
    -- | Mapping from qualifiers to original module names. Single qualifier
    -- may be used for several modules.
  , mhImportQualifiers :: Map ImportQualifier (NonEmpty ModuleName)
    -- | All imports of a given module, including qualified ones.
    -- NB same module name may be present several times with different qualifications
    -- because it may be imported several times.
  , mhImports          :: Map ModuleName (NonEmpty ImportSpec)
  } deriving (Show, Eq, Ord)

instance Pretty ModuleHeader where
  pretty header =
    ppDict "Module" $
      [ "Name"             :-> pretty (mhModName header)
      ] ++
      [ "Exports"          :-> pretty exports
      | Just exports <- [mhExports header]
      ] ++
      [ "ImportQualifiers" :-> ppMap (ppNE <$> mhImportQualifiers header)
      , "Imports"          :-> ppMap (ppNE <$> mhImports header)
      ]

-- | Find out which modules a given @ImportQualifier@ refers to.
resolveQualifier
  :: (MonadError Doc m)
  => ImportQualifier
  -> ModuleHeader
  -> m (Maybe (NonEmpty ImportSpec))
resolveQualifier qual ModuleHeader{mhImports, mhImportQualifiers} =
  case M.lookup qual mhImportQualifiers of
    Nothing    -> pure Nothing
    Just names ->
      fmap (Just . foldr1 (Semigroup.<>)) $ for names $ \modName ->
        case M.lookup modName mhImports of
          Nothing    ->
            -- If module's not found then this is a violation of internal
            -- invariant of ModuleHeader module header datastructure.
            throwError $
              "Internal error: module" <+> pretty modName <+>
              "for qualifier" <+> pretty qual <+> "not found in the imports map"
          Just specs -> return specs

-- | Information about import statement
data ImportSpec = ImportSpec
  { -- | Name of imported module
    ispecModuleName    :: ModuleName
  , ispecQualification :: ImportQualification
  , ispecImportList    :: Maybe ImportList
  } deriving (Show, Eq, Ord)

instance Pretty ImportSpec where
  pretty spec = ppDict "ImportSpec" $
    [ "ModuleName"    :-> pretty (ispecModuleName spec)
    , "Qualification" :-> pretty (ispecQualification spec)
    ] ++
    [ "ImportList"    :-> pretty importList
    | Just importList <- [ispecImportList spec]
    ]

importBringsUnqualifiedNames :: ImportSpec -> Bool
importBringsUnqualifiedNames ImportSpec{ispecQualification} =
  case ispecQualification of
    Qualified _                   -> False
    Unqualified                   -> True
    BothQualifiedAndUnqualified _ -> True

importBringQualifiedNames :: ImportSpec -> Bool
importBringQualifiedNames ImportSpec{ispecQualification} =
  case ispecQualification of
    Qualified _                   -> True
    Unqualified                   -> False
    BothQualifiedAndUnqualified _ -> True

importBringsNamesQualifiedWith :: ImportSpec -> ImportQualifier -> Bool
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
  deriving (Show, Eq, Ord)

instance Pretty ImportQualification where
  pretty = \case
    Qualified qual                   -> "Qualified" <+> pretty qual
    Unqualified                      -> "Unqualified"
    BothQualifiedAndUnqualified qual -> "BothQualifiedAndUnqualified" <+> pretty qual

hasQualifier :: ImportQualifier -> ImportQualification -> Bool
hasQualifier qual = maybe False (== qual) . getQualifier

getQualifier :: ImportQualification -> Maybe ImportQualifier
getQualifier (Qualified q)                   = Just q
getQualifier Unqualified                     = Nothing
getQualifier (BothQualifiedAndUnqualified q) = Just q

-- | User-provided import/hiding list.
data ImportList =
    -- | Explicit import list of an import statement, e.g.
    --
    -- import Foo (x, y, z(Baz))
    -- import Bar ()
    Imported (KeyMap (EntryWithChildren UnqualifiedSymbolName))
    -- | Hiding import list
    --
    -- import Foo hiding (x, y(Bar), z)
    -- import Foo hiding ()
  | Hidden (KeyMap (EntryWithChildren UnqualifiedSymbolName))
  deriving (Show, Eq, Ord)

instance Pretty ImportList where
  pretty importList =
    ppListWithHeader ("Import list[" <> descr <> "]") $ toList items
    where
      (descr, items) = case importList of
        Imported items -> ("Imported", items)
        Hidden   items -> ("Hidden",   items)

data ModuleExports = ModuleExports
  { -- | Toplevel names exported from this particular module as specified in
    -- the header.
    meExportedEntries    :: KeyMap (EntryWithChildren SymbolName)
    -- | Module name here can refer to either real module or to a qualifier.
  , meReexports          :: Set ModuleName
    -- | Whether this module exports some entities that export all children.
  , meHasWildcardExports :: Bool
  } deriving (Show, Eq, Ord)

instance Pretty ModuleExports where
  pretty exports =
    ppDict "ModuleExports"
      [ "ExportedEntries"    :-> ppList (toList $ meExportedEntries exports)
      , "Reexports"          :-> ppSet (meReexports exports)
      , "HasWildcardExports" :-> pretty (meHasWildcardExports exports)
      ]

instance Monoid ModuleExports where
  mempty = ModuleExports mempty mempty False
  mappend (ModuleExports x y z) (ModuleExports x' y' z') =
    ModuleExports (x <> x') (y <> y') (z || z')

data EntryWithChildren name = EntryWithChildren
  { entryName               :: name
  , entryChildrenVisibility :: Maybe ChildrenVisibility
  } deriving (Show, Eq, Ord)

instance (Pretty name) => Pretty (EntryWithChildren name) where
  pretty (EntryWithChildren name children) =
    ppDict "EntryWithChildren" $
      [ "Name"     :-> pretty name
      ] ++
      [ "Children" :-> pretty children'
      | Just children' <- [children]
      ]

mkEntryWithoutChildren :: a -> EntryWithChildren a
mkEntryWithoutChildren name = EntryWithChildren name Nothing

instance (Ord a) => HasKey (EntryWithChildren a) where
  type Key (EntryWithChildren a) = a
  getKey (EntryWithChildren name _) = name

data ChildrenVisibility =
    -- | Wildcard import/export, e.g. Foo(..)
    VisibleAllChildren
    -- | Import/export with explicit list of children, e.g. Foo(Bar, Baz), Quux(foo, bar).
    -- Set is always non-empty.
  | VisibleSpecificChildren (Set UnqualifiedSymbolName)
  deriving (Show, Eq, Ord)

instance Pretty ChildrenVisibility where
  pretty = \case
    VisibleAllChildren               -> "VisibleAllChildren"
    VisibleSpecificChildren children ->
      ppListWithHeader "VisibleSpecificChildren" $ toList children

isChildExported :: UnqualifiedSymbolName -> ChildrenVisibility -> Bool
isChildExported _    VisibleAllChildren                 = True
isChildExported name (VisibleSpecificChildren exported) = S.member name exported
