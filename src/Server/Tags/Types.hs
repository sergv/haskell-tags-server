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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , ModuleName
  , getModuleName
  , mkModuleName
  , ImportQualifier
  , mkImportQualifier
  , getImportQualifier
  , SymbolName
  , getSymbolName
  , mkSymbolName
  , isModuleNameConstituentChar
  , UnqualifiedSymbolName
  , getUnqualifiedSymbolName
  , mkUnqualifiedSymbolName
  , splitQualifiedPart
  , ResolvedSymbol
  , mkSymbol
  , resolvedSymbolName
  , resolvedSymbolType
  , resolvedSymbolParent
  , resolvedSymbolPosition
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
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time.Clock (UTCTime(..))
import Data.Traversable (for)
import System.Directory
import System.FilePath.Find (FileType(Directory), find, fileType, (==?), always)
import qualified Text.PrettyPrint.Leijen.Text as PP
import Text.PrettyPrint.Leijen.Text.Utils

import FastTags (Pos(..), TagVal(..), Type(..), SrcPos(..), Line(..))

import Control.Monad.Filesystem (MonadFS)
import qualified Control.Monad.Filesystem as MonadFS
import Data.CompiledRegex
import Data.KeyMap (KeyMap, HasKey(..))
import Data.Promise (Promise)

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


-- e.g. Foo, Foo.Bar
newtype ModuleName = ModuleName { getModuleName :: Text }
  deriving (Show, Eq, Ord)

instance Pretty ModuleName where
  pretty = PP.text . TL.fromStrict . getModuleName

mkModuleName :: Text -> ModuleName
mkModuleName = ModuleName

-- | Custom module name used for qualification. This is the XXX part of the
-- import statement:
--
-- import Foo.Bar as XXX
-- import qualified Fizz.Buzz as XXX
newtype ImportQualifier = ImportQualifier { getImportQualifier :: ModuleName }
  deriving (Show, Eq, Ord, Pretty)

mkImportQualifier :: ModuleName -> ImportQualifier
mkImportQualifier = ImportQualifier

-- | Name the @ResolvedSymbol@ refers to. Can be either qualified or unqualified.
newtype SymbolName = SymbolName { getSymbolName :: Text }
  deriving (Show, Eq, Ord)

instance Pretty SymbolName where
  pretty = PP.text . TL.fromStrict . getSymbolName

mkSymbolName :: Text -> SymbolName
mkSymbolName = SymbolName

-- | Name the @ResolvedSymbol@ refers to.
newtype UnqualifiedSymbolName = UnqualifiedSymbolName { getUnqualifiedSymbolName :: SymbolName }
  deriving (Show, Eq, Ord)

instance Pretty UnqualifiedSymbolName where
  pretty = pretty . getUnqualifiedSymbolName

mkUnqualifiedSymbolName :: SymbolName -> Maybe UnqualifiedSymbolName
mkUnqualifiedSymbolName name
  | isQualified name = Nothing
  | otherwise        = Just $ UnqualifiedSymbolName name
  where
    isQualified :: SymbolName -> Bool
    isQualified = T.any (== '.') . getSymbolName

-- | Split qualified symbol name (e.g. Foo.Bar.baz) into
-- qualified module part (Foo.Bar) and name part (baz). Return Nothing
splitQualifiedPart
  :: SymbolName
  -> (Maybe ImportQualifier, UnqualifiedSymbolName)
splitQualifiedPart sym =
  case reverse $ T.split (=='.') $ getSymbolName sym of
    []             -> (Nothing, UnqualifiedSymbolName sym)
    [sym']         -> (Nothing, UnqualifiedSymbolName $ SymbolName sym')
    sym' : modPart -> (Just qual, UnqualifiedSymbolName $ SymbolName sym')
      where
        qual = mkImportQualifier $ mkModuleName $ T.intercalate "." $ reverse modPart

isModuleNameConstituentChar :: Char -> Bool
isModuleNameConstituentChar '\'' = True
isModuleNameConstituentChar '_'  = True
isModuleNameConstituentChar c    = isAlphaNum c

-- | A symbolic name that identifier some Haskell entity. Has position,
-- entity type and possibly a parent.
newtype ResolvedSymbol = ResolvedSymbol (Pos TagVal)
  deriving (Show, Eq, Ord)

instance Pretty ResolvedSymbol where
  pretty sym@(ResolvedSymbol _) =
    ppDict "ResolvedSymbol"
      [ "name"     :-> pretty (resolvedSymbolName sym)
      , "type"     :-> ppType (resolvedSymbolType sym)
      , "parent"   :-> pretty (resolvedSymbolParent sym)
      , "position" :-> ppSrcPos (resolvedSymbolPosition sym)
      ]
    where
      ppType :: Type -> Doc
      ppType = showDoc

      ppSrcPos :: SrcPos -> Doc
      ppSrcPos (SrcPos file line _) = docFromString file <> ":" <> pretty (unLine line)

mkSymbol :: Pos TagVal -> ResolvedSymbol
mkSymbol = ResolvedSymbol

resolvedSymbolName :: ResolvedSymbol -> UnqualifiedSymbolName
resolvedSymbolName (ResolvedSymbol (Pos _ (TagVal name _ _))) =
  UnqualifiedSymbolName $ SymbolName name

resolvedSymbolType :: ResolvedSymbol -> Type
resolvedSymbolType (ResolvedSymbol (Pos _ (TagVal _ typ _))) = typ

resolvedSymbolParent :: ResolvedSymbol -> Maybe UnqualifiedSymbolName
resolvedSymbolParent (ResolvedSymbol (Pos _ (TagVal _ _ parent))) =
  UnqualifiedSymbolName . SymbolName <$> parent

resolvedSymbolPosition :: ResolvedSymbol -> SrcPos
resolvedSymbolPosition (ResolvedSymbol (Pos pos _)) = pos

data Module = Module
  { modHeader           :: ModuleHeader
    -- -- | Map for tags that can influence other tags when exporting, e.g.
    -- -- keys are data types and values are their constructors and fields. Thus
    -- -- export list construction Foo(..) will export all constructors and fields
    -- -- for datatype Foo, and Foo will be a key in this map.
  -- , modChildrenMap      :: Map SymbolName (NonEmpty SymbolName)

    -- | Map from children entities to parents containing them. E.g.
    -- constructors are mapped to their corresponding datatypes, typeclass
    -- members - to typeclass names, etc.
  , modParentMap        :: Map UnqualifiedSymbolName UnqualifiedSymbolName
    -- | All symbols defined in this module. Keys are unqualified names.
  , modAllSymbols       :: Map UnqualifiedSymbolName ResolvedSymbol
    -- | File the module was loaded from.
  , modFile             :: FilePath
    -- | Time as reported by getModificationTime.
  , modLastModified     :: UTCTime
  } deriving (Show, Eq, Ord)

moduleNeedsReloading :: (MonadFS m) => Module -> m (Bool, UTCTime)
moduleNeedsReloading m = do
  modifTime <- MonadFS.getModificationTime $ modFile m
  pure $ (modLastModified m /= modifTime, modifTime)

instance Pretty Module where
  pretty mod =
    ppDict "Module"
      [ "Name"          :-> pretty (mhModName (modHeader mod))
      , "File"          :-> pretty (modFile mod)
      , "Last modified" :-> showDoc (modLastModified mod)
      , "Header"        :-> pretty (modHeader mod)
      , "Names"         :-> ppMap (modAllSymbols mod)
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
  { -- | Toplevel names exported from this particular module as specifie in
    -- the header.
    meExportedEntries    :: Map UnqualifiedSymbolName (EntryWithChildren SymbolName)
  , meReexports          :: [ModuleName]
    -- | Whether this module exports some entities that export all children.
  , meHasWildcardExports :: Bool
  -- , meAllExportedNames :: Set SymbolName
  } deriving (Show, Eq, Ord)

instance Pretty ModuleExports where
  pretty exports =
    ppDict "ModuleExports"
      [ "ExportedEntries"    :-> pretty (ppList "[" "]" $ toList $ meExportedEntries exports)
      , "Reexports"          :-> pretty (meReexports exports)
      , "HasWildcardExports" :-> pretty (meHasWildcardExports exports)
      ]

instance Monoid ModuleExports where
  mempty = ModuleExports mempty mempty False
  mappend (ModuleExports x y z) (ModuleExports x' y' z') =
    ModuleExports (x <> x') (y <> y') (z || z')

data EntryWithChildren name = EntryWithChildren name (Maybe ChildrenVisibility)
  deriving (Show, Eq, Ord)

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
    ExportAllChildren
    -- | Import/export with explicit list of children, e.g. Foo(Bar, Baz), Quux(foo, bar).
    -- Set is always non-empty.
  | ExportSpecificChildren (Set UnqualifiedSymbolName)
  deriving (Show, Eq, Ord)

instance Pretty ChildrenVisibility where
  pretty = \case
    ExportAllChildren               -> "ExportAllChildren"
    ExportSpecificChildren children ->
      ppListWithHeader "ExportSpecificChildren" $ toList children

isChildExported :: UnqualifiedSymbolName -> ChildrenVisibility -> Bool
isChildExported _    ExportAllChildren                 = True
isChildExported name (ExportSpecificChildren exported) = S.member name exported
