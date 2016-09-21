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
  , splitQualifiedPart
  , isModuleNameConstituentChar
  , ResolvedSymbol
  , mkSymbol
  , resolvedSymbolName
  , resolvedSymbolType
  , resolvedSymbolParent
  , resolvedSymbolPosition
  , mkSymbolName
  , Module(..)
  , ModuleHeader(..)
  , ImportSpec(..)
  , importBringsUnqualifiedNames
  , importBringQualifiedNames
  , ImpotQualification(..)
  , getQualifier
  , ImportList(..)
  , ModuleExports(..)
  , ExportSpec(..)
  , EntryWithChildren(..)
  , ChildrenExportSpec(..)
  , isChildExported
  ) where

import Control.Monad
import Control.Monad.Base
import Control.Monad.Except
import Data.Char
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time.Clock (UTCTime(..))
import System.Directory
import System.FilePath.Find (FileType(Directory), find, fileType, (==?), always)
import qualified Text.PrettyPrint.Leijen.Text as PP
import Text.PrettyPrint.Leijen.Text.Utils

import FastTags (Pos(..), TagVal(..), Type(..), SrcPos(..), Line(..))

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

-- | Name the @ResolvedSymbol@ refers to.
newtype SymbolName = SymbolName { getSymbolName :: Text }
  deriving (Show, Eq, Ord)

instance Pretty SymbolName where
  pretty = PP.text . TL.fromStrict . getSymbolName

mkSymbolName :: Text -> SymbolName
mkSymbolName = SymbolName

-- | Split qualified symbol name (e.g. Foo.Bar.baz) into
-- qualified module part (Foo.Bar) and name part (baz). Return Nothing
splitQualifiedPart :: (MonadError Doc m) => SymbolName -> m (Maybe ImportQualifier, SymbolName)
splitQualifiedPart sym =
  -- . Atto.parseOnly ((,) <$> optional (pImportQualifier <* dot) <*> pRest)
  case reverse $ T.split (=='.') $ getSymbolName sym of
    []             -> throwError "No components after split"
    [sym']         -> pure (Nothing, mkSymbolName sym')
    sym' : modPart -> pure (Just qual, mkSymbolName sym')
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

resolvedSymbolName :: ResolvedSymbol -> SymbolName
resolvedSymbolName (ResolvedSymbol (Pos _ (TagVal name _ _))) = SymbolName name

resolvedSymbolType :: ResolvedSymbol -> Type
resolvedSymbolType (ResolvedSymbol (Pos _ (TagVal _ typ _))) = typ

resolvedSymbolParent :: ResolvedSymbol -> Maybe SymbolName
resolvedSymbolParent (ResolvedSymbol (Pos _ (TagVal _ _ parent))) = SymbolName <$> parent

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
  , modParentMap        :: Map SymbolName SymbolName
    -- | All symbols defined in this module. Keys are unqualified names.
  , modAllSymbols       :: Map SymbolName ResolvedSymbol
    -- | Module source code.
  , modSource           :: Text
    -- | File the module was loaded from.
  , modFile             :: FilePath
    -- | Time as reported by getModificationTime.
  , modLastModified     :: UTCTime
  } deriving (Show, Eq, Ord)

instance Pretty Module where
  pretty mod =
    ppDict "Module"
      [ "Name"          :-> pretty (mhModName (modHeader mod))
      , "File"          :-> pretty (modFile mod)
      , "Last modified" :-> showDoc (modLastModified mod)
      , "Names"         :-> ppMap (modAllSymbols mod)
      ]

data ModuleHeader = ModuleHeader
  { mhModName          :: ModuleName
    -- | All imports of a given module, including qualified ones.
    -- NB same module name may be present several times with different qualifications
    -- because it may be imported several times.
  , mhImports          :: [ImportSpec]
    -- | Mapping from qualifiers to original module names. Single qualifier
    -- may be used for several modules.
  , mhImportQualifiers :: Map ImportQualifier (NonEmpty ModuleName)
    -- | Exports of a module.
  , mhExports          :: ModuleExports
  } deriving (Show, Eq, Ord)

-- | Information about import statement
data ImportSpec = ImportSpec
  { -- | Name of imported module
    ispecModuleName    :: ModuleName
  , ispecQualification :: ImpotQualification
  , ispecImportList    :: Maybe ImportList
  } deriving (Show, Eq, Ord)

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

data ImpotQualification =
    -- | Qualified import, e.g.
    --
    -- import qualified X as Y
    --
    -- The ModuleName field would store "Y" in this case.
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

getQualifier :: ImpotQualification -> Maybe ImportQualifier
getQualifier (Qualified q)                   = Just q
getQualifier Unqualified                     = Nothing
getQualifier (BothQualifiedAndUnqualified q) = Just q

-- | User-provided import/hiding list.
data ImportList =
    -- | Explicit import list of an import statement, e.g.
    --
    -- import Foo (x, y, z(Baz))
    -- import Bar ()
    Imported [EntryWithChildren]
    -- | Hiding import list
    --
    -- import Foo hiding (x, y(Bar), z)
    -- import Foo hiding ()
  | Hidden [EntryWithChildren]
  deriving (Show, Eq, Ord)

data ModuleExports = ModuleExports
  { -- Toplevel names exported from this particular module.
    meExportedEntries :: KeyMap EntryWithChildren
  , meReexports       :: [ModuleName]
  -- , meAllExportedNames :: Set SymbolName
  } deriving (Show, Eq, Ord)

instance Monoid ModuleExports where
  mempty = ModuleExports mempty mempty
  mappend (ModuleExports x y) (ModuleExports x' y') =
    ModuleExports (x <> x') (y <> y')

data ExportSpec =
    ExportSingleEntry EntryWithChildren
  | ExportModule ModuleName
  deriving (Show, Eq, Ord)

data EntryWithChildren = EntryWithChildren SymbolName (Maybe ChildrenExportSpec)
  deriving (Show, Eq, Ord)

instance HasKey EntryWithChildren where
  type Key EntryWithChildren = SymbolName
  getKey (EntryWithChildren name _) = name

data ChildrenExportSpec =
    -- | Wildcard import/export, e.g. Foo(..)
    ExportAllChidlren
    -- | Import/export with explicit list of children, e.g. Foo(Bar, Baz), Quux(foo, bar)
  | ExportSpecificChildren (Set SymbolName)
  deriving (Show, Eq, Ord)

isChildExported :: SymbolName -> ChildrenExportSpec -> Bool
isChildExported _    ExportAllChidlren                 = True
isChildExported name (ExportSpecificChildren exported) = S.member name exported
