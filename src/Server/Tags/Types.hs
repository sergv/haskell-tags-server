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
  , getImportQualifier
  , SymbolName
  , getSymbolName
  , splitQualifiedPart
  , isModuleNameConstituentChar
  , Symbol
  , mkSymbol
  , symbolName
  , symbolType
  , symbolParent
  , symbolPosition
  , mkSymbolName
  , Module(..)
  , ModuleImport(..)
  , importBringsUnqualifiedNames
  , importBringQualifiedNames
  , Qualification(..)
  , getQualifier
  , ImportList(..)
  , ImportedName(..)
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Except
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Atto
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
import Text.PrettyPrint.Leijen.Text (Pretty(..), Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP
import Text.PrettyPrint.Leijen.Text.Utils

import FastTags (Pos(..), TagVal(..), Type(..), SrcPos)

import Data.CompiledRegex
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
    Found (NonEmpty Symbol)
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
  { tssLoadedModules :: Map ModuleName [Module]
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

-- | Name the @Symbol@ refers to.
newtype SymbolName = SymbolName { getSymbolName :: Text }
  deriving (Show, Eq, Ord)

instance Pretty SymbolName where
  pretty = PP.text . TL.fromStrict . getSymbolName

mkSymbolName :: Text -> SymbolName
mkSymbolName = SymbolName

-- | Split qualified symbol name (e.g. Foo.Bar.baz) into
-- qualified module part (Foo.Bar) and name part (baz). Return Nothing
splitQualifiedPart :: (MonadError Doc m) => SymbolName -> m (Maybe ImportQualifier, SymbolName)
splitQualifiedPart
  = either (throwError . docFromString) return
  . Atto.parseOnly ((,) <$> optional (pImportQualifier <* dot) <*> pRest)
  . getSymbolName
  where
    dot :: Parser Char
    dot = Atto.char '.'
    pRest :: Parser SymbolName
    pRest = mkSymbolName <$> Atto.takeText
    pImportQualifier :: Parser ImportQualifier
    pImportQualifier = ImportQualifier . mkModuleName . T.intercalate "." <$> (pModuleName `Atto.sepBy1` dot)
    pModuleName :: Parser Text
    pModuleName = T.cons
      <$> Atto.satisfy isUpper
      <*> Atto.takeWhile isModuleNameConstituentChar

isModuleNameConstituentChar :: Char -> Bool
isModuleNameConstituentChar '\'' = True
isModuleNameConstituentChar '_'  = True
isModuleNameConstituentChar c    = isAlphaNum c

-- | A symbolic name that identifier some Haskell entity. Has position,
-- entity type and possibly a parent.
newtype Symbol = Symbol (Pos TagVal)
  deriving (Show, Eq, Ord)

mkSymbol :: Pos TagVal -> Symbol
mkSymbol = Symbol

symbolName :: Symbol -> SymbolName
symbolName (Symbol (Pos _ (TagVal name _ _))) = SymbolName name

symbolType :: Symbol -> Type
symbolType (Symbol (Pos _ (TagVal _ typ _))) = typ

symbolParent :: Symbol -> Maybe SymbolName
symbolParent (Symbol (Pos _ (TagVal _ _ parent))) = SymbolName <$> parent

symbolPosition :: Symbol -> SrcPos
symbolPosition (Symbol (Pos pos _)) = pos

data Module = Module
  { -- | All imports of a given module, including qualified ones.
    -- NB same module name may be present several times with different qualifications
    -- because it may be imported several times.
    modImports          :: [ModuleImport]
    -- | Mapping from qualifiers to original module names. Single qualifier
    -- may be used for several modules.
  , modImportQualifiers :: Map ImportQualifier (NonEmpty ModuleName)
    -- | Exports of a module.
  , modExports          :: Map SymbolName Symbol
    -- | Map for tags that can influence other tags when exporting, e.g.
    -- keys are data types and values are their constructors and fields. Thus
    -- export list construction Foo(..) will export all constructors and fields
    -- for datatype Foo, which is a key in this map.
  , modChildrenMap      :: Map SymbolName [SymbolName]
    -- | All symbols defined in this module.
  , modAllSymbols       :: Map SymbolName Symbol
    -- | Module source code.
  , modSource           :: Text
    -- | File the module was loaded from.
  , modFile             :: FilePath
    -- | Time as reported by getModificationTime.
  , modLastModified     :: UTCTime
  } deriving (Show, Eq, Ord)

-- | Information about import statement
data ModuleImport = ModuleImport
  { -- | Name of imported module
    miImportedName  :: ModuleName
  , miQualification :: Qualification
  , miImportList    :: Maybe ImportList
  } deriving (Show, Eq, Ord)

importBringsUnqualifiedNames :: ModuleImport -> Bool
importBringsUnqualifiedNames ModuleImport{miQualification} =
  case miQualification of
    Qualified _                   -> False
    Unqualified                   -> True
    BothQualifiedAndUnqualified _ -> True

importBringQualifiedNames :: ModuleImport -> Bool
importBringQualifiedNames ModuleImport{miQualification} =
  case miQualification of
    Qualified _                   -> True
    Unqualified                   -> False
    BothQualifiedAndUnqualified _ -> True

data Qualification =
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

getQualifier :: Qualification -> Maybe ImportQualifier
getQualifier (Qualified q)                   = Just q
getQualifier Unqualified                     = Nothing
getQualifier (BothQualifiedAndUnqualified q) = Just q

-- | User-provided import/hiding list.
data ImportList =
    -- | Explicit import list of an import statement, e.g.
    --
    -- import Foo (x, y, z)
    -- import Bar ()
    Imported [ImportedName]
    -- | Hiding import list
    --
    -- import Foo hiding (x, y, z)
  | Hidden [ImportedName]
  deriving (Show, Eq, Ord)

-- | Single entity from import/hiding list.
data ImportedName =
    -- | Vanilla import entity, e.g. x, Foo
    ImportSingleEntity SymbolName
    -- | Import with explicit list of children, e.g. Foo(Bar, Baz), Quux(foo, bar)
  | ImportWithSpecifilChildren SymbolName [SymbolName]
    -- | Wildcard import, e.g. Foo(..)
  | ImportWithAllChildren SymbolName
  deriving (Show, Eq, Ord)
