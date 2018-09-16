----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Server.Tags.Types.Imports
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Haskell.Language.Server.Tags.Types.Imports
  ( ImportKey(..)
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
  , ImportListSpec(..)
  , ImportType(..)
  , ImportList(..)
  , EntryWithChildren(..)
  , mkEntryWithoutChildren
  , ChildrenVisibility(..)
  ) where

import Control.DeepSeq

import Data.Hashable
import Data.Map (Map)
import Data.Set (Set)
import Data.Text.Prettyprint.Doc.Ext
import GHC.Generics (Generic)

import Data.KeyMap (KeyMap, HasKey(..))
import Data.SubkeyMap (HasSubkey(..))
import Data.SymbolMap (SymbolMap)
import Data.Symbols

-- | Handle for when particular module enters another module's scope.
data ImportKey = ImportKey
  { -- | Whether this import statement is annotated with {-# SOURCE #-} pragma.
    -- This means that it refers to .hs-boot file, rather than vanilla .hs file.
    ikImportTarget :: !ImportTarget
    -- | Name of imported module
  , ikModuleName   :: !ModuleName
  } deriving (Eq, Ord, Show, Generic)

instance Hashable ImportKey
instance NFData   ImportKey

instance HasSubkey ImportKey where
  type Subkey ImportKey = ModuleName
  getSubkey = ikModuleName

instance Pretty ImportKey where
  pretty ik =
    "ImportKey" <+> pretty (ikImportTarget ik) <+> pretty (ikModuleName ik)


data ImportTarget = VanillaModule | HsBootModule
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance Hashable ImportTarget
instance NFData   ImportTarget

instance Pretty ImportTarget where
  pretty = ppGeneric

-- | Information about import statement
data ImportSpec a = ImportSpec
  { ispecImportKey     :: ImportKey
  , ispecQualification :: ImportQualification
  , ispecImportList    :: ImportListSpec ImportList
  , ispecImportedNames :: a
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance NFData a => NFData (ImportSpec a)

type UnresolvedImportSpec = ImportSpec ()
type ResolvedImportSpec   = ImportSpec SymbolMap

instance Pretty a => Pretty (ImportSpec a) where
  pretty = ppGeneric

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

instance Hashable ImportQualification
instance NFData   ImportQualification

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

instance Hashable ImportType
instance NFData   ImportType

instance Pretty ImportType where
  pretty = ppGeneric

data ImportListSpec a =
    NoImportList
    -- | When we canot precisely analyse an import list it's
    -- conservatively defaulted to "import all".
  | AssumedWildcardImportList
  | SpecificImports a
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

instance NFData a => NFData (ImportListSpec a)

instance Pretty a => Pretty (ImportListSpec a) where
  pretty = ppGeneric

-- | User-provided import/hiding list.
data ImportList = ImportList
  { ilEntries    :: KeyMap Set (EntryWithChildren () UnqualifiedSymbolName)
  , ilImportType :: ImportType
  } deriving (Eq, Ord, Show, Generic)

instance NFData ImportList

instance Pretty ImportList where
  pretty ImportList{ilImportType, ilEntries} =
    ppFoldableHeader ("Import list[" <> pretty ilImportType <> "]") ilEntries

data EntryWithChildren childAnn name = EntryWithChildren
  { entryName               :: name
  , entryChildrenVisibility :: Maybe (ChildrenVisibility childAnn)
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

instance (NFData a, NFData b) => NFData (EntryWithChildren a b)

instance (Pretty ann, Pretty name) => Pretty (EntryWithChildren ann name) where
  pretty = ppGeneric

mkEntryWithoutChildren :: a -> EntryWithChildren ann a
mkEntryWithoutChildren name = EntryWithChildren name Nothing

instance HasKey (EntryWithChildren ann UnqualifiedSymbolName) where
  type Key (EntryWithChildren ann UnqualifiedSymbolName) = UnqualifiedSymbolName
  getKey = entryName

instance HasKey (EntryWithChildren ann SymbolName) where
  type Key (EntryWithChildren ann SymbolName) = SymbolName
  getKey = entryName

data ChildrenVisibility ann =
    -- | Wildcard import/export, e.g. Foo(..)
    VisibleAllChildren
    -- | Import/export with explicit list of children, e.g. Foo(Bar, Baz), Quux(foo, bar).
    -- Set is always non-empty.
  | VisibleSpecificChildren (Map UnqualifiedSymbolName ann)
    -- | Wildcard export with some things added in, so they'll be visible on
    -- wildcard import, e.g.
    -- ErrorCall(..,ErrorCall)
  | VisibleAllChildrenPlusSome (Map UnqualifiedSymbolName ann)
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

instance NFData a => NFData (ChildrenVisibility a)

instance Pretty a => Pretty (ChildrenVisibility a) where
  pretty = ppGeneric
