----------------------------------------------------------------------------
-- |
-- Module      :  Haskell.Language.Server.Tags.Types.Imports
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Haskell.Language.Server.Tags.Types.Imports
  ( ImportKey(..)
  , ImportTarget(..)
  , ImportSpec(..)
  , importBringsUnqualifiedNames
  , importBringsQualifiedNames
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
import Data.Map.Strict (Map)
import Data.Set (Set)
import Prettyprinter.Ext

import Data.KeyMap (KeyMap, HasKey(..))
import Data.SubkeyMap (HasSubkey(..))
import Data.SymbolMap (SymbolMap)
import Data.SymbolMap qualified as SM
import Data.Symbols
import Haskell.Language.Tags.Types

-- | Handle for when particular module enters another module's scope.
data ImportKey = ImportKey
  { -- | Whether this import statement is annotated with {-# SOURCE #-} pragma.
    -- This means that it refers to .hs-boot file, rather than vanilla .hs file.
    ikImportTarget :: !ImportTarget
    -- | Name of imported module
  , ikModuleName   :: !ModuleName
  } deriving (Eq, Ord, Show, Generic)

instance NFData ImportKey

instance HasSubkey ImportKey where
  type Subkey ImportKey = ModuleName
  getSubkey = ikModuleName

instance Pretty ImportKey where
  pretty ik =
    "ImportKey" <+> pretty (ikImportTarget ik) <+> pretty (ikModuleName ik)


data ImportTarget = VanillaModule | HsBootModule
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving Pretty via PPGeneric ImportTarget

instance NFData ImportTarget


-- | Information about import statement
data ImportSpec = ImportSpec
  { ispecImportKey     :: !ImportKey
  , ispecQualification :: !ImportQualification
  , ispecImportList    :: !(ImportListSpec ImportList)
  } deriving (Eq, Ord, Show, Generic)
  deriving Pretty via PPGeneric ImportSpec

instance NFData ImportSpec


-- NB See [Record fields visibility] why this function must not be
-- incorporated into main name resolution logic (i.e. the 'LoadModule' module).
importBringsUnqualifiedNames :: SymbolMap -> ImportSpec -> Maybe SymbolMap
importBringsUnqualifiedNames exportedNames ImportSpec{ispecQualification} =
  case ispecQualification of
    Qualified _                   ->
      case foldMap
        (\(p, children) ->
          -- [Record fields visibility]
          -- Functions associated with a type will likely be names of
          -- fields, which do come unqualified. But actual functions don't (!).
          -- That's why this logic is not incorporated into main name resolution,
          -- but only in search.
          if resolvedSymbolType p == Type
          then filter ((== Function) . resolvedSymbolType) children
          else [])
        (SM.childrenRelations exportedNames) of
        [] -> Nothing
        xs -> Just $ SM.fromList xs
    Unqualified                   -> Just exportedNames
    BothQualifiedAndUnqualified _ -> Just exportedNames

importBringsQualifiedNames :: ImportSpec -> Bool
importBringsQualifiedNames ImportSpec{ispecQualification} =
  case ispecQualification of
    Qualified _                   -> True
    Unqualified                   -> False
    BothQualifiedAndUnqualified _ -> True

importBringsNamesQualifiedWith :: ImportSpec -> ImportQualifier -> Bool
importBringsNamesQualifiedWith ImportSpec{ispecQualification} q =
  getQualifier ispecQualification == Just q

data ImportQualification
  = -- | Qualified import, e.g.
    --
    -- import qualified X as Y
    --
    -- The ModuleName field would store "Y" in this case.
    --
    -- import qualified X - field would store "X"
    Qualified !ImportQualifier
    -- | Vanilla import, e.g.
    --
    -- import X
  | Unqualified
    -- | Vanilla import with explicit alias, e.g.
    --
    -- import X as Y
  | BothQualifiedAndUnqualified !ImportQualifier
  deriving (Eq, Ord, Show, Generic)
  deriving Pretty via PPGeneric ImportQualification

instance NFData ImportQualification

hasQualifier :: ImportQualifier -> ImportQualification -> Bool
hasQualifier qual = maybe False (== qual) . getQualifier

getQualifier :: ImportQualification -> Maybe ImportQualifier
getQualifier (Qualified q)                   = Just q
getQualifier Unqualified                     = Nothing
getQualifier (BothQualifiedAndUnqualified q) = Just q


data ImportType
  = -- | Explicit import list of an import statement, e.g.
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
  deriving Pretty via PPGeneric ImportType

instance NFData ImportType


data ImportListSpec a
  = NoImportList
    -- | When we canot precisely analyse an import list it's
    -- conservatively defaulted to "import all".
  | AssumedWildcardImportList
  | SpecificImports !a
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
  deriving Pretty via PPGeneric (ImportListSpec a)

instance NFData a => NFData (ImportListSpec a)


-- | User-provided import/hiding list.
data ImportList = ImportList
  { ilEntries    :: !(KeyMap Set (EntryWithChildren () UnqualifiedSymbolName))
  , ilImportType :: !ImportType
  } deriving (Eq, Ord, Show, Generic)

instance NFData ImportList

instance Pretty ImportList where
  pretty ImportList{ilImportType, ilEntries} =
    ppFoldableHeader ("Import list[" <> pretty ilImportType <> "]") ilEntries


data EntryWithChildren childAnn name = EntryWithChildren
  { entryName               :: !name
  , entryChildrenVisibility :: !(Maybe (ChildrenVisibility childAnn))
  }
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
  deriving Pretty via PPGeneric (EntryWithChildren childAnn name)

instance (NFData a, NFData b) => NFData (EntryWithChildren a b)

mkEntryWithoutChildren :: a -> EntryWithChildren ann a
mkEntryWithoutChildren name = EntryWithChildren name Nothing

instance HasKey (EntryWithChildren ann UnqualifiedSymbolName) where
  type Key (EntryWithChildren ann UnqualifiedSymbolName) = UnqualifiedSymbolName
  {-# INLINE getKey #-}
  getKey = entryName

instance HasKey (EntryWithChildren ann SymbolName) where
  type Key (EntryWithChildren ann SymbolName) = SymbolName
  {-# INLINE getKey #-}
  getKey = entryName

data ChildrenVisibility ann
  = -- | Wildcard import/export, e.g. Foo(..)
    VisibleAllChildren

  -- | Import/export with explicit list of children, e.g. Foo(Bar, Baz), Quux(foo, bar).
  -- Set is always non-empty.
  | VisibleSpecificChildren
      -- TODO: to support explicit namespaces change ‘ann’ to pair ‘(Namespace, ann)’.
      !(Map UnqualifiedSymbolName ann)

  -- | Wildcard export with some things added in, so they'll be visible on
  -- wildcard import, e.g.
  -- ErrorCall(.., ErrorCall)
  | VisibleAllChildrenPlusSome !(Map UnqualifiedSymbolName ann)
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
  deriving Pretty via PPGeneric (ChildrenVisibility ann)

instance NFData a => NFData (ChildrenVisibility a)
