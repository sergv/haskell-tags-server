----------------------------------------------------------------------------
-- |
-- Module      :  Data.Symbols
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Tuesday, 27 September 2016
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wredundant-constraints          #-}
{-# OPTIONS_GHC -Wsimplifiable-class-constraints #-}

module Data.Symbols
  ( ModuleName
  , getModuleName
  , mkModuleName
  , ImportQualifier
  , mkImportQualifier
  , getImportQualifier
  , SymbolName
  , getSymbolName
  , mkSymbolName
  , UnqualifiedSymbolName
  , unqualSymNameText
  , getUnqualifiedSymbolName
  , isQualified
  , mkUnqualifiedSymbolName
  , splitQualifiedPart
  , ResolvedSymbol
  , mkResolvedSymbol
  , mkResolvedSymbolFromParts
  , resolvedSymbolName
  , resolvedSymbolType
  , resolvedSymbolParent
  , resolvedSymbolPosition
  , resolvedSymbolFile
  ) where

import Control.Arrow ((&&&))
import Control.Applicative
import Control.DeepSeq

import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as Attoparsec
import Data.Char (isUpper)
import Data.Coerce
import Data.Hashable
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc.Ext
import GHC.Generics (Generic)

import Haskell.Language.Lexer.FastTags (Pos(..), TagVal(..), Type(..), SrcPos(..), Line(..))

import Data.KeyMap (HasKey(..))
import Data.Path

-- | e.g. Foo, Foo.Bar. Assume that this is not an import qualifier.
-- Import qualifiers should be labeled as 'ImportQualifer'.
newtype ModuleName = ModuleName { getModuleName :: Text }
  deriving (Eq, Ord, Show, Pretty, Hashable, NFData)

{-# INLINE mkModuleName #-}
mkModuleName :: Text -> ModuleName
mkModuleName = ModuleName

-- | Custom module name used for qualification. This is the XXX part of the
-- import statement:
-- import Foo.Bar as XXX
-- import qualified Fizz.Buzz as XXX
newtype ImportQualifier = ImportQualifier { getImportQualifier :: ModuleName }
  deriving (Eq, Ord, Show, Pretty, Hashable, NFData)

{-# INLINE mkImportQualifier #-}
mkImportQualifier :: ModuleName -> ImportQualifier
mkImportQualifier = ImportQualifier

-- | Name the @ResolvedSymbol@ refers to. Can be either qualified or unqualified.
newtype SymbolName = SymbolName { getSymbolName :: Text }
  deriving (Eq, Ord, Show, Pretty, Hashable, NFData)

{-# INLINE mkSymbolName #-}
mkSymbolName :: Text -> SymbolName
mkSymbolName = SymbolName

-- | Name the @ResolvedSymbol@ refers to.
newtype UnqualifiedSymbolName = UnqualifiedSymbolName { getUnqualifiedSymbolName :: SymbolName }
  deriving (Eq, Ord, Show, Hashable, NFData)

instance Pretty UnqualifiedSymbolName where
  pretty = pretty . getUnqualifiedSymbolName

{-# INLINE unqualSymNameText #-}
unqualSymNameText :: UnqualifiedSymbolName -> Text
unqualSymNameText = coerce

isQualified :: SymbolName -> Bool
isQualified name = case mkUnqualifiedSymbolName name of
  Nothing -> True
  Just _  -> False

mkUnqualifiedSymbolName :: SymbolName -> Maybe UnqualifiedSymbolName
mkUnqualifiedSymbolName name =
  case T.uncons $ getSymbolName name of
    Nothing -> Nothing
    Just (c, cs)
      | isUpper c && T.any (== '.') cs -> Nothing
      | otherwise                      -> Just $ UnqualifiedSymbolName name

-- | Split qualified symbol name (e.g. Foo.Bar.baz) into
-- qualified module part (Foo.Bar) and name part (baz). Return Nothing
splitQualifiedPart
  :: SymbolName
  -> (Maybe ImportQualifier, UnqualifiedSymbolName)
splitQualifiedPart sym =
  case parseOnly pQualifiedName (getSymbolName sym) of
    Left err -> error err -- (Nothing, UnqualifiedSymbolName sym)
    Right x  -> x
  where
    pQualifiedName :: Parser (Maybe ImportQualifier, UnqualifiedSymbolName)
    pQualifiedName = (,)
      <$> optional pQualifier
      <*> (UnqualifiedSymbolName . mkSymbolName <$> Attoparsec.takeWhile (const True))
      <*  endOfInput
    pQualifier :: Parser ImportQualifier
    pQualifier = mkImportQualifier . mkModuleName . T.intercalate "." <$> many1 (pModuleName <* char '.')
    pModuleName :: Parser Text
    pModuleName = T.cons <$> satisfy isUpper <*> takeTill (== '.')

-- | A symbolic name that identifier some Haskell entity. Has position,
-- entity type and possibly a parent.
data ResolvedSymbol = ResolvedSymbol
  { rsFile   :: !(FullPath 'File)
  , rsLine   :: {-# UNPACK #-} !Line
  , rsName   :: {-# UNPACK #-} !Text
  , rsType   :: !Type
  , rsParent :: !(Maybe Text)
  } deriving (Eq, Ord, Show, Generic)

instance Hashable ResolvedSymbol
instance NFData   ResolvedSymbol

instance HasKey ResolvedSymbol where
  type Key ResolvedSymbol = UnqualifiedSymbolName
  {-# INLINE getKey #-}
  getKey = resolvedSymbolName

instance Pretty ResolvedSymbol where
  pretty = ppGeneric

{-# INLINE mkResolvedSymbol #-}
mkResolvedSymbol :: FullPath 'File -> Pos TagVal -> ResolvedSymbol
mkResolvedSymbol rsFile (Pos SrcPos{posLine} TagVal{tvName, tvType, tvParent}) =
  ResolvedSymbol
    { rsFile
    , rsLine   = posLine
    , rsName   = tvName
    , rsType   = tvType
    , rsParent = tvParent
    }

mkResolvedSymbolFromParts
  :: FullPath 'File
  -> Line
  -> UnqualifiedSymbolName       -- ^ Symbol name
  -> Type                        -- ^ Type of entity symbol will refer to
  -> Maybe UnqualifiedSymbolName -- ^ Optional parent
  -> ResolvedSymbol
mkResolvedSymbolFromParts rsFile rsLine name rsType parent =
  ResolvedSymbol
    { rsFile
    , rsLine
    , rsName   = unqualSymNameText name
    , rsType
    , rsParent = unqualSymNameText <$> parent
    }

{-# INLINE resolvedSymbolName #-}
resolvedSymbolName :: ResolvedSymbol -> UnqualifiedSymbolName
resolvedSymbolName = UnqualifiedSymbolName . SymbolName . rsName

{-# INLINE resolvedSymbolType #-}
resolvedSymbolType :: ResolvedSymbol -> Type
resolvedSymbolType = rsType

{-# INLINE resolvedSymbolParent #-}
resolvedSymbolParent :: ResolvedSymbol -> Maybe UnqualifiedSymbolName
resolvedSymbolParent = coerce rsParent

{-# INLINE resolvedSymbolPosition #-}
resolvedSymbolPosition :: ResolvedSymbol -> (FullPath 'File, Line)
resolvedSymbolPosition = rsFile &&& rsLine

{-# INLINE resolvedSymbolFile #-}
resolvedSymbolFile :: ResolvedSymbol -> FullPath 'File
resolvedSymbolFile = rsFile
