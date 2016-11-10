----------------------------------------------------------------------------
-- |
-- Module      :  Data.Symbols
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Tuesday, 27 September 2016
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

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
  , getUnqualifiedSymbolName
  , isQualified
  , mkUnqualifiedSymbolName
  , splitQualifiedPart
  , ResolvedSymbol
  , mkResolvedSymbol
  , resolvedSymbolName
  , resolvedSymbolType
  , resolvedSymbolParent
  , resolvedSymbolPosition
  ) where

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as Attoparsec
import Data.Char (isUpper)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Text.PrettyPrint.Leijen.Text as PP

import FastTags (Pos(..), TagVal(..), Type(..), SrcPos(..), Line(..))

import Data.KeyMap (HasKey(..))
import Text.PrettyPrint.Leijen.Text.Ext

-- | e.g. Foo, Foo.Bar. Assume that this is not an import qualifier.
-- Import qualifiers should be labeled as 'ImportQualifer'.
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
newtype ResolvedSymbol = ResolvedSymbol (Pos TagVal)
  deriving (Show, Eq, Ord)

instance HasKey ResolvedSymbol where
  type Key ResolvedSymbol = UnqualifiedSymbolName
  getKey = resolvedSymbolName

instance Pretty ResolvedSymbol where
  pretty sym@(ResolvedSymbol _) =
    ppDict "ResolvedSymbol" $
      [ "name"     :-> pretty (resolvedSymbolName sym)
      , "type"     :-> ppType (resolvedSymbolType sym)
      ] ++
      [ "parent"   :-> pretty parent
      | Just parent <- [resolvedSymbolParent sym]
      ] ++
      [ "position" :-> ppSrcPos (resolvedSymbolPosition sym)
      ]
    where
      ppType :: Type -> Doc
      ppType = showDoc

      ppSrcPos :: SrcPos -> Doc
      ppSrcPos (SrcPos file line _) = docFromString file <> ":" <> pretty (unLine line)

mkResolvedSymbol :: Pos TagVal -> ResolvedSymbol
mkResolvedSymbol = ResolvedSymbol

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

