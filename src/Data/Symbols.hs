----------------------------------------------------------------------------
-- |
-- Module      :  Data.Symbols
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Tuesday, 27 September 2016
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Symbols
  ( ModuleName
  , getModuleName
  , mkModuleName
  , isModuleNameConstituentChar
  , fileNameToModuleName
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
  , resolvedSymbolParentName
  , resolvedSymbolParent
  , resolvedSymbolPosition
  , resolvedSymbolLine
  , resolvedSymbolFile
  ) where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.DeepSeq
import Control.Monad.Except.Ext

import Data.Attoparsec.Text
import Data.Attoparsec.Text qualified as Attoparsec
import Data.Char (isUpper, isAlphaNum)
import Data.Coerce
import Data.Hashable
import Data.List qualified as L
import Data.Maybe
import Data.Store (Store)
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter.Ext

import Haskell.Language.Lexer.Types (Pos(..), TagVal(..), SrcPos(..))

import Data.ErrorMessage
import Data.KeyMap (HasKey(..))
import Data.Path

import FasterRicherTags.Types

-- | e.g. Foo, Foo.Bar. Assume that this is not an import qualifier.
-- Import qualifiers should be labeled as 'ImportQualifer'.
newtype ModuleName = ModuleName { getModuleName :: Text }
  deriving (Eq, Ord, Show, Pretty, Hashable, NFData, Store)

{-# INLINE mkModuleName #-}
mkModuleName :: Text -> ModuleName
mkModuleName = ModuleName

isModuleNameConstituentChar :: Char -> Bool
isModuleNameConstituentChar = \case
  '\'' -> True
  '_'  -> True
  c    -> isAlphaNum c

-- | Try to infer suitable module name from the file name. Tries to take
-- as much directory names that start with an uppercase letter as possible.
fileNameToModuleName
  :: (WithCallStack, MonadError ErrorMessage m)
  => FullPath 'File -> m ModuleName
fileNameToModuleName fname =
  pure $
    mkModuleName $
      T.intercalate "." $
        reverse $
          L.takeWhile canBeModuleName $
            unPathFragment (unBaseName (dropExtensions fname')) : map (unPathFragment . unBaseName) (reverse dirs)
  where
    (dirs, fname') = splitDirectories fname
    canBeModuleName :: T.Text -> Bool
    canBeModuleName t = case T.uncons t of
      Nothing      -> False
      Just (c, cs) -> isUpper c && T.all isModuleNameConstituentChar cs

-- | Custom module name used for qualification. This is the XXX part of the
-- import statement:
-- import Foo.Bar as XXX
-- import qualified Fizz.Buzz as XXX
newtype ImportQualifier = ImportQualifier { getImportQualifier :: ModuleName }
  deriving (Eq, Ord, Show, Pretty, Hashable, NFData, Store)

{-# INLINE mkImportQualifier #-}
mkImportQualifier :: ModuleName -> ImportQualifier
mkImportQualifier = ImportQualifier

-- | Name the @ResolvedSymbol@ refers to. Can be either qualified or unqualified.
newtype SymbolName = SymbolName { getSymbolName :: Text }
  deriving (Eq, Ord, Show, Pretty, Hashable, NFData, Store)

{-# INLINE mkSymbolName #-}
mkSymbolName :: Text -> SymbolName
mkSymbolName = SymbolName

-- | Name the @ResolvedSymbol@ refers to.
newtype UnqualifiedSymbolName = UnqualifiedSymbolName { getUnqualifiedSymbolName :: SymbolName }
  deriving (Eq, Ord, Show, Hashable, NFData, Store)

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
  , rsName   :: {-# UNPACK #-} !UnqualifiedSymbolName -- !Text
  , rsType   :: !Type
  , rsParent :: !(Maybe ParentTag)
  } deriving (Eq, Ord, Show, Generic)

instance Hashable ResolvedSymbol
instance NFData   ResolvedSymbol
instance Store    ResolvedSymbol

instance Store    ParentTag

deriving via PPGeneric ParentTag instance Pretty ParentTag

instance HasKey ResolvedSymbol where
  type Key ResolvedSymbol = UnqualifiedSymbolName
  {-# INLINE getKey #-}
  getKey = resolvedSymbolName



instance Pretty ResolvedSymbol where
  pretty = ppGeneric

{-# INLINE mkResolvedSymbol #-}
mkResolvedSymbol :: WithCallStack => FullPath 'File -> Pos TagVal -> ResolvedSymbol
mkResolvedSymbol rsFile (Pos SrcPos{posLine} TagVal{tvName, tvType, tvParent}) =
  ResolvedSymbol
    { rsFile
    , rsLine   = posLine
    , rsName   = fromMaybe err $ mkUnqualifiedSymbolName $ mkSymbolName tvName
    , rsType   = tvType
    , rsParent = tvParent
    }
  where
    err = error $ "Invalid tag val name: " ++ T.unpack tvName

mkResolvedSymbolFromParts
  :: FullPath 'File
  -> Line
  -> UnqualifiedSymbolName -- ^ Symbol name
  -> Type                  -- ^ Type of entity symbol will refer to
  -> Maybe ParentTag       -- ^ Optional parent
  -> ResolvedSymbol
mkResolvedSymbolFromParts rsFile rsLine rsName rsType rsParent =
  ResolvedSymbol
    { rsFile
    , rsLine
    , rsName
    , rsType
    , rsParent
    }

{-# INLINE resolvedSymbolName #-}
resolvedSymbolName :: ResolvedSymbol -> UnqualifiedSymbolName
resolvedSymbolName = rsName

{-# INLINE resolvedSymbolType #-}
resolvedSymbolType :: ResolvedSymbol -> Type
resolvedSymbolType = rsType

{-# INLINE resolvedSymbolParentName #-}
resolvedSymbolParentName :: ResolvedSymbol -> Maybe UnqualifiedSymbolName
resolvedSymbolParentName = coerce . fmap ptName . rsParent

{-# INLINE resolvedSymbolParent #-}
resolvedSymbolParent :: ResolvedSymbol -> Maybe ParentTag
resolvedSymbolParent = rsParent

{-# INLINE resolvedSymbolPosition #-}
resolvedSymbolPosition :: ResolvedSymbol -> (FullPath 'File, Line)
resolvedSymbolPosition = rsFile &&& rsLine

{-# INLINE resolvedSymbolLine #-}
resolvedSymbolLine :: ResolvedSymbol -> Line
resolvedSymbolLine = rsLine

{-# INLINE resolvedSymbolFile #-}
resolvedSymbolFile :: ResolvedSymbol -> FullPath 'File
resolvedSymbolFile = rsFile
