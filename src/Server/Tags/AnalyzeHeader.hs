----------------------------------------------------------------------------
-- |
-- Module      :  Server.Tags.AnalyzeHeader
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Thursday, 22 September 2016
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Server.Tags.AnalyzeHeader
  ( analyzeHeader
  ) where

import Control.Arrow (first)
import Control.Monad.Except
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Token (Pos(..), TokenVal(..), Token, posFile, posLine, unLine, TokenVal)
import FastTags (stripNewlines, UnstrippedTokens(..))

import Control.Monad.Logging
import Data.KeyMap (KeyMap)
import qualified Data.KeyMap as KM
import Server.Tags.Types
import Text.PrettyPrint.Leijen.Text.Utils

analyzeHeader
  :: (MonadError Doc m, MonadLog m)
  => [Token]
  -> m (Maybe ModuleHeader, [Token])
analyzeHeader ts =
  case dropWhile ((/= KWModule) . valOf) ts of
    Pos _ KWModule :
      (dropNLs -> Pos _ (T modName) :
        (break ((== KWWhere) . valOf) . dropNLs -> (exportList, Pos _ KWWhere : body))) -> do
      (importSpecs, importQualifiers, body') <- analyzeImports mempty mempty body
      exports                                <- analyzeExports importQualifiers exportList
      let header = ModuleHeader
            { mhModName          = mkModuleName modName
            , mhImports          = importSpecs
            , mhImportQualifiers = importQualifiers
            , mhExports          = exports
            }
      pure (Just header, body')
      -- No header present.
    _ -> pure (Nothing, ts)

pattern PImport    <- Pos _ KWImport
pattern PPattern   <- Pos _ (T "pattern")
pattern PModule    <- Pos _ KWModule
pattern PString    <- Pos _ String
pattern PQualified <- Pos _ (T "qualified")
pattern PName name <- Pos _ (T name)
pattern PAs        <- Pos _ (T "as")
pattern PHiding    <- Pos _ (T "hiding")
pattern PLParen    <- Pos _ LParen
pattern PRParen    <- Pos _ RParen
-- pattern PDot       <- Pos _ Dot
pattern PComma     <- Pos _ Comma

analyzeImports
  :: forall m. (MonadError Doc m, MonadLog m)
  => [ImportSpec]
  -> Map ImportQualifier (NonEmpty ModuleName)
  -> [Token]
  -> m ([ImportSpec], Map ImportQualifier (NonEmpty ModuleName), [Token])
analyzeImports imports qualifiers ts = case dropNLs ts of
  -- Vanilla imports
  PImport :           PQualified : PName name : PAs : PName qualName : rest -> add name (Qualified $ mkQual qualName) rest
  PImport :           PQualified : PName name :                        rest -> add name (Qualified $ mkQual name) rest
  PImport :                        PName name : PAs : PName qualName : rest -> add name (BothQualifiedAndUnqualified $ mkQual qualName) rest
  PImport :                        PName name :                        rest -> add name Unqualified rest
  -- Package-qualified imports
  PImport : PString : PQualified : PName name : PAs : PName qualName : rest -> add name (Qualified $ mkQual qualName) rest
  PImport : PString : PQualified : PName name :                        rest -> add name (Qualified $ mkQual name) rest
  PImport : PString :              PName name : PAs : PName qualName : rest -> add name (BothQualifiedAndUnqualified $ mkQual qualName) rest
  PImport : PString :              PName name :                        rest -> add name Unqualified rest
  _ -> pure (imports, qualifiers, ts)
  where
    mkQual = mkImportQualifier . mkModuleName
    add
      :: Text
      -> ImportQualification
      -> [Token]
      -> m ([ImportSpec], Map ImportQualifier (NonEmpty ModuleName), [Token])
    add name qual toks = do
      (importList, toks') <- analyzeImportList toks
      analyzeImports (newSpec importList : imports) qualifiers toks'
      where
        newSpec importList = ImportSpec
          { ispecModuleName    = mkModuleName name
          , ispecQualification = qual
          , ispecImportList    = importList
          }
    -- Analyze comma-separated list of entries like
    -- - Foo
    -- - Foo(Bar, Baz)
    -- - Quux(..)
    analyzeImportList :: [Token] -> m (Maybe ImportList, [Token])
    analyzeImportList toks =
      case dropNLs toks of
        []                                    -> pure (Nothing, toks)
        PHiding : (dropNLs -> PLParen : rest) -> first (Just . Hidden)   <$> findImportListEntries mempty (dropNLs rest)
        PLParen : rest                        -> first (Just . Imported) <$> findImportListEntries mempty (dropNLs rest)
        _                                     -> pure (Nothing, toks)
      where
        findImportListEntries
          :: [EntryWithChildren]
          -> [Token]
          -> m ([EntryWithChildren], [Token])
        findImportListEntries acc toks =
          case dropNLs toks of
            []                -> pure (reverse acc, [])
            PRParen : rest    -> pure (reverse acc, rest)
            PName name : rest -> do
              (children, rest') <- analyzeChildren "import" $ dropNLs rest
              let newEntry = EntryWithChildren (mkSymbolName name) children
              findImportListEntries (newEntry : acc) $ dropComma rest'
            rest ->
              throwError $ "Unrecognized shape of import list:" <+> pretty (Tokens rest)

analyzeExports
  :: forall m. (MonadError Doc m, MonadLog m)
  => Map ImportQualifier (NonEmpty ModuleName)
  -> [Token]
  -> m (Maybe ModuleExports)
analyzeExports importQualifiers ts =
  case stripNewlines $ UnstrippedTokens ts of
    []            -> pure Nothing
    PLParen : rest -> Just <$> go mempty mempty rest
    toks          ->
      throwError $ "Unrecognized shape of export list:" <+> pretty (Tokens toks)
  where
    -- Analyze comma-separated list of entries like
    -- - Foo
    -- - Foo(Bar, Baz)
    -- - Quux(..)
    -- - pattern PFoo
    -- - module Data.Foo.Bar
    go :: KeyMap EntryWithChildren -> [ModuleName] -> [Token] -> m ModuleExports
    go entries reexports = \case
      []                           -> pure exports
      [PRParen]                    -> pure exports
      PName name : rest            -> do
        (children, rest') <- analyzeChildren "export" rest
        let newEntry = EntryWithChildren (mkSymbolName name) children
        consumeComma (KM.insert newEntry entries) reexports rest'
      PPattern : PName name : rest ->
        consumeComma (KM.insert newEntry entries) reexports rest
        where
          newEntry = mkEntryWithoutChildren $ mkSymbolName name
      PModule  : PName name : rest ->
        consumeComma entries (newReexports ++ reexports) rest
        where
          modName      = mkModuleName name
          newReexports
            = toList
            $ M.findWithDefault (modName :| []) (mkImportQualifier modName) importQualifiers
      toks                      ->
        throwError $ "Unrecognized export list structure:" <+> pretty (Tokens toks)
      where
        exports = ModuleExports
          { meExportedEntries = entries
          , meReexports       = reexports
          }
    -- Continue parsing by consuming comma delimiter.
    consumeComma
      :: KeyMap EntryWithChildren -> [ModuleName] -> [Token] -> m ModuleExports
    consumeComma entries reexports = go entries reexports . dropComma

analyzeChildren
  :: forall m. (MonadError Doc m, MonadLog m)
  => Doc -> [Token] -> m (Maybe ChildrenVisibility, [Token])
analyzeChildren listType = \case
  []                                     -> pure (Nothing, [])
  toks@(PComma : _)                      -> pure (Nothing, toks)
  toks@(PRParen : _)                     -> pure (Nothing, toks)
  -- PLParen : PDot : PDot : PRParen : rest -> pure (Just ExportAllChildren, rest)
  PLParen : PName ".." : PRParen : rest  -> pure (Just ExportAllChildren, rest)
  PLParen : PRParen : rest               -> pure (Nothing, rest)
  PLParen : rest@(PName _ : _)           -> do
    -- (children, rest') <- analyzeCommaSeparatedNameList rest
    (children, rest') <- extractChildren mempty rest
    pure (Just $ ExportSpecificChildren children, rest')
  toks ->
    throwError $ "Cannot handle" <+> listType <+> "children list:" <+> pretty (Tokens toks)
  where
    extractChildren :: Set SymbolName -> [Token] -> m (Set SymbolName, [Token])
    extractChildren names = \case
      []                -> pure (names, [])
      PRParen : rest    -> pure (names, rest)
      PName name : rest ->
        extractChildren (S.insert (mkSymbolName name) names) $ dropComma rest
      toks              ->
        throwError $ "Unrecognized children list structure:" <+> pretty (Tokens toks)

-- analyzeCommaSeparatedNameList
--   :: forall m. (MonadError Doc m)
--   => [Token] -> m (Set SymbolName, [Token])
-- analyzeCommaSeparatedNameList = extractChildren mempty
--   where
--     extractChildren :: Set SymbolName -> [Token] -> m (Set SymbolName, [Token])
--     extractChildren !names = \case
--       []                -> pure (names, [])
--       PRParen : rest    -> pure (names, rest)
--       PName name : rest ->
--         extractChildren (S.insert (mkSymbolName name) names) $ dropComma rest
--       toks              ->
--         throwError $ "Unrecognized children list structure:" <+> pretty (Tokens toks)

-- analyzeCommaSeparatedList
--   :: forall m a. (MonadError Doc m)
--   => ([Token] -> m (a, [Token]))
--   -> [Token]
--   -> m ([a], [Token])
-- analyzeCommaSeparatedList match = extractChildren []
--   where
--     extractChildren :: [a] -> [Token] -> m ([a], [Token])
--     extractChildren xs = \case
--       []                -> pure (reverse xs, [])
--       PRParen : rest    -> pure (reverse xs, rest)
--       toks              -> do
--         (x, rest) <- match toks
--         extractChildren (x : xs) $ dropComma rest

newtype Tokens = Tokens [Token]

instance Pretty Tokens where
  pretty (Tokens [])       = "[]"
  pretty (Tokens ts@(t : _)) =
    ppDict "Tokens"
      [ "file"   :-> showDoc (posFile $ posOf t)
      , "tokens" :-> ppList "[" "]" (map ppTokenVal ts)
      ]
    where
      ppTokenVal :: Pos TokenVal -> Doc
      ppTokenVal (Pos pos tag) =
        pretty (unLine (posLine pos)) <> PP.colon <> showDoc tag

-- | Drop prefix of newlines.
dropNLs :: [Token] -> [Token]
dropNLs (Pos _ (Newline _) : ts) = dropNLs ts
dropNLs ts                       = ts

dropComma :: [Token] -> [Token]
dropComma (Pos _ Comma : ts) = ts
dropComma ts                 = ts
