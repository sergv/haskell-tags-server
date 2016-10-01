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
import qualified Data.List.NonEmpty as NE
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
import Data.Symbols
import Server.Tags.Types
import Text.PrettyPrint.Leijen.Text.Utils

analyzeHeader
  :: (MonadError Doc m, MonadLog m)
  => [Token]
  -> m (Maybe ModuleHeader, [Token])
analyzeHeader ts =
  -- logDebug $ "[analyzeHeader] ts =" <+> pretty (Tokens ts)
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
  => Map ModuleName (NonEmpty ImportSpec)
  -> Map ImportQualifier (NonEmpty ModuleName)
  -> [Token]
  -> m ( Map ModuleName (NonEmpty ImportSpec)
       , Map ImportQualifier (NonEmpty ModuleName)
       , [Token]
       )
analyzeImports imports qualifiers ts = case dropNLs ts of
  -- Vanilla imports
  PImport : (d ->                 PQualified : (d -> PName name : (d -> PAs : (d -> PName qualName : rest))))  -> add name (Qualified $ mkQual qualName) rest
  PImport : (d ->                 PQualified : (d -> PName name :                                    rest))    -> add name (Qualified $ mkQual name) rest
  PImport : (d ->                                    PName name : (d -> PAs : (d -> PName qualName : rest)))   -> add name (BothQualifiedAndUnqualified $ mkQual qualName) rest
  PImport : (d ->                                    PName name :                                    rest)     -> add name Unqualified rest
  -- Package-qualified imports
  PImport : (d -> PString : (d -> PQualified : (d -> PName name : (d -> PAs : (d -> PName qualName : rest))))) -> add name (Qualified $ mkQual qualName) rest
  PImport : (d -> PString : (d -> PQualified : (d -> PName name :                                    rest)))   -> add name (Qualified $ mkQual name) rest
  PImport : (d -> PString : (d ->                    PName name : (d -> PAs : (d -> PName qualName : rest))))  -> add name (BothQualifiedAndUnqualified $ mkQual qualName) rest
  PImport : (d -> PString : (d ->                    PName name :                                    rest))    -> add name Unqualified rest
  _ -> pure (imports, qualifiers, ts)
  where
    d      = dropNLs
    mkQual = mkImportQualifier . mkModuleName
    add
      :: Text
      -> ImportQualification
      -> [Token]
      -> m ( Map ModuleName (NonEmpty ImportSpec)
           , Map ImportQualifier (NonEmpty ModuleName)
           , [Token]
           )
    add name qual toks = do
      (importList, toks') <- analyzeImportList toks
      let spec     = newSpec importList
          imports' = M.alter (upd spec) modName imports
      analyzeImports imports' qualifiers' toks'
      where
        modName :: ModuleName
        modName = mkModuleName name
        qualifiers' :: Map ImportQualifier (NonEmpty ModuleName)
        qualifiers' = case getQualifier qual of
                        Just q  -> M.alter (upd modName) q qualifiers
                        Nothing -> qualifiers
        newSpec :: Maybe ImportList -> ImportSpec
        newSpec importList = ImportSpec
          { ispecModuleName    = modName
          , ispecQualification = qual
          , ispecImportList    = importList
          }
        upd :: a -> Maybe (NonEmpty a) -> Maybe (NonEmpty a)
        upd x prev  = Just $
          case prev of
            Nothing    -> x :| []
            Just specs -> NE.cons x specs
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
          :: KeyMap (EntryWithChildren UnqualifiedSymbolName)
          -> [Token]
          -> m (KeyMap (EntryWithChildren UnqualifiedSymbolName), [Token])
        findImportListEntries acc toks =
          case dropNLs toks of
            []                -> pure (acc, [])
            PRParen : rest    -> pure (acc, rest)
            PName name : rest -> do
              (children, rest') <- analyzeChildren "import" $ dropNLs rest
              case mkUnqualifiedSymbolName (mkSymbolName name) of
                Nothing    ->
                  throwError $ "Invalid qualified entry on import list:" <+> docFromText name
                Just name' ->
                  findImportListEntries (KM.insert newEntry acc) $ dropComma rest'
                  where
                    newEntry = EntryWithChildren name' children
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
    go :: KeyMap (EntryWithChildren SymbolName)
       -> [ModuleName]
       -> [Token]
       -> m ModuleExports
    go entries reexports = \case
      []                           -> pure exports
      [PRParen]                    -> pure exports
      PName name : rest            -> do
        (children, rest') <- analyzeChildren "export" rest
        let name'    = mkSymbolName name
            newEntry = EntryWithChildren name' children
        consumeComma (KM.insert newEntry entries) reexports rest'
      PPattern : PName name : rest ->
        consumeComma (KM.insert newEntry entries) reexports rest
        where
          name'    = mkSymbolName name
          newEntry = mkEntryWithoutChildren name'
      PModule  : PName name : rest ->
        consumeComma entries (newReexports ++ reexports) rest
        where
          modName = mkModuleName name
          newReexports
            = toList
            $ M.findWithDefault (modName :| []) (mkImportQualifier modName) importQualifiers
      toks                      ->
        throwError $ "Unrecognized export list structure:" <+> pretty (Tokens toks)
      where
        exports = ModuleExports
          { meExportedEntries    = entries
          , meReexports          = reexports
          , meHasWildcardExports = getAny $ foldMap exportsAllChildren entries
          }
        exportsAllChildren (EntryWithChildren _ visibility) =
          maybe mempty isExportAllChildren visibility
          where
            isExportAllChildren VisibleAllChildren          = Any True
            isExportAllChildren (VisibleSpecificChildren _) = mempty
    -- Continue parsing by consuming comma delimiter.
    consumeComma
      :: KeyMap (EntryWithChildren SymbolName)
      -> [ModuleName]
      -> [Token]
      -> m ModuleExports
    consumeComma entries reexports = go entries reexports . dropComma

analyzeChildren
  :: forall m. (MonadError Doc m, MonadLog m)
  => Doc -> [Token] -> m (Maybe ChildrenVisibility, [Token])
analyzeChildren listType = \case
  []                                     -> pure (Nothing, [])
  toks@(PComma : _)                      -> pure (Nothing, toks)
  toks@(PRParen : _)                     -> pure (Nothing, toks)
  -- PLParen : PDot : PDot : PRParen : rest -> pure (Just VisibleAllChildren, rest)
  PLParen : PName ".." : PRParen : rest  -> pure (Just VisibleAllChildren, rest)
  PLParen : PRParen : rest               -> pure (Nothing, rest)
  PLParen : rest@(PName _ : _)           -> do
    -- (children, rest') <- analyzeCommaSeparatedNameList rest
    (children, rest') <- extractChildren mempty rest
    pure (Just $ VisibleSpecificChildren children, rest')
  toks ->
    throwError $ "Cannot handle" <+> listType <+> "children list:" <+> pretty (Tokens toks)
  where
    extractChildren
      :: Set UnqualifiedSymbolName
      -> [Token]
      -> m (Set UnqualifiedSymbolName, [Token])
    extractChildren names = \case
      []             -> pure (names, [])
      PRParen : rest -> pure (names, rest)
      PName name : rest
        | Just name' <- mkUnqualifiedSymbolName $ mkSymbolName name ->
          extractChildren (S.insert name' names) $ dropComma rest
      toks           ->
        throwError $ "Unrecognized children list structure:" <+> pretty (Tokens toks)

newtype Tokens = Tokens [Token]

instance Pretty Tokens where
  pretty (Tokens [])       = "[]"
  pretty (Tokens ts@(t : _)) =
    ppDict "Tokens"
      [ "file"   :-> showDoc (posFile $ posOf t)
      , "tokens" :-> ppList (map ppTokenVal ts)
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
