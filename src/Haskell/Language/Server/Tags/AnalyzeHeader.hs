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

module Haskell.Language.Server.Tags.AnalyzeHeader
  ( analyzeHeader
  ) where

import Control.Arrow (first)
import Control.Category ((>>>))
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Data.Char
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Semigroup as Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.PrettyPrint.Leijen.Text as PP

import Token (Pos(..), TokenVal(..), Token, posFile, posLine, unLine, TokenVal, PragmaType(..))
import FastTags (stripNewlines, UnstrippedTokens(..), tokToName)

import Control.Monad.Logging
import Data.KeyMap (KeyMap)
import qualified Data.KeyMap as KM
import Data.SubkeyMap (SubkeyMap)
import qualified Data.SubkeyMap as SubkeyMap
import Data.Symbols
import Haskell.Language.Server.Tags.Types
import Text.PrettyPrint.Leijen.Text.Utils

analyzeHeader
  :: (MonadError Doc m, MonadLog m)
  => [Token]
  -> m (Maybe UnresolvedModuleHeader, [Token])
analyzeHeader ts = do
  logDebug $ "[analyzeHeader] ts =" <+> ppTokens ts
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

pattern PImport       <- Pos _ KWImport
pattern PPattern      <- Pos _ (T "pattern")
pattern PType         <- Pos _ KWType
pattern PModule       <- Pos _ KWModule
pattern PString       <- Pos _ String
pattern PQualified    <- Pos _ (T "qualified")
pattern PName name    <- Pos _ (T name)
pattern PAs           <- Pos _ (T "as")
pattern PHiding       <- Pos _ (T "hiding")
pattern PLParen       <- Pos _ LParen
pattern PRParen       <- Pos _ RParen
pattern PComma        <- Pos _ Comma
pattern PSourcePragma <- Pos _ (Pragma SourcePragma)

analyzeImports
  :: forall m. (MonadError Doc m, MonadLog m)
  => SubkeyMap ImportKey (NonEmpty UnresolvedImportSpec)
  -> Map ImportQualifier (NonEmpty ModuleName)
  -> [Token]
  -> m ( SubkeyMap ImportKey (NonEmpty UnresolvedImportSpec)
       , Map ImportQualifier (NonEmpty ModuleName)
       , [Token]
       )
analyzeImports imports qualifiers ts = do
  logDebug $ "[analyzeImpotrs] ts =" <+> ppTokens ts
  res <- runMaybeT $ do
    -- Drop initial "import" keyword and {-# SOURCE #-} pragma, if any
    (ts, importTarget)   <- case dropNLs ts of
      PImport : (d -> PSourcePragma : rest) -> pure (rest, HsBootModule)
      PImport :                       rest  -> pure (rest, VanillaModule)
      _                                     -> mzero
    let dropSafeImport = \case
          PName "safe" : rest -> rest
          rest                -> rest
        dropPackageImport = \case
          PString : rest      -> rest
          rest                -> rest
        extractQualified = \case
          PQualified : rest -> (rest, True)
          rest              -> (rest, False)

        (ts', isQualified) = dropNLs >>> dropSafeImport >>>
                             dropNLs >>> extractQualified >>>
                             first (dropNLs >>> dropPackageImport) $ ts
    -- Extract import name and renaming alias, if any
    (ts'', name, qualName) <- case dropNLs ts' of
      PName name : (d -> PAs : (d -> PName qualName : rest)) -> pure (rest, name, Just qualName)
      PName name :                                    rest   -> pure (rest, name, Nothing)
      _                                                      -> mzero
    -- Make sence of the data collected before
    let qualType = case (isQualified, qualName) of
                     (True,  Nothing)        -> Qualified $ mkQual name
                     (True,  Just qualName') -> Qualified $ mkQual qualName'
                     (False, Nothing)        -> Unqualified
                     (False, Just qualName') -> BothQualifiedAndUnqualified $ mkQual qualName'
    pure (name, qualType, importTarget, ts'')
  case res of
    Nothing                                 -> pure (imports, qualifiers, ts)
    Just (name, qualType, importTarget, ts) -> add name qualType importTarget ts
  where
    d      = dropNLs
    mkQual = mkImportQualifier . mkModuleName
    add
      :: Text
      -> ImportQualification
      -> ImportTarget
      -> [Token]
      -> m ( SubkeyMap ImportKey (NonEmpty UnresolvedImportSpec)
           , Map ImportQualifier (NonEmpty ModuleName)
           , [Token]
           )
    add name qual importTarget toks = do
      (importList, toks') <- analyzeImportList toks
      let spec     = mkNewSpec importList
          imports' = SubkeyMap.insertWith
                       (Semigroup.<>)
                       (ImportKey importTarget modName)
                       (spec :| [])
                       imports
      analyzeImports imports' qualifiers' toks'
      where
        modName :: ModuleName
        modName = mkModuleName name
        qualifiers' :: Map ImportQualifier (NonEmpty ModuleName)
        qualifiers' = case getQualifier qual of
                        Just q  -> M.insertWith (Semigroup.<>) q (modName :| []) qualifiers
                        Nothing -> qualifiers
        mkNewSpec :: Maybe UnresolvedImportList -> UnresolvedImportSpec
        mkNewSpec importList = ImportSpec
          { ispecImportKey     = ImportKey
              { ikModuleName   = modName
              , ikImportTarget = importTarget
              }
          , ispecQualification = qual
          , ispecImportList    = importList
          }
    -- Analyze comma-separated list of entries, starting at _|_:
    -- - Foo_|_
    -- - Foo_|_(Bar, Baz)
    -- - Foo_|_ hiding (Bar, Baz)
    -- - Quux_|_(..)
    analyzeImportList :: [Token] -> m (Maybe (UnresolvedImportList), [Token])
    analyzeImportList toks = do
      logDebug $ "[analyzeImpotrList] toks =" <+> ppTokens toks
      case dropNLs toks of
        []                                    -> pure (Nothing, toks)
        PHiding : (dropNLs -> PLParen : rest) -> first Just <$> findImportListEntries Hidden mempty (dropNLs rest)
        PLParen : rest                        -> first Just <$> findImportListEntries Imported mempty (dropNLs rest)
        _                                     -> pure (Nothing, toks)
      where
        findImportListEntries
          :: ImportType
          -> KeyMap (EntryWithChildren UnqualifiedSymbolName)
          -> [Token]
          -> m (UnresolvedImportList, [Token])
        findImportListEntries importType acc toks = do
          logDebug $ "[findImportListEntries] toks =" <+> ppTokens toks
          case dropNLs toks of
            []                                                                ->
              pure (importList, [])
            PRParen : rest                                                    ->
              pure (importList, rest)
            PLParen : Pos _ (tokToName -> Just name) : PRParen : rest         ->
              entryWithChildren "operator in import list" name rest
            PLParen : PName name : PRParen : rest                             ->
              entryWithChildren "operator in import list" name rest
            PName name : rest                                                 ->
              entryWithChildren "name in import list" name rest
            PType : PName name : rest                                         ->
              entryWithoutChildren name rest
            PType : PLParen : Pos _ (tokToName -> Just name) : PRParen : rest ->
              entryWithoutChildren name rest
            rest                                                              ->
              throwError $ "Unrecognized shape of import list:" <+> ppTokens rest
          where
            importList :: UnresolvedImportList
            importList = ImportList
              { ilEntries       = acc
              , ilImportType    = importType
              , ilImportedNames = ()
              }
            entryWithChildren :: Doc -> Text -> [Token] -> m (UnresolvedImportList, [Token])
            entryWithChildren descr name rest = do
              (children, rest') <- analyzeChildren descr $ dropNLs rest
              name'             <- mkUnqualName name
              let newEntry = EntryWithChildren name' children
              findImportListEntries importType (KM.insert newEntry acc) $ dropCommas $ dropNLs rest'
            entryWithoutChildren :: Text -> [Token] -> m (UnresolvedImportList, [Token])
            entryWithoutChildren name rest = do
              name' <- mkUnqualName name
              let newEntry = mkEntryWithoutChildren name'
              findImportListEntries importType (KM.insert newEntry acc) $ dropCommas $ dropNLs rest
        mkUnqualName :: Text -> m UnqualifiedSymbolName
        mkUnqualName name =
          case mkUnqualifiedSymbolName (mkSymbolName name) of
            Nothing    ->
              throwError $ "Invalid qualified entry on import list:" <+> docFromText name
            Just name' -> return name'

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
      throwError $ "Unrecognized shape of export list:" <+> ppTokens toks
  where
    -- Analyze comma-separated list of entries like
    -- - Foo
    -- - Foo(Bar, Baz)
    -- - Quux(..)
    -- - pattern PFoo
    -- - module Data.Foo.Bar
    go :: KeyMap (EntryWithChildren SymbolName)
       -> Set ModuleName
       -> [Token]
       -> m ModuleExports
    go entries reexports = \case
      []                                                                   ->
        pure exports
      [PRParen]                                                            ->
        pure exports
      PLParen : PName name : PRParen : rest                                ->
        entryWithChildren "operator in export list" name rest
      PLParen : Pos _ (tokToName -> Just name) : PRParen : rest            ->
        entryWithChildren "operator in export list" name rest
      PName name : rest                                                    ->
        entryWithChildren "name in export list" name rest
      PPattern : PName name : rest                                         ->
        entryWithoutChidren name rest
      PPattern : PLParen : Pos _ (tokToName -> Just name) : PRParen : rest ->
        entryWithoutChidren name rest
      PType : PName name : rest                                            ->
        entryWithoutChidren name rest
      PType : PLParen : Pos _ (tokToName -> Just name) : PRParen : rest    ->
        entryWithoutChidren name rest
      PModule : PName name : rest                                          ->
        consumeComma entries (newReexports <> reexports) rest
        where
          modName = mkModuleName name
          newReexports :: Set ModuleName
          newReexports
            = S.fromList
            $ toList
            $ M.findWithDefault (modName :| []) (mkImportQualifier modName) importQualifiers
      toks                                                                   ->
        throwError $ "Unrecognized export list structure:" <+> ppTokens toks
      where
        exports :: ModuleExports
        exports = ModuleExports
          { meExportedEntries    = entries
          , meReexports          = reexports
          , meHasWildcardExports = getAny $ foldMap exportsAllChildren entries
          }
        entryWithChildren :: Doc -> Text -> [Token] -> m ModuleExports
        entryWithChildren listType name rest = do
          (children, rest') <- analyzeChildren listType rest
          let newEntry = EntryWithChildren (mkSymbolName name) children
          consumeComma (KM.insert newEntry entries) reexports rest'
        entryWithoutChidren :: Text -> [Token] -> m ModuleExports
        entryWithoutChidren name rest = do
          let newEntry = mkEntryWithoutChildren $ mkSymbolName name
          consumeComma (KM.insert newEntry entries) reexports rest
        exportsAllChildren :: EntryWithChildren a -> Any
        exportsAllChildren (EntryWithChildren _ visibility) =
          maybe mempty isExportAllChildren visibility
          where
            isExportAllChildren VisibleAllChildren          = Any True
            isExportAllChildren (VisibleSpecificChildren _) = mempty
    -- Continue parsing by consuming comma delimiter.
    consumeComma
      :: KeyMap (EntryWithChildren SymbolName)
      -> Set ModuleName
      -> [Token]
      -> m ModuleExports
    consumeComma entries reexports = go entries reexports . dropCommas

analyzeChildren
  :: forall m. (MonadError Doc m, MonadLog m)
  => Doc -> [Token] -> m (Maybe ChildrenVisibility, [Token])
analyzeChildren listType toks = do
  logDebug $ "[analyzeChildren] toks =" <+> ppTokens toks
  case toks of
    []                                               -> pure (Nothing, [])
    toks@(PComma : _)                                -> pure (Nothing, toks)
    toks@(PRParen : _)                               -> pure (Nothing, toks)
    toks@(PName _ : _)                               -> pure (Nothing, toks)
    toks@(PModule : _)                               -> pure (Nothing, toks)
    toks@(PPattern : _)                              -> pure (Nothing, toks)
    toks@(PType : _)                                 -> pure (Nothing, toks)
    PLParen : PName ".." : PRParen : rest            -> pure (Just VisibleAllChildren, rest)
    PLParen : PRParen : rest                         -> pure (Nothing, rest)
    PLParen : rest@(PName name : _)
      | isAsciiName name -> do
        (children, rest') <- extractChildren mempty rest
        pure (Just $ VisibleSpecificChildren children, rest')
      | otherwise        -> pure (Nothing, toks)
    PLParen : rest@(PLParen : Pos _ (tokToName -> Just name) : PRParen : _)
      | isAsciiName name ->
        pure (Nothing, toks)
      | otherwise        -> do
        (children, rest') <- extractChildren mempty rest
        pure (Just $ VisibleSpecificChildren children, rest')
    toks ->
      throwError $ "Cannot handle children of" <+> listType <> ":" <+> ppTokens toks
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
          extractChildren (S.insert name' names) $ dropCommas rest
      PLParen : Pos _ (tokToName -> Just name) : PRParen : rest
        | Just name' <- mkUnqualifiedSymbolName $ mkSymbolName name ->
          extractChildren (S.insert name' names) $ dropCommas rest
      toks           ->
        throwError $ "Unrecognized children list structure:" <+> ppTokens toks

isAsciiName :: Text -> Bool
isAsciiName = T.all (\c -> isAlphaNum c || c == '\'' || c == '_' || c == '#')

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

ppTokens :: [Token] -> Doc
ppTokens = pretty . Tokens . take 16

-- | Drop prefix of newlines.
dropNLs :: [Token] -> [Token]
dropNLs (Pos _ (Newline _) : ts) = dropNLs ts
dropNLs ts                       = ts

dropCommas :: [Token] -> [Token]
dropCommas (Pos _ Comma : ts) = dropCommas ts
dropCommas ts                 = ts
