----------------------------------------------------------------------------
-- |
-- Module      :  Server.Tags.LoadModule
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskell.Language.Server.Tags.LoadModule
  ( loadModule
  , loadModuleFromSource
  , resolveModule
  ) where

import Prelude hiding (mod)

import Control.Arrow ((&&&), first)
import Control.Monad.Except (throwError)
import Control.Monad.Except.Ext
import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.State.Strict as SS

import Data.Foldable.Ext
import Data.Functor.Product (Product(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe hiding (Maybe(Just))
import Data.Semigroup as Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Combinators
import Data.Text.Prettyprint.Doc.Ext
import Data.Time.Clock (UTCTime)
import Data.Traversable (for)

import Haskell.Language.Lexer (tokenize)
import Haskell.Language.Lexer.FastTags (Pos, ServerToken, processTokens)

import Control.Monad.Filesystem (MonadFS)
import qualified Control.Monad.Filesystem as MonadFS
import Control.Monad.Filesystem.FileSearch
import Control.Monad.Logging
import Data.ErrorMessage
import qualified Data.KeyMap as KM
import Data.Map.NonEmpty (NonEmptyMap)
import qualified Data.Map.NonEmpty as NEMap
import Data.MonoidalMap (MonoidalMap)
import qualified Data.MonoidalMap as MM
import Data.Path
import Data.SubkeyMap (SubkeyMap)
import qualified Data.SubkeyMap as SubkeyMap
import Data.SymbolMap (SymbolMap)
import qualified Data.SymbolMap as SM
import Data.Symbols
import Haskell.Language.Server.Tags.AnalyzeHeader
import Haskell.Language.Server.Tags.Types
import Haskell.Language.Server.Tags.Types.Imports
import Haskell.Language.Server.Tags.Types.Modules

defaultModuleName :: ModuleName
defaultModuleName = mkModuleName "Main"

-- | Fetch module by it's name from cache or load it. Check modification time
-- of module files and reload if anything changed
loadModule
  :: forall m. (HasCallStack, MonadError ErrorMessage m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => ImportKey
  -> m (NonEmpty ResolvedModule)
loadModule key@ImportKey{ikModuleName, ikImportTarget} = do
  logDebug $ "[loadModule] loading" <+> pretty ikModuleName
  s <- get
  if key `M.member` tssLoadsInProgress s
  then
    throwErrorWithCallStack $ PP.hsep
      [ "Import cycle detected: import of"
      , PP.dquotes (pretty key)
      , "is already in progress."
      , ppFoldableHeaderWith ppNEMap "All imports in progress:" $ tssLoadsInProgress s
      ]
  else do
    mods <- case SubkeyMap.lookup key (tssLoadedModules s) of
      Nothing   -> doLoad
      Just mods -> for mods (reloadIfNecessary key)
    modify (\s' -> s' { tssLoadedModules   = SubkeyMap.insert key mods $ tssLoadedModules s' })
    loadedMods <- gets (SubkeyMap.keys . tssLoadedModules)
    logDebug $ "[loadModule] loaded modules:" <+> ppList loadedMods
    pure mods
  where
    doLoad :: HasCallStack => m (NonEmpty ResolvedModule)
    doLoad = do
      logInfo $ "[loadModule.doLoad] loading module" <+> pretty ikModuleName
      case T.splitOn "." $ getModuleName ikModuleName of
        []     -> throwErrorWithCallStack $ "Invalid module name:" <+> pretty ikModuleName
        x : xs -> do
          TagsServerConf{tsconfSearchDirs, tsconfVanillaExtensions, tsconfHsBootExtensions} <- ask
          candidates <- runFileSearchT tsconfSearchDirs $ findByPathSuffixSansExtension $ mkPathFragment $ x :| xs
          let extensions = case ikImportTarget of
                VanillaModule -> tsconfVanillaExtensions
                HsBootModule  -> tsconfHsBootExtensions
          case filter ((`S.member` extensions) . takeExtension) candidates of
            []     -> throwErrorWithCallStack $
              "Cannot load module " <> pretty ikModuleName Semigroup.<> ": no paths found"
            p : ps -> traverse (loadModuleFromFile key Nothing) $ p :| ps

reloadIfNecessary
  :: (HasCallStack, MonadError ErrorMessage m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => ImportKey
  -> ResolvedModule
  -> m ResolvedModule
reloadIfNecessary key m = do
  (needsReloading, modifTime) <- moduleNeedsReloading m
  if needsReloading
  then do
    logInfo $ "[reloadIfNecessary] reloading module" <+> pretty (mhModName $ modHeader m)
    loadModuleFromFile key (Just modifTime) (modFile m)
  else pure m

loadModuleFromFile
  :: (HasCallStack, MonadError ErrorMessage m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => ImportKey
  -> Maybe UTCTime
  -> FullPath
  -> m ResolvedModule
loadModuleFromFile key@ImportKey{ikModuleName} modifTime filename = do
  logInfo $ "[loadModuleFromFile] loading file" <+> pretty filename
  modifTime'    <- maybe (MonadFS.getModificationTime filename) pure modifTime
  source        <- MonadFS.readFile filename
  unresolvedMod <- loadModuleFromSource (Just ikModuleName) modifTime' filename source
  modify $ \s -> s
    { tssLoadsInProgress = M.insertWith NEMap.union key (NEMap.singleton filename unresolvedMod) $ tssLoadsInProgress s }
  resolved      <- resolveModule checkLoadingModules loadModule unresolvedMod
  modify $ \s -> s
    { tssLoadsInProgress = M.update f key $ tssLoadsInProgress s }
  pure resolved
  where
    f :: NonEmptyMap FullPath v -> Maybe (NonEmptyMap FullPath v)
    f = NEMap.delete filename

checkLoadingModules
  :: forall m. (MonadState TagsServerState m)
  => ImportKey
  -> m (Maybe (NonEmpty UnresolvedModule))
checkLoadingModules key = do
  s <- get
  pure $ case M.lookup key $ tssLoadsInProgress s of
    Just modules -> Just $ NEMap.elemsNE modules `appendToNE` loadedMods
      where
        loadedMods :: [UnresolvedModule]
        loadedMods = map (() <$)
                   $ foldMap toList
                   $ SubkeyMap.lookup key
                   $ tssLoadedModules s
    Nothing      -> Nothing

appendToNE :: NonEmpty a -> [a] -> NonEmpty a
appendToNE (x :| xs) ys = x :| (xs ++ ys)

-- | Load single module from the given file. Does not load any imports or exports.
-- Names in the loaded module are not resolved.
loadModuleFromSource
  :: (HasCallStack, MonadError ErrorMessage m, MonadLog m)
  => Maybe ModuleName
  -> UTCTime
  -> FullPath
  -> TL.Text
  -> m UnresolvedModule
loadModuleFromSource suggestedModuleName modifTime filename source = do
  tokens <- either throwError pure $ tokenize (T.unpack $ unFullPath filename) (TL.toStrict source)
  makeModule suggestedModuleName modifTime filename tokens

makeModule
  :: (HasCallStack, MonadError ErrorMessage m, MonadLog m)
  => Maybe ModuleName -- ^ Suggested module name, will be used if source does not define it's own name.
  -> UTCTime
  -> FullPath
  -> [Pos ServerToken]
  -> m UnresolvedModule
makeModule suggestedModuleName modifTime filename tokens = do
  (header, tokens') <- analyzeHeader tokens
  let syms           :: [ResolvedSymbol]
      errors         :: [String]
      (syms, errors) = first (fmap mkResolvedSymbol) $ processTokens tokens'
      allSymbols     :: SymbolMap
      allSymbols     = SM.fromList syms
  unless (null errors) $
    logError $ ppFoldableHeaderWith docFromString
      ("fast-tags errors while loading" <+> pretty filename)
      errors
  case (suggestedModuleName, header) of
    (Just name, Just ModuleHeader{mhModName}) ->
      unless (name == mhModName) $
        throwErrorWithCallStack $ ppDictHeader "Module name within file differs from the expected module name"
          [ "file"                 --> filename
          , "module name in file"  --> mhModName
          , "expected module name" --> name
          ]
    _ -> pure ()
  let moduleHeader :: UnresolvedModuleHeader
      moduleHeader = fromMaybe defaultHeader header
      mod :: UnresolvedModule
      mod = Module
        { modHeader           = moduleHeader
        , modAllSymbols       = allSymbols
        , modFile             = filename
        , modLastModified     = modifTime
        , modAllExportedNames = ()
        , modIsDirty          = False
        }
  logDebug $ "[makeModule] created module" <+> pretty mod
  pure mod
  where
    defaultHeader :: UnresolvedModuleHeader
    defaultHeader = ModuleHeader
      { mhModName          = fromMaybe defaultModuleName suggestedModuleName
      , mhImports          = mempty
      , mhImportQualifiers = mempty
      , mhExports          = Nothing
      }

resolveModule
  :: forall m. (HasCallStack, MonadError ErrorMessage m, MonadLog m)
  => (ImportKey -> m (Maybe (NonEmpty UnresolvedModule)))
  -> (ImportKey -> m (NonEmpty ResolvedModule))
  -> UnresolvedModule
  -> m ResolvedModule
resolveModule checkIfModuleIsAlreadyBeingLoaded loadMod mod = do
  logDebug $ "[resolveModule] resolving names of module" <+> pretty (mhModName header)
  (imports, symbols) <- resolveSymbols mod
  pure $ mod
    { modHeader           = header { mhImports = imports }
    , modAllExportedNames = symbols
    }
  where
    header :: UnresolvedModuleHeader
    header = modHeader mod
    expandImportQualification :: forall a. (ImportQualification, a) -> [(Maybe ImportQualifier, a)]
    expandImportQualification = \case
      (Unqualified, x)                   -> [(Nothing, x)]
      (Qualified q, x)                   -> [(Just q, x)]
      (BothQualifiedAndUnqualified q, x) -> [(Just q, x), (Nothing, x)]

    resolveImports
      :: HasCallStack
      => SubkeyMap ImportKey (NonEmpty UnresolvedImportSpec)
      -> m ( SubkeyMap ImportKey (NonEmpty ResolvedImportSpec)
           , [(ImportQualification, SymbolMap)]
           )
    resolveImports imports = do
      logDebug $ "[resolveModule.resolveImports] analysing imports of module" <+> pretty (mhModName header)
      SS.runStateT (SubkeyMap.traverseWithKey resolveImport imports) []
      where
        resolveImport
          :: HasCallStack
          => ImportKey
          -> NonEmpty UnresolvedImportSpec
          -> SS.StateT [(ImportQualification, SymbolMap)] m (NonEmpty ResolvedImportSpec)
        resolveImport key importSpecs = do
          logDebug $ "[resolveModule.resolveImports.resolveImport] resolving import" <+> pretty key
          isBeingLoaded <- lift $ checkIfModuleIsAlreadyBeingLoaded key
          case isBeingLoaded of
            -- Standard code path: imported modules are already loaded and
            -- resolved, use what was resolved.
            Nothing -> do
              modules <- lift $ loadMod key
              let importedNames :: SymbolMap
                  importedNames = foldMap modAllExportedNames modules
                  importedMods :: NonEmpty ModuleName
                  importedMods = mhModName . modHeader <$> modules
              for importSpecs $ \spec -> do
                (qual, symMap) <- filterVisibleNames (mhModName header) importedMods importedNames spec
                -- Record which names enter current module's scope under certain
                -- qualification from import spec we're currently analysing.
                modify ((qual, symMap) :)
                pure $ spec { ispecImportedNames = symMap }
            -- Non-standard code path for breaking import cycles: imported module
            -- is already being loaded. In order to break infinite loop, we must
            -- analyse it here and get all the names we interested in, whithout
            -- resolving the module!
            Just modules ->
              quasiResolveImportSpecWithLoadsInProgress (mhModName header) key modules importSpecs

    resolveSymbols
      :: HasCallStack
      => UnresolvedModule -> m (SubkeyMap ImportKey (NonEmpty ResolvedImportSpec), SymbolMap)
    resolveSymbols Module{modHeader = header@ModuleHeader{mhImports, mhModName}, modAllSymbols} = do
      (resolvedImports, filteredNames) <- resolveImports mhImports
      logDebug $ "[resolveModule.resolveImports] analysing export list of module" <+> pretty mhModName
      logDebug $ "[resolveModule.resolveImports] resolved imports" <> PP.line <> ppSubkeyMapWith pretty pretty ppNE resolvedImports
      case mhExports header of
        Nothing                                            ->
          pure (resolvedImports, modAllSymbols)
        Just ModuleExports{meExportedEntries, meReexports} -> do
          logDebug $ "[resolveModule.resolveImports] meReexports =" <+> ppSet meReexports
          let lt, gt :: Set ModuleName
              (lt, reexportsItself, gt) = S.splitMember mhModName meReexports
              -- Names exported via module reexports.
              reexports :: SymbolMap
              reexports = resolveReexports resolvedImports $ Pair lt gt
              namesByNamespace :: Map (Maybe ImportQualifier) SymbolMap
              namesByNamespace = M.fromListWith (<>)
                               $ (Nothing, modAllSymbols)
                               : concatMap expandImportQualification filteredNames
              modulesInScope :: [ModuleName]
              modulesInScope = map ikModuleName $ SubkeyMap.keys mhImports
          -- Names exported from current module, grouped by export qualifier.
          (exportedNames :: MonoidalMap (Maybe ImportQualifier) (Set UnqualifiedSymbolName)) <-
            foldForA meExportedEntries $ \entry -> do
              let (qualifier, name) = splitQualifiedPart $ entryName entry
              case M.lookup qualifier namesByNamespace of
                Nothing -> throwErrorWithCallStack $ PP.hsep
                  [ "Internal error: export qualifier"
                  , PP.dquotes $ pretty qualifier
                  , "of entry"
                  , PP.dquotes $ pretty entry
                  , "has no corresponding qualified imports"
                  ]
                Just sm -> do
                  names <- namesFromEntry mhModName modulesInScope sm $ entry { entryName = name }
                  pure $ MM.singleton qualifier names

          let allSymbols  :: SymbolMap
              allSymbols  = mconcat
                [ if reexportsItself then modAllSymbols else mempty
                , reexports
                , fold
                $ M.intersectionWith SM.leaveNames namesByNamespace
                $ MM.unMonoidalMap exportedNames
                ]
              extra       :: Map UnqualifiedSymbolName (Set UnqualifiedSymbolName)
              extra       = inferExtraParents header
              allSymbols' :: SymbolMap
              allSymbols' = SM.registerChildren extra allSymbols
          logDebug $ "[resolveModule] extra parents =" <+> ppMapWith pretty ppSet extra
          logDebug $ "[resolveModule] allSymbols' =" <+> pretty allSymbols'
          pure (resolvedImports, allSymbols')

resolveReexports
  :: (Foldable f, Foldable g)
  => SubkeyMap ImportKey (f ResolvedImportSpec)
  -> g ModuleName
  -> SymbolMap
resolveReexports resolvedImports modNames =
  foldFor modNames $ \modName ->
    foldFor (SubkeyMap.lookupSubkey modName resolvedImports) $ \imports ->
      foldFor imports $ \ImportSpec{ispecImportedNames, ispecQualification} ->
        case ispecQualification of
          -- See https://ro-che.info/articles/2012-12-25-haskell-module-system-p1 for details.
          -- Excerpt from Haskell Report:
          -- “The form module M names the set of all entities that are in scope
          --  with both an unqualified name e and a qualified name M.e”.
          Qualified _                   -> mempty
          Unqualified                   -> ispecImportedNames
          BothQualifiedAndUnqualified _ -> ispecImportedNames

-- | Take modules we're currently loading and try to infer names they're exporting
-- without fully resolving them. This is needed to break import cycles in simple
-- cases, that can be resolved. For now, only two simple cases are considered:
-- 1. Module that exports only names defined locally within it.
-- 2. Module may reexport arbitrary names, but we're only importing names
-- defined locally in the module.
quasiResolveImportSpecWithLoadsInProgress
  :: (HasCallStack, MonadState [(ImportQualification, SymbolMap)] m, MonadError ErrorMessage m, Traversable t)
  => ModuleName
  -> ImportKey
  -> NonEmpty UnresolvedModule -- ^ Modules in progress that are being loaded and are going to be anayzed here
  -> t UnresolvedImportSpec    -- ^ Import specs to resolve
  -> m (t ResolvedImportSpec)
quasiResolveImportSpecWithLoadsInProgress mainModuleName key modules importSpecs = do
  let modules' :: NonEmpty (UnresolvedModule, Set SymbolName)
      modules' = (id &&& S.mapMonotonic getUnqualifiedSymbolName . SM.keysSet . modAllSymbols) <$> modules
      importsLocalSymbolsMap :: SymbolMap
      importsLocalSymbolsMap = foldMap (modAllSymbols . fst) modules'
      importsLocalSymbolsNames :: Set UnqualifiedSymbolName
      importsLocalSymbolsNames = SM.keysSet importsLocalSymbolsMap
  for importSpecs $ \spec@ImportSpec{ispecQualification, ispecImportList} -> do
    names <- case ispecImportList of
      -- If there's no import list then ensure that either
      -- 1. There's no export list and therefore all exported names
      -- must be defined locally.
      -- 2. There's an export list but it exports *only* names
      -- defined locally.
      Nothing ->
        foldForA modules' $ \(Module{modHeader = ModuleHeader{mhExports, mhModName}, modAllSymbols}, localSymbolsNames) ->
          case mhExports of
            Nothing -> pure modAllSymbols
            Just ModuleExports{meExportedEntries}
              | exportedNames `S.isSubsetOf` localSymbolsNames -> do
                let unqualifiedExports :: Set (Maybe UnqualifiedSymbolName)
                    unqualifiedExports  = S.map mkUnqualifiedSymbolName exportedNames
                    unqualifiedExports' :: Set UnqualifiedSymbolName
                    unqualifiedExports' = S.mapMonotonic fromJust $ S.delete Nothing unqualifiedExports
                if S.member Nothing unqualifiedExports
                then throwErrorWithCallStack $ ppFoldableHeader
                  ("Cannot resolve import cycle: module" <+> pretty mhModName <+>
                   "reexports names from its imports:")
                  (filter isQualified $ toList exportedNames)
                else
                  pure $ modAllSymbols `SM.intersectAgainst` unqualifiedExports'
              | otherwise ->
                throwErrorWithCallStack errMsg
              where
                exportedNames :: Set SymbolName
                exportedNames = KM.keysSet meExportedEntries
                errMsg = ppFoldableHeader
                  ("Cannot resolve import cycle: module" <+> pretty mhModName <+>
                   "exports names not defined locally:")
                  (exportedNames `S.difference` localSymbolsNames)
      -- If there's an import list then try resolving all imported
      -- names among local definitions af the imported module.
      Just ImportList{ilEntries} -> do
        let importedNames = KM.keysSet ilEntries
        if importedNames `S.isSubsetOf` importsLocalSymbolsNames
        then do
          let resolvedNames = importsLocalSymbolsMap `SM.intersectAgainst` importedNames
          pure resolvedNames
        else throwErrorWithCallStack $
          "Import cycle detected: module" <+> PP.dquotes (pretty mainModuleName) <+>
             "imports names from module" <+> PP.dquotes (pretty key) <+>
             "that are not defined there and thus cannot be simply resolved." <+>
             ppFoldableHeader "Names:" importedNames
    modify ((ispecQualification, names) :)
    pure $ spec { ispecImportedNames = names }

-- | Some tags should get extra children-parent relationships, that were not
-- evident by looking at tag definitions alone.
-- For instance, in ghc 8.0 exports can be of the form "module Mod(FooType(.., Foo', Bar,)) where",
-- which means that additional pattern synonyms Foo' and Bar' are associated with
-- FooType from now on.
-- However, this effect should only be visible in module exports. Within module,
-- there should be no such link between extra children and a parent.
inferExtraParents :: UnresolvedModuleHeader -> Map UnqualifiedSymbolName (Set UnqualifiedSymbolName)
inferExtraParents ModuleHeader{mhExports} = M.fromListWith (<>) entries
  where
    entries :: [(UnqualifiedSymbolName, Set UnqualifiedSymbolName)]
    entries =
      foldFor mhExports $ \ModuleExports{meExportedEntries} ->
        foldFor meExportedEntries $ \EntryWithChildren{entryName, entryChildrenVisibility} ->
          case entryChildrenVisibility of
            Just VisibleAllChildren                         -> mempty
            Just (VisibleSpecificChildren _)                -> mempty
            Just (VisibleAllChildrenPlusSome extraChildren) ->
              [(snd $ splitQualifiedPart entryName, extraChildren)]
            Nothing                                         -> mempty

-- | Find out which names and under qualification come out of an import spec.
filterVisibleNames
  :: (HasCallStack, MonadError ErrorMessage m, Foldable f)
  => ModuleName
  -> f ModuleName
  -> SymbolMap            -- ^ All names from the set of imports.
  -> UnresolvedImportSpec -- ^ Import spec for this particular set of imports.
  -> m (ImportQualification, SymbolMap)
filterVisibleNames moduleName importedMods allImportedNames ImportSpec{ispecQualification, ispecImportList} = do
  visibleNames <- case ispecImportList of
    Nothing                                  ->
      pure allImportedNames
    Just ImportList{ilImportType, ilEntries} -> do
      let f = case ilImportType of
                Imported -> SM.leaveNames
                Hidden   -> SM.removeNames
      importedNames <- foldMapA (namesFromEntry moduleName importedMods allImportedNames) ilEntries
      pure $ f allImportedNames importedNames
  pure (ispecQualification, visibleNames)

-- | Get names referred to by @EntryWithChildren@ given a @SymbolMap@
-- of names currently in scope.
namesFromEntry
  :: forall m f. (HasCallStack, MonadError ErrorMessage m, Foldable f)
  => ModuleName
  -> f ModuleName
  -> SymbolMap
  -> EntryWithChildren UnqualifiedSymbolName
  -> m (Set UnqualifiedSymbolName)
namesFromEntry moduleName importedMods allImportedNames (EntryWithChildren sym visibility) =
  S.insert sym <$>
  case visibility of
    Nothing                                         -> pure mempty
    Just VisibleAllChildren                         -> childrenSymbols
    Just (VisibleSpecificChildren children)         -> pure children
    Just (VisibleAllChildrenPlusSome extraChildren) ->
      (extraChildren <>) <$> childrenSymbols
  where
    childrenSymbols :: HasCallStack => m (Set UnqualifiedSymbolName)
    childrenSymbols =
      case SM.lookupChildren sym allImportedNames of
        Nothing       -> throwErrorWithCallStack $ ppFoldableHeader
          ("Imported parent" <+> pretty sym <+> "not found in symbol map of imports of module" <+> pretty moduleName <> ":")
          importedMods
        Just children -> pure children

