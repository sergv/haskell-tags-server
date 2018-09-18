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

import Control.Arrow (first)
import Control.Monad.Except.Ext
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer (MonadWriter(..))
import qualified Control.Monad.Writer as Lazy
import qualified Control.Monad.Writer.Strict as Strict

import qualified Data.ByteString as BS
import Data.Foldable.Ext
import Data.Functor.Product (Product(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe hiding (Maybe(Just))
import qualified Data.Monoid as Monoid
import Data.Semigroup as Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Ext
import Data.Time.Clock (UTCTime)
import Data.Traversable (for)
import Data.Void (Void)

import Haskell.Language.Lexer (tokenize)
import Haskell.Language.Lexer.FastTags (Pos, ServerToken, processTokens)
import qualified Haskell.Language.Lexer.FastTags as FastTags

import Control.Monad.Filesystem (MonadFS)
import qualified Control.Monad.Filesystem as MonadFS
import Control.Monad.Filesystem.FileSearch
import Control.Monad.Logging
import Data.ErrorMessage
import Data.KeyMap (KeyMap)
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
  :: forall m. (WithCallStack, MonadError ErrorMessage m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => ImportKey
  -> m (Maybe (NonEmpty ResolvedModule))
loadModule key@ImportKey{ikModuleName, ikImportTarget} = do
  logInfo $ "[loadModule] loading" <+> pretty ikModuleName
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
      Nothing -> do
        mods' <- doLoad
        for_ mods' $ \mods'' ->
          modify (\s' -> s' { tssLoadedModules = SubkeyMap.insert key mods'' $ tssLoadedModules s' })
        pure mods'
      Just ms -> do
        logDebug $ "[loadModule] module was loaded before, reusing:" <+> pretty key
        (ms', Any anyReloaded) <- Strict.runWriterT $ for ms $ \m -> do
          m' <- reloadIfNecessary key m
          case m' of
            Nothing  -> pure m
            Just m'' -> m'' <$ tell (Any True)
        when anyReloaded $
          modify $ \s' -> s' { tssLoadedModules = SubkeyMap.insert key ms' $ tssLoadedModules s' }
        pure $ Just ms'
    for mods $ \mods' -> do
      loadedMods <- gets (SubkeyMap.keys . tssLoadedModules)
      logDebug $ "[loadModule] loaded modules:" ## ppList loadedMods
      pure mods'
  where
    doLoad :: WithCallStack => m (Maybe (NonEmpty ResolvedModule))
    doLoad = do
      logDebug $ "[loadModule.doLoad] module was not loaded before, loading now:" <+> pretty ikModuleName
      case T.splitOn "." $ getModuleName ikModuleName of
        []     -> throwErrorWithCallStack $ "Invalid module name:" <+> pretty ikModuleName
        x : xs -> do
          TagsServerConf{tsconfSearchDirs, tsconfVanillaExtensions, tsconfHsBootExtensions, tsconfNameResolution} <- ask
          candidates <- runFileSearchT tsconfSearchDirs $
            findByPathSuffixSansExtension $ mkSinglePathFragment <$> x :| xs
          let extensions = case ikImportTarget of
                VanillaModule -> tsconfVanillaExtensions
                HsBootModule  -> tsconfHsBootExtensions
              msg        = "Cannot load module " <> pretty ikModuleName Semigroup.<> ": no paths found"
          case (tsconfNameResolution, filter ((`S.member` extensions) . takeExtension) candidates) of
            (NameResolutionStrict, []) -> throwErrorWithCallStack msg
            (NameResolutionLax,    []) -> do
              logWarning msg
              pure Nothing
            (_, p : ps)                ->
              Just <$> traverse (loadModuleFromFile key Nothing) (p :| ps)

-- TODO: consider using hashes to track whether a module needs reloading?
reloadIfNecessary
  :: (WithCallStack, MonadError ErrorMessage m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => ImportKey
  -> ResolvedModule
  -> m (Maybe ResolvedModule)
reloadIfNecessary key m = do
  (needsReloading, modifTime) <- moduleNeedsReloading m
  if needsReloading
  then do
    logInfo $ "[reloadIfNecessary] reloading module" <+> pretty (mhModName $ modHeader m)
    Just <$> loadModuleFromFile key (Just modifTime) (modFile m)
  else pure Nothing

loadModuleFromFile
  :: (WithCallStack, MonadError ErrorMessage m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
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
  :: forall m. MonadState TagsServerState m
  => ImportKey
  -> m (Maybe (NonEmpty UnresolvedModule, [ResolvedModule]))
checkLoadingModules key = do
  TagsServerState{tssLoadsInProgress, tssLoadedModules} <- get
  pure $ case M.lookup key tssLoadsInProgress of
    Just modules -> Just (NEMap.elemsNE modules, loadedMods)
      where
        loadedMods :: [ResolvedModule]
        loadedMods = foldMap toList $ SubkeyMap.lookup key tssLoadedModules
    Nothing      -> Nothing

-- | Load single module from the given file. Does not load any imports or exports.
-- Names in the loaded module are not resolved.
loadModuleFromSource
  :: (WithCallStack, MonadError ErrorMessage m, MonadLog m)
  => Maybe ModuleName
  -> UTCTime
  -> FullPath
  -> BS.ByteString
  -> m UnresolvedModule
loadModuleFromSource suggestedModuleName modifTime filename source =
  makeModule suggestedModuleName modifTime filename tokens
  where
    tokens =
      tokenize (T.unpack $ unFullPath filename) source

makeModule
  :: (WithCallStack, MonadError ErrorMessage m, MonadLog m)
  => Maybe ModuleName -- ^ Suggested module name, will be used if source does not define it's own name.
  -> UTCTime
  -> FullPath
  -> [Pos ServerToken]
  -> m UnresolvedModule
makeModule suggestedModuleName modifTime filename tokens = do
  (header, tokens') <- analyzeHeader tokens
  let syms           :: [ResolvedSymbol]
      errors         :: [Doc Void]
      (syms, errors) = first (fmap mkResolvedSymbol . FastTags.removeDuplicatePatterns)
                     $ processTokens tokens'
      allSymbols     :: SymbolMap
      allSymbols     = SM.fromList syms
  unless (null errors) $
    logError $ ppFoldableHeaderWith id
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
  logVerboseDebug $ "[makeModule] created module" <+> pretty mod
  pure mod
  where
    defaultHeader :: UnresolvedModuleHeader
    defaultHeader = ModuleHeader
      { mhModName          = fromMaybe defaultModuleName suggestedModuleName
      , mhImports          = mempty
      , mhImportQualifiers = mempty
      , mhExports          = NoExports
      }

resolveModule
  :: forall m. (WithCallStack, MonadError ErrorMessage m, MonadReader TagsServerConf m, MonadLog m)
  => (ImportKey -> m (Maybe (NonEmpty UnresolvedModule, [ResolvedModule])))
  -> (ImportKey -> m (Maybe (NonEmpty ResolvedModule)))
  -> UnresolvedModule
  -> m ResolvedModule
resolveModule checkIfModuleIsAlreadyBeingLoaded loadMod mod = do
  logDebug $ "[resolveModule] resolving names of module" <+> pretty (mhModName unresHeader)
  (imports, symbols) <- resolveSymbols mod
  logVerboseDebug $ ppDictHeader ("[resolveModule] Resolved items for module" <+> pretty (mhModName unresHeader))
    [ "imports"                          :-> ppSubkeyMapWith pretty pretty ppNE imports
    , "all symbols exported by a module" --> symbols
    ]
  pure $ mod
    { modHeader           = unresHeader { mhImports = imports }
    , modAllExportedNames = symbols
    }
  where
    unresHeader :: UnresolvedModuleHeader
    unresHeader = modHeader mod
    unresFile :: FullPath
    unresFile = modFile mod

    resolveImports
      :: WithCallStack
      => SubkeyMap ImportKey (NonEmpty UnresolvedImportSpec)
      -> m ( SubkeyMap ImportKey (NonEmpty ResolvedImportSpec)
           , [(ImportQualification, ModuleName, SymbolMap)]
           )
    resolveImports imports = do
      logDebug $
        "[resolveModule.resolveImports] analysing imports of module" <+> pretty (mhModName unresHeader) <+>
        "from" <+> pretty unresFile
      Lazy.runWriterT (SubkeyMap.traverseMaybeWithKey resolveImport imports)
      where
        resolveImport
          :: WithCallStack
          => ImportKey
          -> NonEmpty UnresolvedImportSpec
          -> Lazy.WriterT [(ImportQualification, ModuleName, SymbolMap)] m (Maybe (NonEmpty ResolvedImportSpec))
        resolveImport key importSpecs = do
          isBeingLoaded <- lift $ checkIfModuleIsAlreadyBeingLoaded key
          case isBeingLoaded of
            -- Standard code path: imported modules are already loaded and
            -- resolved, use what was resolved.
            Nothing -> do
              logDebug $ "[resolveModule.resolveImports.resolveImport] Resolving import" <+> pretty (ikModuleName key)
              modules <- lift $ loadMod key
              case modules of
                Nothing       -> do
                  -- The import is not found. Record that it brings no names
                  -- under relevant qualifiers so that later on those names
                  -- could be defaulted as coming from the export list.
                  for_ importSpecs $ \ImportSpec{ispecImportKey = ImportKey{ikModuleName = importedModName}, ispecQualification} ->
                    tell [(ispecQualification, importedModName, mempty)]
                  pure Nothing
                Just modules' -> do
                  let importedNames :: SymbolMap
                      importedNames = foldMap modAllExportedNames modules'
                      importedMods :: NonEmpty ModuleName
                      importedMods = mhModName . modHeader <$> modules'
                  fmap Just $ for importSpecs $ \spec@ImportSpec{ispecImportKey = ImportKey{ikModuleName = importedModName}, ispecQualification} -> do
                    symMap <- filterVisibleNames importedModName importedMods importedNames spec
                    -- Record which names enter current module's scope under certain
                    -- qualification from import spec we're currently analysing.
                    tell [(ispecQualification, importedModName, symMap)]
                    pure $ spec { ispecImportedNames = symMap }
            -- Non-standard code path for breaking import cycles: imported module
            -- is already being loaded. In order to break infinite loop, we must
            -- analyse it here and get all the names we interested in, whithout
            -- resolving the module!
            Just (toLoad, _alreadyLoaded) -> do
              let toResolve = filter ((unresFile /=) . modFile) $ toList toLoad
              logDebug $ "[resolveModule.resolveImports.resolveImport] Module" <+>
                pretty (ikModuleName key) <+>
                "is already being loaded. This happened while loading" <+> pretty
                (mhModName (modHeader mod)) <+> "from" <+> pretty (modFile mod) <> ". Initiating cycle resolution procedures for modules" ##
                pretty (map modFile toResolve)
              Just <$> quasiResolveImportSpecWithLoadsInProgress
                (lift . checkIfModuleIsAlreadyBeingLoaded)
                (lift . loadMod)
                (mhModName unresHeader)
                (modFile mod)
                key
                toResolve
                importSpecs

    resolveSymbols
      :: WithCallStack
      => UnresolvedModule -> m (SubkeyMap ImportKey (NonEmpty ResolvedImportSpec), SymbolMap)
    resolveSymbols Module{modHeader = header@ModuleHeader{mhImports, mhModName}, modAllSymbols} = do
      logDebug $ "[resolveModule.resolveSymbols] Resolving symbols of module" <+> pretty mhModName
      (resolvedImports, namesAndQualifiersFromImports) <- resolveImports mhImports
      logDebug $ "[resolveModule.resolveSymbols] analysing export list of module" <+> pretty mhModName
      logVerboseDebug $ "[resolveModule.resolveSymbols] Resolved imports" ## ppSubkeyMapWith pretty pretty ppNE resolvedImports
      case mhExports header of
        NoExports                                                     ->
          pure (resolvedImports, modAllSymbols)
        EmptyExports                                                  ->
          pure (resolvedImports, modAllSymbols)
        SpecificExports ModuleExports{meExportedEntries, meReexports} -> do
          logVerboseDebug $ "[resolveModule.resolveSymbols] reexports of module" <+> pretty mhModName <> ":" ## ppSet meReexports
          let lt, gt :: Set ModuleName
              (lt, reexportsItself, gt) = S.splitMember mhModName meReexports
              -- Names exported via module reexports.
              reexports :: SymbolMap
              reexports = resolveReexports resolvedImports $ Pair lt gt
              -- Names defined in current module grouped by their
              -- qualifier (a "namespace").
              namesInScopeByNamespace :: Map (Maybe ImportQualifier) SymbolMap
              namesInScopeByNamespace
                = M.fromListWith (<>)
                $ (Nothing, modAllSymbols)
                : (Just $ mkImportQualifier mhModName, modAllSymbols)
                : concatMap expandImportQualification namesAndQualifiersFromImports
              modulesInScope :: [ModuleName]
              modulesInScope = map ikModuleName $ SubkeyMap.keys mhImports
          -- Names exported from current module, grouped by export qualifier and
          -- resolving to a location within the export list.
          (exportedFromExportList :: MonoidalMap (Maybe ImportQualifier) (Map UnqualifiedSymbolName ResolvedSymbol)) <-
            foldForA meExportedEntries $ \entry -> do
              let (name, PosAndType pos typ)   = entryName entry
                  name' :: UnqualifiedSymbolName
                  (qualifier, name') = splitQualifiedPart name
              case M.lookup qualifier namesInScopeByNamespace of
                Nothing -> throwErrorWithCallStack $ ppDictHeader (PP.hsep
                  [ "Internal error: export qualifier"
                  , PP.dquotes $ pretty qualifier
                  , "has no corresponding qualified imports"
                  ])
                  [ "import entry"                  --> entry
                  , "namesInScopeByNamespace"       :-> ppMap namesInScopeByNamespace
                  , "namesAndQualifiersFromImports" --> namesAndQualifiersFromImports
                  ]
                Just sm -> do
                  presentChildren <- childrenNamesFromEntry mhModName modulesInScope sm $ name' <$ entry
                  let childrenType :: FastTags.Type
                      childrenType = case typ of
                        FastTags.Type   -> FastTags.Constructor
                        FastTags.Family -> FastTags.Type
                        typ'            -> typ'
                      names :: Map UnqualifiedSymbolName ResolvedSymbol
                      names
                        = M.insert name' (mkResolvedSymbolFromParts pos name' typ Nothing)
                        $ M.fromSet (\childName -> mkResolvedSymbolFromParts pos childName childrenType (Just name')) presentChildren
                  pure $ MM.singleton qualifier names
          let presentExports :: SymbolMap
              presentExports
                = fold
                $ M.intersectionWith SM.restrictKeys namesInScopeByNamespace
                $ fmap M.keysSet
                $ MM.unMonoidalMap exportedFromExportList
          logVerboseDebug $ "[resolveSymbols] exportedFromExportList =" ## ppMonoidalMapWith pretty ppMap exportedFromExportList
          let allSymbols  :: SymbolMap
              allSymbols  = mconcat
                [ if reexportsItself then modAllSymbols else mempty
                , reexports
                , presentExports
                ]
              extra       :: Map UnqualifiedSymbolName (Set UnqualifiedSymbolName)
              extra       = inferExtraParents header
              allSymbols' :: SymbolMap
              allSymbols' = SM.registerChildren extra allSymbols
          logVerboseDebug $ "[resolveModule.resolveSymbols] inferred extra parents =" ##
            ppMapWith pretty ppSet extra
          -- Names from default namespace that are exported but not
          -- imported/defined locally. We default them to tags that
          -- come from the corresponding export list.
          let unresolvedExports :: SymbolMap
              unresolvedExports =
                SM.withoutKeys
                  (SM.fromList (foldMap M.elems exportedFromExportList))
                  (SM.keysSet allSymbols')
          unless (SM.null unresolvedExports) $
            logWarning $ "[resolveSymbols] unresolved exports (will consider them coming from the export list) for" <+> pretty mhModName <> ":" ## pretty unresolvedExports
          pure (resolvedImports, allSymbols' <> unresolvedExports)

expandImportQualification
  :: forall a. (ImportQualification, ModuleName, a)
  -> [(Maybe ImportQualifier, a)]
expandImportQualification = \case
  (Unqualified, name, x)                   ->
    [(Just $ mkImportQualifier name, x), (Nothing, x)]
  (Qualified q, _, x)                      ->
    [(Just q, x)]
  (BothQualifiedAndUnqualified q, name, x) ->
    [(Just $ mkImportQualifier name, x), (Just q, x), (Nothing, x)]

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
  :: forall m f t. (WithCallStack, Traversable t, Foldable f)
  => (MonadWriter [(ImportQualification, ModuleName, SymbolMap)] m, MonadError ErrorMessage m, MonadReader TagsServerConf m, MonadLog m)
  => (ImportKey -> m (Maybe (NonEmpty UnresolvedModule, [ResolvedModule])))
  -> (ImportKey -> m (Maybe (NonEmpty ResolvedModule)))
  -> ModuleName                -- ^ Module we're currently analysing.
  -> FullPath
  -> ImportKey                 -- ^ Import of the main module that caused the cycle.
  -> f UnresolvedModule        -- ^ Modules in progress that are being loaded and are going to be anayzed here
  -> t UnresolvedImportSpec    -- ^ Import specs to resolve
  -> m (t ResolvedImportSpec)
quasiResolveImportSpecWithLoadsInProgress
  checkIfModuleIsAlreadyBeingLoaded
  loadMod
  mainModName
  mainModFile
  mainModImport
  modulesAlreadyLoading
  importSpecs =
  for importSpecs $ \spec@ImportSpec{ispecImportKey = ImportKey{ikModuleName = importedModName}, ispecQualification, ispecImportList} -> do
    names <- processImports $ case ispecImportList of
      -- If there's no import list then ensure that either:
      -- 1. There's no export list and therefore all exported names
      -- must be defined locally.
      -- 2. There's an export list but it exports *only* names
      -- defined locally.
      NoImportList              -> Nothing
      AssumedWildcardImportList -> Nothing
      SpecificImports ImportList{ilEntries, ilImportType} ->
        case ilImportType of
          Imported -> Just ilEntries
          Hidden   -> Nothing
    -- Record which names enter current module's scope under certain
    -- qualification from import spec we're currently analysing.
    tell [(ispecQualification, importedModName, names)]
    pure $ spec { ispecImportedNames = names }
  where
    processImports :: Maybe (KeyMap Set (EntryWithChildren () UnqualifiedSymbolName)) -> m SymbolMap
    processImports wantedNames =
      foldForA modulesAlreadyLoading $ \Module{modHeader = ModuleHeader{mhExports, mhModName = importedModName, mhImports}, modFile = importedModFile, modAllSymbols} ->
        case mhExports of
          NoExports    -> pure modAllSymbols
          EmptyExports -> pure modAllSymbols
          SpecificExports ModuleExports{meReexports, meExportedEntries}
            | S.null meReexports || allWantedNamesDefinedLocally
            , S.size unqualifiedExports == S.size exportedNames ->
              case wantedSet of
                Nothing -> pure $ modAllSymbols `SM.restrictKeys` unqualifiedExports
                Just wanted
                  | wanted `S.isSubsetOf` unqualifiedExports
                  -> pure $ modAllSymbols `SM.restrictKeys` wanted
                  | otherwise
                  -> pure mempty
                    -- -- TODO: decide whether to actually throw an error or return mempty.
                    -- -- pure mempty -- This module could not have been imported in reality - discard it.
                    -- throwErrorWithCallStack $ ppDictHeader
                    --   "This module could not have been imported in reality"
                    --   [ "importedModName"    --> importedModName
                    --   , "expected"           :-> ppSet wanted
                    --   , "meReexports"        :-> ppSet meReexports
                    --   , "unqualifiedExports" :-> ppSet unqualifiedExports
                    --   , "exportedNames"      :-> ppSet exportedNames
                    --   ]
            -- If all we have are reexports...
            | Just wantedNames' <- wantedNames
            , not (S.null meReexports) && not allWantedNamesDefinedLocally
            , S.null exportedNames -> do
            -- , S.size unqualifiedExports == S.size exportedNames ->
              let reexportedImports :: [UnresolvedImportSpec]
                  reexportedImports =
                    foldMap (foldMap toList) $ M.restrictKeys (SubkeyMap.toSubmap mhImports) meReexports
                  candidateReexports :: [UnresolvedImportSpec]
                  candidateReexports =
                    filter (canBringNamesIntoScope wantedNames') reexportedImports

              (canLoad :: [UnresolvedImportSpec], cannotLoad :: [ModuleName]) <-
                foldForA candidateReexports $ \imp@ImportSpec{ispecImportKey} -> do
                  isBeingLoaded <- checkIfModuleIsAlreadyBeingLoaded ispecImportKey
                  pure $ case isBeingLoaded of
                    Nothing -> -- Not being loaded, may load
                      ([imp], mempty)
                    Just (inProgress, _finishedLoading) -> -- Already being loaded - too bad
                      -- TODO: use '_finishedLoading' as it may help to
                      -- resolve some names.
                      (mempty, mhModName . modHeader <$> toList inProgress)
              res <- resolveLoop loadMod wantedNames' canLoad
              case res of
                Nothing -> do
                  logDebug $ ppDictHeader "Import cycle debug info"
                    [ "canLoad"            --> canLoad
                    , "cannotLoad"         --> cannotLoad
                    , "meReexports"        :-> ppSet meReexports
                    , "wantedNames'"       :-> ppKeyMapWith pretty pretty wantedNames'
                    , "reexportedImports"  --> reexportedImports
                    , "candidateReexports" --> candidateReexports
                    ]
                  throwErrorWithCallStack $ ppDictHeader
                    (PP.hsep
                      [ "Cannot resolve reexport import cycle: module"
                      , PP.dquotes (pretty mainModName)
                      , "imports names from"
                      , pretty mainModImport
                      , "that reexports names from modules that are already being loaded:"
                      ])
                    [ "Wanted names"   :-> ppKeyMapWith pretty pretty wantedNames'
                    , "Unable to load" --> cannotLoad
                    ]
                Just syms -> pure syms
            | otherwise -> do
              strictness <- asks tsconfNameResolution
              case strictness of
                NameResolutionStrict -> throwErrorWithCallStack errMsg
                NameResolutionLax    -> pure mempty
            where
              wantedSet :: Maybe (Set UnqualifiedSymbolName)
              wantedSet = KM.keysSet <$> wantedNames
              allWantedNamesDefinedLocally =
                maybe False (`SM.isSubsetNames` modAllSymbols) wantedSet

              exportedNames :: Set SymbolName
              exportedNames = KM.keysSet meExportedEntries
              unqualifiedExports :: Set UnqualifiedSymbolName
              unqualifiedExports
                = S.mapMonotonic fromJust
                $ S.delete Nothing
                $ S.map mkUnqualifiedSymbolName exportedNames
              errMsg :: Doc Void
              errMsg = ppFoldableHeader
                ("Cannot resolve import cycle: module" <+> PP.dquotes (pretty mainModName) <+> PP.parens (pretty mainModFile) <+>
                "imports names from" <+> pretty (ikModuleName mainModImport) <>
                 ", but a candidate for that import," <+> pretty importedModName <+> PP.parens (pretty importedModFile) <> ", exports names that are not defined locally:")
                (exportedNames `S.difference` S.mapMonotonic getUnqualifiedSymbolName (SM.keysSet modAllSymbols))

resolveLoop
  :: forall m. Monad m
  => (ImportKey -> m (Maybe (NonEmpty ResolvedModule)))
  -> KeyMap Set (EntryWithChildren () UnqualifiedSymbolName)
  -> [UnresolvedImportSpec]
  -> m (Maybe SymbolMap)
resolveLoop loadMod wantedNames = go (KM.keysSet wantedNames) mempty
  where
    go
      :: Set UnqualifiedSymbolName
      -> SymbolMap
      -> [UnresolvedImportSpec]
      -> m (Maybe SymbolMap)
    go wanted found = \case
      _ | S.null wanted -> pure $ Just found
      []                -> pure Nothing
      ImportSpec{ispecImportKey} : specs -> do
        mod <- loadMod ispecImportKey
        case mod of
          Nothing   -> go wanted  found  specs
          Just mods -> go wanted' found' specs
            where
              found' :: SymbolMap
              found' = foldMap ((`SM.restrictKeys` wanted) . modAllExportedNames) mods
              wanted' = wanted S.\\ SM.keysSet found'

canBringNamesIntoScope
  :: KeyMap Set (EntryWithChildren () UnqualifiedSymbolName)
  -> UnresolvedImportSpec
  -> Bool
canBringNamesIntoScope names ImportSpec{ispecImportList} =
  case ispecImportList of
    NoImportList                                        -> True
    AssumedWildcardImportList                           -> True
    SpecificImports ImportList{ilEntries, ilImportType} ->
      case ilImportType of
        Imported ->
          or $ M.intersectionWith
            canBringNamesViaImport
            (S.map entryChildrenVisibility <$> KM.toMap names)
            (S.map entryChildrenVisibility <$> KM.toMap ilEntries)
        Hidden   ->
          not $ M.null $ M.differenceWith
            hideNames
            (S.map entryChildrenVisibility <$> KM.toMap names)
            (S.map entryChildrenVisibility <$> KM.toMap ilEntries)
      where
        canBringNamesViaImport
          :: Set (Maybe (ChildrenVisibility ()))
          -> Set (Maybe (ChildrenVisibility ()))
          -> Bool
        canBringNamesViaImport wanted present =
          Nothing `S.member` wanted && Nothing `S.member` present ||
          presentWildcards ||
          not (S.null (wantedChildren `S.intersection` presentChildren))
          where
            wantedChildren, presentChildren :: Set UnqualifiedSymbolName
            (Monoid.Any presentWildcards, presentChildren) = foldMap analyse present
            (_,                           wantedChildren)  = foldMap analyse wanted
        hideNames
          :: Set (Maybe (ChildrenVisibility ()))
          -> Set (Maybe (ChildrenVisibility ()))
          -> Maybe (Set (Maybe (ChildrenVisibility ())))
        hideNames wanted present
          | Nothing `S.member` present || presentWildcards
          = Nothing
          | otherwise
          = nothingIfEmpty $ S.map (fmap (hideChildren presentChildren)) $ wanted S.\\ present
          where
            presentChildren :: Set UnqualifiedSymbolName
            (Monoid.Any presentWildcards, presentChildren) = foldMap analyse present
        analyse
          :: Maybe (ChildrenVisibility ())
          -> (Monoid.Any, Set UnqualifiedSymbolName)
        analyse = \case
          Nothing                                 -> (Monoid.Any False, mempty)
          Just VisibleAllChildren                 -> (Monoid.Any True, mempty)
          Just (VisibleSpecificChildren children) -> (mempty, M.keysSet children)
          Just (VisibleAllChildrenPlusSome extra) -> (Monoid.Any True, M.keysSet extra)

hideChildren :: Set UnqualifiedSymbolName -> ChildrenVisibility a -> ChildrenVisibility a
hideChildren toHide = \case
  VisibleAllChildren            -> VisibleAllChildren
  VisibleSpecificChildren xs    -> VisibleSpecificChildren $ M.withoutKeys xs toHide
  VisibleAllChildrenPlusSome xs -> VisibleAllChildrenPlusSome $ M.withoutKeys xs toHide

nothingIfEmpty :: Set a -> Maybe (Set a)
nothingIfEmpty xs
  | S.null xs = Nothing
  | otherwise = Just xs

-- | Some tags should get extra children-parent relationships, that were not
-- evident by looking at tag definitions alone.
-- For instance, in ghc 8.0 exports can be of the form
--
-- > module Mod(FooType(.., Foo', Bar,)) where
--
-- which means that additional pattern synonyms Foo' and Bar' are associated with
-- @FooType@ from now on.
--
-- However, this effect should only be visible in module exports.
-- Within module, there should be no such link between extra children
-- and a parent.
inferExtraParents :: UnresolvedModuleHeader -> Map UnqualifiedSymbolName (Set UnqualifiedSymbolName)
inferExtraParents ModuleHeader{mhExports} = M.fromListWith (<>) entries
  where
    entries :: [(UnqualifiedSymbolName, Set UnqualifiedSymbolName)]
    entries =
      foldFor mhExports $ \ModuleExports{meExportedEntries} ->
        foldFor meExportedEntries $ \EntryWithChildren{entryName = (name, _), entryChildrenVisibility} ->
          case entryChildrenVisibility of
            Just VisibleAllChildren                         -> mempty
            Just (VisibleSpecificChildren _)                -> mempty
            Just (VisibleAllChildrenPlusSome extraChildren) ->
              [(snd $ splitQualifiedPart name, M.keysSet extraChildren)]
            Nothing                                         -> mempty

-- | Find out which names under qualification come out of an import spec.
filterVisibleNames
  :: (WithCallStack, MonadError ErrorMessage m, MonadReader TagsServerConf m, Foldable f)
  => ModuleName
  -> f ModuleName         -- ^ Imported modules for error reporting.
  -> SymbolMap            -- ^ All names from the set of imports.
  -> UnresolvedImportSpec -- ^ Import spec for this particular set of imports.
  -> m SymbolMap
filterVisibleNames moduleName importedMods allImportedNames ImportSpec{ispecImportList} = do
  visibleNames <- case ispecImportList of
    NoImportList                                        ->
      pure allImportedNames
    AssumedWildcardImportList                           ->
      pure allImportedNames
    SpecificImports ImportList{ilImportType, ilEntries} -> do
      let f = case ilImportType of
                Imported -> SM.restrictKeys
                Hidden   -> SM.withoutKeys
      importedNames <- foldMapA (allNamesFromEntry moduleName importedMods allImportedNames) ilEntries
      pure $ f allImportedNames importedNames
  pure visibleNames

-- | Get names referred to by @EntryWithChildren@ given a @SymbolMap@
-- of names currently in scope.
allNamesFromEntry
  :: forall m f ann. (WithCallStack, MonadError ErrorMessage m, MonadReader TagsServerConf m, Foldable f)
  => ModuleName
  -> f ModuleName -- ^ Imported modules for error reporting.
  -> SymbolMap
  -> EntryWithChildren ann UnqualifiedSymbolName
  -> m (Set UnqualifiedSymbolName)
allNamesFromEntry moduleName importedMods allImportedNames entry@(EntryWithChildren sym _) =
  S.insert sym <$>
  childrenNamesFromEntry moduleName importedMods allImportedNames entry

-- | Get children names only referred to by @EntryWithChildren@ given
-- a @SymbolMap@ of names currently in scope.
childrenNamesFromEntry
  :: forall m f ann. (WithCallStack, MonadError ErrorMessage m, MonadReader TagsServerConf m, Foldable f)
  => ModuleName
  -> f ModuleName -- ^ Imported modules for error reporting.
  -> SymbolMap
  -> EntryWithChildren ann UnqualifiedSymbolName
  -> m (Set UnqualifiedSymbolName)
childrenNamesFromEntry moduleName importedMods allImportedNames (EntryWithChildren sym visibility) =
  case visibility of
    Nothing                                         -> pure mempty
    Just VisibleAllChildren                         -> childrenSymbols
    Just (VisibleSpecificChildren children)         -> pure $ M.keysSet children
    Just (VisibleAllChildrenPlusSome extraChildren) ->
      (M.keysSet extraChildren <>) <$> childrenSymbols
  where
    childrenSymbols :: WithCallStack => m (Set UnqualifiedSymbolName)
    childrenSymbols = do
      nameResolution <- asks tsconfNameResolution
      case (nameResolution, SM.lookupChildren sym allImportedNames) of
        (NameResolutionStrict, Nothing)       ->
          throwErrorWithCallStack $ ppFoldableHeader
            ("Imported symbol with children" <+> PP.squotes (pretty sym) <+>
             "not found in the imports symbol map for the module" <+> PP.squotes (pretty moduleName) <> ":")
            importedMods
        (NameResolutionLax,    Nothing)       -> pure mempty
        (_,                    Just children) -> pure children
