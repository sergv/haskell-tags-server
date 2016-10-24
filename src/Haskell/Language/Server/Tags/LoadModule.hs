----------------------------------------------------------------------------
-- |
-- Module      :  Server.Tags.LoadModule
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Haskell.Language.Server.Tags.LoadModule
  ( loadModule
  , loadModuleFromSource
  , resolveModuleExports
  -- * Utils
  , vanillaExtensions
  , hsBootExtensions
  ) where

import Control.Arrow ((&&&), first)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.State.Strict as SS
import Data.Foldable
import Data.Functor.Product (Product(..))
import Data.List.Extra (nubOrd)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe hiding (Maybe(Just))
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Traversable (for)
import System.FilePath
import qualified Text.PrettyPrint.Leijen.Text as PP
import Text.PrettyPrint.Leijen.Text.Utils

import FastTags (tokenizeInput, processTokens)
import Token (Token)

import Control.Monad.Filesystem (MonadFS)
import qualified Control.Monad.Filesystem as MonadFS
import Control.Monad.Logging
import Data.Foldable.Ext
import qualified Data.KeyMap as KM
import Data.Map.NonEmpty (NonEmptyMap)
import qualified Data.Map.NonEmpty as NEMap
import Data.MonoidalMap (MonoidalMap)
import qualified Data.MonoidalMap as MM
import Data.SubkeyMap (SubkeyMap)
import qualified Data.SubkeyMap as SubkeyMap
import Data.SymbolMap (SymbolMap)
import qualified Data.SymbolMap as SM
import Data.Symbols
import Haskell.Language.Server.Tags.AnalyzeHeader
import Haskell.Language.Server.Tags.Types

vanillaExtensions :: Set String
vanillaExtensions = S.fromList [".hs", ".lhs", ".hsc", ".chs"]

hsBootExtensions :: Set String
hsBootExtensions = S.fromList [".hs-boot", ".lhs-boot"]

defaultModuleName :: ModuleName
defaultModuleName = mkModuleName "Main"

-- | Fetch module by it's name from cache or load it. Check modification time
-- of module files and reload if anything changed
loadModule
  :: forall m. (MonadError Doc m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => ImportKey
  -> m (NonEmpty ResolvedModule)
loadModule key@ImportKey{ikModuleName, ikImportTarget} = do
  logDebug $ "[loadModule] loading" <+> pretty ikModuleName
  s <- get
  if key `M.member` tssLoadsInProgress s
  then
    throwError $ "Import cycle detected: import of" <+> PP.dquotes (pretty key) <+> "is already in progress." <+>
      ppFoldableWithHeader "All imports in progress:" (ppNEMap <$> tssLoadsInProgress s)
  else do
    mods       <- case SubkeyMap.lookup key (tssLoadedModules s) of
                    Nothing   -> doLoad key
                    Just mods -> for mods (reloadIfNecessary key)
    modify (\s -> s { tssLoadedModules   = SubkeyMap.insert key mods $ tssLoadedModules s })
    loadedMods <- gets (SubkeyMap.keys . tssLoadedModules)
    logDebug $ "[loadModule] loaded modules:" <+> ppList loadedMods
    pure mods
  where
    extensions :: Set String
    extensions = case ikImportTarget of
                   VanillaModule -> vanillaExtensions
                   HsBootModule  -> hsBootExtensions
    isHaskellSource :: FilePath -> Maybe FilePath
    isHaskellSource path
      | takeExtension path `S.member` extensions = Just path
      | otherwise                                = Nothing
    doLoad :: ImportKey -> m (NonEmpty ResolvedModule)
    doLoad key@ImportKey{ikModuleName} = do
      logInfo $ "[loadModule.doLoad] loading module" <+> showDoc ikModuleName
      srcDirs <- asks tsconfSourceDirectories
      let possibleShallowPaths = [ root </> filenamePart <> ext
                                 | root <- toList srcDirs
                                 , ext  <- toList extensions
                                 ]
      shallowPaths   <- filterM MonadFS.doesFileExist possibleShallowPaths
      recursiveDirs  <- asks tsconfRecursiveSourceDirectories
      recursivePaths <- foldMapA (MonadFS.findRec MonadFS.isNotIgnoredDir isHaskellSource) recursiveDirs

      let allPaths = nubOrd $ shallowPaths ++ recursivePaths
      case allPaths of
        []     -> throwError $ ppFoldableWithHeader
                    ("Cannot load module " <> pretty ikModuleName <> ": no attempted paths exist:")
                    allPaths
        p : ps -> traverse (loadModuleFromFile key Nothing) (p :| ps)
      where
        filenamePart :: FilePath
        filenamePart = T.unpack
                     $ T.map (\c -> if c == '.' then pathSeparator else c)
                     $ getModuleName ikModuleName

reloadIfNecessary
  :: (MonadError Doc m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => ImportKey
  -> ResolvedModule
  -> m ResolvedModule
reloadIfNecessary key m = do
  (needsReloading, modifTime) <- moduleNeedsReloading m
  if needsReloading
  then do
    logInfo $ "[reloadIfNecessary] reloading module" <+> showDoc (mhModName $ modHeader m)
    loadModuleFromFile key (Just modifTime) (modFile m)
  else pure m

loadModuleFromFile
  :: (MonadError Doc m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => ImportKey
  -> Maybe UTCTime
  -> FilePath
  -> m ResolvedModule
loadModuleFromFile key@ImportKey{ikModuleName} modifTime filename = do
  logInfo $ "[loadModuleFromFile] loading file " <> showDoc filename
  modifTime'    <- maybe (MonadFS.getModificationTime filename) pure modifTime
  source        <- MonadFS.readFile filename
  unresolvedMod <- loadModuleFromSource (Just ikModuleName) modifTime' filename source
  modify $ \s -> s
    { tssLoadsInProgress = M.insertWith NEMap.union key (NEMap.singleton filename unresolvedMod) $ tssLoadsInProgress s }
  resolved      <- resolveModuleExports checkLoadingModules loadModule unresolvedMod
  modify $ \s -> s
    { tssLoadsInProgress = M.update f key $ tssLoadsInProgress s }
  pure resolved
  where
    f :: NonEmptyMap FilePath v -> Maybe (NonEmptyMap FilePath v)
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
  :: (MonadError Doc m, MonadLog m)
  => Maybe ModuleName
  -> UTCTime
  -> FilePath
  -> T.Text
  -> m UnresolvedModule
loadModuleFromSource suggestedModuleName modifTime filename source = do
  tokens <- either (throwError . docFromString) pure $ tokenizeInput filename False source
  makeModule suggestedModuleName modifTime filename tokens

makeModule
  :: (MonadError Doc m, MonadLog m)
  => Maybe ModuleName -- ^ Suggested module name, will be used if source does not define it's own name.
  -> UTCTime
  -> FilePath
  -> [Token]
  -> m UnresolvedModule
makeModule suggestedModuleName modifTime filename tokens = do
  (header, tokens') <- analyzeHeader tokens
  let syms           :: [ResolvedSymbol]
      errors         :: [String]
      (syms, errors) = first (fmap mkResolvedSymbol) $ processTokens tokens'
      allSymbols     :: SymbolMap
      allSymbols     = SM.fromList syms
  unless (null errors) $
    logError $ ppFoldableWithHeader
      ("fast-tags errors while loading" <+> showDoc filename)
      (map docFromString errors)
  case (suggestedModuleName, header) of
    (Just name, Just ModuleHeader{mhModName}) ->
      unless (name == mhModName) $
        throwError $ ppDict "Module name in file differs from the expected module name"
          [ "file"                 :-> pretty filename
          , "module name in file"  :-> pretty mhModName
          , "expected module name" :-> pretty name
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

resolveModuleExports
  :: forall m. (MonadError Doc m, MonadLog m)
  => (ImportKey -> m (Maybe (NonEmpty UnresolvedModule)))
  -> (ImportKey -> m (NonEmpty ResolvedModule))
  -> UnresolvedModule
  -> m ResolvedModule
resolveModuleExports checkIfModuleIsAlreadyBeingLoaded loadMod mod = do
  logDebug $ "[resolveModuleExports] resolving exports of module" <+> pretty (mhModName header)
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
      :: SubkeyMap ImportKey (NonEmpty UnresolvedImportSpec)
      -> m ( SubkeyMap ImportKey (NonEmpty ResolvedImportSpec)
           , [(ImportQualification, SymbolMap)]
           )
    resolveImports imports = do
      logDebug $ "[resolveModuleExports.resolveImports] analyzing imports of module" <+> pretty (mhModName header)
      SS.runStateT (SubkeyMap.traverseWithKey resolveImport imports) []
      where
        resolveImport
          :: ImportKey
          -> NonEmpty UnresolvedImportSpec
          -> SS.StateT [(ImportQualification, SymbolMap)] m (NonEmpty ResolvedImportSpec)
        resolveImport key importSpecs = do
          logDebug $ "[resolveModuleExports.resolveImports.resolveImport] resolving import" <+> pretty key
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
                (qual, symMap) <- filterVisibleNames importedMods importedNames spec
                -- Record which names enter current module's scope under certain
                -- qualification from import spec we're currently analyzing.
                modify ((qual, symMap) :)
                pure $ spec { ispecImportedNames = symMap}
            -- Non-standard code path for breaking import cycles: imported module
            -- is already being loaded. In order to break infinite loop, we must
            -- analyze it here and get all the names we interested in, whithout
            -- resolving the module!
            Just modules ->
              quasiResolveImportSpecWithLoadsInProgress (mhModName header) key modules importSpecs

    resolveSymbols :: UnresolvedModule -> m (SubkeyMap ImportKey (NonEmpty ResolvedImportSpec), SymbolMap)
    resolveSymbols Module{modHeader = header@ModuleHeader{mhImports, mhModName}, modAllSymbols} = do
      (resolvedImports, filteredNames) <- resolveImports mhImports
      logDebug $ "[resolveModuleExports.resolveImports] analyzing export list of module" <+> pretty mhModName
      case mhExports header of
        Nothing                                            ->
          pure (resolvedImports, modAllSymbols)
        Just ModuleExports{meExportedEntries, meReexports} -> do
          let lt, gt :: Set ModuleName
              (lt, reexportsItself, gt) = S.splitMember mhModName meReexports
              -- Names exported via module reexports.
              reexports :: SymbolMap
              reexports =
                foldMap
                  (\modName -> foldMap (foldMap ispecImportedNames) $ SubkeyMap.lookupSubkey modName resolvedImports)
                  (Pair lt gt)
              namesByNamespace :: Map (Maybe ImportQualifier) SymbolMap
              namesByNamespace = M.fromListWith (<>)
                               $ (Nothing, modAllSymbols)
                               : concatMap expandImportQualification filteredNames
              modulesInScope :: [ModuleName]
              modulesInScope = map ikModuleName $ SubkeyMap.keys mhImports
          -- Names exported from current module, grouped by export qualifier.
          (exportedNames :: MonoidalMap (Maybe ImportQualifier) (Set UnqualifiedSymbolName)) <-
            flip foldMapA meExportedEntries $ \entry -> do
              let (qualifier, name) = splitQualifiedPart $ entryName entry
              case M.lookup qualifier namesByNamespace of
                Nothing -> throwError $
                  "Internal error: export qualifier" <+> showDoc qualifier <+>
                  "has no corresponding qualified imports"
                Just sm -> do
                  names <- namesFromEntry modulesInScope sm $ entry { entryName = name }
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
          logDebug $ "[resolveModuleExports] extra parents =" <+> ppMap (ppSet <$> extra)
          logDebug $ "[resolveModuleExports] allSymbols' =" <+> pretty allSymbols'
          pure (resolvedImports, allSymbols')

-- | Take modules we're currently loading and try to infer names they're exporting
-- without fully resolving them. This is needed to break import cycles in simple
-- cases, that can be resolved. For now, only two simple cases are considered:
-- 1. Module that exports only names defined locally within it.
-- 2. Module may reexport arbitrary names, but we're only importing names
-- defined locally in the module.
quasiResolveImportSpecWithLoadsInProgress
  :: (MonadState [(ImportQualification, SymbolMap)] m, MonadError Doc m, Traversable t)
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
        flip foldMapA modules' $ \(Module{modHeader = ModuleHeader{mhExports, mhModName}, modAllSymbols}, localSymbolsNames) ->
          case mhExports of
            Nothing -> pure modAllSymbols
            Just ModuleExports{meExportedEntries}
              | exportedNames `S.isSubsetOf` localSymbolsNames -> do
                let unqualifiedExports :: Set (Maybe UnqualifiedSymbolName)
                    unqualifiedExports  = S.map mkUnqualifiedSymbolName exportedNames
                    unqualifiedExports' :: Set UnqualifiedSymbolName
                    unqualifiedExports' = S.mapMonotonic fromJust $ S.delete Nothing unqualifiedExports
                if S.member Nothing unqualifiedExports
                then throwError $ ppFoldableWithHeader
                  ("Cannot resolve import cycle: module" <+> pretty mhModName <+>
                   "reexports names from its imports:")
                  (filter isQualified $ toList exportedNames)
                else
                  pure $ modAllSymbols `SM.intersectAgainst` unqualifiedExports'
              | otherwise ->
                throwError errMsg
              where
                exportedNames :: Set SymbolName
                exportedNames = KM.keysSet meExportedEntries
                errMsg = ppFoldableWithHeader
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
        else throwError $
          "Import cycle detected: module" <+> PP.dquotes (pretty mainModuleName) <+>
             "imports names from module" <+> PP.dquotes (pretty key) <+>
             "that are not defined there and thus cannot be simply resolved." <+>
             ppFoldableWithHeader "Names:" importedNames
    modify ((ispecQualification, names) :)
    pure $ spec { ispecImportedNames = names }

-- | Some tags should get extra children-parent relationships, that were not
-- evident by looking at tag definitions alone.
--
-- For instance, in ghc 8.0 exports can be of the form "module Mod(FooType(.., Foo', Bar,)) where",
-- which means that additional pattern synonyms Foo' and Bar' are associated with
-- FooType from now on.
--
-- However, this effect should only be visible in module exports. Within module,
-- there should be no such link between extra children and a parent.
inferExtraParents :: UnresolvedModuleHeader -> Map UnqualifiedSymbolName (Set UnqualifiedSymbolName)
inferExtraParents ModuleHeader{mhExports} = M.fromListWith (<>) entries
  where
    entries :: [(UnqualifiedSymbolName, Set UnqualifiedSymbolName)]
    entries =
      flip foldMap mhExports $ \ModuleExports{meExportedEntries} ->
        flip foldMap meExportedEntries $ \EntryWithChildren{entryName, entryChildrenVisibility} ->
          case entryChildrenVisibility of
            Just VisibleAllChildren                         -> mempty
            Just (VisibleSpecificChildren _)                -> mempty
            Just (VisibleAllChildrenPlusSome extraChildren) ->
              [(snd $ splitQualifiedPart entryName, extraChildren)]
            Nothing                                         -> mempty

-- | Find out which names and under qualification come out of an import spec.
filterVisibleNames
  :: (MonadError Doc m, Foldable f)
  => f ModuleName
  -> SymbolMap            -- ^ All names from the set of imports.
  -> UnresolvedImportSpec -- ^ Import spec for this particular set of imports.
  -> m (ImportQualification, SymbolMap)
filterVisibleNames importedMods allImportedNames ImportSpec{ispecQualification, ispecImportList} = do
  visibleNames <- case ispecImportList of
    Nothing                                  ->
      pure allImportedNames
    Just ImportList{ilImportType, ilEntries} -> do
      let f = case ilImportType of
                Imported -> SM.leaveNames
                Hidden   -> SM.removeNames
      importedNames <- foldMapA (namesFromEntry importedMods allImportedNames) ilEntries
      pure $ f allImportedNames importedNames
  pure (ispecQualification, visibleNames)

-- | Get names referred to by @EntryWithChildren@ given a @SymbolMap@
-- of names currently in scope.
namesFromEntry
  :: (MonadError Doc m, Foldable f)
  => f ModuleName
  -> SymbolMap
  -> EntryWithChildren UnqualifiedSymbolName
  -> m (Set UnqualifiedSymbolName)
namesFromEntry importedMods allImportedNames (EntryWithChildren sym visibility) =
  S.insert sym <$>
  case visibility of
    Nothing                                         -> pure mempty
    Just VisibleAllChildren                         -> childrenSymbols
    Just (VisibleSpecificChildren children)         -> pure children
    Just (VisibleAllChildrenPlusSome extraChildren) ->
      (extraChildren <>) <$> childrenSymbols
  where
    childrenSymbols =
      case SM.lookupChildren sym allImportedNames of
        Nothing       -> throwError $ "Imported parent" <+> pretty sym <+>
          "not found in symbol map of modules" <+> ppList (toList importedMods)
        Just children -> pure children

