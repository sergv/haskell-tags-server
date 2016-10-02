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

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Server.Tags.LoadModule (loadModule) where

import Control.Arrow (first)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe hiding (Maybe(Just))
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Traversable (for)
import System.FilePath
import qualified Text.PrettyPrint.Leijen.Text as PP

import FastTags (tokenizeInput, processTokens)
import Token (Token)

import Control.Monad.Filesystem (MonadFS)
import qualified Control.Monad.Filesystem as MonadFS
import Control.Monad.Logging
import Data.Foldable.Ext
import Data.MonoidalMap (MonoidalMap)
import qualified Data.MonoidalMap as MM
import Data.SymbolMap (SymbolMap)
import qualified Data.SymbolMap as SM
import Data.Symbols
import Server.Tags.AnalyzeHeader
import Server.Tags.Types
import Text.PrettyPrint.Leijen.Text.Utils

recognizedExtensions :: [String]
recognizedExtensions = ["hs", "lhs", "hsc", "chs"]

-- | Fetch module by it's name from cache or load it. Check modification time
-- of module files and reload if anything changed
loadModule
  :: forall m. (MonadError Doc m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => ModuleName
  -> m (NonEmpty Module)
loadModule name = do
  modules <- gets tssLoadedModules
  mods    <- case M.lookup name modules of
                Nothing   -> doLoad name
                Just mods -> for mods reloadIfFileChanged
  modify (\s -> s { tssLoadedModules = M.insert name mods $ tssLoadedModules s })
  loadedMods <- gets (M.keys . tssLoadedModules)
  logDebug $ "[loadModule] loaded modules are:" <+> ppList loadedMods
  return mods
  where
    doLoad :: ModuleName -> m (NonEmpty Module)
    doLoad name = do
      logInfo $ "[loadModule.doLoad] loading module" <+> showDoc name
      srcDirs <- asks tsconfSourceDirectories
      let possiblePaths = [ root </> filenamePart <.> ext
                          | root <- S.toList srcDirs
                          , ext  <- recognizedExtensions
                          ]
      actualPaths <- filterM MonadFS.doesFileExist possiblePaths
      case actualPaths of
        []     -> throwError $
                  "Cannot load module " <> pretty name <> ": no attempted paths exist:" PP.<$>
                    PP.nest 2 (pretty possiblePaths)
        p : ps -> traverse (loadModuleFromFile name Nothing) (p :| ps)
      where
        filenamePart :: FilePath
        filenamePart = T.unpack
                     $ T.map (\c -> if c == '.' then pathSeparator else c)
                     $ getModuleName name

reloadIfFileChanged
  :: (MonadError Doc m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => Module
  -> m Module
reloadIfFileChanged m = do
  (needsReloading, modifTime) <- moduleNeedsReloading m
  if needsReloading
  then do
    logInfo $ "[reloadIfFileChanged] reloading module" <+> showDoc (mhModName $ modHeader m)
    loadModuleFromFile (mhModName (modHeader m)) (Just modifTime) $ modFile m
  else return m

-- | Recursively load module from file - load given filename, then load all its
-- imports.
loadModuleFromFile
  :: (MonadError Doc m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => ModuleName
  -> Maybe UTCTime
  -> FilePath
  -> m Module
loadModuleFromFile moduleName modifTime filename = do
  logInfo $ "[loadModuleFromFile] loading file " <> showDoc filename
  source     <- MonadFS.readFile filename
  modifTime' <- maybe (MonadFS.getModificationTime filename) return modifTime
  tokens     <- either (throwError . docFromString) return $ tokenizeInput filename False source
  makeModule moduleName modifTime' filename tokens

makeModule
  :: (MonadError Doc m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => ModuleName
  -> UTCTime
  -> FilePath
  -> [Token]
  -> m Module
makeModule moduleName modifTime filename tokens = do
  (header, tokens') <- analyzeHeader tokens
  let syms           :: [ResolvedSymbol]
      errors         :: [String]
      (syms, errors) = first (fmap mkSymbol) $ processTokens tokens'
      allSymbols     :: SymbolMap
      allSymbols     = SM.fromList syms
  unless (null errors) $
    logError $ ppListWithHeader
      ("fast-tags errors while loading" <+> showDoc filename)
      (map docFromString errors)
  case header of
    Nothing                      -> return ()
    Just ModuleHeader{mhModName} ->
      unless (moduleName == mhModName) $
        throwError $ ppDict "Module name in file differst from the expected module name"
          [ "file"                 :-> pretty filename
          , "module name in file"  :-> pretty mhModName
          , "expected module name" :-> pretty moduleName
          ]
  let moduleHeader = fromMaybe defaultHeader header
  exportedNames <- resolveModuleExports moduleHeader allSymbols
  let mod = Module
        { modHeader          = moduleHeader
        , modAllSymbols      = allSymbols
        , modFile            = filename
        , modLastModified    = modifTime
        , meAllExportedNames = exportedNames
        , meIsDirty          = False
        }
  logDebug $ "[loadModuleFromFile] loaded module" <+> pretty mod
  return mod
  where
    defaultHeader  :: ModuleHeader
    defaultHeader  = ModuleHeader
      { mhModName          = moduleName
      , mhImports          = mempty
      , mhImportQualifiers = mempty
      , mhExports          = Nothing
      }

resolveModuleExports
  :: (MonadError Doc m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => ModuleHeader
  -> SymbolMap
  -> m SymbolMap
resolveModuleExports header@ModuleHeader{mhImports} allLocalSymbols =
  case mhExports header of
    Nothing                                            ->
      pure allLocalSymbols
    Just ModuleExports{meExportedEntries, meReexports} -> do
      imports <- M.traverseWithKey (\modName importSpecs -> (importSpecs,) <$> loadModule modName) mhImports
      let (lt, reexportsItself, gt) = S.splitMember (mhModName header) meReexports
      exports <- (<>)
                   <$> foldMapA (fmap (foldMap meAllExportedNames) . loadModule) lt
                   <*> foldMapA (fmap (foldMap meAllExportedNames) . loadModule) gt
      (filteredNames :: [(ImportQualification, SymbolMap)]) <-
        flip foldMapA imports $ \(importSpecs, modules) -> do
          let importedNames :: SymbolMap
              importedNames = foldMap meAllExportedNames modules
              importedMods :: [ModuleName]
              importedMods = map (mhModName . modHeader) $ toList modules
          traverse (filterVisibleNames importedMods importedNames) $ toList importSpecs
      let namesByNamespace :: Map (Maybe ImportQualifier) SymbolMap
          namesByNamespace = M.fromListWith (<>)
                           $ (Nothing, allLocalSymbols)
                           : concatMap expandImportQualification filteredNames

          modulesInScope :: [ModuleName]
          modulesInScope = M.keys mhImports

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
      pure $ mconcat
        [ if reexportsItself then allLocalSymbols else mempty
        , exports
        , fold
        $ M.intersectionWith SM.leaveNames namesByNamespace
        $ MM.unMonoidalMap exportedNames
        ]
    where
      expandImportQualification :: (ImportQualification, a) -> [(Maybe ImportQualifier, a)]
      expandImportQualification = \case
        (Unqualified, x)                   -> [(Nothing, x)]
        (Qualified q, x)                   -> [(Just q, x)]
        (BothQualifiedAndUnqualified q, x) -> [(Just q, x), (Nothing, x)]

filterVisibleNames
  :: (MonadError Doc m)
  => [ModuleName]
  -> SymbolMap  -- ^ All names from the set of imports.
  -> ImportSpec -- ^ Import spec for this particular set of imports.
  -> m (ImportQualification, SymbolMap)
filterVisibleNames importedMods allImportedNames ImportSpec{ispecQualification, ispecImportList} = do
  vilibleNames <- case ispecImportList of
                    Nothing                 -> pure allImportedNames
                    Just (Imported entries) -> do
                      importedNames <- foldMapA (namesFromEntry importedMods allImportedNames) entries
                      pure $ SM.leaveNames allImportedNames importedNames
                    Just (Hidden entries)   -> do
                      hiddenNames <- foldMapA (namesFromEntry importedMods allImportedNames) entries
                      pure $ SM.removeNames allImportedNames hiddenNames
  pure (ispecQualification, vilibleNames)

-- | Get names referred to by @EntryWithChildren@ given a @SymbolMap@
-- of names currently in scope.
namesFromEntry
  :: (MonadError Doc m)
  => [ModuleName]
  -> SymbolMap
  -> EntryWithChildren UnqualifiedSymbolName
  -> m (Set UnqualifiedSymbolName)
namesFromEntry importedMods allImportedNames (EntryWithChildren sym visibility) =
  S.insert sym <$>
  case visibility of
    Nothing                                 -> pure mempty
    Just VisibleAllChildren                 ->
      case SM.lookupChildren sym allImportedNames of
        Nothing       -> throwError $ "Imported parent" <+> pretty sym <+>
          "not found in symbol map of modules" <+> ppList importedMods
        Just children -> pure children
    Just (VisibleSpecificChildren children) -> pure children

