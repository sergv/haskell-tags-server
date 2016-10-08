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

module Haskell.Language.Server.Tags.LoadModule
  ( loadModule
  , namesFromEntry
  ) where

import Control.Arrow (first)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.State.Strict as SS
import Data.Foldable
import Data.Functor.Product (Product(..))
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
import Data.SubkeyMap (SubkeyMap)
import qualified Data.SubkeyMap as SubkeyMap
import Data.SymbolMap (SymbolMap)
import qualified Data.SymbolMap as SM
import Data.Symbols
import Haskell.Language.Server.Tags.AnalyzeHeader
import Haskell.Language.Server.Tags.Types
import Text.PrettyPrint.Leijen.Text.Utils

vanillaExtensions :: [String]
vanillaExtensions = ["hs", "lhs", "hsc", "chs"]

hsBootExtensions :: [String]
hsBootExtensions = ["hs-boot", "lhs-boot"]

-- | Fetch module by it's name from cache or load it. Check modification time
-- of module files and reload if anything changed
loadModule
  :: forall m. (MonadError Doc m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => ImportKey
  -> m (NonEmpty ResolvedModule)
loadModule key@ImportKey{ikModuleName, ikImportTarget} = do
  logDebug $ "[loadModule] loading" <+> pretty ikModuleName
  modules <- gets tssLoadedModules
  mods    <- case SubkeyMap.lookup key modules of
                Nothing   -> doLoad ikModuleName
                Just mods -> for mods reloadIfNecessary
  modify (\s -> s { tssLoadedModules = SubkeyMap.insert key mods $ tssLoadedModules s })
  loadedMods <- gets (SubkeyMap.keys . tssLoadedModules)
  logDebug $ "[loadModule] loaded modules are:" <+> ppList loadedMods
  return mods
  where
    extensions :: [String]
    extensions = case ikImportTarget of
                   VanillaModule -> vanillaExtensions
                   HsBootModule  -> hsBootExtensions
    doLoad :: ModuleName -> m (NonEmpty ResolvedModule)
    doLoad name = do
      logInfo $ "[loadModule.doLoad] loading module" <+> showDoc name
      srcDirs <- asks tsconfSourceDirectories
      let possiblePaths = [ root </> filenamePart <.> ext
                          | root <- S.toList srcDirs
                          , ext  <- extensions
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

reloadIfNecessary
  :: (MonadError Doc m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => ResolvedModule
  -> m ResolvedModule
reloadIfNecessary m = do
  (needsReloading, modifTime) <- moduleNeedsReloading m
  if needsReloading
  then do
    logInfo $ "[reloadIfNecessary] reloading module" <+> showDoc (mhModName $ modHeader m)
    loadModuleFromFile (mhModName (modHeader m)) (Just modifTime) $ modFile m
  else return m

-- | Recursively load module from file - load given filename, then load all its
-- imports.
loadModuleFromFile
  :: (MonadError Doc m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => ModuleName
  -> Maybe UTCTime
  -> FilePath
  -> m ResolvedModule
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
  -> m ResolvedModule
makeModule moduleName modifTime filename tokens = do
  (header, tokens') <- analyzeHeader tokens
  let syms           :: [ResolvedSymbol]
      errors         :: [String]
      (syms, errors) = first (fmap mkSymbol) $ processTokens tokens'
      allSymbols     :: SymbolMap
      allSymbols     = SM.fromList syms
  unless (null errors) $
    logError $ ppFoldableWithHeader
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
  mod' <- resolveModuleExports mod
  logDebug $ "[loadModuleFromFile] loaded module" <+> pretty mod
  return mod'
  where
    defaultHeader :: UnresolvedModuleHeader
    defaultHeader = ModuleHeader
      { mhModName          = moduleName
      , mhImports          = SubkeyMap.empty
      , mhImportQualifiers = mempty
      , mhExports          = Nothing
      }

resolveModuleExports
  :: forall m a. (MonadError Doc m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => Module a
  -> m ResolvedModule
resolveModuleExports mod = do
  let header = modHeader mod
  logDebug $ "[resolveModuleExports] resolving exports of module" <+> pretty (mhModName header)
  (imports, symbols) <- resolveSymbols mod
  return $ mod
    { modHeader           = header { mhImports = imports }
    , modAllExportedNames = symbols
    }
  where
    expandImportQualification :: forall a. (ImportQualification, a) -> [(Maybe ImportQualifier, a)]
    expandImportQualification = \case
      (Unqualified, x)                   -> [(Nothing, x)]
      (Qualified q, x)                   -> [(Just q, x)]
      (BothQualifiedAndUnqualified q, x) -> [(Just q, x), (Nothing, x)]

    resolveImports
      :: SubkeyMap ImportKey (NonEmpty (ImportSpec a))
      -> m ( SubkeyMap ImportKey (NonEmpty ResolvedImportSpec)
           , [(ImportQualification, SymbolMap)]
           )
    resolveImports imports =
      SS.runStateT (SubkeyMap.traverseWithKey collect imports) []
      where
        collect
          :: ImportKey
          -> NonEmpty (ImportSpec a)
          -> SS.StateT [(ImportQualification, SymbolMap)] m (NonEmpty ResolvedImportSpec)
        collect importKey importSpecs = do
          modules <- lift $ loadModule importKey
          let importedNames :: SymbolMap
              importedNames = foldMap modAllExportedNames modules
              importedMods :: NonEmpty ModuleName
              importedMods = mhModName . modHeader <$> modules
          for importSpecs $ \spec -> do
            (qual, symMap) <- filterVisibleNames importedMods importedNames spec
            modify ((qual, symMap) :)
            pure $ symMap <$ spec

    resolveSymbols :: Module a -> m (SubkeyMap ImportKey (NonEmpty ResolvedImportSpec), SymbolMap)
    resolveSymbols Module{modHeader = header@ModuleHeader{mhImports}, modAllSymbols} = do
      (resolvedImports, filteredNames) <- resolveImports mhImports
      case mhExports header of
        Nothing                                            ->
          pure (resolvedImports, modAllSymbols)
        Just ModuleExports{meExportedEntries, meReexports} -> do
          let lt, gt :: Set ModuleName
              (lt, reexportsItself, gt) = S.splitMember (mhModName header) meReexports
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
          exports <- foldMapA
                       (fmap (foldMap modAllExportedNames) . loadModule)
                       (foldMap
                          (\modName -> fromMaybe mempty $ SubkeyMap.lookupSubkeyKeys modName resolvedImports)
                          (Pair lt gt))
          let allSymbols = mconcat
                [ if reexportsItself then modAllSymbols else mempty
                , exports
                , fold
                $ M.intersectionWith SM.leaveNames namesByNamespace
                $ MM.unMonoidalMap exportedNames
                ]
          pure (resolvedImports, allSymbols)

-- | Find out which names and under qualification come out of an import spec.
filterVisibleNames
  :: (MonadError Doc m, Foldable f)
  => f ModuleName
  -> SymbolMap    -- ^ All names from the set of imports.
  -> ImportSpec a -- ^ Import spec for this particular set of imports.
  -> m (ImportQualification, SymbolMap)
filterVisibleNames importedMods allImportedNames ImportSpec{ispecQualification, ispecImportList} = do
  vilibleNames <- case ispecImportList of
                    Nothing                                  ->
                      pure allImportedNames
                    Just ImportList{ilImportType, ilEntries} -> do
                      let f = case ilImportType of
                                Imported -> SM.leaveNames
                                Hidden   -> SM.removeNames
                      importedNames <- foldMapA (namesFromEntry importedMods allImportedNames) ilEntries
                      pure $ f allImportedNames importedNames
  pure (ispecQualification, vilibleNames)

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
    Nothing                                 -> pure mempty
    Just VisibleAllChildren                 ->
      case SM.lookupChildren sym allImportedNames of
        Nothing       -> throwError $ "Imported parent" <+> pretty sym <+>
          "not found in symbol map of modules" <+> ppList (toList importedMods)
        Just children -> pure children
    Just (VisibleSpecificChildren children) -> pure children

