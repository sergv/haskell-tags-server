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
  , loadModuleFromSource
  , namesFromEntry
  , resolveModuleExports
  -- * Utils
  , vanillaExtensions
  , hsBootExtensions
  ) where

import Control.Arrow (first)
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
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Traversable (for)
import System.FilePath

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
  modules <- gets tssLoadedModules
  mods    <- case SubkeyMap.lookup key modules of
                Nothing   -> doLoad ikModuleName
                Just mods -> for mods reloadIfNecessary
  modify (\s -> s { tssLoadedModules = SubkeyMap.insert key mods $ tssLoadedModules s })
  loadedMods <- gets (SubkeyMap.keys . tssLoadedModules)
  logDebug $ "[loadModule] loaded modules are:" <+> ppList loadedMods
  return mods
  where
    extensions :: Set String
    extensions = case ikImportTarget of
                   VanillaModule -> vanillaExtensions
                   HsBootModule  -> hsBootExtensions
    isHaskellSource :: FilePath -> Maybe FilePath
    isHaskellSource path
      | takeExtension path `S.member` extensions = Just path
      | otherwise                                = Nothing
    doLoad :: ModuleName -> m (NonEmpty ResolvedModule)
    doLoad name = do
      logInfo $ "[loadModule.doLoad] loading module" <+> showDoc name
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
                    ("Cannot load module " <> pretty name <> ": no attempted paths exist:")
                    allPaths
        p : ps -> traverse (loadModuleFromFile (Just name) Nothing) (p :| ps)
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
    loadModuleFromFile (Just (mhModName (modHeader m))) (Just modifTime) (modFile m)
  else return m

loadModuleFromFile
  :: (MonadError Doc m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => Maybe ModuleName
  -> Maybe UTCTime
  -> FilePath
  -> m ResolvedModule
loadModuleFromFile suggestedModuleName modifTime filename = do
  logInfo $ "[loadModuleFromFile] loading file " <> showDoc filename
  modifTime'    <- maybe (MonadFS.getModificationTime filename) return modifTime
  source        <- MonadFS.readFile filename
  unresolvedMod <- loadModuleFromSource suggestedModuleName modifTime' filename source
  resolveModuleExports loadModule unresolvedMod

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
  tokens <- either (throwError . docFromString) return $ tokenizeInput filename False source
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
  => (ImportKey -> m (NonEmpty ResolvedModule))
  -> UnresolvedModule
  -> m ResolvedModule
resolveModuleExports loadMod mod = do
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
      :: SubkeyMap ImportKey (NonEmpty UnresolvedImportSpec)
      -> m ( SubkeyMap ImportKey (NonEmpty ResolvedImportSpec)
           , [(ImportQualification, SymbolMap)]
           )
    resolveImports imports =
      SS.runStateT (SubkeyMap.traverseWithKey collect imports) []
      where
        collect
          :: ImportKey
          -> NonEmpty UnresolvedImportSpec
          -> SS.StateT [(ImportQualification, SymbolMap)] m (NonEmpty ResolvedImportSpec)
        collect importKey importSpecs = do
          modules <- lift $ loadMod importKey
          let importedNames :: SymbolMap
              importedNames = foldMap modAllExportedNames modules
              importedMods :: NonEmpty ModuleName
              importedMods = mhModName . modHeader <$> modules
          for importSpecs $ \spec -> do
            (qual, symMap) <- filterVisibleNames importedMods importedNames spec
            modify ((qual, symMap) :)
            pure $ symMap <$ spec

    resolveSymbols :: UnresolvedModule -> m (SubkeyMap ImportKey (NonEmpty ResolvedImportSpec), SymbolMap)
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
                       (fmap (foldMap modAllExportedNames) . loadMod)
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

