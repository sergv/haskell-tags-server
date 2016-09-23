----------------------------------------------------------------------------
-- |
-- Module      :  Server.Tags.Search
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
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Server.Tags.Search (findSymbol) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import Data.Foldable (toList)
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Traversable (for)
import System.FilePath
import Text.PrettyPrint.Leijen.Text.Utils

import qualified Data.KeyMap as KM
import Control.Monad.Filesystem (MonadFS)
import Control.Monad.Logging
import Server.Tags.LoadModule
import Server.Tags.Types

findSymbol
  :: (MonadError Doc m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => FilePath   -- ^ File name
  -> SymbolName -- ^ Symbol to find. Can be either qualified, unqualified, ascii name/utf name/operator
  -> m [ResolvedSymbol] -- ^ Found tags, may be empty when nothing was found.
findSymbol filename sym = do
  modName <- fileNameToModuleName filename
  mods    <- loadModule modName
  fmap concat $ traverse (findInModule sym) mods
  -- evalStateT (fmap concat $ traverse (findInModule sym) mods) mempty

-- | Try to find out what @sym@ refers to in the context of module @mod@.
findInModule
  :: forall m. (MonadError Doc m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => SymbolName
  -> Module
  -> m [ResolvedSymbol]
findInModule sym mod = do
  (qualifier, sym') <- splitQualifiedPart sym
  case qualifier of
    -- Unqualified name
    Nothing -> do
      let localSyms       = maybeToList $ M.lookup sym' $ modAllSymbols mod
          relevantImports = map ispecModuleName
                          $ filter importBringsUnqualifiedNames
                          $ mhImports header
      (localSyms ++) <$> lookupInModulesExports relevantImports sym'
    -- Qualified name
    Just qualifier' ->
      case M.lookup qualifier' $ mhImportQualifiers header of
        Nothing       ->
          throwError $ "Qualifier" <+> showDoc qualifier' <+> "not listed among module's import qualifiers:" <+> showDoc (mhImportQualifiers header)
        Just modNames ->
          lookupInModulesExports (toList modNames) sym'
  where
    header :: ModuleHeader
    header = modHeader mod

lookupInModulesExports
  :: forall m. (MonadError Doc m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => [ModuleName]
  -> SymbolName
  -> m [ResolvedSymbol]
lookupInModulesExports modNames name =
  fmap concat $ for modNames $ \modName -> do
    mods <- loadModule modName
    fmap concat $ for mods $ \mod@Module{modHeader, modAllSymbols} -> do
      let exports               = mhExports modHeader
          lookupInCurrentModule =
            case M.lookup name modAllSymbols of
              Nothing  ->
                -- | If name is exported but is not defined in current module
                -- then it is either defined by template Haskell or preprocessor,
                -- in which case we're out of luck, or it is just re-exported
                -- and it it still possible to find it.
                lookupInModulesExports (map ispecModuleName $ mhImports modHeader) name
                -- throwError missingMsg
              Just sym -> pure [sym]
          -- missingMsg            =
          --   "Internal error: exported symbol" <+> pretty name <+> "is missing from module" <+> pretty mod

      case exports of
        -- No export list - only names from this module are reexported.
        Nothing                                            ->
          lookupInCurrentModule
        Just ModuleExports{meExportedEntries, meReexports} ->
          -- NB: each name can be reexported from only one source. Thus, name
          -- cannot be exported by module and at the same time be also present in
          -- some of the reexported modules.
          case KM.lookup name meExportedEntries of
            Nothing ->
              -- It is enough to resolve parent only once, because exponting everything
              -- from a grandparent does not automatically exports children two
              -- levels deep.
              case resolveParent mod name >>= \name' -> KM.lookup name' meExportedEntries of
                Just (EntryWithChildren _ (Just children))
                  | isChildExported name children -> lookupInCurrentModule
                _                                 -> lookupInModulesExports meReexports name
            Just _  -> lookupInCurrentModule

resolveParent :: Module -> SymbolName -> Maybe SymbolName
resolveParent Module{modParentMap} name = M.lookup name modParentMap

-- | Try to infer suitable module name from the file name. Tries to take
-- as much directory names that start with the uppercase letter as possible.
fileNameToModuleName :: (MonadError Doc m) => FilePath -> m ModuleName
fileNameToModuleName fname =
  case reverse $ splitDirectories fname of
    []          -> throwError "Cannot convert empty file name to module name"
    fname: dirs ->
      pure $
      mkModuleName $
      T.pack $
      intercalate "." $
      reverse $
      takeWhile canBeModuleName $
      dropExtension fname : dirs
  where
    canBeModuleName :: FilePath -> Bool
    canBeModuleName []     = False
    canBeModuleName (c:cs) = isUpper c && all isModuleNameConstituentChar cs
