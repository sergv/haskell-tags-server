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

module Haskell.Language.Server.Tags.Search (findSymbol) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import Data.Foldable (toList)
import Data.List
import qualified Data.Text as T
import System.FilePath
import Text.PrettyPrint.Leijen.Text.Utils

import Control.Monad.Filesystem (MonadFS)
import Control.Monad.Logging
import Data.Foldable.Ext
import qualified Data.SymbolMap as SM
import Data.Symbols
import Haskell.Language.Server.Tags.LoadModule
import Haskell.Language.Server.Tags.Types

findSymbol
  :: (MonadError Doc m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => FilePath   -- ^ File name
  -> SymbolName -- ^ Symbol to find. Can be either qualified, unqualified, ascii name/utf name/operator
  -> m [ResolvedSymbol] -- ^ Found tags, may be empty when nothing was found.
findSymbol filename sym = do
  modName <- fileNameToModuleName filename
  foldMapA (findInModule sym) =<< loadModule (ImportKey VanillaModule modName)

-- | Try to find out what @sym@ refers to in the context of module @mod@.
findInModule
  :: forall m. (MonadError Doc m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => SymbolName
  -> ResolvedModule
  -> m [ResolvedSymbol]
findInModule sym mod =
  case qualifier of
    -- Unqualified name
    Nothing -> do
      let localSyms       = maybe [] toList $ SM.lookup sym' $ modAllSymbols mod
          relevantImports = filter importBringsUnqualifiedNames
                          $ foldMap toList
                          $ mhImports header
      (localSyms ++) <$> lookUpInImportedModules sym' relevantImports
    -- Qualified name
    Just qualifier' -> do
      resolvedSpecs <- resolveQualifier qualifier' header
      case resolvedSpecs of
        Nothing     ->
          throwError $ "Qualifier" <+> showDoc qualifier' <+>
            "not listed among module's import qualifiers:" <+> showDoc (mhImportQualifiers header)
        Just specs ->
          lookUpInImportedModules sym' specs
  where
    qualifier :: Maybe ImportQualifier
    sym'      :: UnqualifiedSymbolName
    (qualifier, sym') = splitQualifiedPart sym
    header :: ResolvedModuleHeader
    header = modHeader mod

lookUpInImportedModules
  :: forall m f. (MonadError Doc m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => (Foldable f)
  => UnqualifiedSymbolName
  -> f ResolvedImportSpec
  -> m [ResolvedSymbol]
lookUpInImportedModules name specs = do
  logDebug $ "[lookUpInImportedModules] searching for name" <+> pretty name <+>
    "in modules" <+> pretty (map (ikModuleName . ispecImportKey) $ toList specs)
  flip foldMapA specs $ \ImportSpec{ispecImportKey, ispecImportList} -> do
    let nameVisible =
          case ispecImportList of
            Nothing                          -> True
            Just ImportList{ilImportedNames} -> SM.member name ilImportedNames
    if nameVisible
    then do
      mods <- loadModule ispecImportKey
      flip foldMapA mods $ \mod -> do
        logDebug $ "[lookUpInImportedModules] searching for name" <+> pretty name <+> "in module" <+> pretty (ikModuleName ispecImportKey)
        lookUpInImportedModule name mod
    else pure mempty


    -- -- foldMapA (lookUpInImportedModule name) =<< loadModule ispecModuleName
    --
    -- let nameVisible =
    --       case ispecImportList of
    --         Nothing               -> True
    --         Just (Imported names) ->
    --           KM.member name names
    --         Just (Hidden names)   -> KM.notMember name names
    -- if not nameVisible
    -- then pure mempty
    -- else do
    --   logDebug $ "[lookUpInImportedModules] searching for name" <+> pretty name <+> "in module" <+> pretty ispecModuleName
    --   loadModule ispecModuleName >>= foldMapA (lookUpInImportedModule name)

lookUpInImportedModule
  :: forall m. (MonadError Doc m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => UnqualifiedSymbolName
  -> ResolvedModule
  -> m [ResolvedSymbol]
lookUpInImportedModule name importedMod =
  case SM.lookup name $ modAllExportedNames importedMod of
    Nothing   -> pure []
    Just syms -> pure $ toList syms
  -- case mhExports $ modHeader importedMod of
  --   -- No export list - only names from this module are reexported.
  --   Nothing                                            ->
  --     pure $ maybe [] toList $ SM.lookup name $ modAllSymbols importedMod
  --   Just ModuleExports{meExportedEntries, meReexports, meHasWildcardExports} ->
  --     -- NB: each name can be reexported from only one source. Thus, name
  --     -- cannot be exported by module and at the same time be also present in
  --     -- some of the reexported modules.
  --     case KM.lookup (getUnqualifiedSymbolName name) meExportedEntries of
  --       Nothing ->
  --         -- It is enough to resolve parent only once, because exponting
  --         -- everything from a grandparent does not automatically exports
  --         -- children two levels deep.
  --         case resolveParent importedMod name >>= \parents ->
  --                error "TODO" $
  --                KM.lookup (getUnqualifiedSymbolName name') meExportedEntries of
  --           Just entry@(EntryWithChildren _ (Just children))
  --             | isChildExported name children -> lookupExportedInCurrentModuleAndImports entry
  --           _
  --             | meHasWildcardExports ->
  --               findInModule (getUnqualifiedSymbolName name) importedMod
  --             | otherwise            ->
  --               flip foldMapA meReexports $ \modName ->
  --                 case M.lookup modName $ mhImports $ modHeader importedMod of
  --                   Nothing    ->
  --                     throwError $
  --                       "Internal error: reexported module" <+> pretty modName <+>
  --                         "not found in imports map"
  --                   Just specs ->
  --                     lookUpInImportedModules name $ toList specs
  --       Just entry -> lookupExportedInCurrentModuleAndImports entry
  -- where
  --   lookupExportedInCurrentModuleAndImports = lookupExportedSymbolInModuleAndImports name importedMod
  --
-- -- | Search for symbol in module given that the symbol is exported from module.
-- lookupExportedSymbolInModuleAndImports
--   :: forall m. (MonadError Doc m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
--   => UnqualifiedSymbolName
--   -> Module
--   -> EntryWithChildren SymbolName
--   -> m [ResolvedSymbol]
-- lookupExportedSymbolInModuleAndImports
--   name
--   Module{modHeader, modAllSymbols}
--   (EntryWithChildren exportedName _) =
--   case fst $ splitQualifiedPart exportedName of
--     Nothing        ->
--       case SM.lookup name modAllSymbols of
--         Just syms -> pure $ toList syms
--         Nothing   ->
--           -- If name is exported but is not defined in current module
--           -- then it is either defined by template Haskell or preprocessor,
--           -- in which case we're out of luck, or it is just re-exported
--           -- and it it still possible to find it.
--           lookUpInImportedModules name
--             $ filter importBringsUnqualifiedNames
--             $ foldMap toList
--             $ mhImports modHeader
--     -- If export has qualifiers then it must be a reexport.
--     Just qualifier ->
--       lookUpInImportedModules name
--         $ filter (`importBringsNamesQualifiedWith` qualifier)
--         $ foldMap toList
--         $ mhImports modHeader
--
-- resolveParent :: Module -> UnqualifiedSymbolName -> Maybe (Set UnqualifiedSymbolName)
-- resolveParent Module{modAllSymbols} name = SM.lookupParent name modAllSymbols

-- | Try to infer suitable module name from the file name. Tries to take
-- as much directory names that start with the uppercase letter as possible.
fileNameToModuleName :: (MonadError Doc m) => FilePath -> m ModuleName
fileNameToModuleName fname =
  case reverse $ splitDirectories fname of
    []            -> throwError "Cannot convert empty file name to module name"
    fname' : dirs ->
      pure $
      mkModuleName $
      T.pack $
      intercalate "." $
      reverse $
      takeWhile canBeModuleName $
      dropExtension fname' : dirs
  where
    canBeModuleName :: FilePath -> Bool
    canBeModuleName []     = False
    canBeModuleName (c:cs) = isUpper c && all isModuleNameConstituentChar cs
