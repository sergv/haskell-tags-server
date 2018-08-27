----------------------------------------------------------------------------
-- |
-- Module      :  Server.Tags.Search
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskell.Language.Server.Tags.Search (findSymbol) where

import Control.Monad.Except.Ext
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import Data.Foldable.Ext (toList, foldMapA, foldForA)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc.Ext

import Control.Monad.Filesystem (MonadFS)
import Control.Monad.Logging
import Data.ErrorMessage
import Data.Path
import qualified Data.SymbolMap as SM
import Data.Symbols
import Haskell.Language.Server.Tags.LoadModule
import Haskell.Language.Server.Tags.Types

findSymbol
  :: (HasCallStack, MonadError ErrorMessage m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => FullPath           -- ^ File name
  -> SymbolName         -- ^ Symbol to find. Can be either qualified, unqualified, ascii name/utf name/operator
  -> m [ResolvedSymbol] -- ^ Found tags, may be empty when nothing was found.
findSymbol filename sym = do
  modName <- fileNameToModuleName filename
  foldMapA (findInModule sym) =<< loadModule (ImportKey VanillaModule modName)

-- | Try to find out what @sym@ refers to in the context of module @mod@.
findInModule
  :: forall m. (HasCallStack, MonadError ErrorMessage m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
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
          throwErrorWithCallStack $ "Qualifier" <+> ppShow qualifier' <+>
            "not listed among module's import qualifiers:" <+> ppShow (mhImportQualifiers header)
        Just specs ->
          lookUpInImportedModules sym' specs
  where
    qualifier :: Maybe ImportQualifier
    sym'      :: UnqualifiedSymbolName
    (qualifier, sym') = splitQualifiedPart sym
    header :: ResolvedModuleHeader
    header = modHeader mod

lookUpInImportedModules
  :: forall m f. (HasCallStack, MonadError ErrorMessage m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => (Foldable f)
  => UnqualifiedSymbolName
  -> f ResolvedImportSpec
  -> m [ResolvedSymbol]
lookUpInImportedModules name specs = do
  logDebug $ "[lookUpInImportedModules] searching for name" <+> pretty name <+>
    "in modules" <+> pretty (map (ikModuleName . ispecImportKey) $ toList specs)
  foldForA specs $ \ImportSpec{ispecImportKey, ispecImportedNames} -> do
    let nameVisible = SM.member name ispecImportedNames
    if nameVisible
    then do
      mods <- loadModule ispecImportKey
      foldForA mods $ \mod -> do
        logDebug $ "[lookUpInImportedModules] searching for name" <+> pretty name <+> "in module" <+> pretty (ikModuleName ispecImportKey)
        lookUpInImportedModule name mod
    else pure mempty

lookUpInImportedModule
  :: forall m. (HasCallStack, MonadError ErrorMessage m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => UnqualifiedSymbolName
  -> ResolvedModule
  -> m [ResolvedSymbol]
lookUpInImportedModule name importedMod =
  case SM.lookup name $ modAllExportedNames importedMod of
    Nothing   -> pure []
    Just syms -> pure $ toList syms

-- | Try to infer suitable module name from the file name. Tries to take
-- as much directory names that start with the uppercase letter as possible.
fileNameToModuleName
  :: (HasCallStack, MonadError ErrorMessage m)
  => FullPath -> m ModuleName
fileNameToModuleName fname =
  case reverse $ splitDirectories fname of
    []            -> throwErrorWithCallStack "Cannot convert empty file name to module name"
    fname' : dirs ->
      pure $
      mkModuleName $
      T.intercalate "." $
      reverse $
      takeWhile canBeModuleName $
      map unPathFragment $
      dropExtension fname' : dirs
  where
    canBeModuleName :: T.Text -> Bool
    canBeModuleName t = case T.uncons t of
      Nothing      -> False
      Just (c, cs) -> isUpper c && T.all isModuleNameConstituentChar cs
