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

module Server.Tags.Search (findSymbol, findInModule) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Traversable (for)
import System.FilePath
import Text.PrettyPrint.Leijen.Text (Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP
import Text.PrettyPrint.Leijen.Text.Utils

-- import Control.Monad.Logging
import Server.Tags.LoadModule
import Server.Tags.Types

findSymbol
  :: (MonadError Doc m, MonadState TagsServerState m, MonadReader TagsServerConf m)
  => FilePath   -- ^ File name
  -> SymbolName -- ^ Symbol to find. Can be either qualified, unqualified, ascii name/utf name/operator
  -> m [ResolvedSymbol] -- ^ Found tags, may be empty when nothing was found.
findSymbol filename sym =
      fileNameToModuleName filename
  >>= loadModule
  >>= fmap concat . traverse (findInModule sym)

-- | Try to find out what @sym@ refers to in the context of module @mod@.
findInModule
  :: forall m. (MonadError Doc m, MonadState TagsServerState m, MonadReader TagsServerConf m)
  => SymbolName
  -> Module
  -> m [ResolvedSymbol]
findInModule sym mod = do
  (qualifier, sym') <- splitQualifiedPart sym
  case qualifier of
    -- Unqualified name
    Nothing -> do
      let localSyms = maybeToList $ M.lookup sym' $ modAllSymbols mod
      importedSyms <- for (filter importBringsUnqualifiedNames $ modImports mod) $ \ModuleImport{miImportedName} ->
                        lookupInExports miImportedName sym'
      return $ localSyms ++ concat importedSyms
    -- Qualified name
    Just qualifier' ->
      case M.lookup qualifier' (modImportQualifiers mod) of
        Nothing       ->
          throwError $ "Qualifier" PP.<+> showDoc qualifier' PP.<+> "not listed among module's import qualifiers:" PP.<+> showDoc (modImportQualifiers mod)
        Just modNames ->
          concat <$> for modNames (\modName -> lookupInExports modName sym')
  where
    lookupInExports :: ModuleName -> SymbolName -> m [ResolvedSymbol]
    lookupInExports modName sym = do
      mods <- loadModule modName
      return $ mapMaybe (M.lookup sym . modExports) mods

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
