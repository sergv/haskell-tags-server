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
import Text.PrettyPrint.Leijen.Text.Utils

-- import Control.Monad.Logging
import Server.Tags.LoadModule
import Server.Tags.Types

findSymbol
  :: (MonadError Doc m, MonadState TagsServerState m, MonadReader TagsServerConf m)
  => FilePath   -- ^ File name
  -> SymbolName -- ^ Symbol to find. Can be either qualified, unqualified, ascii name/utf name/operator
  -> m [Symbol] -- ^ Found tags, may be empty when nothing was found.
findSymbol filename sym =
      fileNameToModuleName filename
  >>= retrieveModule
  >>= fmap concat . traverse (findInModule sym)

findInModule
  :: forall m. (MonadError Doc m, MonadState TagsServerState m, MonadReader TagsServerConf m)
  => SymbolName
  -> Module
  -> m [Symbol]
findInModule sym mod = do
  (qualifier, sym') <- splitQualifiedPart sym
  case qualifier of
    Nothing -> do
      let localSyms = maybeToList $ M.lookup sym' $ modAllSymbols mod
      importedSyms <- for (filter importBringsUnqualifiedNames $ modImports mod) $ \ModuleImport{miImportedName} ->
                        lookupInExports miImportedName sym'
      return $ localSyms ++ concat importedSyms
    Just qualifier' ->
      case M.lookup qualifier' (modImportQualifiers mod) of
        Nothing       ->
          throwError $ "Qualifier " <> showDoc qualifier' <> " not listed among module's import qualifiers: " <> showDoc (modImportQualifiers mod)
        Just modNames ->
          concat <$> for modNames (\modName -> lookupInExports modName sym')
  where
    lookupInExports :: ModuleName -> SymbolName -> m [Symbol]
    lookupInExports modName sym = do
      mods <- retrieveModule modName
      return $ mapMaybe (M.lookup sym . modExports) mods

-- | Try to infer suitable module name from the file name. Tries to take
-- as much directory names that start with the uppercase letter as possible.
fileNameToModuleName :: (MonadError Doc m) => FilePath -> m ModuleName
fileNameToModuleName fname =
  case reverse dirs of
    []           -> throwError "Cannot convert empty file name to module name"
    fname: dirs' ->
      pure $
      mkModuleName $
      T.pack $
      intercalate "." $
      reverse $
      takeWhile isValidDirName $
      dropExtension fname : dirs'
  where
    dirs :: [FilePath]
    dirs = splitDirectories fname
    isValidDirName :: FilePath -> Bool
    isValidDirName []     = False
    isValidDirName (c:cs) = isUpper c && all isModuleNameConstituentChar cs
