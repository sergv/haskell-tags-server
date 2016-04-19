----------------------------------------------------------------------------
-- |
-- Module      :  Search
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
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Search (findSymbol, findInModule) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Array as A
import Data.Char
-- import qualified Data.Foldable as F
import Data.List hiding (find)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import System.FilePath

import Text.Regex.TDFA
import qualified Text.Regex.TDFA.Text as TDFA

import LoadModule
-- import Logging
import Types

findSymbol :: (Functor m, MonadError String m, MonadState ServerState m, MonadReader (ServerConfig s) m, MonadIO m)
           => FilePath -> SymbolName -> m [Symbol]
findSymbol filename sym = do
  modules <- gets stateModules
  modName <- fileNameToModuleName filename
  case M.lookup modName modules of
    Nothing  -> do
      mod <- loadModuleFromFile filename
      findInModule sym mod
    Just mods ->
      concat <$> mapM (findInModule sym) mods

findInModule :: forall m s. (Functor m, MonadError String m, MonadState ServerState m, MonadReader (ServerConfig s) m, MonadIO m)
             => SymbolName -> Module -> m [Symbol]
findInModule sym mod
  | Just (qualifier, sym') <- splitQualified sym =
    case M.lookup qualifier (modImportQualifiers mod) of
      Nothing      ->
        throwError $ "qualifier " ++ show qualifier ++ " not listed among module's import qualifiers: " ++ show (modImportQualifiers mod)
      Just modName -> lookupInExports modName sym'
  | otherwise = do
    importedSyms <- forM (filter (importsUnqualifiedNames . snd) $ modImports mod) $ \(modName, _) ->
      lookupInExports modName sym
    return $ maybeToList (M.lookup sym (modAllSymbols mod)) ++ concat importedSyms
  where
    lookupInExports :: ModuleName -> SymbolName -> m [Symbol]
    lookupInExports modName sym = do
      mods <- retrieveModule modName
      return $ mapMaybe (M.lookup sym . modExports) mods

-- | Split qualified symbol name (e.g. Foo.Bar.baz) into
-- qualified module part (Foo.Bar) and name part (baz).
splitQualified :: SymbolName -> Maybe (ModuleName, SymbolName)
splitQualified = \(SymbolName name) ->
  case matchOnce re name of
    Nothing      -> Nothing
    Just matches ->
      Just ( ModuleName $ extract (matches A.! 1) name
           , SymbolName $ extract (matches A.! 3) name
           )
  where
    re :: Regex
    re = either error id $ TDFA.compile
           (defaultCompOpt { multiline = False })
           defaultExecOpt
           "^(([[:upper:]][[:alnum:]_']*\\.)*[[:upper:]][[:alnum:]_']*)\\.(.*)$"
           -- "^((?:[A-Z][a-zA-Z_0-9']*\\.)*[A-Z][a-zA-Z_0-9']*)\\.(.*)$"

fileNameToModuleName :: (Functor m, MonadError String m) => FilePath -> m ModuleName
fileNameToModuleName fname =
  case reverse dirs of
    []           -> throwError "Cannot convert empty file name to module name"
    fname: dirs' ->
      return $
      ModuleName $
      T.pack $
      intercalate "." $
      reverse $
      takeWhile (\xs -> not (null xs) && isUpper (head xs) && all isAlphaNum xs) $
      dropExtension fname : dirs'
  where
    dirs = splitDirectories fname

