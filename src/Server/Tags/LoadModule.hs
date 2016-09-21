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
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

module Server.Tags.LoadModule (loadModule) where

-- import Control.Applicative
import Control.Arrow (first, (&&&))
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
-- import Data.Char
-- import qualified Data.List as L
import Data.Time.Clock (UTCTime)
import Data.List.NonEmpty (NonEmpty(..))
-- import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (for)
-- import System.Directory
import System.FilePath
import Text.PrettyPrint.Leijen.Text (Doc, Pretty(..))
import qualified Text.PrettyPrint.Leijen.Text as PP

import Token (Pos(..), Line(..), SrcPos(..), TokenVal(..), Token)
import FastTags (tokenizeInput, processTokens, TagVal(..))
-- import FastTags (process) -- processAll,
-- import Language.Haskell.Exts.Extension
-- import Language.Haskell.Exts.Parser
-- import qualified Language.Haskell.Exts.Annotated.Syntax as HSE


import Control.Monad.Filesystem (MonadFS)
import Control.Monad.Logging
import qualified Control.Monad.Filesystem as MonadFS
import Server.Tags.Types
import Text.PrettyPrint.Leijen.Text.Utils

recognizedExtensions :: [String]
recognizedExtensions = ["hs", "lhs", "hsc", "chs"]

-- | Fetch module by it's name from cache or load it. Check modification time
-- of module files and reload if anything changed
loadModule
  :: forall m. (MonadError Doc m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => ModuleName -> m (NonEmpty Module)
loadModule name = do
  modules <- gets tssLoadedModules
  mods    <- case M.lookup name modules of
                Nothing   -> doLoad name
                Just mods -> for mods reloadIfFileChanged
  modify (\s -> s { tssLoadedModules = M.insert name mods $ tssLoadedModules s })
  return mods
  where
    doLoad :: ModuleName -> m (NonEmpty Module)
    doLoad name = do
      logDebug $ "[loadModule.doLoad] loading module" <+> showDoc name
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
  modifTime <- MonadFS.getModificationTime $ modFile m
  if modLastModified m /= modifTime
  then do
    logDebug $ "[reloadIfFileChanged] reloading module " <> showDoc (mhModName $ modHeader m)
    loadModuleFromFile (mhModName (modHeader m)) (Just modifTime) $ modFile m
  else return m

analyzeHeader :: [Token] -> (Maybe ModuleHeader, [Token])
analyzeHeader ts = (Nothing, ts)

-- | Recursively load module from file - load given filename, then load all its
-- imports.
loadModuleFromFile
  :: (MonadError Doc m, MonadState TagsServerState m, MonadReader TagsServerConf m, MonadLog m, MonadFS m)
  => ModuleName
  -> Maybe UTCTime
  -> FilePath
  -> m Module
loadModuleFromFile moduleName modifTime filename = do
  logDebug $ "[loadModuleFromFile] loading file " <> showDoc filename
  source <- MonadFS.readFile filename
  tokens <- either (throwError . docFromString) return $ tokenizeInput filename False source
  let (header, tokens') = analyzeHeader tokens
      syms :: [ResolvedSymbol]
      (syms, errors)    = first (fmap mkSymbol) $ processTokens tokens'
      allSymbols :: Map SymbolName ResolvedSymbol
      allSymbols        = M.fromList $ map (resolvedSymbolName &&& id) syms
      parentSymbols :: Map SymbolName SymbolName
      parentSymbols     = M.fromList $ mapMaybe (\sym -> (,resolvedSymbolName sym) <$> resolvedSymbolParent sym) syms
      defaultHeader :: ModuleHeader
      defaultHeader     = ModuleHeader
        { mhModName          = moduleName
        , mhImports          = mempty
        , mhImportQualifiers = mempty
        , mhExports          = mempty
        }
  case header of
    Nothing                      -> return ()
    Just ModuleHeader{mhModName} ->
      unless (moduleName == mhModName) $
        throwError $ ppDict "Module name in file differst from the expected module name"
          [ "file"                 :-> pretty filename
          , "module name in file"  :-> pretty mhModName
          , "expected module name" :-> pretty moduleName
          ]
  modifTime' <- maybe (MonadFS.getModificationTime filename) return modifTime
  let mod = Module
        { modHeader       = fromMaybe defaultHeader header
        , modParentMap    = parentSymbols
        , modAllSymbols   = allSymbols
        , modSource       = source
        , modFile         = filename
        , modLastModified = modifTime'
        }
  logDebug $ "[loadModuleFromFile] loaded " <> pretty mod
  pure mod
