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

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Search where

import Control.Applicative
import Control.Arrow
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Array as A
import Data.Char
-- import qualified Data.Foldable as F
import Data.List hiding (find)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.FilePath

import FastTags (Pos(..), TagVal(..), process) -- processAll,
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Parser
import qualified Language.Haskell.Exts.Syntax as HSE
import Text.Regex.TDFA
import qualified Text.Regex.TDFA.Text as TDFA

import Logging
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
        throwError $ "qualifier not listed among module's import qualifiers: " ++ show qualifier
      Just modName -> lookupInExports modName sym'
  | otherwise = do
    importedSyms <- forM (modImports mod) $ \modName ->
      lookupInExports modName sym
    return $ maybeToList (M.lookup sym (modAllSymbols mod)) ++ concat importedSyms
  where
    lookupInExports :: ModuleName -> SymbolName -> m [Symbol]
    lookupInExports modName sym = do
      mods <- retrieveModule modName
      return $ concatMap (maybeToList . M.lookup sym . modExports) mods

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

-- Fetch module by it's name from cache or load it. Check modification time
-- of module files and reload if anything changed
retrieveModule :: forall m s. (Functor m, MonadError String m, MonadState ServerState m, MonadReader (ServerConfig s) m, MonadIO m)
               => ModuleName -> m [Module]
retrieveModule modName = do
  debugM $ "retrieving module " <> getModuleName modName
  modules <- gets stateModules
  mods <- case M.lookup modName modules of
             Nothing   -> loadModule modName
             Just mods ->
               forM mods $ \m -> do
                 modTime <- liftIO $ getModificationTime (modFile m)
                 if modLastModified m /= modTime
                   then loadModuleFromFile (modFile m)
                   else return m
  modify (\s -> s { stateModules = M.insert modName mods $ stateModules s })
  return mods
  where
    loadModule :: ModuleName -> m [Module]
    loadModule (ModuleName modName) = do
      srcDirs <- asks confSourceDirectories
      debugM $ "loading module " <> modName <> " in directories " <> show' (S.toList srcDirs)
      let possiblePaths = [ root </> filenamePart <.> ext
                          | root <- S.toList srcDirs
                          , ext  <- ["hs", "lhs", "hsc"]
                          ]
      actualPaths <- filterM (liftIO . doesFileExist) possiblePaths
      mapM loadModuleFromFile actualPaths
      where
        filenamePart :: FilePath
        filenamePart = T.unpack $ T.map (\c -> if c == '.' then pathSeparator else c) modName

-- | Recursively load module from file - load given filename, then load all its
-- imports.
loadModuleFromFile :: (Functor m, MonadError String m, MonadState ServerState m, MonadReader (ServerConfig s) m, MonadIO m)
                   => FilePath -> m Module
loadModuleFromFile filename = do
  debugM $ "loading module from file " <> T.pack filename
  source <- liftIO $ T.readFile filename
  let (allTags, _warnings) = process filename False source
      allSymbols           = M.fromList $ map (\tag@(Pos _ (TagVal _ name _)) -> (SymbolName name, tag)) allTags
      moduleHeader         = extractHeader source
      moduleImports        = extractImports source
      interface            = fromMaybe T.empty moduleHeader <>
                             T.singleton '\n' <>
                             moduleImports
  debugM $ "parsing\n>>>>\n" <> interface <> "\n<<<<"
  case parseWithMode parseMode (T.unpack interface) of
    ParseFailed loc msg -> do
      errorM $ "failed to parse module header and imports: " <> interface
      throwError $ "failed to parse module header and imports at " ++ show loc ++ ": " ++ msg
    ParseOk (HSE.Module _loc _name _pragmas _warnings exportSpec imports _body) -> do
      let importedModules  = map (convertModName . HSE.importModule) imports
          importQualifiers =
            M.fromList $ catMaybes $
            zipWith (\x name -> ((,name) . convertModName) <$> HSE.importAs x) imports importedModules
      -- tie the knot
      debugM $ "loading imported modules " <> show' (map getModuleName importedModules)
      imports <- local (\conf -> conf { confSourceDirectories = S.insert (rootFromFileName filename) $ confSourceDirectories conf }) $
                 mapM retrieveModule importedModules
      exports <- case exportSpec of
                   -- if there's no export spec then everything is exported
                   Nothing    -> return allSymbols
                   Just specs -> do
                     let exportsOfImports =
                           M.fromList $
                           zipWith (curry (id *** map modExports)) importedModules imports
                     M.unions <$> mapM (exportSpecToMap allSymbols exportsOfImports) specs
      lastModified <- liftIO $ getModificationTime filename
      return $ Module importedModules importQualifiers exports allSymbols source filename lastModified
  where
    parseMode :: ParseMode
    parseMode = defaultParseMode { parseFilename         = filename
                                 , baseLanguage          = Haskell2010
                                 , ignoreLanguagePragmas = False
                                 , ignoreLinePragmas     = True
                                 }

    extractHeader :: Text -> Maybe Text
    extractHeader source
      | "module" `T.isInfixOf` source =
        let (_prefix, headerAndBody) = T.breakOn "module" source
            (header, _body)          = T.breakOn "where" headerAndBody
        in Just $ header <> " where"
      | otherwise = Nothing

    convertModName :: HSE.ModuleName -> ModuleName
    convertModName (HSE.ModuleName name) = ModuleName $ T.pack name

    convertName :: HSE.Name -> SymbolName
    convertName (HSE.Ident name)  = SymbolName $ T.pack name
    convertName (HSE.Symbol name) = SymbolName $ T.pack name

    exportSpecToMap :: (MonadError String m)
                    => Map SymbolName Symbol
                    -> Map ModuleName [Map SymbolName Symbol]
                    -> HSE.ExportSpec
                    -> m (Map SymbolName Symbol)
    exportSpecToMap allSymbols exportsOfImports (HSE.EVar _ qname) =
      convertQName allSymbols exportsOfImports qname
    exportSpecToMap allSymbols exportsOfImports (HSE.EAbs qname) =
      convertQName allSymbols exportsOfImports qname
    exportSpecToMap allSymbols exportsOfImports (HSE.EThingAll qname) =
      convertQName allSymbols exportsOfImports qname
    exportSpecToMap allSymbols exportsOfImports (HSE.EThingWith qname cs) =
      throwError "HSE.EThingWith not supported yet"
      -- (<>) <$> convertQName allSymbols exportsOfImports name `
      --      <*> mapM (convertCName allSymbols)
    exportSpecToMap allSymbols exportsOfImports (HSE.EModuleContents modName) =
      let modName' = convertModName modName
      in case M.lookup modName' exportsOfImports of
           Nothing -> throwError $ "cannot find exported module " ++ show modName' ++ " among imports of " ++ filename
           Just exports -> return $ M.unions exports

    convertQName :: (MonadError String m)
                 => Map SymbolName Symbol
                 -> Map ModuleName [Map SymbolName Symbol]
                 -> HSE.QName
                 -> m (Map SymbolName Symbol)
    convertQName allSymbols exportsOfImports qname =
      case qname of
        HSE.Qual modName name ->
          throwError $ "unsure whether modName in Qual is a raw qualifier or resolved module name: " ++
            "HSE.Qual: modName = " ++ show modName ++ ", name = " ++ show name
        HSE.UnQual name ->
          let symName = convertName name
          in case M.lookup symName allSymbols of
               Nothing  -> throwError $ "exported symbol " ++ show symName ++ " not detected by tags"
               Just sym -> return $ M.singleton symName sym
        HSE.Special _spec ->
          throwError "ERROR: HSE.Special should not occur in the export list (or should it? - please recheck)"


-- NB won't handle indented module, but nobody seems to do that anyway
extractImports :: Text -> Text
extractImports = T.unlines . keepImports . T.lines
  where
    keepImports :: [Text] -> [Text]
    -- keepImports = filter ("import " `T.isInfixOf`)
    keepImports [] = []
    keepImports (t:ts)
      | "import" `T.isPrefixOf` t = t : keepBlock ts
      | otherwise                 = keepImports ts

    keepBlock :: [Text] -> [Text]
    keepBlock []         = []
    keepBlock ts'@(t:ts) =
      case T.uncons t of
        Nothing        -> t : keepBlock ts
        Just (' ', cs) -> t : keepBlock ts
        _              -> keepImports ts'

-- | Strip trailing part that corresponds to module names and return presumable
-- project root the @filename@ belongs to.
rootFromFileName :: FilePath -> FilePath
rootFromFileName =
  joinPath .
  reverse .
  dropWhile (\dir -> null dir || isUpper (head dir)) .
  tail .
  reverse .
  splitDirectories

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

