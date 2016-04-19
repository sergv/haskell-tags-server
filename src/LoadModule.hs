----------------------------------------------------------------------------
-- |
-- Module      :  LoadModule
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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

module LoadModule (retrieveModule, loadModuleFromFile) where

import Control.Applicative
import Control.Arrow
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Traversable as Tr
import System.Directory
import System.FilePath

import FastTags (process) -- processAll,
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Parser
import qualified Language.Haskell.Exts.Syntax as HSE

import Logging
import Types

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
                 modTime <- liftIO $ getModificationTime $ modFile m
                 if modLastModified m /= modTime
                   then loadModuleFromFile $ modFile m
                   else return m
  modify (\s -> s { stateModules = M.insert modName mods $ stateModules s })
  return mods
  where
    loadModule :: ModuleName -> m [Module]
    loadModule (ModuleName modName) = do
      srcDirs <- asks confSourceDirectories
      debugM $ "loading module " <> modName
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
  let allTags    = fst $ process filename False source
      allSymbols = M.fromList $ map ((symbolName &&& id) . Symbol) allTags
      -- todo include children of dependent modules
      children   = M.fromListWith (++) $
                   map (\(child, parent) -> (parent, [child])) $
                   M.toList $
                   M.mapMaybe symbolParent allSymbols
      interface :: Text
      interface  = extractModuleInterface source
  -- debugM $ "parsing module interface\n>>>>\n" <> interface <> "\n<<<<"
  case parseWithMode (parseMode filename) (T.unpack interface) of
    ParseFailed loc msg -> do
      errorM $ "failed to parse module header and imports:\n" <>
        "haskel-src-exts error message: " <> show' loc <> ": " <> T.pack msg <> "\n" <>
        ">>>>" <> interface <> "\n<<<<"
      throwError $ "failed to parse module header and imports at " ++ show loc ++ ": " ++ msg
    ParseOk (HSE.Module _loc _name _pragmas _warnings exportSpec importDecls _body) -> do
      let importedModules  = mkModules importDecls
          importQualifiers = mkQualifiersMap importedModules
      exports <- case exportSpec of
                   -- if there's no export spec then everything is exported
                   Nothing    -> return allSymbols
                   Just specs -> do
                     -- tie the knot
                     debugM $ "loading imported modules " <> show' (map (getModuleName . fst) importedModules)
                     imports' <- local (\conf ->
                                          let srcDirs' = S.insert (rootFromFileName filename) $ confSourceDirectories conf
                                          in conf { confSourceDirectories = srcDirs' }) $
                                 mapM (retrieveModule . fst) importedModules
                     let importedModsMap =
                           zipWith (\(modName, qual) modss -> (modName, qual, modss))
                                   importedModules
                                   imports'
                         convertSpec = exportSpecToMap
                                         allSymbols
                                         children
                                         importedModsMap
                                         importQualifiers
                     M.unions <$> mapM convertSpec specs
      lastModified <- liftIO $ getModificationTime filename
      debugM $ "inferred exports " <> show' (M.keys exports)
      return $ Module
                 { modImports          = importedModules
                 , modImportQualifiers = importQualifiers
                 , modExports          = exports
                 , modChildrenMap      = children
                 , modAllSymbols       = allSymbols
                 , modSource           = source
                 , modFile             = filename
                 , modLastModified     = lastModified
                 }
  where
    mkModules :: [HSE.ImportDecl] -> [(ModuleName, Qualification)]
    mkModules = map (\decl ->
                      let modName = convertModName (HSE.importModule decl)
                      in (modName, mkQualification modName decl))

    mkQualifiersMap :: [(ModuleName, Qualification)] -> Map ModuleName ModuleName
    mkQualifiersMap importedModules =
       M.fromList $
       mapMaybe (\(name, qual) -> (, name) <$> getQualifier qual) importedModules

    mkSymbolMap :: NonEmpty Symbol -> Map SymbolName Symbol
    mkSymbolMap = M.fromList . map (symbolName &&& id) . NE.toList

    exportSpecToMap :: (MonadError String m, Functor m)
                    => Map SymbolName Symbol
                    -> Map SymbolName [SymbolName]
                    -> [(ModuleName, Qualification, [Module])]
                    -> Map ModuleName ModuleName
                    -> HSE.ExportSpec
                    -> m (Map SymbolName Symbol)
    exportSpecToMap allSymbols _ imports importQualifiers (HSE.EVar _ qname) =
      mkSymbolMap . NE.map fst <$> convertQName allSymbols imports importQualifiers qname
    exportSpecToMap allSymbols _ imports importQualifiers (HSE.EAbs qname) =
      mkSymbolMap . NE.map fst <$> convertQName allSymbols imports importQualifiers qname
    exportSpecToMap allSymbols childrenMap imports importQualifiers (HSE.EThingAll qname) = do
      syms <- convertQName allSymbols imports importQualifiers qname
      liftM M.unions $ forM (NE.toList syms) $ \(sym, mod) -> do
        let childrenMap' = maybe childrenMap modChildrenMap mod
        case M.lookup (symbolName sym) childrenMap' of
          Nothing ->
            throwError $ "no items export with wildcard export for " ++ T.unpack (getSymbolName $ symbolName sym)
          Just children -> do
            let allSymbols' = maybe allSymbols modAllSymbols mod
            case Tr.traverse (`M.lookup` allSymbols') children of
              Nothing -> throwError $ "module " ++ maybe filename modFile mod ++
                         " has modChildrenMap entries that don't resolve to " ++
                         "any symbol in that module: " ++ show children
              Just children' ->
                return $ mkSymbolMap $ sym :| children'
    exportSpecToMap allSymbols _ imports importQualifiers (HSE.EThingWith qname (map convertCName -> cs)) = do
      syms <- convertQName allSymbols imports importQualifiers qname
      liftM M.unions $ forM (NE.toList syms) $ \(sym, mod) -> do
        -- if mod is Nothing then sym comes from the module we're currently loading
        -- and thus should be found within allSymbols map rather than exports map
        -- which is not finish at this time.
        let candidateSyms = maybe allSymbols modExports mod
        case Tr.traverse (`M.lookup` candidateSyms) cs of
          -- not all exported symbols found - this is the wrong module
          Nothing      -> return M.empty
          Just symbols -> return $ mkSymbolMap $ sym :| symbols
      -- (<>) <$> convertQName modName allSymbols name `
      --      <*> mapM (convertCName allSymbols)
    exportSpecToMap _          _ imports _ (HSE.EModuleContents (convertModName -> exportedModName)) =
      case filter (\(name, _, _) -> name == exportedModName) imports of
        []       ->
          case filter (\(_, qual, _) -> maybe False (== exportedModName) (getQualifier qual)) imports of
            [] ->
              throwError $ "cannot find exported module " ++ show exportedModName ++ " among imports of " ++ filename ++ "\nimports = " ++ show (map (\(name, qual, _) -> (name, qual)) imports)
            -- don't reexport through qualified module reexport since GHC doesn't
            -- reexport.
            _ -> return M.empty
        imports' ->
          return $ M.unions $ concatMap (\(_, _, mods) -> map modExports mods) imports'

-- | extract module header (the "module ... where" part) and imports
extractModuleInterface :: Text -> Text
extractModuleInterface source = interface
  where
    moduleHeader  = extractHeader source
    moduleImports = extractImports source
    interface     = fromMaybe T.empty moduleHeader <>
                    T.singleton '\n' <>
                    moduleImports

    extractHeader :: Text -> Maybe Text
    extractHeader source
      | Just line <- find ("module" `T.isPrefixOf`) lines =
        let (_prefix, headerAndBody) = T.breakOn "module" source
            (header, _body)          = T.breakOn "where" headerAndBody
        in Just $ header <> " where"
      | otherwise = Nothing

    lines = T.lines source

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
            Nothing       -> t : keepBlock ts
            Just (' ', _) -> t : keepBlock ts
            _             -> keepImports ts'

parseMode :: FilePath -> ParseMode
parseMode filename =
  defaultParseMode
    { parseFilename         = filename
    , baseLanguage          = Haskell2010
    , ignoreLanguagePragmas = False
    , ignoreLinePragmas     = True
    }

mkQualification :: ModuleName -> HSE.ImportDecl -> Qualification
mkQualification modName decl
  | HSE.importQualified decl =
    Qualified $ maybe modName convertModName $ HSE.importAs decl
  | otherwise                =
    maybe Unqualified (BothQualifiedAndUnqualified . convertModName) $ HSE.importAs decl

convertModName :: HSE.ModuleName -> ModuleName
convertModName (HSE.ModuleName name) = ModuleName $ T.pack name

convertCName :: HSE.CName -> SymbolName
convertCName (HSE.VarName name) = convertName name
convertCName (HSE.ConName name) = convertName name

convertName :: HSE.Name -> SymbolName
convertName (HSE.Ident name)  = SymbolName $ T.pack name
convertName (HSE.Symbol name) = SymbolName $ T.pack name

-- | Resolve haskell-src-exts qualified name into symbol along with its module.
-- If symbol module is Nothing then the symbol is from the module we're currently
-- loading (aka "this" module).
convertQName :: (MonadError String m)
             => Map SymbolName Symbol
             -> [(ModuleName, Qualification, [Module])]
             -> Map ModuleName ModuleName
             -> HSE.QName
             -> m (NonEmpty (Symbol, Maybe Module))
convertQName thisModSymbols thisModImports thisModQualifiers qname =
  case qname of
    HSE.Qual modName (convertName -> symName) ->
      -- The thisModImports arg will be necessary here since current module
      -- cannot define qualified names - they must come from other modules.
      --
      -- NB modName in HSE.Qual is a raw qualifier
      case M.lookup (convertModName modName) thisModQualifiers of
        Nothing          ->
          throwError $ "unrecognised qualifier: " ++ show modName
        Just realModName ->
          case concatMap (\(_, _, mods) -> mods) $
               filter (\(name, _, _) -> name == realModName) thisModQualImports of
            []       ->
              throwError $ "qualified name comes from unimported module: " ++ show qname
            qualImps ->
              case mapMaybe (\mod -> (,Just mod) <$> M.lookup symName (modExports mod))
                            qualImps of
                []     ->
                  throwError $ "exported symbol " ++ show qname ++ " not detected by tags"
                s:syms -> return $ s :| syms
      -- throwError $ "unsure whether modName in Qual is a raw qualifier or resolved module name: " ++
      --   "HSE.Qual: modName = " ++ show modName ++ ", name = " ++ show name
      where
        thisModQualImports =
          filter (\(_, qual, _) -> importsQualifiedNames qual) thisModImports
    HSE.UnQual (convertName -> symName) ->
      case M.lookup symName thisModSymbols of
        Nothing  ->
          case mapMaybe (\mod -> (,Just mod) <$> M.lookup symName (modExports mod))
                        (concatMap (\(_, _, mods) -> mods) thisModUnqualImports) of
            []     -> throwError $ "exported symbol " ++ show qname ++ " not detected by tags"
            s:syms -> return $ s :| syms
        Just sym -> return $ (sym, Nothing) :| []
      where
        thisModUnqualImports =
          filter (\(_, qual, _) -> importsUnqualifiedNames qual) thisModImports
    HSE.Special _spec ->
      throwError "ERROR: HSE.Special should not occur in the export list (or should it? - please recheck)"

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
