----------------------------------------------------------------------------
-- |
-- Module      :  Server
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

module Server
  ( ServerConfig(..)
  , runServer
  )
where

import Control.Applicative
import Control.Arrow
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Array as A
import Data.Char
import Data.IORef
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.Socket (PortNumber)
import System.Directory
import System.Exit
import System.FilePath
import System.IO

import Data.BERT
import FastTags (Pos(..), SrcPos(..), TagVal(..), processAll, process)
import qualified Network.BERT.Server as BERT
import qualified Network.BERT.Transport as BERT
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Parser
import qualified Language.Haskell.Exts.Syntax as HSE
import Text.Regex.TDFA
import qualified Text.Regex.TDFA.Text as TDFA


data ServerConfig = ServerConfig
  { confSourceDirectories :: Set FilePath -- ^ directories with haskell files to index
  , confCabalDirectories  :: Set FilePath -- ^ directories with cabal packages to index
  , confPort              :: PortNumber
  , confLazyTagging       :: Bool       -- ^ whether to read and compute tags lazily
  }
  deriving (Show, Eq, Ord)

-- e.g. Foo, Foo.Bar
newtype ModuleName = ModuleName { getModuleName :: Text }
  deriving (Show, Eq, Ord)

type Symbol = Pos TagVal

newtype SymbolName = SymbolName { getSymbolName :: Text }
  deriving (Show, Eq, Ord)

data Module = Module
  { modImports          :: [ModuleName] -- ^ all imports of a given module, even qualified ones
  , modImportQualifiers :: Map ModuleName ModuleName -- ^ mapping from qualifiers to original module names
  , modExports          :: Map SymbolName Symbol
  , modAllSymbols       :: Map SymbolName Symbol
  , modSource           :: Text
  , modFile             :: FilePath
  }
  deriving (Show, Eq, Ord)

data ServerState = ServerState
  { stateModules :: Map ModuleName [Module]
  }
  deriving (Show, Eq, Ord)

emptyServerState :: ServerState
emptyServerState = ServerState M.empty

runServer :: ServerConfig -> IO ()
runServer conf = do
  tcpServer <- BERT.tcpServer (confPort conf)
  stateRef  <- newIORef emptyServerState
  unless (confLazyTagging conf) $ do
    putStrLn "collecting tags"
    putStrLn "NOT IMPLEMENTED YET: eager tags collection"
    exitFailure
  BERT.serve tcpServer (go stateRef)
  where
    go :: IORef ServerState -> String -> String -> [Term] -> IO BERT.DispatchResult
    go stateRef "tags-server" "find" args =
      case args of
        [BinaryTerm filename, BinaryTerm symbol] -> do
          state <- readIORef stateRef
          (res, state') <- runStateT
                             (runReaderT
                               (runExceptT
                                  (findSymbol
                                     (UTF8.toString filename)
                                     (SymbolName $ strToText symbol)))
                               conf)
                             state
          writeIORef stateRef state'
          case res of
            Left msg ->
              return $ BERT.Success $ TupleTerm
                [ AtomTerm "error"
                , BinaryTerm (UTF8.fromString msg)
                ]
            Right [] ->
              return $ BERT.Success $ TupleTerm
                [ AtomTerm "error"
                , BinaryTerm (UTF8.fromString "no symbols found")
                ]
            Right [sym] ->
              return $ BERT.Success $ TupleTerm
                [ AtomTerm "loc_known"
                , symbolToBERT sym
                ]
            Right symbols ->
              return $ BERT.Success $ TupleTerm
                [ AtomTerm "loc_ambiguous"
                , ListTerm $ map symbolToBERT symbols
                ]
        _                                    ->
          return $ BERT.Success $ TupleTerm
            [ AtomTerm "error"
            , BinaryTerm (UTF8.fromString $ "invalid number of arguments: " ++ show args)
            ]
    go _ "tags-server" _ _ = return BERT.NoSuchFunction
    go _ _             _ _ = return BERT.NoSuchModule

    strToText :: UTF8.ByteString -> Text
    strToText = T.pack . UTF8.toString

    symbolToBERT :: Symbol -> Term
    symbolToBERT (Pos (SrcPos filename line) _) =
      TupleTerm
        [ BinaryTerm (UTF8.fromString filename)
        , IntTerm line
        ]


findSymbol :: (Functor m, MonadError String m, MonadState ServerState m, MonadReader ServerConfig m, MonadIO m)
           => FilePath -> SymbolName -> m [Symbol]
findSymbol filename sym = do
  modules <- gets stateModules
  modName <- fileNameToModuleName filename
  case M.lookup modName modules of
    Nothing  -> do
      mod <- loadModuleFromFile filename
      findInModule sym mod
    Just mods -> do
      concat <$> mapM (findInModule sym) mods

findInModule :: forall m. (Functor m, MonadError String m, MonadState ServerState m, MonadReader ServerConfig m, MonadIO m)
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
    return $ maybe [] (:[]) (M.lookup sym (modAllSymbols mod)) ++ concat importedSyms
  where
    lookupInExports :: ModuleName -> SymbolName -> m [Symbol]
    lookupInExports modName sym = do
      mods <- retrieveModule modName
      return $ concatMap (maybe [] (:[]) . M.lookup sym . modExports) mods

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

retrieveModule :: forall m. (Functor m, MonadError String m, MonadState ServerState m, MonadReader ServerConfig m, MonadIO m)
               => ModuleName -> m [Module]
retrieveModule modName = do
  modules <- gets stateModules
  case M.lookup modName modules of
    Nothing   -> do
      mods <- loadModule modName
      modify (\s -> s { stateModules = M.insert modName mods $ stateModules s })
      return mods
    Just mods -> return mods
  where
    loadModule :: ModuleName -> m [Module]
    loadModule (ModuleName modName) = do
      srcDirs <- asks confSourceDirectories
      let possiblePaths = [ root </> filenamePart <.> ext
                          | root <- srcDirs
                          , ext  <- ["hs", "lhs", "hsc"]
                          ]
      actualPaths <- filterM (liftIO . doesFileExist) possiblePaths
      liftIO $ putStrLn $ "srcDirs = " <> show srcDirs
      liftIO $ putStrLn $ "actualPaths = " <> show actualPaths
      liftIO $ hFlush stdout
      mapM loadModuleFromFile actualPaths
      where
        filenamePart :: FilePath
        filenamePart = T.unpack $ T.map (\c -> if c == '.' then pathSeparator else c) modName

-- | Recursively load module from file - load given filename, then load all its
-- imports.
loadModuleFromFile :: (Functor m, MonadError String m, MonadState ServerState m, MonadReader ServerConfig m, MonadIO m)
                   => FilePath -> m Module
loadModuleFromFile filename = do
  source <- liftIO $ T.readFile filename
  let (allTags, _warnings) = process filename False source
      allSymbols           = M.fromList $ map (\tag@(Pos _ (TagVal _ name _)) -> (SymbolName name, tag)) allTags
      moduleHeader         = extractHeader source
      moduleImports        = extractImports source
      interface            = fromMaybe T.empty moduleHeader <>
                             T.singleton '\n' <>
                             moduleImports
  liftIO $ T.putStrLn $ "parsing\n>>>>\n" <> interface <> "\n<<<<"
  case parseWithMode parseMode (T.unpack interface) of
    ParseFailed loc msg -> do
      liftIO $ T.putStrLn $ "failed to parse module header and imports: " <> interface
      throwError $ "failed to parse module header and imports at " ++ show loc ++ ": " ++ msg
    ParseOk (HSE.Module _loc _name _pragmas _warnings exportSpec imports _body) -> do
      let importedModules  = map (convertModName . HSE.importModule) imports
          importQualifiers =
            M.fromList $ catMaybes $
            zipWith (\x name -> ((,name) . convertModName) <$> HSE.importAs x) imports importedModules
      -- tie the knot
      imports <- local (\conf -> conf { confSourceDirectories = rootFromFileName filename : confSourceDirectories conf }) $
                 mapM retrieveModule importedModules
      let exportsOfImports = M.fromList $ zipWith (curry (id *** map modExports)) importedModules imports
      exports <- case exportSpec of
                   -- if there's no export spec then everything is exported
                   Nothing    -> return allSymbols
                   Just specs -> M.unions <$>
                     mapM (exportSpecToMap allSymbols exportsOfImports) specs
      return $ Module importedModules importQualifiers exports allSymbols source filename
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

    extractImports :: Text -> Text
    extractImports = T.unlines . filter ("import" `T.isInfixOf`) . T.lines

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
        HSE.Special spec ->
          throwError "ERROR: HSE.Special should not occur in the export list (or should it? - please recheck)"

-- | Strip trailing part that corresponds to module names and return presumable
-- project root the @filename@ belongs to.
rootFromFileName :: FilePath -> FilePath
rootFromFileName =
  joinPath . reverse . dropWhile (\dir -> null dir || isUpper (head dir)) . tail . reverse . splitDirectories

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
