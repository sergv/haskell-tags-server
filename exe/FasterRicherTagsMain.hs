-- |
-- Module:     FasterRicherTags
-- Copyright:  (c) Sergey Vinokurov 2025
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module FasterRicherTagsMain (main) where

import Debug.Trace qualified
import Prettyprinter.Instances ()

import Prelude hiding (mod)

import Control.Concurrent
import Control.Monad
import Control.Monad.Error.Class (MonadError)
import Control.Monad.ErrorExcept
import Control.Monad.State
import Control.Monad.Writer
import Data.Bifunctor
import Data.Coerce
import Data.Foldable
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Map.NonEmpty (NonEmptyMap)
import Data.Map.NonEmpty qualified as NEMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Ord (comparing)
import Data.SymbolMap qualified as SM
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Data.Traversable
import Options.Applicative
import Prettyprinter ((<+>))
import Prettyprinter.Combinators
import Prettyprinter.Show (ppShow)
import System.Directory.OsPath
import System.Directory.OsPath.Types ()
import System.Exit
import System.File.OsPath
import System.IO (stderr, stdout)
import System.OsPath
import System.OsPath.Ext

import Control.Monad.Filesystem (MonadFS)
import Control.Monad.Filesystem qualified as MonadFS
import Control.Monad.Logging
import Control.Monad.Logging.Simple
import Data.ErrorMessage
import Data.Filesystem.Find
import Data.Ignores
import Data.MonoidalMap (MonoidalMap(..))
import Data.Path (FullPath, FileType(..))
import Data.Path qualified as Path
import Data.Symbols
import Haskell.Language.Lexer (modeFromFilename)
import Haskell.Language.Server.Tags
import Haskell.Language.Server.Tags.LoadFiles
import Haskell.Language.Server.Tags.LoadModule
import Haskell.Language.Server.Tags.Search (findSymbol, classifyPath, findSymbolInFiles, loadMany)
import Haskell.Language.Server.Tags.SearchM
import Haskell.Language.Server.Tags.Types
import Haskell.Language.Server.Tags.Types.Imports
import Haskell.Language.Server.Tags.Types.Modules

import FasterRicherTags.CompactFormat

headerWithDesc :: String -> InfoMod a
headerWithDesc x = header x <> progDesc x

data Command
  = Generate GenConfig
  | Search SearchConfig

data GenConfig = GenConfig
  { gcfgNullSeparated :: Bool
  }

flagNullSep :: Parser Bool
flagNullSep = switch $
  long "null" <>
  help "Input files on stdin are null-separated"

genParser :: Parser GenConfig
genParser = do
  gcfgNullSeparated <- flagNullSep
  pure GenConfig{..}

generateInfo :: ParserInfo GenConfig
generateInfo =
  info
    (helper <*> genParser)
    (fullDesc <> headerWithDesc "Generate tags in compact sexp-based format from files specified on stdin")

data SearchConfig = SearchConfig
  { scfgSymbol        :: !Text
  , scfgFile          :: !Text
  , scfgNullSeparated :: !Bool
  }

searchParser :: Parser SearchConfig
searchParser = do
  scfgNullSeparated <- flagNullSep
  scfgSymbol        <- fmap T.pack $ strArgument $
    metavar "SYMBOL" <>
    help "Symbol to search for, literal string."
  scfgFile          <- fmap T.pack $ strArgument $
    metavar "FILE" <>
    help "Resolve searched symbols against imports in this file."
  pure SearchConfig{..}

searchInfo :: ParserInfo SearchConfig
searchInfo =
  info
    (helper <*> searchParser)
    (fullDesc <> headerWithDesc "Generate tags in compact sexp-based format from files specified on stdin")

commandParser :: Parser Command
commandParser =
  hsubparser $
    command "generate" (Generate    <$> generateInfo)  <>
    command "search"   (Search      <$> searchInfo)

progInfo :: ParserInfo Command
progInfo = info
  (helper <*> commandParser)
  (fullDesc <> header "Like ‘fast-tags’ but produces extra info about where names can be imported from (both which modules and which packages) by tracking name reexports.")

-- shouldCollectHaskellFile :: AbsDir -> AbsFile -> Basename OsPath -> IO (Maybe OsPath)
-- shouldCollectHaskellFile absDir absFile (Basename basePath) = undefined

main :: IO ()
main = do
  cmd <-
    customExecParser (prefs (showHelpOnEmpty <> noBacktrack <> multiSuffix "*")) progInfo

  case cmd of
    Generate cfg -> generate cfg
    Search   cfg -> search cfg

  -- GenConfig{cfgExcludedDirBasenameGlobs, cfgExcludedDirAbsPaths, cfgDirsToAnalyze} <-
  --   customExecParser (prefs (showHelpOnEmpty <> noBacktrack <> multiSuffix "*")) progInfo
  --
  -- cfgDirsToAnalyze' <- for cfgDirsToAnalyze $ \dir -> do
  --   exists <- doesDirectoryExist dir
  --   unless exists $
  --     die $ renderString $ "Directory" <+> squotes (ppShow dir) <+> "does not exist"
  --   pure $ AbsDir dir
  --
  -- dirIgnores <- Data.Ignores.mkIgnores cfgExcludedDirAbsPaths cfgExcludedDirBasenameGlobs
  --
  -- jobs <- getNumCapabilities
  --
  -- findRec FollowSymlinks jobs
  --   (\x y -> not $ isIgnored dirIgnores x y)
  --   shouldCollectHaskellFile
  --   cfgDirsToAnalyze'
  --
  -- -- _ <- T.decodeUtf8 <$> readFile' cfgInputFile

  pure ()

generate :: GenConfig -> IO ()
generate GenConfig{gcfgNullSeparated} = do

  let sep :: Char
      !sep
        | gcfgNullSeparated = '\0'
        | otherwise         = '\n'

  files <- map (pathFromText . TL.toStrict) . filter (not . TL.null) . TL.split (== sep) <$> TL.getContents

  let conf = defaultTagsServerConf

  -- (res, logs) <-
  --   runWriterT $ runSimpleLoggerT (Just (Custom (tell . (:[])))) Debug $
  res <-
    runSimpleLoggerT (Nothing @(Destination IO)) Debug $
      runErrorExceptT $ do
        (unresolvedMods :: Map ImportKey (NonEmpty UnresolvedModule)) <-
          fmap (M.unionsWith (<>)) $ for files $ \modPath -> do
            (loadMany conf =<<) $ liftIO $ do
              modPath' <- Path.fromFileOsPath modPath
              isFile   <- doesFileExist $ Path.toOsPath modPath'
              unless isFile $
                die $ "Input path does not point to file: " ++ show modPath'
              pure modPath'

        let ls = LoadState
              { lsLoadedModules   = mempty
              , lsLoadsInProgress = mempty
              , lsUnloadedFiles   = mempty
              }

            unresolvedMods' =
              M.filterWithKey
                (\k _ -> not $ T.null $ getModuleName $ ikModuleName k)
                unresolvedMods
        -- hPutDocLn stderr $ "Import keys:" ## pretty mods'

        -- TODO: T.null . getModuleName . ikModuleName

        -- liftIO $ hPutDocLn stderr $ "unresolved modules count =" <+> pretty (length mods')
        -- liftIO $ hPutDocLn stderr $ "unique unresolved modules count =" <+> pretty (M.size unresolvedMods)
        -- liftIO $ hPutDocLn stderr $ "non-unique unresolved modules:" ## pretty (filter ((> 1) . length . snd) (M.toList unresolvedMods))

        (resolvedMods, ls') <-
          (`runStateT` ls) $
            -- (`M.traverseMaybeWithKey` unresolvedMods) $ \importKey _ ->
            --   doResolve unresolvedMods tsconfNameResolution importKey
            loadAllFilesIntoState unresolvedMods' conf

        pure (resolvedMods, ls', unresolvedMods')

  -- putStrLn $ "logs = " ++ show logs

  (resolvedMods :: Map ImportKey (NonEmpty ResolvedModule), _ls, _unresolvedMods) <- case res of
    Left err -> die $ renderString $ "Error during load:" ## pretty err
    Right x  -> pure x

  -- hPutDocLn stderr $ "Loaded (resolved) modules count =" <+> pretty (M.size resolvedMods)
  --
  -- hPutDocLn stderr $ "Loaded (resolved) modules count from state =" <+> pretty (M.size (lsLoadedModules ls))
  --
  -- hPutDocLn stderr $ "Not loaded:" ## pretty (M.keys (M.difference unresolvedMods resolvedMods))

  -- hPutDocLn stderr $ "Loaded modules:" ## pretty (M.keys (lsLoadedModules ls))
  -- hPutDocLn stderr $ "Loaded modules:" ## ppMap (length <$> lsLoadedModules ls)

  -- hPutDocLn stderr $ "Loads in progress:" ## ppMap (lsLoadsInProgress ls)
  -- hPutDocLn stderr $ "Unloaded modules:" ## ppMap (lsUnloadedFiles ls)

  -- die $ renderString $ "Resolved module keys:" ## pretty (M.keys resolvedMods)

  -- putDocLn $ "Resolved modules:" ## ppMap resolvedMods



  -- putDocLn $ pretty $ M.lookup (ImportKey VanillaModule (mkModuleName "Happy.Frontend.Parser")) resolvedMods

  -- putDocLn $ pretty $ filter ((== "/home/sergey/projects/haskell/packages/all-packages/happy-lib-2.1.3/frontend/src/Happy/Frontend/Parser.hs") . fst) $ (`M.foldMapWithKey` resolvedMods) $ \importKey (resolvedMod :: NonEmpty ResolvedModule) ->
  --   case ikImportTarget importKey of
  --     VanillaModule ->
  --       map
  --         (\m -> (Path.unFullPath (modFile m), SM.toList (modAllSymbols m)))
  --         (toList resolvedMod)
  --     HsBootModule  -> mempty

  -- hPutDocLn stderr $ pretty $ (`M.foldMapWithKey` resolvedMods) $ \importKey (resolvedMod :: NonEmpty ResolvedModule) ->
  --   case ikImportTarget importKey of
  --     VanillaModule ->
  --       map
  --         (\m -> (ikModuleName importKey, Path.unFullPath (modFile m), SM.toList (modAllSymbols m)))
  --         (toList resolvedMod)
  --     HsBootModule  -> mempty

  -- writeTo stdout $ (`M.foldMapWithKey` resolvedMods) $ \importKey (resolvedMod :: NonEmpty ResolvedModule) ->
  --   -- todo: ignore ‘Main’ module?
  --   case ikImportTarget importKey of
  --     VanillaModule ->
  --       map
  --         (\m -> (Path.unFullPath (modFile m), SM.toList (modAllSymbols m)))
  --         (toList resolvedMod)
  --     HsBootModule  -> mempty

  let byFile :: Map (FullPath 'File) (NonEmpty (ImportKey, ResolvedModule))
      byFile =
        M.fromListWith (<>) $
          foldMap (\(importKey, mods) -> map (\m -> (modFile m, NE.singleton (importKey, m))) $ toList mods) $
            M.toList resolvedMods

  writeTo stdout $ (`M.foldMapWithKey` byFile) $ \path (mods :: NonEmpty (ImportKey, ResolvedModule)) ->
    let mods' :: [ResolvedModule]
        mods' =
          (`mapMaybe` toList mods) $ \(importKey, mod) ->
            -- todo: ignore ‘Main’ module?
            case ikImportTarget importKey of
              VanillaModule -> Just mod
              HsBootModule  -> Nothing
    in [(Path.unFullPath path, SM.toList $ foldMap modAllSymbols mods')]

-- writeTo stdout $ (`foldMap` resolvedModules) $ \

-- hPutDocLn stderr $ "res = " ## pretty ls

-- contents <- map (BSS.toShort . BSL.toStrict) . BSL.split sep <$> BSL.getContents

search :: SearchConfig -> IO ()
search SearchConfig{scfgNullSeparated, scfgSymbol, scfgFile} = do

  let sep :: Char
      !sep
        | scfgNullSeparated = '\0'
        | otherwise         = '\n'

  files <- map (pathFromText . TL.toStrict) . filter (not . TL.null) . TL.split (== sep) <$> TL.getContents

  let conf = defaultTagsServerConf

  res <-
    runSimpleLoggerT (Nothing @(Destination IO)) Debug $
    -- runSimpleLoggerT (Just Stderr) Debug $
      runErrorExceptT $ do

        path <- Path.mkFullPath scfgFile

        symbols <- findSymbolInFiles
          conf
          files
          ScopeCurrentModule
          path
          (mkSymbolName scfgSymbol)

        Debug.Trace.traceM $ renderString $ "search" ##
          ppSet symbols

        pure ()

  case res of
    Left err -> die $ renderString $ "Error during search:" ## pretty err
    Right x  -> pure x
