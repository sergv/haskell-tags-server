-- |
-- Module:     FasterRicherTags
-- Copyright:  (c) Sergey Vinokurov 2025
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module FasterRicherTagsMain (main) where

import Control.Concurrent
import Control.Monad
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
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes)
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
import System.Directory.OsPath.Types
import System.Exit
import System.File.OsPath
import System.IO (stderr, stdout)
import System.OsPath
import System.OsPath.Ext

import Control.Monad.Logging.Simple
import Data.Filesystem.Find
import Data.Ignores
import Data.Path qualified as Path
import Data.Symbols
import Haskell.Language.Server.Tags
import Haskell.Language.Server.Tags.LoadFiles
import Haskell.Language.Server.Tags.Types
import Haskell.Language.Server.Tags.Types.Imports
import Haskell.Language.Server.Tags.Types.Modules

import FasterRicherTags.CompactFormat

data Config = Config
  {
  --   cfgExcludedDirBasenameGlobs :: ![Basename Text]
  -- , cfgExcludedDirAbsPaths      :: ![Text]
  -- , cfgDirsToAnalyze            :: ![OsPath]
  -- ,
    cfgNullSeparated            :: Bool
  }

optsParser :: Parser Config
optsParser = do
  -- cfgExcludedDirBasenameGlobs <- many $ fmap (Basename . T.pack) $ strOption $
  --   long "input" <>
  --   metavar "DIR-BASENAME-GLOB" <>
  --   help "Glob pattern matching directory basename to not descend into"
  --
  -- cfgExcludedDirAbsPaths <- many $ fmap T.pack $ strOption $
  --   long "input" <>
  --   metavar "ABS-DIR" <>
  --   help "Glob pattern matching directory basename to not descend into"
  --
  -- cfgDirsToAnalyze <- many $ argument (eitherReader (bimap show id . encodeUtf)) $
  --   metavar "FILE" <>
  --   help "An input directory to descend into"

  cfgNullSeparated <- switch $
    long "null" <>
    help "Input files on stdin are null-separated"

  pure Config{..}

progInfo :: ParserInfo Config
progInfo = info
  (helper <*> optsParser)
  (fullDesc <> header "Like ‘fast-tags’ but produces extra info about where names can be imported from (both which modules and which packages) by tracking name reexports.")

-- shouldCollectHaskellFile :: AbsDir -> AbsFile -> Basename OsPath -> IO (Maybe OsPath)
-- shouldCollectHaskellFile absDir absFile (Basename basePath) = undefined

main :: IO ()
main = do
  Config{cfgNullSeparated} <-
    customExecParser (prefs (showHelpOnEmpty <> noBacktrack <> multiSuffix "*")) progInfo

  let !sep
        | cfgNullSeparated = '0'
        | otherwise        = '\n'

  files <- map (pathFromText . TL.toStrict) . filter (not . TL.null) . TL.split (== sep) <$> TL.getContents

  let conf = defaultTagsServerConf

  hPutDocLn stderr $ "files count" <+> pretty (length files)

  -- (res, logs) <-
  --   runWriterT $ runSimpleLoggerT (Just (Custom (tell . (:[])))) Debug $
  res <-
    runSimpleLoggerT (Nothing @(Destination IO)) Debug $
      runErrorExceptT $ do
        mods <- fmap catMaybes $ for files $ \path -> do
          path'  <- liftIO $ Path.fromFileOsPath path
          isFile <- liftIO $ doesFileExist $ Path.toOsPath path'
          unless isFile $
            liftIO $ die $ "Input path does not point to file: " ++ show path'
          loadMod conf path'

        let mods' :: [(ImportKey, UnresolvedModule)]
            mods' = L.sortBy (comparing fst) $ filter (not . T.null . getModuleName . ikModuleName . fst) mods
            unresolvedMods :: Map ImportKey (NonEmpty UnresolvedModule)
            unresolvedMods =
              M.fromListWith (<>) (map (second (:| [])) mods')
            ls    = LoadState
              { lsLoadedModules   = mempty
              , lsLoadsInProgress = mempty
              , lsUnloadedFiles   = mempty
              }
        -- hPutDocLn stderr $ "Import keys:" ## pretty mods'

        -- TODO: T.null . getModuleName . ikModuleName

        -- liftIO $ hPutDocLn stderr $ "unresolved modules count =" <+> pretty (length mods')
        -- liftIO $ hPutDocLn stderr $ "unique unresolved modules count =" <+> pretty (M.size unresolvedMods)
        -- liftIO $ hPutDocLn stderr $ "non-unique unresolved modules:" ## pretty (filter ((> 1) . length . snd) (M.toList unresolvedMods))

        (resolvedMods, ls') <-
          (`runStateT` ls) $
            loadAllFilesIntoState unresolvedMods conf

        pure (resolvedMods, ls', unresolvedMods)

  -- putStrLn $ "logs = " ++ show logs

  (resolvedMods, _ls, _unresolvedMods) <- case res of
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
  --
  -- putDocLn $ "Resolved modules:" ## ppMap resolvedMods

  writeTo stdout $ (`M.foldMapWithKey` resolvedMods) $ \importKey (resolvedMod :: NonEmpty ResolvedModule) ->
    case ikImportTarget importKey of
      VanillaModule ->
        map
          (\m -> (Path.unFullPath (modFile m), SM.toList (modAllSymbols m)))
          (toList resolvedMod)
      HsBootModule  -> mempty

  -- writeTo stdout $ (`foldMap` resolvedModules) $ \

  -- hPutDocLn stderr $ "res = " ## pretty ls

  -- contents <- map (BSS.toShort . BSL.toStrict) . BSL.split sep <$> BSL.getContents


  -- Config{cfgExcludedDirBasenameGlobs, cfgExcludedDirAbsPaths, cfgDirsToAnalyze} <-
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
