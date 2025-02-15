-- |
-- Module:     FasterRicherTags
-- Copyright:  (c) Sergey Vinokurov 2025
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module FasterRicherTagsMain (main) where

import Control.Concurrent
import Control.Monad
import Control.Monad.ErrorExcept
import Control.Monad.Writer
import Data.Bifunctor
import Data.Coerce
import Data.Foldable
import Data.List qualified as L
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Data.Traversable
import Options.Applicative
-- import Prettyprinter
import Prettyprinter.Combinators
import Prettyprinter.Show (ppShow)
import System.Directory.OsPath
import System.Directory.OsPath.Types
import System.Exit
import System.File.OsPath
import System.OsPath
import System.OsPath.Ext

import Control.Monad.Logging.Simple
import Data.Filesystem.Find
import Data.Ignores
import Data.Path qualified
import Haskell.Language.Server.Tags
import Haskell.Language.Server.Tags.Types

data Config = Config
  { cfgNullSeparated :: Bool
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

  putStrLn $ "length files = " ++ show (length files)

  let conf = defaultTagsServerConf


  -- (res, logs) <-
  --   runWriterT $ runSimpleLoggerT (Just (Custom (tell . (:[])))) Debug $
  res <-
    runSimpleLoggerT (Nothing @(Destination IO)) Debug $
      runErrorExceptT $
        fmap catMaybes $ for files $ \path -> do
          path'  <- liftIO $ Data.Path.fromFileOsPath path
          isFile <- liftIO $ doesFileExist $ Data.Path.toOsPath path'
          unless isFile $
            liftIO $ die $ "Input path does not point to file: " ++ show path'
          loadMod conf path'

  -- putStrLn $ "logs = " ++ show logs

  case res of
    Left err   -> die $ renderString $ "Error during load:" ## pretty err
    Right mods -> do
      let mods' = L.sortBy (comparing fst) mods
      putDocLn $ "Import keys:" ## pretty mods'

      -- putDocLn $ "res = " ## pretty res'

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
