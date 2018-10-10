----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.ErrorExcept
import Control.Monad.Except

import Data.Foldable (for_)
import qualified Data.List as L
import qualified Data.Set as S
import Data.Text.Prettyprint.Doc.Ext
import Network.Socket (PortNumber)
import Options.Applicative
import System.Directory
import System.Exit
import System.IO

import Control.Monad.Filesystem (SearchCfg(..))
import qualified Control.Monad.Filesystem as MonadFS
import Control.Monad.Logging
import Control.Monad.Logging.Simple
import Data.ErrorMessage
import Data.Path (mkFullPath)
import Haskell.Language.Server.BERT
import Haskell.Language.Server.Tags
import Haskell.Language.Server.Tags.Types (NameResolutionStrictness(..))

data ProgramConfig = ProgramConfig
  { cfgSourceDirectories :: [FilePath] -- ^ Directories with haskell files to index
  , cfgDirTrees          :: [FilePath] -- ^ Directories that should be searched recursively - i.e.
                                       -- all their subdirectories are added to cfgSourceDirectories
  , cfgPort              :: PortNumber
    -- ^ Whether to eagerly read and resolve all tags at the start or to
    -- lazily load only required modules on a per-request basis.|
  , cfgEagerTagging      :: Bool
  , cfgNameResolution    :: NameResolutionStrictness
  , cfgDebugVerbosity    :: Severity
  } deriving (Eq, Ord, Show)

optsParser :: Parser ProgramConfig
optsParser = ProgramConfig
  <$> many
        (strOption
           (long "dir" <>
            help "add directory with haskell files to index" <>
            metavar "DIR"))
  <*> many
        (strOption
           (long "recursive" <>
            help "recursively add directory tree with haskell files to index" <>
            metavar "DIR"))
  <*> option (fmap fromIntegral auto)
        (short 'p' <>
         long "port" <>
         help "port to listen to" <>
         value defaultPort <>
         metavar "PORT")
  <*> switch
        (long "eager-tagging" <>
         help "whether to load all tags at application start")
  <*> flag NameResolutionLax NameResolutionStrict
        (long "strict" <>
         help "Resolve names strictly forbidding any situations like exported name not being defined in a module.")
  <*> option (eitherReader readSeverity)
        (long "verbosity" <>
         value Error <>
         showDefaultWith showSeverity <>
         help ("Debug verbosity. Known values: " ++ L.intercalate ", " knownSeverities ++ "."))

progInfo :: ParserInfo ProgramConfig
progInfo = info
  (helper <*> optsParser)
  (fullDesc <> header "Server for navigating Haskell programs")

main :: IO ()
main = do
  ProgramConfig{cfgSourceDirectories, cfgDirTrees, cfgPort, cfgEagerTagging, cfgNameResolution, cfgDebugVerbosity} <- execParser progInfo
  -- validate that specified directories actually exist
  for_ cfgSourceDirectories ensureDirExists
  for_ cfgDirTrees ensureDirExists

  runSimpleLoggerT (Just Stderr) cfgDebugVerbosity $ do
    tagsServer <- runErrorExceptT $ do
      cfgSourceDirectories' <- S.fromList <$> traverse mkFullPath cfgSourceDirectories
      cfgDirTrees'          <- S.fromList <$> traverse mkFullPath cfgDirTrees
      let searchDirs = SearchCfg
            { scShallowPaths   = cfgSourceDirectories'
            , scRecursivePaths = cfgDirTrees'
            , scIgnoredDirs    = MonadFS.versionControlDirs
            , scIgnoredGlobs   = MonadFS.defaultIgnoredGlobs
            }
          conf = defaultTagsServerConf
            { tsconfEagerTagging   = cfgEagerTagging
            , tsconfNameResolution = cfgNameResolution
            }
      logDebug $ ppDictHeader "Staring server with search dirs"
        [ "Search conf" --> searchDirs
        ]
      startTagsServer searchDirs conf :: ErrorExceptT ErrorMessage (SimpleLoggerT IO) TagsServer
    case tagsServer of
      Left err         -> liftIO $ putDocLn $ pretty err
      Right tagsServer' -> do
        bertServer <- runBertServer cfgPort $ tsRequestHandler tagsServer'
        waitForBertServerStart bertServer
        -- Wait for tag server to finish
        void $ waitForTagsServerFinish tagsServer'

ensureDirExists :: FilePath -> IO ()
ensureDirExists dir = do
  exists <- doesDirectoryExist dir
  unless exists $ do
    hPutStrLn stderr $ "error: directory " ++ dir ++ " does not exist"
    exitFailure
