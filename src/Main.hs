----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.Foldable (for_)
import qualified Data.Set as S
import Data.Text.Prettyprint.Doc.Ext
import Network.Socket (PortNumber)
import Options.Applicative
import System.Directory
import System.Exit
import System.IO

import Control.Monad.Filesystem.FileSearch (SearchCfg(..))
import Control.Monad.Logging
import Control.Monad.Logging.Simple
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
  <*> option verbosityArg
        (long "verbosity" <>
         value Error <>
         help "Debug verbosity - debug, info, warning, error. Default: error")
  where
    verbosityArg :: ReadM Severity
    verbosityArg = eitherReader $ \case
      "debug"   -> pure Debug
      "info"    -> pure Info
      "warning" -> pure Warning
      "error"   -> pure Error
      s         -> throwError $ "Invalid verbosity: " ++ s

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

  cfgSourceDirectories' <- S.fromList <$> traverse mkFullPath cfgSourceDirectories
  cfgDirTrees'          <- S.fromList <$> traverse mkFullPath cfgDirTrees
  let conf  = defaultTagsServerConf
                { tsconfSearchDirs     = (tsconfSearchDirs defaultTagsServerConf)
                    { shallowPaths   = cfgSourceDirectories'
                    , recursivePaths = cfgDirTrees'
                    }
                , tsconfEagerTagging   = cfgEagerTagging
                , tsconfNameResolution = cfgNameResolution
                }
      state = TagsServerState
                { tssLoadedModules   = mempty
                , tssLoadsInProgress = mempty
                }
  runSimpleLoggerT (Just Stderr) cfgDebugVerbosity $ do
    logDebug $ ppDictHeader "Staring server with search cfg"
      [ "Search conf" :-> pretty (tsconfSearchDirs conf)
      ]
    result <- runExceptT $ startTagsServer conf state
    case result of
      Left err         -> liftIO $ putDocLn $ pretty err
      Right tagsServer -> do
        bertServer <- runBertServer cfgPort $ tsRequestHandler tagsServer
        waitForBertServerStart bertServer
        -- Wait for tag server to finish
        void $ waitForTagsServerFinish tagsServer

ensureDirExists :: FilePath -> IO ()
ensureDirExists dir = do
  exists <- doesDirectoryExist dir
  unless exists $ do
    hPutStrLn stderr $ "error: directory " ++ dir ++ " does not exist"
    exitFailure
