----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.Foldable (for_)
import Data.Set (Set)
import qualified Data.Set as S
import Network.Socket (PortNumber)
import Options.Applicative
import System.Directory
import System.Exit
import System.IO

import Control.Monad.Logging
import Control.Monad.Logging.Simple
import Haskell.Language.Server.BERT
import Haskell.Language.Server.Tags
import Text.PrettyPrint.Leijen.Text.Utils

data ProgramConfig = ProgramConfig
  { cfgSourceDirectories :: Set FilePath -- ^ Directories with haskell files to index
  , cfgCabalDirectories  :: Set FilePath -- ^ Directories with cabal packages to index
  , cfgDirTrees          :: Set FilePath -- ^ Directories that should be searched recursively - i.e.
                                         -- all their subdirectories are added to cfgSourceDirectories
  , cfgPort              :: PortNumber
  , cfgLazyTagging       :: Bool         -- ^ Whether to read and compute tags lazily
  , cfgDebugVerbosity    :: Severity
  } deriving (Show, Eq, Ord)

optsParser :: Parser ProgramConfig
optsParser = ProgramConfig
  <$> (S.fromList <$>
         many
           (strOption
              (long "dir" <>
               help "add directory with haskell files to index" <>
               metavar "DIR")))
  <*> (S.fromList <$>
         many
           (strOption
              (long "package" <>
               help "add directory with cabal package to index" <>
               metavar "DIR")))
  <*> (S.fromList <$>
         many
           (strOption
              (long "recursive" <>
               help "recursively add directory tree with haskell files to index" <>
               metavar "DIR")))
  <*> option (fmap fromIntegral auto)
        (short 'p' <>
         long "port" <>
         help "port to listen to" <>
         value defaultPort <>
         metavar "PORT")
  <*> switch
        (long "lazy-tagging" <>
         help "whether to compute tags lazily - only for files asked")
  <*> option verbosityArg
        ( long "verbosity" <>
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
  ProgramConfig{cfgSourceDirectories, cfgCabalDirectories, cfgDirTrees, cfgPort, cfgLazyTagging, cfgDebugVerbosity} <- execParser progInfo
  -- validate that specified directories actually exist
  for_ cfgSourceDirectories ensureDirExists
  unless (S.null cfgCabalDirectories) $ do
    hPutStrLn stderr "NOT IMPLEMENTED YET: analysis of cabal packages"
    exitFailure
  for_ cfgCabalDirectories ensureDirExists
  for_ cfgDirTrees ensureDirExists
  let conf  = TagsServerConf
                { tsconfSourceDirectories = cfgSourceDirectories
                , tsconfCabalDirectories  = cfgCabalDirectories
                , tsconfLazyTagging       = cfgLazyTagging
                }
      state = TagsServerState
                { tssLoadedModules = mempty
                }
  conf'  <- canonicalizeConfPaths =<< addRecursiveRootsToConf cfgDirTrees conf
  result <- runSimpleLoggerT (Just Stderr) cfgDebugVerbosity
          $ runExceptT
          $ startTagsServer conf' state
  case result of
    Left err         -> putDocLn err
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
