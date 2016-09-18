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
import Server.BERT
import Server.Tags
import Text.PrettyPrint.Leijen.Text.Utils

data ProgramConfig = ProgramConfig
  { cfgSourceDirectories :: Set FilePath -- ^ directories with haskell files to index
  , cfgCabalDirectories  :: Set FilePath -- ^ directories with cabal packages to index
  , cfgDirTrees          :: Set FilePath -- ^ directories that should be searched recursively - i.e.
                                         -- all their subdirectories are added to cfgSourceDirectories
  , cfgPort              :: PortNumber
  , cfgLazyTagging       :: Bool         -- ^ whether to read and compute tags lazily
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

progInfo :: ParserInfo ProgramConfig
progInfo = info optsParser fullDesc

main :: IO ()
main = do
  ProgramConfig{cfgSourceDirectories, cfgCabalDirectories, cfgDirTrees, cfgPort, cfgLazyTagging} <- execParser progInfo
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
  result <- runSimpleLoggerT (Just Stderr) Debug
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
