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

{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as S
import Network.Socket (PortNumber)
import Options.Applicative
import System.Directory
import System.Exit
import System.IO

import qualified Network.BERT.Transport as BERT

import Logging
import Server


data ProgramConfig = ProgramConfig
  { progSourceDirectories :: Set FilePath -- ^ directories with haskell files to index
  , progCabalDirectories  :: Set FilePath -- ^ directories with cabal packages to index
  , progDirTrees          :: Set FilePath -- ^ directories that should be searched recursively - i.e.
                                          -- all their subdirectories are added to progSourceDirectories
  , progPort              :: PortNumber
  , progLazyTagging       :: Bool       -- ^ whether to read and compute tags lazily
  } deriving (Show, Eq, Ord)

opts :: ParserInfo ProgramConfig
opts = info parser fullDesc
  where
    parser = ProgramConfig
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

main :: IO ()
main = do
  ProgramConfig {..} <- execParser opts
  -- validate that specified directories actually exist
  forM_ (S.toList progSourceDirectories) ensureDirExists
  unless (S.null progCabalDirectories) $ do
    hPutStrLn stderr "NOT IMPLEMENTED YET: analysis of cabal packages"
    exitFailure
  forM_ (S.toList progCabalDirectories) ensureDirExists
  forM_ (S.toList progDirTrees) ensureDirExists
  serv <- BERT.tcpServer progPort
  let conf = ServerConfig
               { confSourceDirectories = progSourceDirectories
               , confCabalDirectories  = progCabalDirectories
               , confLazyTagging       = progLazyTagging
               , confServer            = serv
               }
  handler <- streamHandler stderr DEBUG
  initLogger (setHandlers [handler] . setLevel DEBUG)
  runServerWithRecursiveDirs conf progDirTrees
  where
    ensureDirExists dir = do
      exists <- doesDirectoryExist dir
      unless exists $ do
        hPutStrLn stderr $ "error: directory " ++ dir ++ " does not exist"
        exitFailure
