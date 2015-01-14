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

module Main where

import Control.Applicative
import Control.Monad
import qualified Data.Set as S
import Options.Applicative
import System.Directory
import System.Exit
import System.IO

import Server

opts :: ParserInfo ServerConfig
opts = info parser fullDesc
  where
    parser = ServerConfig
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
               <*> (fromIntegral <$>
                      option auto
                        (short 'p' <>
                         long "port" <>
                         help "port to listen to" <>
                         metavar "PORT"))
               <*> switch
                     (long "lazy-tagging" <>
                      help "whether to compute tags lazily - only for files asked")

main :: IO ()
main = do
  conf <- execParser opts
  -- validate that specified directories actually exist
  forM_ (S.toList $ confSourceDirectories conf) checkDirExists
  unless (S.null $ confCabalDirectories conf) $ do
    hPutStrLn stderr $ "NOT IMPLEMENTED YET: analysis of cabal packages"
    exitFailure
  forM_ (S.toList $ confCabalDirectories conf) checkDirExists
  srcDirs'   <- S.fromList <$> (mapM canonicalizePath $ S.toList $ confSourceDirectories conf)
  cabalDirs' <- S.fromList <$> (mapM canonicalizePath $ S.toList $ confCabalDirectories conf)
  runServer $ conf { confSourceDirectories = srcDirs'
                   , confCabalDirectories = cabalDirs'
                   }
  where
    checkDirExists :: FilePath -> IO ()
    checkDirExists dir = do
      exists <- doesDirectoryExist dir
      unless exists $ do
        hPutStrLn stderr $ "error: directory " ++ dir ++ " does not exist"
        exitFailure
