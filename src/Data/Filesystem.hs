----------------------------------------------------------------------------
-- |
-- Module      :  Data.Filesystem
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

#ifdef mingw32_HOST_OS
#define WINDOWS 1
#endif

module Data.Filesystem
  ( findRecur
  , findRecursive
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Control.Exception

import Data.Foldable.Ext
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.NBSem
import Data.Path
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Streaming.Filesystem as Streaming
import qualified Data.Text as T
import GHC.Conc (getNumCapabilities)
import GHC.Stack.Ext (WithCallStack)
import qualified System.FilePath as FilePath

#ifndef WINDOWS
import System.Posix.Files as Posix
#endif

import Data.Path.Internal

findRecursive
  :: forall a. WithCallStack
  => Int                         -- ^ Extra search threads to run in parallel.
  -> (FullPath 'Dir -> Bool)     -- ^ Whether to visit directory.
  -> (FullPath 'File -> Maybe a) -- ^ What to do with a file.
  -> (a -> IO ())                -- ^ Consume output
  -> Set (FullPath 'Dir)         -- ^ Dirs to search non-recursively.
  -> Set (FullPath 'Dir)         -- ^ Dirs to search recursively.
  -> IO ()
findRecursive extraJobs dirPred filePred consumeOutput shallowDirs recursiveDirs = do
  sem <- newNBSem extraJobs
  findRecursive' sem
  where
    findRecursive' :: NBSem -> IO ()
    findRecursive' sem =
      foldr
        (goDirRec (const False))
        (foldr
          (goDirRec dirPred)
          (pure ())
          recursiveDirs)
        shallowDirs
      where
        goDirRec :: (FullPath 'Dir -> Bool) -> FullPath 'Dir -> IO () -> IO ()
        goDirRec dirPred' = goDirAsyncOrSync . (T.unpack . unFullPath)
          where
            goDirAsyncOrSync :: FilePath -> IO () -> IO ()
            goDirAsyncOrSync root next = do
              acquired <- tryAcquireNBSem sem
              if acquired
              then
                withAsync (goDir root `finally` releaseNBSem sem) $ \yAsync ->
                  next *> wait yAsync
              else goDir root *> next
            goDir :: FilePath -> IO ()
            goDir root =
              bracket (Streaming.openDirStream root) Streaming.closeDirStream goDirStream
              where
                goDirStream :: Streaming.DirStream -> IO ()
                goDirStream stream = go
                  where
                    go :: IO ()
                    go = do
                      x <- Streaming.readDirStream stream
                      for_ x $ \y -> do
                        let y' :: FilePath
                            y' = root FilePath.</> y
                        let doFile =
                              for_ (filePred y'') consumeOutput *> go
                              where
                                y'' :: FullPath 'File
                                y'' = FullPath $ T.pack y'
                            doDir =
                              if dirPred' y''
                              then goDirAsyncOrSync y' go
                              else go
                              where
                                y'' :: FullPath 'Dir
                                y'' = FullPath $ T.pack y'
#ifdef WINDOWS
                        ft <- Streaming.getFileType y'
                        case ft of
                          Streaming.FTOther        -> go
                          Streaming.FTFile         -> doFile
                          Streaming.FTFileSym      -> doFile
                          Streaming.FTDirectory    -> doDir
                          Streaming.FTDirectorySym -> doDir
#else
                        status <- Posix.getSymbolicLinkStatus y'
                        if | Posix.isRegularFile  status -> doFile
                           | Posix.isDirectory    status -> doDir
                           | Posix.isSymbolicLink status -> do
                             status' <- try $ Posix.getFileStatus y'
                             case status' of
                               Left (_ :: IOException) -> go
                               Right status''
                                 | Posix.isRegularFile status'' -> doFile
                                 | Posix.isDirectory   status'' -> doDir
                                 | otherwise                    -> go
                           | otherwise                   -> go
#endif


findRecur
  :: forall k v. Ord k
  => Set (BaseName 'Dir)
  -> Set (FullPath 'Dir)
  -> Set (FullPath 'Dir)
  -> (FullPath 'File -> Maybe (k, v))
  -> IO (Map k v)
findRecur ignoredDirs shallowPaths recursivePaths f = do
  n       <- getNumCapabilities
  results <- newTMQueueIO
  let collect :: (k, v) -> IO ()
      collect = atomically . writeTMQueue results

      consumeOutput !xs = do
        res <- atomically $ readTMQueue results
        case res of
          Nothing     -> pure xs
          Just (k, v) -> consumeOutput $ M.insert k v xs

      doFind =
        findRecursive n ((`S.notMember` ignoredDirs) . takeFileName) f collect shallowPaths recursivePaths
  withAsync (doFind `finally` atomically (closeTMQueue results)) $ \searchAsync ->
    consumeOutput M.empty <* wait searchAsync
