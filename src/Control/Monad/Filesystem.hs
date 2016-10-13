----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Filesystem
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Sunday, 18 September 2016
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Filesystem
  ( MonadFS(..)
  , findRec
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.DList (DList)
import qualified Data.DList as DL
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.IO
import Data.Time.Clock (UTCTime)
import qualified System.Directory
import System.FilePath

import Prelude hiding (readFile)

import Control.Monad.Ext
import Data.Foldable.Ext

-- | Monad for interaction with filesystem.
class (Monad m) => MonadFS m where
  getModificationTime  :: FilePath -> m UTCTime
  readFile             :: FilePath -> m Text
  doesFileExist        :: FilePath -> m Bool
  doesDirectoryExist   :: FilePath -> m Bool
  canonicalizePath     :: FilePath -> m FilePath
  getDirectoryContents :: FilePath -> m [FilePath]

instance MonadFS IO where
  getModificationTime  = System.Directory.getModificationTime
  readFile             = Data.Text.IO.readFile
  doesFileExist        = System.Directory.doesFileExist
  doesDirectoryExist   = System.Directory.doesDirectoryExist
  canonicalizePath     = System.Directory.canonicalizePath
  getDirectoryContents = System.Directory.getDirectoryContents

instance (MonadFS m) => MonadFS (ExceptT e m) where
  getModificationTime  = lift . getModificationTime
  readFile             = lift . readFile
  doesFileExist        = lift . doesFileExist
  doesDirectoryExist   = lift . doesDirectoryExist
  canonicalizePath     = lift . canonicalizePath
  getDirectoryContents = lift . getDirectoryContents

instance (MonadFS m) => MonadFS (ReaderT r m) where
  getModificationTime  = lift . getModificationTime
  readFile             = lift . readFile
  doesFileExist        = lift . doesFileExist
  doesDirectoryExist   = lift . doesDirectoryExist
  canonicalizePath     = lift . canonicalizePath
  getDirectoryContents = lift . getDirectoryContents

instance (MonadFS m) => MonadFS (StateT s m) where
  getModificationTime  = lift . getModificationTime
  readFile             = lift . readFile
  doesFileExist        = lift . doesFileExist
  doesDirectoryExist   = lift . doesDirectoryExist
  canonicalizePath     = lift . canonicalizePath
  getDirectoryContents = lift . getDirectoryContents

findRec
  :: forall m a. (MonadFS m)
  => (FilePath -> Bool)
  -> (FilePath -> Maybe a)
  -> FilePath
  -> m [a] -- ^ Absolute files paths that match
findRec visitPred collectPred = fmap toList . go
  where
    go :: FilePath -> m (DList a)
    go root = do
      contents      <- map (root </>) . filter isValidDirName <$> getDirectoryContents root
      (dirs, files) <- partitionM doesDirectoryExist contents
      let interestingFiles = mapMaybe collectPred files
      children      <- foldMapA go (filter visitPred dirs)
      pure $ DL.fromList interestingFiles <> children

isValidDirName :: FilePath -> Bool
isValidDirName "."  = False
isValidDirName ".." = False
isValidDirName _    = True
