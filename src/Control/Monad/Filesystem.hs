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

module Control.Monad.Filesystem
  ( MonadFS(..)
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Time.Clock (UTCTime)
import Data.Text (Text)
import qualified Data.Text.IO
import qualified System.Directory

import Prelude hiding (readFile)

-- | Monad for interaction with filesystem.
class (Monad m) => MonadFS m where
  getModificationTime :: FilePath -> m UTCTime
  readFile            :: FilePath -> m Text
  doesFileExist       :: FilePath -> m Bool

instance MonadFS IO where
  getModificationTime = System.Directory.getModificationTime
  readFile = Data.Text.IO.readFile
  doesFileExist = System.Directory.doesFileExist

instance (MonadFS m) => MonadFS (ExceptT e m) where
  getModificationTime = lift . getModificationTime
  readFile            = lift . readFile
  doesFileExist       = lift . doesFileExist

instance (MonadFS m) => MonadFS (ReaderT r m) where
  getModificationTime = lift . getModificationTime
  readFile            = lift . readFile
  doesFileExist       = lift . doesFileExist

instance (MonadFS m) => MonadFS (StateT s m) where
  getModificationTime = lift . getModificationTime
  readFile            = lift . readFile
  doesFileExist       = lift . doesFileExist
