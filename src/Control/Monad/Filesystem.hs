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

module Control.Monad.Filesystem (MonadFS(..)) where

import Prelude hiding (readFile)

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO
import Data.Time.Clock (UTCTime)

import Data.Path (FullPath, BasePath)
import qualified Data.Path as Path

-- | Monad for interaction with filesystem.
class Monad m => MonadFS m where
  getModificationTime  :: FullPath -> m UTCTime
  readFile             :: FullPath -> m TL.Text
  doesFileExist        :: FullPath -> m Bool
  doesDirectoryExist   :: FullPath -> m Bool
  getDirectoryContents :: FullPath -> m [BasePath]

instance MonadFS IO where
  {-# INLINE getModificationTime  #-}
  {-# INLINE readFile             #-}
  {-# INLINE doesFileExist        #-}
  {-# INLINE doesDirectoryExist   #-}
  {-# INLINE getDirectoryContents #-}
  getModificationTime  = Path.getModificationTime
  readFile             = Data.Text.Lazy.IO.readFile . T.unpack . Path.unFullPath
  doesFileExist        = Path.doesFileExist
  doesDirectoryExist   = Path.doesDirectoryExist
  getDirectoryContents = Path.getDirectoryContents

instance MonadFS m => MonadFS (ExceptT e m) where
  {-# INLINE getModificationTime  #-}
  {-# INLINE readFile             #-}
  {-# INLINE doesFileExist        #-}
  {-# INLINE doesDirectoryExist   #-}
  {-# INLINE getDirectoryContents #-}
  getModificationTime  = lift . getModificationTime
  readFile             = lift . readFile
  doesFileExist        = lift . doesFileExist
  doesDirectoryExist   = lift . doesDirectoryExist
  getDirectoryContents = lift . getDirectoryContents

instance MonadFS m => MonadFS (ReaderT r m) where
  {-# INLINE getModificationTime  #-}
  {-# INLINE readFile             #-}
  {-# INLINE doesFileExist        #-}
  {-# INLINE doesDirectoryExist   #-}
  {-# INLINE getDirectoryContents #-}
  getModificationTime  = lift . getModificationTime
  readFile             = lift . readFile
  doesFileExist        = lift . doesFileExist
  doesDirectoryExist   = lift . doesDirectoryExist
  getDirectoryContents = lift . getDirectoryContents

instance MonadFS m => MonadFS (StateT s m) where
  {-# INLINE getModificationTime  #-}
  {-# INLINE readFile             #-}
  {-# INLINE doesFileExist        #-}
  {-# INLINE doesDirectoryExist   #-}
  {-# INLINE getDirectoryContents #-}
  getModificationTime  = lift . getModificationTime
  readFile             = lift . readFile
  doesFileExist        = lift . doesFileExist
  doesDirectoryExist   = lift . doesDirectoryExist
  getDirectoryContents = lift . getDirectoryContents

