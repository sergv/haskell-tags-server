----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Filesystem
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Sunday, 18 September 2016
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Filesystem (MonadFS(..)) where

import Prelude hiding (readFile)

import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Reader
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import Control.Monad.Writer as Lazy
import Control.Monad.Writer.Strict as Strict

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)

import Data.ErrorMessage
import Data.Path (FullPath, FileType(..))
import qualified Data.Path as Path

-- | Monad for interaction with filesystem.
class Monad m => MonadFS m where
  getModificationTime  :: FullPath 'File -> m UTCTime
  readFile             :: FullPath 'File -> m BS.ByteString
  doesFileExist        :: FullPath 'File -> m Bool
  doesDirectoryExist   :: FullPath 'Dir  -> m Bool
  listDirectory        :: FullPath 'Dir  -> m ([FullPath 'File], [FullPath 'Dir])

instance {-# OVERLAPS #-} (Monad m, MonadBase IO m) => MonadFS (ExceptT ErrorMessage m) where
  {-# INLINE getModificationTime  #-}
  {-# INLINE readFile             #-}
  {-# INLINE doesFileExist        #-}
  {-# INLINE doesDirectoryExist   #-}
  {-# INLINE listDirectory        #-}
  getModificationTime  = Path.getModificationTime
  readFile             = liftBase . BS.readFile . T.unpack . Path.unFullPath
  doesFileExist        = Path.doesFileExist
  doesDirectoryExist   = Path.doesDirectoryExist
  listDirectory        = Path.listDirectory

instance {-# OVERLAPPABLE #-} MonadFS m => MonadFS (ExceptT e m) where
  {-# INLINE getModificationTime  #-}
  {-# INLINE readFile             #-}
  {-# INLINE doesFileExist        #-}
  {-# INLINE doesDirectoryExist   #-}
  {-# INLINE listDirectory        #-}
  getModificationTime  = lift . getModificationTime
  readFile             = lift . readFile
  doesFileExist        = lift . doesFileExist
  doesDirectoryExist   = lift . doesDirectoryExist
  listDirectory        = lift . listDirectory

instance MonadFS m => MonadFS (ReaderT r m) where
  {-# INLINE getModificationTime  #-}
  {-# INLINE readFile             #-}
  {-# INLINE doesFileExist        #-}
  {-# INLINE doesDirectoryExist   #-}
  {-# INLINE listDirectory        #-}
  getModificationTime  = lift . getModificationTime
  readFile             = lift . readFile
  doesFileExist        = lift . doesFileExist
  doesDirectoryExist   = lift . doesDirectoryExist
  listDirectory        = lift . listDirectory

instance (MonadFS m, Monoid w) => MonadFS (Lazy.WriterT w m) where
  {-# INLINE getModificationTime  #-}
  {-# INLINE readFile             #-}
  {-# INLINE doesFileExist        #-}
  {-# INLINE doesDirectoryExist   #-}
  {-# INLINE listDirectory        #-}
  getModificationTime  = lift . getModificationTime
  readFile             = lift . readFile
  doesFileExist        = lift . doesFileExist
  doesDirectoryExist   = lift . doesDirectoryExist
  listDirectory        = lift . listDirectory

instance (MonadFS m, Monoid w) => MonadFS (Strict.WriterT w m) where
  {-# INLINE getModificationTime  #-}
  {-# INLINE readFile             #-}
  {-# INLINE doesFileExist        #-}
  {-# INLINE doesDirectoryExist   #-}
  {-# INLINE listDirectory        #-}
  getModificationTime  = lift . getModificationTime
  readFile             = lift . readFile
  doesFileExist        = lift . doesFileExist
  doesDirectoryExist   = lift . doesDirectoryExist
  listDirectory        = lift . listDirectory

instance MonadFS m => MonadFS (Lazy.StateT s m) where
  {-# INLINE getModificationTime  #-}
  {-# INLINE readFile             #-}
  {-# INLINE doesFileExist        #-}
  {-# INLINE doesDirectoryExist   #-}
  {-# INLINE listDirectory        #-}
  getModificationTime  = lift . getModificationTime
  readFile             = lift . readFile
  doesFileExist        = lift . doesFileExist
  doesDirectoryExist   = lift . doesDirectoryExist
  listDirectory        = lift . listDirectory

instance MonadFS m => MonadFS (Strict.StateT s m) where
  {-# INLINE getModificationTime  #-}
  {-# INLINE readFile             #-}
  {-# INLINE doesFileExist        #-}
  {-# INLINE doesDirectoryExist   #-}
  {-# INLINE listDirectory        #-}
  getModificationTime  = lift . getModificationTime
  readFile             = lift . readFile
  doesFileExist        = lift . doesFileExist
  doesDirectoryExist   = lift . doesDirectoryExist
  listDirectory        = lift . listDirectory
