----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Filesystem.FileSearch.Class
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Thursday, 10 November 2016
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

module Control.Monad.Filesystem.FileSearch.Class
  ( MonadFileSearch(..)
  , FindEntry
  , mkFindEntry
  , findEntryRoot
  , findEntryBasePath
  , findEntryFullPath
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Control.Monad.EitherK
import Data.Path

data FindEntry = FindEntry
  { findEntryRoot     :: FullPath
  , findEntryBasePath :: BasePath
  , findEntryFullPath :: FullPath
  } deriving (Show, Eq, Ord)

mkFindEntry :: FullPath -> BasePath -> FindEntry
mkFindEntry root base = FindEntry
  { findEntryRoot     = root
  , findEntryBasePath = base
  , findEntryFullPath = root </> base
  }

class Monad m => MonadFileSearch m where
  findByPathSuffixSansExtension :: PathFragment -> m [FullPath]
  findRec                       :: (FindEntry -> Maybe a) -> m [a]

instance MonadFileSearch m => MonadFileSearch (ExceptT e m) where
  {-# INLINABLE findByPathSuffixSansExtension #-}
  {-# INLINABLE findRec                       #-}
  findByPathSuffixSansExtension = lift . findByPathSuffixSansExtension
  findRec                       = lift . findRec

instance MonadFileSearch m => MonadFileSearch (StateT s m) where
  {-# INLINABLE findByPathSuffixSansExtension #-}
  {-# INLINABLE findRec                       #-}
  findByPathSuffixSansExtension = lift . findByPathSuffixSansExtension
  findRec                       = lift . findRec

instance MonadFileSearch m => MonadFileSearch (ReaderT r m) where
  {-# INLINABLE findByPathSuffixSansExtension #-}
  {-# INLINABLE findRec                       #-}
  findByPathSuffixSansExtension = lift . findByPathSuffixSansExtension
  findRec                       = lift . findRec

instance (MonadFileSearch m, Monoid w) => MonadFileSearch (WriterT w m) where
  {-# INLINABLE findByPathSuffixSansExtension #-}
  {-# INLINABLE findRec                       #-}
  findByPathSuffixSansExtension = lift . findByPathSuffixSansExtension
  findRec                       = lift . findRec

instance MonadFileSearch m => MonadFileSearch (EitherKT e m) where
  {-# INLINABLE findByPathSuffixSansExtension #-}
  {-# INLINABLE findRec                       #-}
  findByPathSuffixSansExtension = lift . findByPathSuffixSansExtension
  findRec                       = lift . findRec
