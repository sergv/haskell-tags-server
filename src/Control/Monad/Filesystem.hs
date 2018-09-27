----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Filesystem
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Sunday, 18 September 2016
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Filesystem
  ( MonadFS(..)
  , SearchCfg(..)
  , versionControlDirs
  ) where

import Prelude hiding (readFile)

import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Reader
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import Control.Monad.Writer as Lazy
import Control.Monad.Writer.Strict as Strict

import qualified Data.ByteString as BS
import Data.Semigroup as Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc.Ext
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

import Data.ErrorMessage
import Data.Filesystem
import Data.Path (FullPath, FileType(..), BaseName)
import qualified Data.Path as Path

-- | Directories to search in.
data SearchCfg = SearchCfg
  { -- | Directories with files of interest. The files will be looked up in
    -- these directories but not in their children.
    scShallowPaths   :: !(Set (FullPath 'Dir))
    -- | Directories with file hierarchies containing files of interest. The
    -- files will be looked up in both the directroies and all of their children.
  , scRecursivePaths :: !(Set (FullPath 'Dir))
  , scIgnoredDirs    :: !(Set (BaseName 'Dir))
  } deriving (Eq, Ord, Show, Generic)

instance Semigroup SearchCfg where
  {-# INLINE (<>) #-}
  (<>) (SearchCfg x y z) (SearchCfg x' y' z') =
    SearchCfg (x <> x') (y <> y') (z <> z')

instance Monoid SearchCfg where
  {-# INLINE mempty  #-}
  {-# INLINE mappend #-}
  mempty = SearchCfg mempty mempty mempty
  mappend = (Semigroup.<>)

instance Pretty SearchCfg where
  pretty = ppGeneric


-- | Monad for interaction with filesystem.
class Monad m => MonadFS m where
  getModificationTime  :: FullPath 'File -> m UTCTime
  readFile             :: FullPath 'File -> m BS.ByteString
  doesFileExist        :: FullPath 'File -> m Bool
  doesDirectoryExist   :: FullPath 'Dir  -> m Bool
  listDirectory        :: FullPath 'Dir  -> m ([FullPath 'File], [FullPath 'Dir])
  findRec
    :: Ord a
    => SearchCfg -> (FullPath 'File -> Maybe a) -> m (Set a)

instance {-# OVERLAPS #-} (Monad m, MonadBase IO m) => MonadFS (ExceptT ErrorMessage m) where
  {-# INLINE getModificationTime  #-}
  {-# INLINE readFile             #-}
  {-# INLINE doesFileExist        #-}
  {-# INLINE doesDirectoryExist   #-}
  {-# INLINE listDirectory        #-}
  {-# INLINE findRec              #-}
  getModificationTime  = Path.getModificationTime
  readFile             = liftBase . BS.readFile . T.unpack . Path.unFullPath
  doesFileExist        = Path.doesFileExist
  doesDirectoryExist   = Path.doesDirectoryExist
  listDirectory        = Path.listDirectory
  findRec SearchCfg{scShallowPaths, scRecursivePaths, scIgnoredDirs} f = liftBase $
    findRecur scIgnoredDirs scShallowPaths scRecursivePaths f

instance {-# OVERLAPPABLE #-} MonadFS m => MonadFS (ExceptT e m) where
  {-# INLINE getModificationTime  #-}
  {-# INLINE readFile             #-}
  {-# INLINE doesFileExist        #-}
  {-# INLINE doesDirectoryExist   #-}
  {-# INLINE listDirectory        #-}
  {-# INLINE findRec              #-}
  getModificationTime  = lift . getModificationTime
  readFile             = lift . readFile
  doesFileExist        = lift . doesFileExist
  doesDirectoryExist   = lift . doesDirectoryExist
  listDirectory        = lift . listDirectory
  findRec cfg          = lift . findRec cfg

instance MonadFS m => MonadFS (ReaderT r m) where
  {-# INLINE getModificationTime  #-}
  {-# INLINE readFile             #-}
  {-# INLINE doesFileExist        #-}
  {-# INLINE doesDirectoryExist   #-}
  {-# INLINE listDirectory        #-}
  {-# INLINE findRec              #-}
  getModificationTime  = lift . getModificationTime
  readFile             = lift . readFile
  doesFileExist        = lift . doesFileExist
  doesDirectoryExist   = lift . doesDirectoryExist
  listDirectory        = lift . listDirectory
  findRec cfg          = lift . findRec cfg

instance (MonadFS m, Monoid w) => MonadFS (Lazy.WriterT w m) where
  {-# INLINE getModificationTime  #-}
  {-# INLINE readFile             #-}
  {-# INLINE doesFileExist        #-}
  {-# INLINE doesDirectoryExist   #-}
  {-# INLINE listDirectory        #-}
  {-# INLINE findRec              #-}
  getModificationTime  = lift . getModificationTime
  readFile             = lift . readFile
  doesFileExist        = lift . doesFileExist
  doesDirectoryExist   = lift . doesDirectoryExist
  listDirectory        = lift . listDirectory
  findRec cfg          = lift . findRec cfg

instance (MonadFS m, Monoid w) => MonadFS (Strict.WriterT w m) where
  {-# INLINE getModificationTime  #-}
  {-# INLINE readFile             #-}
  {-# INLINE doesFileExist        #-}
  {-# INLINE doesDirectoryExist   #-}
  {-# INLINE listDirectory        #-}
  {-# INLINE findRec              #-}
  getModificationTime  = lift . getModificationTime
  readFile             = lift . readFile
  doesFileExist        = lift . doesFileExist
  doesDirectoryExist   = lift . doesDirectoryExist
  listDirectory        = lift . listDirectory
  findRec cfg          = lift . findRec cfg

instance MonadFS m => MonadFS (Lazy.StateT s m) where
  {-# INLINE getModificationTime  #-}
  {-# INLINE readFile             #-}
  {-# INLINE doesFileExist        #-}
  {-# INLINE doesDirectoryExist   #-}
  {-# INLINE listDirectory        #-}
  {-# INLINE findRec              #-}
  getModificationTime  = lift . getModificationTime
  readFile             = lift . readFile
  doesFileExist        = lift . doesFileExist
  doesDirectoryExist   = lift . doesDirectoryExist
  listDirectory        = lift . listDirectory
  findRec cfg          = lift . findRec cfg

instance MonadFS m => MonadFS (Strict.StateT s m) where
  {-# INLINE getModificationTime  #-}
  {-# INLINE readFile             #-}
  {-# INLINE doesFileExist        #-}
  {-# INLINE doesDirectoryExist   #-}
  {-# INLINE listDirectory        #-}
  {-# INLINE findRec              #-}
  getModificationTime  = lift . getModificationTime
  readFile             = lift . readFile
  doesFileExist        = lift . doesFileExist
  doesDirectoryExist   = lift . doesDirectoryExist
  listDirectory        = lift . listDirectory
  findRec cfg          = lift . findRec cfg

versionControlDirs :: Set (BaseName 'Dir)
versionControlDirs = S.fromList
  [ ".git"
  , "_darcs"
  , ".hg"
  , ".svn"
  ]
