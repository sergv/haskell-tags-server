----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Filesystem
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Sunday, 18 September 2016
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Filesystem
  ( MonadFS(..)
  , SearchCfg(..)
  ) where

import Prelude hiding (readFile)

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.ByteString qualified as BS
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generically(..))
import Prettyprinter.Ext

import Data.Path (FullPath, FileType(..), BaseName)
import Data.Path qualified as Path

-- | Directories to search in.
data SearchCfg = SearchCfg
  { -- | Directories with files of interest. The files will be looked up in
    -- these directories but not in their children.
    scShallowPaths   :: !(Set (FullPath 'Dir))
    -- | Directories with file hierarchies containing files of interest. The
    -- files will be looked up in both the directroies and all of their children.
  , scRecursivePaths :: !(Set (FullPath 'Dir))
  , scIgnoredDirs    :: !(Set (BaseName 'Dir))
    -- | The globs will be matched against full paths.
  , scIgnoredGlobs   :: !(Set Text)
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (Semigroup, Monoid) via Generically SearchCfg
  deriving Pretty via PPGeneric SearchCfg

-- | Monad for interaction with filesystem.
class Monad m => MonadFS m where
  readFile             :: FullPath 'File -> m BS.ByteString
  doesFileExist        :: FullPath 'File -> m Bool
  doesDirectoryExist   :: FullPath 'Dir  -> m Bool

instance {-# OVERLAPS #-} (Monad m, MonadBaseControl IO m, MonadMask m) => MonadFS m where
  {-# INLINE readFile             #-}
  {-# INLINE doesFileExist        #-}
  {-# INLINE doesDirectoryExist   #-}
  readFile             = liftBase . BS.readFile . T.unpack . Path.unFullPath
  doesFileExist        = Path.doesFileExist
  doesDirectoryExist   = Path.doesDirectoryExist

instance MonadFS m => MonadFS (ReaderT r m) where
  {-# INLINE readFile             #-}
  {-# INLINE doesFileExist        #-}
  {-# INLINE doesDirectoryExist   #-}
  readFile             = lift . readFile
  doesFileExist        = lift . doesFileExist
  doesDirectoryExist   = lift . doesDirectoryExist
