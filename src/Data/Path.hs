----------------------------------------------------------------------------
-- |
-- Module      :  Data.Path
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Thursday, 10 November 2016
----------------------------------------------------------------------------

{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wredundant-constraints          #-}
{-# OPTIONS_GHC -Wsimplifiable-class-constraints #-}

module Data.Path
  ( FullPath
  , unFullPath
  , MkFullPath(..)
  , doesFileExist
  , doesDirectoryExist
  , getDirectoryContents
  , getModificationTime
  , splitDirectories
  , PathFragment
  , mkPathFragment
  , unPathFragment
  , Extension
  , mkExtension
  , unExtension
  -- * Overloaded operations
  , JoinPaths(..)
  , Contains(..)
  , AddExtension(..)
  , DropExtension(..)
  , TakeFileName(..)
  , TakeExtension(..)
  , MakeRelative(..)
  -- * Base path
  , BaseName
  , unBaseName
  , BasePath
  , mkBasePath
  , bpFileName
  , bpExtension
  , bpBaseName
  ) where

import Control.Monad.Base
import Data.Coerce
import Data.Function
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup as Semigroup
import Data.Semigroup.Foldable.Class (foldMap1)
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc.Ext (Pretty(..))
import Data.Time.Clock (UTCTime)
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

-- | Absolute path, canonicalised and normalised. Provides strictest invariants
-- but must be created in derivatives of IO monad.
-- Invariant: does not end with \/.
newtype FullPath = FullPath { unFullPath :: Text }
  deriving (Eq, Ord, Show, Pretty, IsString)

class MkFullPath a m where
  mkFullPath :: a -> m FullPath

instance MonadBase IO m => MkFullPath FilePath.FilePath m where
  mkFullPath path =
    liftBase $
    FullPath . T.pack . FilePath.dropTrailingPathSeparator . FilePath.normalise <$> Directory.canonicalizePath path

-- MkFullPath FilePath.FilePath m
instance MonadBase IO m => MkFullPath Text m where
  {-# INLINE mkFullPath #-}
  mkFullPath = mkFullPath . T.unpack

 -- MkFullPath Text m =>
instance MonadBase IO m => MkFullPath PathFragment m where
  {-# INLINE mkFullPath #-}
  mkFullPath = mkFullPath . unPathFragment

doesFileExist :: MonadBase IO m => FullPath -> m Bool
doesFileExist =
  liftBase . Directory.doesFileExist . T.unpack . unFullPath

doesDirectoryExist :: MonadBase IO m => FullPath -> m Bool
doesDirectoryExist =
  liftBase . Directory.doesDirectoryExist . T.unpack . unFullPath

getDirectoryContents :: MonadBase IO m => FullPath -> m [BasePath]
getDirectoryContents (FullPath path) = liftBase $
  map mkBasePath <$> Directory.getDirectoryContents (T.unpack path)

getModificationTime :: MonadBase IO m => FullPath -> m UTCTime
getModificationTime =
  liftBase . Directory.getModificationTime . T.unpack . unFullPath

splitDirectories :: FullPath -> [PathFragment]
splitDirectories =
  map (PathFragment . T.pack) . FilePath.splitDirectories . T.unpack . unFullPath

{-# INLINE makeRelativeText #-}
makeRelativeText :: Text -> Text -> Text
makeRelativeText x y = T.pack $ FilePath.makeRelative (T.unpack x) (T.unpack y)

-- | Path fragment, possibly with some directories but without etxension.
-- Invariant: does not start with \/, does not end with \/.
newtype PathFragment = PathFragment { unPathFragment :: Text }
  deriving (Eq, Ord, Show, Pretty, IsString)

newtype PathJoin = PathJoin { unPathJoin :: Text }

instance Semigroup PathJoin where
  (<>) = coerce joinPath

mkPathFragment :: NonEmpty Text -> PathFragment
mkPathFragment = PathFragment . unPathJoin . foldMap1 PathJoin

class JoinPaths a b c | a b -> c where
  (</>) :: a -> b -> c

  {-# INLINE (</>) #-}
  default (</>) :: (Coercible a Text, Coercible b Text, Coercible c Text) => a -> b -> c
  (</>) = coerce joinPath

instance JoinPaths FullPath     PathFragment FullPath
instance JoinPaths FullPath     BaseName     FullPath
instance JoinPaths PathFragment FullPath     PathFragment
instance JoinPaths PathFragment PathFragment PathFragment
instance JoinPaths PathFragment BaseName     PathFragment
instance JoinPaths BaseName     FullPath     PathFragment
instance JoinPaths BaseName     PathFragment PathFragment
instance JoinPaths BaseName     BaseName     PathFragment

instance JoinPaths FullPath     BasePath     FullPath where
  {-# INLINE (</>) #-}
  (</>) p BasePath{bpFileName} = p </> bpFileName

instance JoinPaths PathFragment BasePath     PathFragment where
  {-# INLINE (</>) #-}
  (</>) p BasePath{bpFileName} = p </> bpFileName

class Contains a where
  isInfixOf :: Text -> a -> Bool
  {-# INLINE isInfixOf #-}
  default isInfixOf :: Coercible a Text => Text -> a -> Bool
  isInfixOf = coerce T.isInfixOf

instance Contains FullPath
instance Contains PathFragment
instance Contains BaseName

-- | E.g. “.hs”.
newtype Extension = Extension { unExtension :: Text }
  deriving (Eq, Ord, Show, IsString)

mkExtension :: Text -> Extension
mkExtension = Extension

class AddExtension a where
  (<.>) :: a -> Extension -> a
  {-# INLINE (<.>) #-}
  default (<.>) :: Coercible a Text => a -> Extension -> a
  (<.>) = coerce addExt

instance AddExtension FullPath
instance AddExtension PathFragment
instance AddExtension BaseName

class DropExtension a where
  dropExtension :: a -> a
  {-# INLINE dropExtension #-}
  default dropExtension :: Coercible a Text => a -> a
  dropExtension = coerce dropExt

instance DropExtension FullPath
instance DropExtension PathFragment
instance DropExtension BaseName

class TakeFileName a where
  takeFileName :: a -> BaseName
  {-# INLINE takeFileName #-}
  default takeFileName :: Coercible a Text => a -> BaseName
  takeFileName = coerce getFileName

instance TakeFileName FullPath
instance TakeFileName PathFragment
instance TakeFileName BasePath where
  {-# INLINE takeFileName #-}
  takeFileName = bpFileName

class TakeExtension a where
  takeExtension :: a -> Extension
  {-# INLINE takeExtension #-}
  default takeExtension :: Coercible a Text => a -> Extension
  takeExtension = coerce getExtension

instance TakeExtension FullPath
instance TakeExtension PathFragment
instance TakeExtension BasePath where
  {-# INLINE takeExtension #-}
  takeExtension = bpExtension

class MakeRelative a b c | a b -> c where
  makeRelative
    :: a -- ^ Root to make relative to
    -> b -- ^ What to transform
    -> c
  {-# INLINE makeRelative #-}
  default makeRelative
    :: (Coercible a Text, Coercible b Text, Coercible c Text)
    => a -> b -> c
  makeRelative = coerce makeRelativeText

instance MakeRelative FullPath     FullPath     PathFragment
instance MakeRelative FullPath     PathFragment PathFragment
instance MakeRelative PathFragment PathFragment PathFragment

-- | File basename without directory but with extension.
newtype BaseName = BaseName { unBaseName :: PathFragment }
  deriving (Eq, Ord, Show, Pretty, IsString)

data BasePath = BasePath
  { bpFileName  :: BaseName
  , bpExtension :: Extension
  , bpBaseName  :: Text
  }

mkBasePath :: FilePath.FilePath -> BasePath
mkBasePath p = BasePath
  { bpFileName  = BaseName $ PathFragment $ T.pack p'
  , bpExtension = Extension $ T.pack ext
  , bpBaseName  = T.pack base
  }
  where
    p'          = FilePath.takeFileName p
    (base, ext) = FilePath.splitExtension p'

instance Show BasePath where
  show = show . bpFileName

instance Eq BasePath where
  (==) = (==) `on` bpFileName

instance Ord BasePath where
  compare = compare `on` bpFileName

-- Internals

getFileName :: Text -> Text
getFileName = T.pack . FilePath.takeFileName . T.unpack

getExtension :: Text -> Text
getExtension = T.pack . FilePath.takeExtension . T.unpack

joinPath :: Text -> Text -> Text
joinPath x y = x <> T.singleton FilePath.pathSeparator <> y

addExt :: Text -> Text -> Text
addExt path ext = path <> T.singleton FilePath.extSeparator Semigroup.<> ext

dropExt :: Text -> Text
dropExt = T.pack . FilePath.dropExtension . T.unpack
