----------------------------------------------------------------------------
-- |
-- Module      :  Data.Path
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Thursday, 10 November 2016
----------------------------------------------------------------------------

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wredundant-constraints          #-}
{-# OPTIONS_GHC -Wsimplifiable-class-constraints #-}

#ifdef mingw32_HOST_OS
#define WINDOWS 1
#endif

module Data.Path
  ( FullPath
  , unFullPath
  , MkFullPath(..)
  , doesFileExist
  , doesDirectoryExist
  , listDirectory
  , getModificationTime
  , splitDirectories
  , PathFragment
  , mkPathFragment
  , mkSinglePathFragment
  , unPathFragment
  , Extension
  , mkExtension
  , unExtension
  -- * Overloaded operations
  , JoinPaths(..)
  , Contains(..)
  , AddExtension(..)
  , DropExtensions(..)
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
  deriving (Show, Pretty, IsString)

#ifdef WINDOWS
instance Eq FullPath where
  {-# INLINE (==) #-}
  (==) = coerce ((==) `on` T.toCaseFold)

instance Ord FullPath where
  {-# INLINE compare #-}
  compare = coerce (compare `on` T.toCaseFold)
#else
deriving instance Eq  FullPath
deriving instance Ord FullPath
#endif

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

listDirectory :: MonadBase IO m => FullPath -> m [BasePath]
listDirectory (FullPath path) = liftBase $
  map mkBasePath <$> Directory.listDirectory (T.unpack path)

getModificationTime :: MonadBase IO m => FullPath -> m UTCTime
getModificationTime =
  liftBase . Directory.getModificationTime . T.unpack . unFullPath

splitDirectories :: FullPath -> [BaseName]
splitDirectories =
  map (BaseName . PathFragment . T.pack) . FilePath.splitDirectories . T.unpack . unFullPath

{-# INLINE makeRelativeText #-}
makeRelativeText :: Text -> Text -> Text
makeRelativeText x y = T.pack $ FilePath.makeRelative (T.unpack x) (T.unpack y)

-- | Path fragment, possibly with some directories but without etxension.
-- Invariant: does not start with \/, does not end with \/.
newtype PathFragment = PathFragment { unPathFragment :: Text }
  deriving (Show, Pretty, IsString)

#ifdef WINDOWS
instance Eq PathFragment where
  {-# INLINE (==) #-}
  (==) = coerce ((==) `on` T.toCaseFold)

instance Ord PathFragment where
  {-# INLINE compare #-}
  compare = coerce (compare `on` T.toCaseFold)
#else
deriving instance Eq  PathFragment
deriving instance Ord PathFragment
#endif

newtype PathJoin = PathJoin { unPathJoin :: Text }

instance Semigroup PathJoin where
  (<>) = coerce joinPath

mkPathFragment :: NonEmpty Text -> PathFragment
mkPathFragment = PathFragment . unPathJoin . foldMap1 PathJoin

mkSinglePathFragment :: Text -> PathFragment
mkSinglePathFragment = PathFragment

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
  deriving (Show, IsString)

#ifdef WINDOWS
instance Eq Extension where
  {-# INLINE (==) #-}
  (==) = coerce ((==) `on` T.toCaseFold)

instance Ord Extension where
  {-# INLINE compare #-}
  compare = coerce (compare `on` T.toCaseFold)
#else
deriving instance Eq  Extension
deriving instance Ord Extension
#endif

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

class DropExtensions a where
  dropExtensions :: a -> a
  {-# INLINE dropExtensions #-}
  default dropExtensions :: Coercible a Text => a -> a
  dropExtensions = coerce dropExts

instance DropExtensions FullPath
instance DropExtensions PathFragment
instance DropExtensions BaseName

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
  deriving (Show, Pretty, IsString)

#ifdef WINDOWS
instance Eq BaseName where
  {-# INLINE (==) #-}
  (==) = coerce ((==) `on` T.toCaseFold)

instance Ord BaseName where
  {-# INLINE compare #-}
  compare = coerce (compare `on` T.toCaseFold)
#else
deriving instance Eq  BaseName
deriving instance Ord BaseName
#endif

data BasePath = BasePath
  { bpFileName  :: BaseName
  , bpExtension :: Extension
  }

mkBasePath :: FilePath.FilePath -> BasePath
mkBasePath p = BasePath
  { bpFileName  = BaseName $ PathFragment $ T.pack p'
  , bpExtension = Extension $ T.pack ext
  }
  where
    p'  = FilePath.takeFileName p
    ext = FilePath.takeExtensions p'

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
getExtension =
  T.cons FilePath.extSeparator . T.takeWhileEnd (/= FilePath.extSeparator)
-- T.pack . FilePath.takeExtension . T.unpack

joinPath :: Text -> Text -> Text
joinPath x y = x <> T.singleton FilePath.pathSeparator <> y

addExt :: Text -> Text -> Text
addExt path ext = path <> extSeparator Semigroup.<> ext

{-# INLINE dropExts #-}
dropExts :: Text -> Text
dropExts =
  T.pack . FilePath.dropExtensions . T.unpack
-- path = case T.splitOn extSeparator path of
--   []    -> path
--   p : _ -> p

extSeparator :: Text
extSeparator = T.singleton FilePath.extSeparator
