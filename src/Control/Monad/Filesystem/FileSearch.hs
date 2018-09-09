----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Filesystem.FileSearch
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Thursday, 10 November 2016
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}

module Control.Monad.Filesystem.FileSearch
  ( FileSearchT
  , SearchCfg(..)
  , runFileSearchT
  , versionControlDirs

  , module Control.Monad.Filesystem.FileSearch.Class
  ) where

import Control.Monad.Except.Ext
import Control.Monad.Ext
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.DList (DList)
import qualified Data.DList as DL
import qualified Data.List as L
import Data.Maybe
import Data.Semigroup as Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Ext

import Control.Monad.Filesystem as MonadFS
import Control.Monad.Filesystem.FileSearch.Class
import Data.ErrorMessage
import Data.Foldable.Ext
import Data.Path

-- | Directories to search in.
data SearchCfg = SearchCfg
  { -- | Directories with files of interest. The files will be looked up in
    -- these directories but not in their children.
    shallowPaths   :: Set FullPath
    -- | Directories with file hierarchies containing files of interest. The
    -- files will be looked up in both the directroies and all of their children.
  , recursivePaths :: Set FullPath
  , ignoredDirs    :: Set BaseName
  } deriving (Eq, Ord, Show)

instance Semigroup SearchCfg where
  (<>) (SearchCfg x y z) (SearchCfg x' y' z') =
    SearchCfg (x <> x') (y <> y') (z <> z')

instance Monoid SearchCfg where
  mempty = SearchCfg mempty mempty mempty
  mappend = (Semigroup.<>)

instance Pretty SearchCfg where
  pretty SearchCfg{shallowPaths, recursivePaths, ignoredDirs} =
    ppDictHeader "SearchCfg"
      [ "shallowPaths"   :-> ppSet shallowPaths
      , "recursivePaths" :-> ppSet recursivePaths
      , "ignoredDirs"    :-> ppSet ignoredDirs
      ]

newtype FileSearchT m a = FileSearchT (ReaderT SearchCfg m a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError e
    , MonadWriter w
    , MonadState s
    , MonadFS
    )

runFileSearchT :: Monad m => SearchCfg -> FileSearchT m a -> m a
runFileSearchT env (FileSearchT action) = runReaderT action env

instance MonadTrans FileSearchT where
  {-# INLINE lift #-}
  lift = FileSearchT . lift

instance MonadReader r m => MonadReader r (FileSearchT m) where
  {-# INLINE ask #-}
  ask = lift ask
  {-# INLINE local #-}
  local f (FileSearchT action) = FileSearchT $ do
    env <- ask
    lift $ local f (runReaderT action env)

instance (MonadError ErrorMessage m, MonadFS m) => MonadFileSearch (FileSearchT m) where
  findByPathSuffixSansExtension components = FileSearchT $ do
    cfg <- ask
    findAllMatching cfg checkPath
    where
      components' = toList components
      checkPath :: FindEntry -> Maybe FullPath
      checkPath entry
        | isTarget fullPath = Just fullPath
        | otherwise         = Nothing
        where
          fullPath = findEntryFullPath entry
      isTarget :: FullPath -> Bool
      isTarget candidate = components' `L.isSuffixOf` candidateComponents
        where
          candidateComponents = map unBaseName (toList (splitDirectories (dropExtensions candidate)))
  findRec f = FileSearchT $ do
    cfg <- ask
    findAllMatching cfg f

findAllMatching
  :: forall m a. MonadFS m
  => SearchCfg
  -> (FindEntry -> Maybe a) -- ^ Predicate for files to collect.
  -> m [a]
findAllMatching SearchCfg{shallowPaths, recursivePaths, ignoredDirs} collectPred = do
  shallowResults   <- foldForA shallowPaths $ \path ->
    mapMaybe (collectPred . mkFindEntry path) <$> MonadFS.listDirectory path
  recursiveResults <- foldForA recursivePaths $ \path ->
    findRecursive ((`S.notMember` ignoredDirs) . bpFileName . findEntryBasePath) collectPred path
  return $ shallowResults ++ recursiveResults

findRecursive
  :: forall m a. MonadFS m
  => (FindEntry -> Bool)    -- ^ Predicate for directories to visit.
  -> (FindEntry -> Maybe a) -- ^ Predicate for files to collect.
  -> FullPath               -- ^ Recursion root.
  -> m [a]
findRecursive visitPred collectPred = fmap toList . go
  where
    go :: FullPath -> m (DList a)
    go root = do
      contents      <- map (mkFindEntry root) <$>
                       MonadFS.listDirectory root
      (dirs, files) <- partitionM (MonadFS.doesDirectoryExist . findEntryFullPath) contents
      let interestingFiles = mapMaybe collectPred files
      children      <- foldMapA (go . findEntryFullPath) $ filter visitPred dirs
      pure $ DL.fromList interestingFiles <> children

versionControlDirs :: Set BaseName
versionControlDirs = S.fromList
  [ ".git"
  , "_darcs"
  , ".hg"
  , ".svn"
  ]
