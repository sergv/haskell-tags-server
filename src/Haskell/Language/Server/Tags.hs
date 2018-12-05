----------------------------------------------------------------------------
-- |
-- Module      :  Server.Tags
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Monday, 15 August 2016
-- The actual server that handles tags
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Haskell.Language.Server.Tags
  ( startTagsServer
  , stopTagsServer
  , waitForTagsServerFinish
  , TagsServer(tsRequestHandler)
  , RequestHandler
  , TagsServerConf(..)
  , defaultTagsServerConf
  , TagsServerState(..)
  , emptyTagsServerState
  ) where

import Control.Concurrent
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except.Ext
import Control.Monad.Trans.Control

import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as M
import Data.Semigroup.Foldable
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc.Ext (Pretty(..), (##))
import qualified System.FSNotify as FSNotify

import qualified Data.Promise as Promise

import Control.Monad.Filesystem (MonadFS(..), SearchCfg(..))
import qualified Control.Monad.Filesystem as MonadFS
import Control.Monad.Logging
import Data.CompiledRegex
import Data.ErrorMessage
import Data.Map.NonEmpty (NonEmptyMap)
import qualified Data.Map.NonEmpty as NEMap
import Data.Path
import Data.Symbols (fileNameToModuleName)
import Haskell.Language.Server.Tags.LoadFiles
import Haskell.Language.Server.Tags.LoadModule
import Haskell.Language.Server.Tags.Search
import Haskell.Language.Server.Tags.SearchM
import Haskell.Language.Server.Tags.Types
import Haskell.Language.Server.Tags.Types.Imports
import Haskell.Language.Server.Tags.Types.Modules

data TagsServer = TagsServer
  { -- | The way to communicate with running tags server: send requests and
    -- get promises of responses.
    tsRequestHandler :: RequestHandler
    -- | Lock that becomes available when server exits.
  , tsFinishState    :: MVar TagsServerState
    -- | Id of the requent serving thread.
  , tsThreadId       :: !ThreadId
  , tsFileWatch      :: FSNotify.WatchManager
  }

stopTagsServer :: MonadBase IO m => TagsServer -> m ()
stopTagsServer TagsServer{tsThreadId, tsFileWatch} = liftBase $ do
  FSNotify.stopManager tsFileWatch
  killThread tsThreadId

-- | Block until tags server stops.
waitForTagsServerFinish :: MonadBase IO m => TagsServer -> m TagsServerState
waitForTagsServerFinish = liftBase . readMVar . tsFinishState

searchCfgIgnoredRE
  :: MonadError ErrorMessage m
  => SearchCfg
  -> m CompiledRegex
searchCfgIgnoredRE SearchCfg{scIgnoredGlobs} =
  fileGlobsToRegex (scIgnoredGlobs <> MonadFS.defaultIgnoredGlobs)

preloadFiles
  :: (WithCallStack,  MonadError ErrorMessage m, MonadLog m, MonadFS m)
  => SearchCfg
  -> TagsServerConf
  -> TagsServerState
  -> m TagsServerState
preloadFiles SearchCfg{scShallowPaths, scRecursivePaths, scIgnoredDirs, scIgnoredGlobs} _ s
  | S.null scShallowPaths && S.null scRecursivePaths && S.null scIgnoredDirs && S.null scIgnoredGlobs
  = pure s
preloadFiles searchCfg conf s = do
  ignoredGlobsRE <- searchCfgIgnoredRE searchCfg
  knownFiles     <- MonadFS.findRec searchCfg ignoredGlobsRE (loadMod conf)
  let s' = s
        { tssKnownFiles =
          M.fromList (concatMap (\(impKey, fs) -> map (,impKey) fs) $ M.toList $ toList . NEMap.keysNE <$> knownFiles) <>
          tssKnownFiles s
        }
  if tsconfEagerTagging conf
  then do
    logInfo "[preloadFiles] collecting tags eagerly..."
    s'' <- loadAllFilesIntoState (fold1 . NEMap.elemsNE <$> knownFiles) conf s'
    logInfo "[preloadFiles] collecting tags eagerly... OK"
    pure s''
  else pure s'
    { tssUnloadedFiles =
      M.unionWith (<>) (fold1 . NEMap.elemsNE <$> knownFiles) (tssUnloadedFiles s)
    }

watchDirs
  :: (MonadError ErrorMessage m, MonadBase IO m)
  => TagsServerConf
  -> FSNotify.WatchManager
  -> Chan SomeRequest
  -> SearchCfg
  -> m ()
watchDirs conf manager reqChan searchCfg@SearchCfg{scShallowPaths, scRecursivePaths} = do
  ignoredGlobsRE <- searchCfgIgnoredRE searchCfg
  let shouldAct :: FSNotify.Event -> Bool
      shouldAct event =
        not (FSNotify.eventIsDirectory event) &&
        case classifyPath conf path' of
          Nothing -> False
          Just{}  -> not (reMatches ignoredGlobsRE path)
        where
          path' = mkSinglePathFragment path
          path = T.pack $ FSNotify.eventPath event
      reportEvent' :: FilePath -> (FullPath 'File -> FSNotifyEvent) -> IO ()
      reportEvent' path f = do
        path' <- runExceptT $ mkFullPath path
        case path' of
          Left _       -> pure ()
          Right path'' -> writeChan reqChan $ SomeRequest (FSNotifyReq $ f path'') ()
      reportEvent :: FSNotify.Event -> IO ()
      reportEvent = \case
        FSNotify.Added    path _ _ -> reportEvent' path FSAdded
        FSNotify.Modified path _ _ -> reportEvent' path FSModified
        FSNotify.Removed  path _ _ -> reportEvent' path FSRemoved
        FSNotify.Unknown{}         -> pure ()
  liftBase $ do
    for_ scShallowPaths $ \dir ->
      FSNotify.watchDir manager (toFilePath dir) shouldAct reportEvent
    for_ scRecursivePaths $ \dir ->
      FSNotify.watchTree manager (toFilePath dir) shouldAct reportEvent

-- | Start new tags server thread that will serve requests supplied via returned
-- RequestHandler.
startTagsServer
  :: forall m. (WithCallStack, MonadBase IO m, MonadBaseControl IO m, MonadCatch m, MonadError ErrorMessage m, MonadLog m, MonadFS m, MonadMask m)
  => SearchCfg
  -> TagsServerConf
  -> m TagsServer
startTagsServer searchCfg conf = do
  tsFileWatch   <- liftBase FSNotify.startManager
  initState     <- preloadFiles searchCfg conf emptyTagsServerState
  reqChan       <- liftBase newChan
  tsFinishState <- liftBase newEmptyMVar
  tsThreadId    <- liftBaseDiscard forkIO $ handleRequests tsFinishState reqChan tsFileWatch initState
  watchDirs conf tsFileWatch reqChan searchCfg
  let tsRequestHandler :: RequestHandler
      tsRequestHandler = \case
        req@QueryReq{} -> do
          respPromise <- Promise.newPromise
          writeChan reqChan (SomeRequest (UserReq req) respPromise)
          pure respPromise
  pure TagsServer
    { tsRequestHandler
    , tsFinishState
    , tsThreadId
    , tsFileWatch
    }
  where
    handleRequests
      :: MVar TagsServerState
      -> Chan SomeRequest
      -> FSNotify.WatchManager
      -> TagsServerState
      -> m ()
    handleRequests doneLock reqChan manager = go
      where
        go s = go =<< (handleReq s `onException` liftBase (putMVar doneLock s))
        handleReq
          :: TagsServerState
          -> m TagsServerState
        handleReq serverState = do
          req <- liftBase $ readChan reqChan
          case req of
            SomeRequest (FSNotifyReq event) () -> do
              logInfo $ "[startTagsServer.handleReq] file notification event:" ## pretty event
              case event of
                FSAdded path -> do
                  mmods <- loadMod conf path
                  pure $ case mmods of
                    Nothing             -> serverState
                    Just (impKey, mods) -> serverState
                      { tssUnloadedFiles =
                        M.insertWith (<>) impKey (fold1 $ NEMap.elemsNE mods) $ tssUnloadedFiles serverState
                      , tssKnownFiles    =
                        M.insert path impKey $ tssKnownFiles serverState
                      }
                FSRemoved path ->
                  pure $ case M.updateLookupWithKey (\_ _ -> Nothing) path $ tssKnownFiles serverState of
                    (Nothing,     _)              -> serverState
                    (Just target, tssKnownFiles') -> serverState
                      { tssLoadedModules =
                        M.delete target $ tssLoadedModules serverState
                      , tssUnloadedFiles =
                        M.delete target $ tssUnloadedFiles serverState
                      , tssKnownFiles    = tssKnownFiles'
                      }
                FSModified path ->
                  pure $ case M.lookup path $ tssKnownFiles serverState of
                    Nothing     -> serverState
                    Just impKey -> serverState
                      { tssLoadedModules =
                        M.adjust (fmap (\m -> m { modIsDirty = True })) impKey $ tssLoadedModules serverState
                      , tssUnloadedFiles =
                        M.adjust (fmap (\m -> m { modIsDirty = True })) impKey $ tssUnloadedFiles serverState
                      }
            SomeRequest (UserReq request) respPromise -> do
              -- (request, responsePromise) <- liftBase $ readChan reqChan
              logInfo $ "[startTagsServer.handleReq] request:" ## pretty request
              case request of
                QueryReq filename request' ns -> do
                  let loadedNS   = tssNamespace serverState
                      searchCfg' = SearchCfg
                        { scShallowPaths   = nsShallowDirs   ns S.\\ nsShallowDirs loadedNS
                        , scRecursivePaths = nsRecursiveDirs ns S.\\ nsRecursiveDirs loadedNS
                        , scIgnoredDirs    = scIgnoredDirs searchCfg
                        , scIgnoredGlobs   = nsIgnoredGlobs  ns S.\\ nsIgnoredGlobs loadedNS
                        }
                  serverState' <- preloadFiles searchCfg' conf serverState
                  let serverState'' = serverState' { tssNamespace = tssNamespace serverState' <> ns }
                  watchDirs conf manager reqChan searchCfg'
                  (response, serverState''') <- runSearchT conf serverState'' $
                    case request' of
                      FindSymbol scope symbol -> do
                        symbols <- findSymbol id scope filename symbol
                        pure $ case toList symbols of
                          []   -> NotFound
                          s:ss -> Found $ s :| ss
                      FindSymbolByRegex scope regexp -> do
                        symbols <- findSymbolByRegexp id scope filename regexp
                        pure $ case toList symbols of
                          []   -> NotFound
                          s:ss -> Found $ s :| ss
                  logInfo $ "[startTagsServer.handleReq] response:" ## either pretty pretty response
                  Promise.putValue respPromise response
                  pure serverState'''

-- todo: handle header files here
classifyPath :: TakeExtension a => TagsServerConf -> a -> Maybe ImportTarget
classifyPath TagsServerConf{tsconfVanillaExtensions, tsconfHsBootExtensions} path
  | ext `S.member` tsconfVanillaExtensions = Just VanillaModule
  | ext `S.member` tsconfHsBootExtensions  = Just HsBootModule
  | otherwise                              = Nothing
  where
    ext  = takeExtension path

loadMod
  :: (MonadFS m, MonadError ErrorMessage m, MonadLog m)
  => TagsServerConf
  -> FullPath 'File
  -> m (Maybe (ImportKey, NonEmptyMap (FullPath 'File) (NonEmpty UnresolvedModule)))
loadMod conf filename =
  case classifyPath conf filename of
    Nothing         -> pure Nothing
    Just importType -> do
      modTime       <- MonadFS.getModificationTime filename
      suggestedName <- fileNameToModuleName filename
      unresolvedMod@Module{modHeader = ModuleHeader{mhModName}} <-
        readFileAndLoad (Just suggestedName) modTime filename
      unresolvedMod `seq` pure (Just (ImportKey importType mhModName, NEMap.singleton filename (unresolvedMod :| [])))

