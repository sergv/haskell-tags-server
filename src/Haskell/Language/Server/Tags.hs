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

  , loadMod
  ) where

import Control.Concurrent
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except.Ext
import Control.Monad.Trans.Control
import qualified Data.ByteString.Lazy as BSL
import Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit.Zlib as Zlib
import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as M
import Data.Semigroup.Foldable
import qualified Data.Set as S
import qualified Data.Store as Store
import Prettyprinter.Ext (Pretty(..), (##), (<+>))
import qualified System.Directory as Directory
import System.IO

import qualified Data.Promise as Promise

import Control.Monad.Filesystem (MonadFS(..), SearchCfg(..))
import qualified Control.Monad.Filesystem as MonadFS
import Control.Monad.Logging
import Data.CompiledRegex
import Data.ErrorMessage
import Data.Map.NonEmpty (NonEmptyMap)
import qualified Data.Map.NonEmpty as NEMap
import Data.Path
import Data.Symbols (fileNameToModuleName, resolvedSymbolFile)
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
  }

stopTagsServer :: MonadBase IO m => TagsServer -> m ()
stopTagsServer TagsServer{tsThreadId} = liftBase $ do
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
preloadFiles SearchCfg{scShallowPaths, scRecursivePaths} _ s
  | S.null scShallowPaths && S.null scRecursivePaths
  = pure s
preloadFiles searchCfg conf s = do
  ignoredGlobsRE <- searchCfgIgnoredRE searchCfg
  knownFiles     <- MonadFS.findRec searchCfg ignoredGlobsRE (loadMod conf) (const (pure Nothing))
  let s' = s
        { tssKnownFiles =
            M.fromList (concatMap (\(impKey, fs) -> map (,impKey) fs) $ M.toList $ toList . NEMap.keysNE <$> knownFiles) <>
              tssKnownFiles s
        }
  if tsconfEagerTagging conf
  then do
    logInfo "[preloadFiles] collecting tags eagerly..."
    ls <- loadAllFilesIntoState (fold1 . NEMap.elemsNE <$> knownFiles) conf $ tssLoadState s'
    logInfo "[preloadFiles] collecting tags eagerly... OK"
    pure s' { tssLoadState = ls }
  else do
    let ls = tssLoadState s
    pure s'
      { tssLoadState = ls
          { lsUnloadedFiles =
              M.unionWith (<>) (fold1 . NEMap.elemsNE <$> knownFiles) (lsUnloadedFiles ls)
          }
      }

-- | Start new tags server thread that will serve requests supplied via returned
-- RequestHandler.
startTagsServer
  :: forall m. (WithCallStack, MonadBase IO m, MonadBaseControl IO m, MonadCatch m, MonadError ErrorMessage m, MonadLog m, MonadFS m, MonadMask m)
  => SearchCfg
  -> TagsServerConf
  -> m TagsServer
startTagsServer searchCfg conf = do
  initState     <- case tsconfSerialisedState conf of
    Nothing   -> pure emptyTagsServerState
    Just file -> do
      exists <- liftBase $ Directory.doesFileExist file
      if exists
      then do
        contents <- liftBase $ withFile file ReadMode $ \h ->
          C.runConduit $
            C.sourceHandleUnsafe h .| Zlib.decompress Zlib.defaultWindowBits .| C.sinkLbs
        case Store.decode $ BSL.toStrict contents of
          Left msg -> throwErrorWithCallStack $ "Failed to read server state from" <+> pretty file ## pretty (show msg)
          Right x  -> pure x
      else pure emptyTagsServerState
  initState'    <- preloadFiles searchCfg conf initState
  reqChan       <- liftBase newChan
  tsFinishState <- liftBase newEmptyMVar
  tsThreadId    <- liftBaseDiscard forkIO $ handleRequests tsFinishState reqChan initState'
  let tsRequestHandler :: RequestHandler
      tsRequestHandler = \case
        req@QueryReq{} -> do
          respPromise <- Promise.newPromise
          writeChan reqChan (SomeRequest (UserReq req) respPromise)
          pure respPromise
        req@FinishReq{} -> do
          respPromise <- Promise.newPromise
          writeChan reqChan (SomeRequest (UserReq req) respPromise)
          pure respPromise
  pure TagsServer
    { tsRequestHandler
    , tsFinishState
    , tsThreadId
    }
  where
    handleRequests
      :: MVar TagsServerState
      -> Chan SomeRequest
      -> TagsServerState
      -> m ()
    handleRequests doneLock reqChan = go
      where
        go :: TagsServerState -> m ()
        go s = do
          req <- liftBase $ readChan reqChan
          s'  <- handleReq req s `onException` serialiseState s
          case s' of
            Nothing  -> pure ()
            Just s'' -> go s''
        serialiseState :: TagsServerState -> m ()
        serialiseState s = do
          case tsconfSerialisedState conf of
            Nothing   -> pure ()
            Just dest -> do
              logInfo $ "[startTagsServer.handleReq] storing state in" <+> pretty dest
              liftBase $ C.runConduitRes $
                C.sourceLbs (BSL.fromStrict (Store.encode s)) .| Zlib.compress 9 Zlib.defaultWindowBits .| C.sinkFileCautious dest
          liftBase $ putMVar doneLock s
        handleReq
          :: SomeRequest
          -> TagsServerState
          -> m (Maybe TagsServerState)
        handleReq req serverState = case req of
          SomeRequest (FSNotifyReq event) () -> do
            logInfo $ "[startTagsServer.handleReq] file notification event:" ## pretty event
            fmap Just $ case event of
              FSAdded path -> do
                mmods <- loadMod conf path
                pure $ case mmods of
                  Nothing             -> serverState
                  Just (impKey, mods) -> serverState
                    { tssLoadState =
                        let s = tssLoadState serverState
                        in s
                          { lsUnloadedFiles =
                              M.insertWith (<>) impKey (fold1 $ NEMap.elemsNE mods) $ lsUnloadedFiles s
                          }
                    , tssKnownFiles    =
                        M.insert path impKey $ tssKnownFiles serverState
                    }
              FSRemoved path ->
                pure $ case M.updateLookupWithKey (\_ _ -> Nothing) path $ tssKnownFiles serverState of
                  (Nothing,     _)              -> serverState
                  (Just target, tssKnownFiles') -> serverState
                    { tssLoadState =
                        let s = tssLoadState serverState
                        in s
                          { lsLoadedModules = M.delete target $ lsLoadedModules s
                          , lsUnloadedFiles = M.delete target $ lsUnloadedFiles s
                          }
                    , tssKnownFiles    = tssKnownFiles'
                    }
              FSModified path ->
                pure $ case M.lookup path $ tssKnownFiles serverState of
                  Nothing     -> serverState
                  Just impKey -> serverState
                    { tssLoadState =
                        let s = tssLoadState serverState
                        in s
                          { lsLoadedModules =
                              M.adjust (fmap (\m -> m { modIsDirty = True })) impKey $ lsLoadedModules s
                          , lsUnloadedFiles =
                              M.adjust (fmap (\m -> m { modIsDirty = True })) impKey $ lsUnloadedFiles s
                          }
                    }
          SomeRequest (UserReq request) respPromise -> do
            -- (request, responsePromise) <- liftBase $ readChan reqChan
            logInfo $ "[startTagsServer.handleReq] request:" ## pretty request
            case request of
              FinishReq -> do
                serialiseState serverState
                Promise.putValue respPromise (Right ())
                pure Nothing
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

                (response, loadState) <- runSearchT conf (tssLoadState serverState'') $ do
                  symbols <- case request' of
                    FindSymbol scope symbol ->
                      findSymbol scope filename symbol
                    FindSymbolByRegex scope regexp ->
                      findSymbolByRegexp scope filename regexp
                  logInfo $ "[startTagsServer.handleReq] requested namespace:" ## pretty ns
                  pure $ case filter (isPathWithinNamespace ns . resolvedSymbolFile) $ toList symbols of
                    []   -> NotFound
                    s:ss -> Found $ s :| ss
                logInfo $ "[startTagsServer.handleReq] response:" ## either pretty pretty response
                Promise.putValue respPromise response
                pure $ Just $ serverState'' { tssLoadState = loadState }

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

