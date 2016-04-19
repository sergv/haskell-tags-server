----------------------------------------------------------------------------
-- |
-- Module      :  Server
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Server
  ( ServerConfig(..)
  , emptyServerConfig
  , defaultPort
  , runServerWithRecursiveDirs
  )
where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
-- import qualified Data.Foldable as F
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Network.Socket (PortNumber)
import System.Directory
import System.Exit
import System.FilePath.Find (FileType(Directory), find, fileType, (==?), always)

import Data.BERT
import FastTags (Pos(..), SrcPos(..), TagVal(..))
import qualified Network.BERT.Server as BERT
import qualified Network.BERT.Transport as BERT
import qualified Data.ByteString.Lazy.UTF8 as UTF8

import Logging
import Search
import Types

default (Text, Integer)

defaultPort :: PortNumber
defaultPort = 10000

runServerWithRecursiveDirs :: forall s. (BERT.Server s) => ServerConfig s -> Set FilePath -> IO ()
runServerWithRecursiveDirs conf dirTrees = do
  debugM $ "runServerWithRecursiveDirs: source directories = " <> T.take 1000 (show' (confSourceDirectories conf))
  srcDirs   <- S.fromList <$> mapM canonicalizePath (S.toList (confSourceDirectories conf))
  cabalDirs <- S.fromList <$> mapM canonicalizePath (S.toList (confCabalDirectories conf))
  dirs      <- forM (S.toList dirTrees) $
                 find always (fileType ==? Directory) <=< canonicalizePath
  debugM $ "dirs = " <> T.take 1000 (show' (S.fromList $ concat dirs))
  runServer $ conf { confSourceDirectories = srcDirs <> S.fromList (concat dirs)
                   , confCabalDirectories  = cabalDirs
                   }

runServer :: (BERT.Server s) => ServerConfig s -> IO ()
runServer conf = do
  stateRef <- newIORef emptyServerState
  unless (confLazyTagging conf) $ do
    infoM "collecting tags"
    errorM "NOT IMPLEMENTED YET: eager tags collection"
    exitFailure
  BERT.serve (confServer conf) (go stateRef)
  where
    go :: IORef ServerState -> String -> String -> [Term] -> IO BERT.DispatchResult
    go _stateRef "tags-server" "find-regexp" args =
      case args of
        [BinaryTerm _filename, BinaryTerm _regexp] ->
          return $ BERT.Success $ TupleTerm
            [ AtomTerm "error"
            , BinaryTerm "find-regexp not implemented yet"
            ]
        _                                    ->
          return $ BERT.Success $ TupleTerm
            [ AtomTerm "error"
            , BinaryTerm (UTF8.fromString $ "invalid number of arguments: " ++ show args)
            ]
    go stateRef "tags-server" "find" args =
      case args of
        [BinaryTerm filename, BinaryTerm symbol] -> do
          state <- readIORef stateRef
          (res, state') <- runStateT
                             (runReaderT
                               (runExceptT
                                  (findSymbol
                                     (UTF8.toString filename)
                                     (SymbolName $ strToText symbol)))
                               conf)
                             state
          writeIORef stateRef state'
          case res of
            Left msg ->
              return $ BERT.Success $ TupleTerm
                [ AtomTerm "error"
                , BinaryTerm (UTF8.fromString msg)
                ]
            Right [] ->
              return $ BERT.Success $ TupleTerm
                [ AtomTerm "not_found" ]
            Right [sym] ->
              return $ BERT.Success $ TupleTerm
                [ AtomTerm "loc_known"
                , symbolToBERT sym
                ]
            Right symbols ->
              return $ BERT.Success $ TupleTerm
                [ AtomTerm "loc_ambiguous"
                , ListTerm $ map symbolToBERT symbols
                ]
        _                                    ->
          return $ BERT.Success $ TupleTerm
            [ AtomTerm "error"
            , BinaryTerm (UTF8.fromString $ "invalid number of arguments: " ++ show args)
            ]
    go _ "tags-server" _ _ = return BERT.NoSuchFunction
    go _ _             _ _ = return BERT.NoSuchModule

    strToText :: UTF8.ByteString -> Text
    strToText = T.pack . UTF8.toString

    symbolToBERT :: Symbol -> Term
    symbolToBERT (Symbol (Pos (SrcPos filename line) (TagVal _ _ typ _))) =
      TupleTerm
        [ BinaryTerm (UTF8.fromString filename)
        , IntTerm line
        , AtomTerm $ show typ
        ]

