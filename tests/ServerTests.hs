----------------------------------------------------------------------------
-- |
-- Module      :  ServerTests
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module ServerTests (tests) where

import Control.Applicative
import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.Set as S
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit

import Data.BERT
import Network.BERT.Client
import Network.BERT.Transport

import Server

import LogCollectingServer

testDataDir :: FilePath
testDataDir = "test-data"

testsConfig :: (Server s) => s -> ServerConfig s
testsConfig serv = ServerConfig (S.singleton testDataDir) S.empty True serv

data TestData =
    AtomicTest
      String   -- ^ name
      String   -- ^ symbol
      FilePath -- ^ filepath relative to testDataDir
      Term     -- ^ expected
  | GroupTest
      String -- ^ name
      [TestData]

testData :: TestData
testData = GroupTest "server tests"
  [ AtomicTest
      "single module"
      "foo"
      "0000single_module/SingleModule.hs"
      (TupleTerm [ AtomTerm "loc_known"
                 , TupleTerm [ BinaryTerm "SingleModule.hs"
                             , IntTerm 16
                             , AtomTerm "Function" ]])
  , GroupTest "imports"
      [ AtomicTest
          "wildcard import"
          "baz"
           "ModuleWithImports.hs"
           (TupleTerm [ AtomTerm "loc_known"
                      , TupleTerm [ BinaryTerm "Imported1.hs"
                                  , IntTerm 16
                                  , AtomTerm "Function" ]])
      , AtomicTest
          "import list"
          "baz2"
          "ModuleWithImports.hs"
          (TupleTerm [ AtomTerm "loc_known"
                     , TupleTerm [ BinaryTerm "Imported2.hs"
                                 , IntTerm 16
                                 , AtomTerm "Function" ]])
      ]
  , GroupTest "export list" []
  ]

tests :: TestTree
tests = makeTest testData
  where
    makeTest :: TestData -> TestTree
    makeTest (GroupTest name xs) = testGroup name $ map makeTest xs
    makeTest (AtomicTest name sym filename expected) =
      withResource connect closeConnection $ \getConn ->
        mkTest name getConn sym filename expected

data ServerConnection =
    ExistingServer TCP
  | LocalServer LogCollectingServer ThreadId TCP

instance Transport ServerConnection where
  runSession (ExistingServer tcp)  session = runSession tcp session
  runSession (LocalServer _ _ tcp) session = runSession tcp session
  closeConnection (ExistingServer tcp)           = closeConnection tcp
  closeConnection (LocalServer _ servThread tcp) = do
    closeConnection tcp
    killThread servThread

connect :: IO ServerConnection
connect =
  -- Try to connect to server. If attempt succeeds then server is running
  -- and there's no need to run server ourselves.
  tryConnect >>= either (const startServer) (return . ExistingServer)
  where
    startServer :: IO ServerConnection
    startServer = do
      serv       <- logCollectingServer defaultPort
      servThread <- forkIO (runServerWithRecursiveDirs
                              (testsConfig serv)
                              (S.singleton testDataDir))
      waitUntilStart serv
      conn       <- tryConnect
      case conn of
        Left err    -> throwIO $ ErrorCall $
          "Failed to connect to locally started server\n" ++ show err
        Right conn' ->
          return $ LocalServer serv servThread conn'
    -- err e = throwIO $ ErrorCall $ "Failed to connect; is tags-server running?\n" ++ show e

    tryConnect :: IO (Either IOException TCP)
    tryConnect =
      (Right <$> tcpClient "localhost" defaultPort) `catch`
        \(e :: IOException) -> return $ Left e

mkTest :: String -> IO ServerConnection -> String -> FilePath -> Term -> TestTree
mkTest name getConn sym filename expected = testCase name $ do
  conn <- getConn
  f    <- canonicalizePath $ testDataDir </> filename
  r    <- call conn "tags-server" "find" [ BinaryTerm (UTF8.fromString f)
                                         , BinaryTerm (UTF8.fromString sym)
                                         ]
  logs <- case conn of
            ExistingServer _     -> return []
            LocalServer serv _ _ -> do
              logs <- getLogs serv
              return $ "" : "Logs:" : logs
  case r of
    Left err  -> do
      assertFailure $ show err ++ unlines logs
    Right res ->
      unless (actual == expected) $ do
        assertFailure $ msg ++ unlines logs
      where
        actual = relativizeFilepaths res
        msg = "expected: " ++ show expected ++ "\n but got: " ++ show actual

relativizeFilepaths :: Term -> Term
relativizeFilepaths term =
  case term of
    TupleTerm [a@(AtomTerm "loc_known"), loc] ->
      TupleTerm [a, fixLoc loc]
    TupleTerm [a@(AtomTerm "loc_ambiguous"), ListTerm locs] ->
      TupleTerm [a, ListTerm $ map fixLoc locs]
    x -> x
  where
    fixLoc :: Term -> Term
    fixLoc (TupleTerm [BinaryTerm path, line, typ]) =
      TupleTerm [BinaryTerm $ toFilename path, line, typ]
    fixLoc x = error $ "invalid symbol location term: " ++ show x
    toFilename :: UTF8.ByteString -> UTF8.ByteString
    toFilename = UTF8.fromString . takeFileName . UTF8.toString
