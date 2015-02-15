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
import Text.Regex.TDFA
import qualified Text.Regex.TDFA.Text as TDFA

import Data.BERT
import Network.BERT.Client
import Network.BERT.Transport

import Server

import LogCollectingServer

testDataDir :: FilePath
testDataDir = "test-data"

testsConfig :: (Server s) => s -> ServerConfig s
testsConfig serv = ServerConfig (S.singleton testDataDir) S.empty True serv

type SymbolType = String

data Response =
    Known FilePath Int SymbolType
  | Ambiguous [(FilePath, Int, SymbolType)]
  | NotFound

  -- | Error Regex
  --
-- mkRegex :: String -> Regex
-- mkRegex re =
--   either error id $ TDFA.compile
--     (defaultCompOpt { multiline = False, caseSensitive = True })
--     (defaultExecOpt { captureGroups = False })
--     re

responseToTerm :: Response -> Term
responseToTerm resp =
  case resp of
    Known filename line typ ->
      TupleTerm [AtomTerm "loc_known", mkLoc (filename, line, typ)]
    Ambiguous xs ->
      TupleTerm [AtomTerm "loc_ambiguous", ListTerm (map mkLoc xs)]
    NotFound ->
      TupleTerm [AtomTerm "not_found" ]
  where
    mkLoc (filename, line, typ) =
      TupleTerm [ BinaryTerm $ UTF8.fromString filename
                , IntTerm line
                , AtomTerm typ ]

data TestData =
    AtomicTest
      String   -- ^ name
      String   -- ^ symbol
      FilePath -- ^ filepath relative to testDataDir
      Response -- ^ expected response
  | GroupTest
      String -- ^ name
      [TestData]

testData :: TestData
testData = GroupTest "server tests"
  [ AtomicTest
      "single module"
      "foo"
      "0000single_module/SingleModule.hs"
      (Known "SingleModule.hs" 16 "Function")
  , GroupTest "imports"
      [ AtomicTest
          "wildcard import"
          "baz"
           "0001module_with_imports/ModuleWithImports.hs"
           (Known "Imported1.hs" 16 "Function")
      , AtomicTest
          "import list"
          "baz2"
          "0001module_with_imports/ModuleWithImports.hs"
           (Known "Imported2.hs" 16 "Function")
      ]
  , GroupTest "export list"
      [ AtomicTest
          "import module with export list"
          "foo"
          "0002export_lists/ModuleWithImportsThatHaveExportsList.hs"
          (Known "ModuleWithExportList.hs" 16 "Function")
      , AtomicTest
          "import module with export list #2"
          "bar"
          "0002export_lists/ModuleWithImportsThatHaveExportsList.hs"
          (Known "ModuleWithExportList.hs" 19 "Function")
      , AtomicTest
          "import module with export list #3"
          "baz"
          "0002export_lists/ModuleWithImportsThatHaveExportsList.hs"
          NotFound
      , AtomicTest
          "import module with multiline export list"
          "foo2"
          "0002export_lists/ModuleWithImportsThatHaveExportsList.hs"
          (Known "ModuleWithMultilineExportList.hs" 20 "Function")
      , AtomicTest
          "import module with multiline export list #2"
          "bar2"
          "0002export_lists/ModuleWithImportsThatHaveExportsList.hs"
          (Known "ModuleWithMultilineExportList.hs" 23 "Function")
      , AtomicTest
          "import module with multiline export list #2"
          "baz2"
          "0002export_lists/ModuleWithImportsThatHaveExportsList.hs"
          NotFound
      ]
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

mkTest :: String -> IO ServerConnection -> String -> FilePath -> Response -> TestTree
mkTest name getConn sym filename resp = testCase name $ do
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
        actual   = relativizeFilepaths res
        expected = responseToTerm resp
        msg      = "expected: " ++ show expected ++ "\n but got: " ++ show actual

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
