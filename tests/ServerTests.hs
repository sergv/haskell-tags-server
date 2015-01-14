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

module ServerTests (testsConfig, tests) where

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

testDataDir :: FilePath
testDataDir = "test-data"

testsConfig :: ServerConfig
testsConfig = ServerConfig (S.singleton testDataDir) S.empty 10000 True

tests :: TestTree
tests =
  withResource connect closeConnection $ \getConn ->
    testGroup "server tests"
      [ mkTest "single module" getConn
          "SingleModule.hs" "foo"
          (TupleTerm [AtomTerm "loc_known", TupleTerm [BinaryTerm "SingleModule.hs", IntTerm 16, AtomTerm "Function"]])
      , testGroup "imports"
          [ mkTest "wildcard import" getConn
              "ModuleWithImports.hs" "baz"
              (TupleTerm [AtomTerm "loc_known", TupleTerm [BinaryTerm "Imported1.hs", IntTerm 16, AtomTerm "Function"]])
          , mkTest "import list" getConn
              "ModuleWithImports.hs" "baz2"
              (TupleTerm [AtomTerm "loc_known", TupleTerm [BinaryTerm "Imported2.hs", IntTerm 16, AtomTerm "Function"]])
          ]
      , testGroup "export list"
          [
          ]
      ]

connect :: IO TCP
connect =
  tcpClient "localhost" (confPort testsConfig) `catch` \(e :: IOException) ->
    throwIO $ ErrorCall $ "Failed to connect; is tags-server running?\n" ++ show e

mkTest :: (Transport t) => String -> IO t -> FilePath -> String -> Term -> TestTree
mkTest name getConn filename sym expected = testCase name $ do
  conn <- getConn
  f    <- canonicalizePath $ testDataDir </> filename
  r    <- call conn "tags-server" "find" [ BinaryTerm (UTF8.fromString f)
                                         , BinaryTerm (UTF8.fromString sym)
                                         ]
  case r of
    Left err -> assertFailure $ show err
    Right r  -> relativize r @?= expected -- assertFailure (show (r :: Term))

relativize :: Term -> Term
relativize term =
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

