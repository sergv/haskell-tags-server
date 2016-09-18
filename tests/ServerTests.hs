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

import Control.Exception
import Control.Monad
import Control.Monad.Except
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.PrettyPrint.Leijen.Text as PP

import Data.BERT
import Network.BERT.Client
import Network.BERT.Transport

import Server.BERT
import Server.Tags
import Text.PrettyPrint.Leijen.Text.Utils

import ServerTests.LogCollectingServer

-- | Directory with test projects.
testDataDir :: FilePath
testDataDir = "test-data"

testsConfig :: FilePath -> TagsServerConf
testsConfig srcDir = emptyTagsServerConf
  { tsconfSourceDirectories =
      S.insert srcDir' $ tsconfSourceDirectories emptyTagsServerConf
  , tsconfLazyTagging       = True
  }
  where
    srcDir' :: FilePath
    srcDir' = testDataDir </> srcDir

type SymbolType = String

-- | Type that encodes all possible BERT responses.
data BertResponse =
    Known FilePath Int SymbolType
  | Ambiguous [(FilePath, Int, SymbolType)]
  | NotFound

responseToTerm :: BertResponse -> Term
responseToTerm resp =
  case resp of
    Known filename line typ ->
      TupleTerm [AtomTerm "loc_known", mkSymbol (filename, line, typ)]
    Ambiguous xs ->
      TupleTerm [AtomTerm "loc_ambiguous", ListTerm (map mkSymbol xs)]
    NotFound ->
      AtomTerm "not_found"
  where
    mkSymbol (filename, line, typ) = TupleTerm
      [ BinaryTerm $ UTF8.fromString filename
      , IntTerm line
      , AtomTerm typ
      ]

data TestData =
    AtomicTest
      String          -- ^ Test name
      FilePath        -- ^ Working directory under testDataDir
      ( UTF8.ByteString -- ^ Symbol
      , FilePath        -- ^ Filepath within the working directory
      )
      BertResponse    -- ^ Expected response
  | GroupTest
      String     -- ^ name
      [TestData] -- ^ Children tests

testData :: TestData
testData = GroupTest "server tests"
  [ AtomicTest
      "single module"
      "0000single_module"
      ("foo", "SingleModule.hs")
      (Known "SingleModule.hs" 16 "Function")
  -- , GroupTest "imports"
  --     [ GroupTest "vanilla"
  --         [ AtomicTest name sym "0001module_with_imports" "ModuleWithImports.hs" res
  --         | (name, sym, res) <-
  --           [ ("wildcard import 1"
  --             , "foo"
  --             , Known "Imported1.hs" 16 "Function"
  --             )
  --           , ("wildcard import 2"
  --             , "bar"
  --             , Known "Imported1.hs" 19 "Function"
  --             )
  --           , ( "local def in presence of wildcard import"
  --             , "baz"
  --             , Known "ModuleWithImports.hs" 19 "Function"
  --             )
  --           , ( "import list - imported name"
  --             , "foo2"
  --             , Known "Imported2.hs" 16 "Function"
  --             )
  --           -- import lists are not implemented yet
  --           -- , ( "import list - not imported name"
  --           --   , "bar2"
  --           --   , NotFound
  --           --   )
  --           ]
  --         ]
  --     -- test extraction and subsequent parsing of multiline import list
  --     , GroupTest "multiline import list"
  --         [ AtomicTest name sym "0001module_with_imports" "ModuleWithMultilineImportList.hs" res
  --         | (name, sym, res) <-
  --           [ ("import 1"
  --             , "foo"
  --             , Known "Imported1.hs" 16 "Function"
  --             )
  --           , ("import 2"
  --             , "bar"
  --             , Known "Imported1.hs" 19 "Function"
  --             )
  --           , ( "local def in presence of wildcard import"
  --             , "baz"
  --             , Known "ModuleWithMultilineImportList.hs" 26 "Function"
  --             )
  --           , ( "import 3"
  --             , "foo2"
  --             , Known "Imported2.hs" 16 "Function"
  --             )
  --           , ( "import 4"
  --             , "bar2"
  --             , Known "Imported2.hs" 19 "Function"
  --             )
  --           ]
  --         ]
  --     , GroupTest "qualified import with alias"
  --         [ AtomicTest name sym "0001module_with_imports" "ModuleWithQualifiedImport.hs" res
  --         | (name, sym, res) <-
  --           [ ("Imp.foo"
  --             , "Imp.foo"
  --             , Known "Imported1.hs" 16 "Function"
  --             )
  --           , ("Imp.bar"
  --             , "Imp.bar"
  --             , Known "Imported1.hs" 19 "Function"
  --             )
  --           , ("foo - unqualified query"
  --             , "foo"
  --             , NotFound
  --             )
  --           , ("bar - unqualified query"
  --             , "bar"
  --             , NotFound
  --             )
  --           , ( "local def"
  --             , "baz"
  --             , Known "ModuleWithQualifiedImport.hs" 18 "Function"
  --             )
  --           ]
  --         ]
  --     , GroupTest "qualified import without alias"
  --         [ AtomicTest name sym "0001module_with_imports" "ModuleWithQualifiedImportNoAlias.hs" res
  --         | (name, sym, res) <-
  --           [ ("Imported1.foo"
  --             , "Imported1.foo"
  --             , Known "Imported1.hs" 16 "Function"
  --             )
  --           , ("Imported1.bar"
  --             , "Imported1.bar"
  --             , Known "Imported1.hs" 19 "Function"
  --             )
  --           , ("foo - unqualified query"
  --             , "foo"
  --             , NotFound
  --             )
  --           , ("bar - unqualified query"
  --             , "bar"
  --             , NotFound
  --             )
  --           , ( "local def"
  --             , "baz"
  --             , Known "ModuleWithQualifiedImportNoAlias.hs" 18 "Function"
  --             )
  --           ]
  --         ]
  --     -- sophisticated import lists are not supported yet
  --     -- , GroupTest "hiding"
  --     --     [ AtomicTest name sym "0001module_with_imports" "ModuleWithImportsAndHiding.hs" res
  --     --     | (name, sym, res) <-
  --     --       [ ("wildcard import 1"
  --     --         , "foo"
  --     --         , Known "Imported1.hs" 16 "Function"
  --     --         )
  --     --       , ("wildcard import 2"
  --     --         , "bar"
  --     --         , Known "Imported1.hs" 19 "Function"
  --     --         )
  --     --       , ( "local def in presence of wildcard import"
  --     --         , "baz"
  --     --         , Known "ModuleWithImportsAndHiding.hs" 19 "Function"
  --     --         )
  --     --       , ( "import list - not hidden name"
  --     --         , "foo2"
  --     --         , Known "Imported2.hs" 16 "Function"
  --     --         )
  --     --       , ( "import list - hidden name"
  --     --         , "bar2"
  --     --         , NotFound
  --     --         )
  --     --       ]
  --     --     ]
  --     -- , GroupTest "empty import list" $
  --     --     [ AtomicTest name sym "0001module_with_imports" "ModuleWithEmptyImportList.hs" res
  --     --     | (name, sym, res) <-
  --     --       [ ("wildcard import 1"
  --     --         , "foo"
  --     --         , NotFound
  --     --         )
  --     --       , ("wildcard import 2"
  --     --         , "bar"
  --     --         , NotFound
  --     --         )
  --     --       , ( "local def in presence of wildcard import"
  --     --         , "baz"
  --     --         , Known "ModuleWithEmptyImportList.hs" 19 "Function"
  --     --         )
  --     --       , ( "import list - imported name"
  --     --         , "foo2"
  --     --         , Known "Imported2.hs" 16 "Function"
  --     --         )
  --     --       , ( "import list - not imported name"
  --     --         , "bar2"
  --     --         , NotFound
  --     --         )
  --     --       ]
  --     --     ]
  --     ]
  -- , GroupTest "export list"
  --     [ GroupTest "vanilla export list"
  --         [ AtomicTest name sym "0002export_lists" "ModuleWithImportsThatHaveExportsList.hs" res
  --         | (name, sym, res) <-
  --           [ ( "import module with export list"
  --             , "foo"
  --             , Known "ModuleWithExportList.hs" 16 "Function"
  --             )
  --           , ( "import module with export list #2"
  --             , "bar"
  --             , Known "ModuleWithExportList.hs" 19 "Function"
  --             )
  --           , ( "import module with export list #3"
  --             , "baz"
  --             , NotFound
  --             )
  --           , ( "import module with multiline export list"
  --             , "foo2"
  --             , Known "ModuleWithMultilineExportList.hs" 20 "Function"
  --             )
  --           , ( "import module with multiline export list #2"
  --             , "bar2"
  --             , Known "ModuleWithMultilineExportList.hs" 23 "Function"
  --             )
  --           , ( "import module with multiline export list #3"
  --             , "baz2"
  --             , NotFound
  --             )
  --           ]
  --         ]
  --     , GroupTest "wildcard export list"
  --         [ AtomicTest name sym "0002export_lists" "ModuleWithImportsThatHaveExportsList.hs" res
  --         | (name, sym, res) <-
  --           [ ( "import exported name"
  --             , "Foo"
  --             , Known "ModuleWithWildcardExport.hs" 19 "Type"
  --             )
  --           , ( "import wildcard-exported name #2"
  --             , "Bar"
  --             , Known "ModuleWithWildcardExport.hs" 19 "Constructor"
  --             )
  --           , ( "import wildcard-exported name #3"
  --             , "Baz"
  --             , Known "ModuleWithWildcardExport.hs" 20 "Constructor"
  --             )
  --           , ( "import wildcard-exported name #4"
  --             , "getBar"
  --             , Known "ModuleWithWildcardExport.hs" 19 "Function"
  --             )
  --           , ( "import wildcard-exported name #5"
  --             , "getBaz"
  --             , Known "ModuleWithWildcardExport.hs" 20 "Function"
  --             )
  --           ]
  --         ]
  --     , GroupTest "explicit export list"
  --         [ AtomicTest name sym "0002export_lists" "ModuleWithImportsThatHaveExportsList.hs" res
  --         | (name, sym, res) <-
  --           [ ( "import exported name"
  --             , "Foo2"
  --             , Known "ModuleWithExplicitExport.hs" 19 "Type"
  --             )
  --           , ( "import wildcard-exported name #2"
  --             , "Bar2"
  --             , Known "ModuleWithExplicitExport.hs" 19 "Constructor"
  --             )
  --           , ( "import wildcard-exported name #3"
  --             , "Baz2"
  --             , NotFound
  --             )
  --           , ( "import wildcard-exported name #4"
  --             , "getBar2"
  --             , Known "ModuleWithExplicitExport.hs" 19 "Function"
  --             )
  --           , ( "import wildcard-exported name #5"
  --             , "getBaz2"
  --             , NotFound
  --             )
  --           ]
  --         ]
  --     , GroupTest "reexport"
  --         [ AtomicTest name sym "0002export_lists" "ModuleWithImportsThatHaveReexports.hs" res
  --         | (name, sym, res) <-
  --           [ ( "import non-exported name"
  --             , "baz"
  --             , NotFound
  --             )
  --           , ( "import re-exported name without qualification"
  --             , "foo"
  --             , Known "ModuleWithExportList.hs" 16 "Function"
  --             )
  --           , ( "import re-exported name without qualification #2"
  --             , "bar"
  --             , Known "ModuleWithExportList.hs" 19 "Function"
  --             )
  --           , ( "import re-exported name without qualification #3"
  --             , "foo2"
  --             , Known "ModuleWithMultilineExportList.hs" 20 "Function"
  --             )
  --           , ( "import re-exported name with qualification"
  --             , "bar2"
  --             , Known "ModuleWithMultilineExportList.hs" 23 "Function"
  --             )
  --           ]
  --         ]
  --     , GroupTest "module reexport"
  --         [ AtomicTest name sym "0002export_lists" "ModuleWithImportsThatHaveModuleReexports.hs" res
  --         | (name, sym, res) <-
  --           [ ( "import non-exported & non-reexported name"
  --             , "baz"
  --             , NotFound
  --             )
  --           , ( "name imported through reexporting module"
  --             , "foo"
  --             , Known "ModuleWithExportList.hs" 16 "Function"
  --             )
  --           , ( "name imported through reexporting module #2"
  --             , "bar"
  --             , Known "ModuleWithExportList.hs" 19 "Function"
  --             )
  --           , ( "name imported through module reexporting with a qualifier"
  --             , "foo2"
  --             , NotFound
  --             )
  --           , ( "name imported through module reexporting with a qualifier #2"
  --             , "bar2"
  --             , NotFound
  --             )
  --           ]
  --         ]
  --     ]
  -- , GroupTest "module header detection"
  --     [ AtomicTest name sym "0003module_header_detection" "ModuleWithCommentsResemblingModuleHeader.hs" res
  --     | (name, sym, res) <-
  --       [ ( "name defined locally"
  --         , "foo"
  --         , Known "ModuleWithCommentsResemblingModuleHeader.hs" 11 "Function"
  --         )
  --       , ( "imported name"
  --         , "bar"
  --         , Known "EmptyModule.hs" 3 "Function"
  --         )
  --       ]
  --     ]
  ]

tests :: TestTree
tests = makeTest testData
  where
    makeTest :: TestData -> TestTree
    makeTest (GroupTest name xs) = testGroup name $ map makeTest xs
    makeTest (AtomicTest name srcDir (sym, filename) expected) =
      withResource (connect srcDir S.empty) closeConnection $ \getConn ->
        mkFindSymbolTest name getConn sym srcDir filename expected

data ServerConnection =
    ExistingServer TCP
  | LocalServer LogCollectingServer TCP

instance Transport ServerConnection where
  runSession (ExistingServer tcp) session = runSession tcp session
  runSession (LocalServer _ tcp)  session = runSession tcp session
  closeConnection (ExistingServer tcp) = closeConnection tcp
  closeConnection (LocalServer _ tcp)  = closeConnection tcp

reportErr :: String -> IO a
reportErr = throwIO . ErrorCall

connect :: FilePath -> Set FilePath -> IO ServerConnection
connect sourceDir dirTree =
  -- Try to connect to server. If attempt succeeds then server is running
  -- and there's no need to run server ourselves.
  tryConnect >>= either (const startLocalServer) (return . ExistingServer)
  where
    startLocalServer :: IO ServerConnection
    startLocalServer = do
      conf <- canonicalizeConfPaths =<< addRecursiveRootsToConf dirTree (testsConfig sourceDir)
      serv <- runExceptT $ mkLogCollectingServer conf defaultPort
      case serv of
        Left err -> throwIO $ ErrorCall $
          "Failed to start local server\n" ++ show err
        Right serv' -> do
          waitUntilStart serv'
          conn <- tryConnect
          case conn of
            Left err    -> reportErr $
              "Failed to connect to locally started server\n" ++ show err
            Right conn' ->
              return $ LocalServer serv' conn'
    -- err e = throwIO $ ErrorCall $ "Failed to connect; is tags-server running?\n" ++ show e

    tryConnect :: IO (Either IOException TCP)
    tryConnect =
      (Right <$> tcpClient "localhost" defaultPort) `catch` (pure . Left)

mkFindSymbolTest
  :: String
  -> IO ServerConnection
  -> UTF8.ByteString
  -> FilePath
  -> FilePath
  -> BertResponse
  -> TestTree
mkFindSymbolTest name getConn sym dir filename resp = testCase name $ do
  conn <- getConn
  f    <- UTF8.fromString <$> canonicalizePath (testDataDir </> dir </> filename)
  r    <- call conn "tags-server" "find" [ BinaryTerm f
                                         , BinaryTerm sym
                                         ]
  logs <- case conn of
            ExistingServer _   -> return mempty
            LocalServer serv _ -> do
              logs <- getLogs serv
              pure $ "Logs:" <> PP.indent 2 (PP.vcat logs)
  case r of
    Left err  ->
      assertFailure $ displayDocString $ showDoc err PP.<$> logs
    Right res ->
      unless (actual == expected) $
        assertFailure $ displayDocString $ msg PP.<$> showDoc logs
      where
        actual   = relativizeFilepaths res
        expected = responseToTerm resp
        msg      = docFromString $ "expected: " ++ show expected ++ "\n but got: " ++ show actual

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
