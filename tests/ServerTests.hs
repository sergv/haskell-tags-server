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

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Except
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Set (Set)
import qualified Data.Set as S
import Network.Socket (PortNumber)
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.PrettyPrint.Leijen.Text as PP

import Data.BERT
import Network.BERT.Client
import Network.BERT.Transport

import PortPool
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
      String     -- ^ Name
      [TestData] -- ^ Children tests

testData :: TestData
testData = GroupTest "server tests"
  [ AtomicTest
      "single module"
      "0000single_module"
      ("foo", "SingleModule.hs")
      (Known "SingleModule.hs" 16 "Function")
  , GroupTest "imports"
      [ mkTests "vanilla"
          "0001module_with_imports"
          "ModuleWithImports.hs"
          [ ("wildcard import 1"
            , "foo"
            , Known "Imported1.hs" 16 "Function"
            )
          , ("wildcard import 2"
            , "bar"
            , Known "Imported1.hs" 19 "Function"
            )
          , ( "local def in presence of wildcard import"
            , "baz"
            , Known "ModuleWithImports.hs" 19 "Function"
            )
          , ( "import list - imported name"
            , "foo2"
            , Known "Imported2.hs" 16 "Function"
            )
          -- TODO: import lists are not implemented yet
          , ( "import list - not imported name"
            , "bar2"
            , NotFound
            )
          ]
      -- test extraction and subsequent parsing of multiline import list
      , mkTests
          "multiline import list"
          "0001module_with_imports"
          "ModuleWithMultilineImportList.hs"
          [ ("import 1"
            , "foo"
            , Known "Imported1.hs" 16 "Function"
            )
          , ("import 2"
            , "bar"
            , Known "Imported1.hs" 19 "Function"
            )
          , ( "local def in presence of wildcard import"
            , "baz"
            , Known "ModuleWithMultilineImportList.hs" 26 "Function"
            )
          , ( "import 3"
            , "foo2"
            , Known "Imported2.hs" 16 "Function"
            )
          , ( "import 4"
            , "bar2"
            , Known "Imported2.hs" 19 "Function"
            )
          ]
      , mkTests
          "qualified import with alias"
          "0001module_with_imports"
          "ModuleWithQualifiedImport.hs"
          [ ("Imp.foo"
            , "Imp.foo"
            , Known "Imported1.hs" 16 "Function"
            )
          , ("Imp.bar"
            , "Imp.bar"
            , Known "Imported1.hs" 19 "Function"
            )
          , ("foo - unqualified query"
            , "foo"
            , NotFound
            )
          , ("bar - unqualified query"
            , "bar"
            , NotFound
            )
          , ( "local def"
            , "baz"
            , Known "ModuleWithQualifiedImport.hs" 18 "Function"
            )
          ]
      , mkTests
          "qualified import without alias"
          "0001module_with_imports"
          "ModuleWithQualifiedImportNoAlias.hs"
          [ ("Imported1.foo"
            , "Imported1.foo"
            , Known "Imported1.hs" 16 "Function"
            )
          , ("Imported1.bar"
            , "Imported1.bar"
            , Known "Imported1.hs" 19 "Function"
            )
          , ("foo - unqualified query"
            , "foo"
            , NotFound
            )
          , ("bar - unqualified query"
            , "bar"
            , NotFound
            )
          , ( "local def"
            , "baz"
            , Known "ModuleWithQualifiedImportNoAlias.hs" 18 "Function"
            )
          ]
      -- TODO: sophisticated import lists are not supported yet
      , mkTests
          "hiding"
          "0001module_with_imports"
          "ModuleWithImportsAndHiding.hs"
          [ ("wildcard import 1"
            , "foo"
            , Known "Imported1.hs" 16 "Function"
            )
          , ("wildcard import 2"
            , "bar"
            , Known "Imported1.hs" 19 "Function"
            )
          , ( "local def in presence of wildcard import"
            , "baz"
            , Known "ModuleWithImportsAndHiding.hs" 19 "Function"
            )
          , ( "import list - not hidden name"
            , "foo2"
            , Known "Imported2.hs" 16 "Function"
            )
          , ( "import list - hidden name"
            , "bar2"
            , NotFound
            )
          ]
      , mkTests
          "empty import list"
          "0001module_with_imports"
          "ModuleWithEmptyImportList.hs"
          [ ("wildcard import 1"
            , "foo"
            , NotFound
            )
          , ("wildcard import 2"
            , "bar"
            , NotFound
            )
          , ( "local def in presence of wildcard import"
            , "baz"
            , Known "ModuleWithEmptyImportList.hs" 19 "Function"
            )
          , ( "import list - imported name"
            , "foo2"
            , Known "Imported2.hs" 16 "Function"
            )
          , ( "import list - not imported name"
            , "bar2"
            , NotFound
            )
          ]
      ]
  , GroupTest "export list"
      [ mkTests
          "vanilla export list"
          "0002export_lists"
          "ModuleWithImportsThatHaveExportsList.hs"
          [ ( "import module with export list"
            , "foo"
            , Known "ModuleWithExportList.hs" 16 "Function"
            )
          , ( "import module with export list #2"
            , "bar"
            , Known "ModuleWithExportList.hs" 19 "Function"
            )
          , ( "import module with export list #3"
            , "baz"
            , NotFound
            )
          , ( "import module with multiline export list"
            , "foo2"
            , Known "ModuleWithMultilineExportList.hs" 20 "Function"
            )
          , ( "import module with multiline export list #2"
            , "bar2"
            , Known "ModuleWithMultilineExportList.hs" 23 "Function"
            )
          , ( "import module with multiline export list #3"
            , "baz2"
            , NotFound
            )
          ]
      , mkTests
          "wildcard export list"
          "0002export_lists"
          "ModuleWithImportsThatHaveExportsList.hs"
          [ ( "import exported name"
            , "Foo"
            , Known "ModuleWithWildcardExport.hs" 19 "Type"
            )
          , ( "import wildcard-exported name #1"
            , "Bar"
            , Known "ModuleWithWildcardExport.hs" 19 "Constructor"
            )
          , ( "import wildcard-exported name #2"
            , "Baz"
            , Known "ModuleWithWildcardExport.hs" 20 "Constructor"
            )
          , ( "import wildcard-exported name #3"
            , "getBar"
            , Known "ModuleWithWildcardExport.hs" 19 "Function"
            )
          , ( "import wildcard-exported name #4"
            , "getBaz"
            , Known "ModuleWithWildcardExport.hs" 20 "Function"
            )
          ]
      , mkTests
          "explicit export list"
          "0002export_lists"
          "ModuleWithImportsThatHaveExportsList.hs"
          [ ( "import exported name"
            , "Foo2"
            , Known "ModuleWithExplicitExport.hs" 19 "Type"
            )
          , ( "import explicitly exported name #1"
            , "Bar2"
            , Known "ModuleWithExplicitExport.hs" 19 "Constructor"
            )
          , ( "import explicitly exported name #2"
            , "Baz2"
            , NotFound
            )
          , ( "import explicitly exported name #3"
            , "getBar2"
            , Known "ModuleWithExplicitExport.hs" 19 "Function"
            )
          , ( "import explicitly exported name #4"
            , "getBaz2"
            , NotFound
            )
          ]
      , mkTests
          "reexport"
          "0002export_lists"
          "ModuleWithImportsThatHaveReexports.hs"
          [ ( "import non-exported name"
            , "baz"
            , NotFound
            )
          , ( "import re-exported name without qualification"
            , "foo"
            , Known "ModuleWithExportList.hs" 16 "Function"
            )
          , ( "import re-exported name without qualification #2"
            , "bar"
            , Known "ModuleWithExportList.hs" 19 "Function"
            )
          , ( "import re-exported name without qualification #3"
            , "foo2"
            , Known "ModuleWithMultilineExportList.hs" 20 "Function"
            )
          , ( "import re-exported name with qualification"
            , "bar2"
            , Known "ModuleWithMultilineExportList.hs" 23 "Function"
            )
          ]
      , mkTests
          "module reexport"
          "0002export_lists"
          "ModuleWithImportsThatHaveModuleReexports.hs"
          [ ( "import non-exported & non-reexported name"
            , "baz"
            , NotFound
            )
          , ( "import name exported via module that reexports itself"
            , "test"
            , Known "ModuleWithModuleReexport.hs" 20 "Function"
            )
          , ( "name imported through reexporting module #1"
            , "foo"
            , Known "ModuleWithExportList.hs" 16 "Function"
            )
          , ( "name imported through reexporting module #2"
            , "bar"
            , Known "ModuleWithExportList.hs" 19 "Function"
            )
          , ( "name imported through module reexporting with a qualifier #1"
            , "foo2"
            , Known "ModuleWithMultilineExportList.hs" 20 "Function"
            )
          , ( "name imported through module reexporting with a qualifier #2"
            , "bar2"
            , Known "ModuleWithMultilineExportList.hs" 23 "Function"
            )
          , ( "name imported through module reexporting with a qualifier #3"
            , "baz2"
            , NotFound
            )
          ]
      ]
  , mkTests
      "module header detection"
      "0003module_header_detection"
      "ModuleWithCommentsResemblingModuleHeader.hs"
      [ ( "name defined locally"
        , "foo"
        , Known "ModuleWithCommentsResemblingModuleHeader.hs" 11 "Function"
        )
      , ( "imported name"
        , "bar"
        , Known "EmptyModule.hs" 3 "Function"
        )
      ]
  , mkTests
      "typeclass export"
      "0004typeclass_export_associated_types"
      "MainModule.hs"
      [ ( "name defined locally"
        , "foo"
        , Known "MainModule.hs" 19 "Function"
        )
      , ( "typeclass member function"
        , "wrap"
        , Known "ModuleWithTypeclass.hs" 25 "Function"
        )
      , ( "associated public type family"
        , "TestFam"
        , Known "ModuleWithTypeclass.hs" 23 "Family"
        )
      , ( "imported constructor of public associated type"
        , "IntBox"
        , Known "ModuleWithTypeclass.hs" 28 "Constructor"
        )
      , ( "imported field accessor of public associated type"
        , "unIntBox"
        , Known "ModuleWithTypeclass.hs" 29 "Function"
        )
      , ( "associated private type family"
        , "PrivateFam"
        , NotFound
        )
      , ( "imported constructor of private associated type"
        , "IntBoxPrivate"
        , NotFound
        )
      , ( "imported field accessor of private associated type"
        , "unIntBoxPrivate"
        , NotFound
        )
      ]
  ]

mkTests
  :: String
  -> FilePath -- ^ Working directory under testDataDir
  -> FilePath -- ^ Filepath within the working directory
  -> [(String, UTF8.ByteString, BertResponse)]
  -> TestData
mkTests groupName dir file requests = GroupTest groupName ts
  where
    ts = [ AtomicTest name dir (sym, file) response
         | (name, sym, response) <- requests
         ]

tests :: TestTree
tests =
  withResource
    (getNumCapabilities >>= \caps -> newPortPool caps (defaultPort + 1))
    (const (return ()))
    (\pool -> makeTest pool testData)
  where
    makeTest :: IO PortPool -> TestData -> TestTree
    makeTest pool (GroupTest name xs) = testGroup name $ map (makeTest pool) xs
    makeTest pool (AtomicTest name srcDir (sym, filename) expected) =
      withResource (connect pool srcDir S.empty) closeConnection $ \getConn ->
        mkFindSymbolTest name getConn sym srcDir filename expected

data ServerConnection =
    ExistingServer TCP
  | LocalServer LogCollectingServer TCP

instance Transport ServerConnection where
  runSession (ExistingServer tcp) session = runSession tcp session
  runSession (LocalServer _ tcp)  session = runSession tcp session
  closeConnection (ExistingServer tcp)   = closeConnection tcp
  closeConnection (LocalServer serv tcp) = do
    stopLogCollectingServer serv
    closeConnection tcp

reportErr :: String -> IO a
reportErr = throwIO . ErrorCall

connect :: IO PortPool -> FilePath -> Set FilePath -> IO ServerConnection
connect pool sourceDir dirTree =
  -- Try to connect to server. If attempt succeeds then server is running
  -- and there's no need to run server ourselves.
  tryConnect defaultPort >>= either (const startLocalServer) (return . ExistingServer)
  where
    startLocalServer :: IO ServerConnection
    startLocalServer = do
      pool' <- pool
      withPort pool' $ \port -> do
        conf <- canonicalizeConfPaths =<< addRecursiveRootsToConf dirTree (testsConfig sourceDir)
        serv <- runExceptT $ mkLogCollectingServer conf port
        case serv of
          Left err -> throwIO $ ErrorCall $
            "Failed to start local server\n" ++ show err
          Right serv' -> do
            waitUntilStart serv'
            conn <- tryConnect port
            case conn of
              Left err    -> reportErr $
                "Failed to connect to locally started server\n" ++ show err
              Right conn' ->
                return $ LocalServer serv' conn'

    tryConnect :: PortNumber -> IO (Either IOException TCP)
    tryConnect port =
      (Right <$> tcpClient "localhost" port) `catch` (pure . Left)

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
              pure $ "Logs:" PP.</> PP.indent 2 (PP.vcat logs)
  case r of
    Left err  ->
      assertFailure $ displayDocString $ showDoc err PP.<$> logs
    Right res ->
      if responseType actual == responseType expected
      then
        unless (actual == expected) $
          assertFailure $ displayDocString $ msg PP.<$> showDoc logs
      else
        assertFailure $
          case extractResponseError actual of
            Nothing  ->
              displayDocString $ msg PP.<$> logs
            Just msg ->
              displayDocString $
                "Error from server:" PP.<$> PP.nest 2 (docFromByteString msg) PP.<$> logs
      where
        actual   = relativizeFilepaths res
        expected = responseToTerm resp
        msg      = docFromString $ "expected: " ++ show expected ++ "\n but got: " ++ show actual

responseType :: Term -> Maybe String
responseType (TupleTerm (AtomTerm x : _)) = Just x
responseType _                            = Nothing

extractResponseError :: Term -> Maybe UTF8.ByteString
extractResponseError (TupleTerm [AtomTerm "error", BinaryTerm msg]) = Just msg
extractResponseError _                                              = Nothing

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
