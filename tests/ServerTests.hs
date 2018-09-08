----------------------------------------------------------------------------
-- |
-- Module      :  ServerTests
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module ServerTests (tests) where

import Control.Concurrent
import Control.Exception (IOException, throwIO, ErrorCall(..))
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Filesystem
import Control.Monad.Trans.Control

import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.Set as S
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Ext
import Data.Void (Void)
import Network.Socket (PortNumber)
import Test.Tasty
import Test.Tasty.HUnit

import Data.BERT
import Network.BERT.Client
import Network.BERT.Transport

import Control.Monad.Filesystem.FileSearch (SearchCfg(..))
import Data.Path
import Haskell.Language.Server.BERT
import Haskell.Language.Server.Tags
import PortPool

import ServerTests.LogCollectingServer

-- | Directory with test projects.
testDataDir :: PathFragment
testDataDir = "test-data"

mkTestsConfig :: MonadBase IO m => WorkingDirectory -> m TagsServerConf
mkTestsConfig srcDir = liftBase $ do
  searchDirsCfg <- case srcDir of
    ShallowDir   dir -> do
      dir' <- mkFullPath $ testDataDir </> dir
      pure defaultSearchDirsCfg { shallowPaths = S.singleton dir' }
    RecursiveDir dir -> do
      dir' <- mkFullPath $ testDataDir </> dir
      pure defaultSearchDirsCfg { recursivePaths = S.singleton dir' }
  pure defaultTagsServerConf
    { tsconfSearchDirs   = searchDirsCfg
    , tsconfEagerTagging = False
    }
  where
    defaultSearchDirsCfg = tsconfSearchDirs defaultTagsServerConf

type SymbolType = String

-- | Type that encodes all possible BERT responses.
data ServerResponse =
    Known BaseName Int SymbolType
  | Ambiguous [(BaseName, Int, SymbolType)]
  | NotFound
  deriving (Eq, Ord, Show)

responseToTerm :: ServerResponse -> Term
responseToTerm resp =
  case resp of
    Known filename line typ ->
      TupleTerm [AtomTerm "loc_known", mkSymbol (filename, line, typ)]
    Ambiguous xs ->
      TupleTerm [AtomTerm "loc_ambiguous", ListTerm (map mkSymbol xs)]
    NotFound ->
      AtomTerm "not_found"
  where
    mkSymbol :: (BaseName, Int, SymbolType) -> Term
    mkSymbol (filename, line, typ) = TupleTerm
      [ BinaryTerm $ baseNameToUTF8 filename
      , IntTerm line
      , AtomTerm typ
      ]

data WorkingDirectory =
    ShallowDir PathFragment
  | RecursiveDir PathFragment
  deriving (Eq, Ord, Show)

data ServerTest = ServerTest
  { stTestName         :: String
  , stWorkingDirectory :: WorkingDirectory
  , stFile             :: BaseName
  , stSymbol           :: UTF8.ByteString
  , stExpectedResponse :: ServerResponse
  } deriving (Eq, Ord, Show)

data TestSet a =
    AtomicTest a
  | GroupTest String [TestSet a]
  deriving (Functor, Foldable, Traversable)

group :: String -> [a] -> TestSet a
group name = GroupTest name . map AtomicTest

mkQualUnqualTest :: (String, b, Maybe b, c) -> TestSet (String, b, c)
mkQualUnqualTest (name, unqualSym, qualSym, response) =
  case qualSym of
    Nothing       -> AtomicTest (name, unqualSym, response)
    Just qualSym' ->
      GroupTest name
        [ AtomicTest ("unqualified", unqualSym, response)
        , AtomicTest ("qualified", qualSym', response)
        ]

withWorkingDir
  :: WorkingDirectory     -- ^ Working directory under testDataDir
  -> TestSet
       ( String          -- ^ Test name
       , BaseName        -- ^ Filepath within the working directory
       , UTF8.ByteString -- ^ Symbol to search for
       , ServerResponse  -- ^ Expected response
       )
  -> TestSet ServerTest
withWorkingDir dir =
  fmap $ \(name, file, sym, response) -> ServerTest
    { stTestName         = name
    , stWorkingDirectory = dir
    , stFile             = file
    , stSymbol           = sym
    , stExpectedResponse = response
    }

withFile
  :: a                   -- ^ Filepath within the working directory
  -> TestSet
       ( String          -- ^ Test name
       , UTF8.ByteString -- ^ Symbol to search for
       , ServerResponse  -- ^ Expected response
       )
  -> TestSet
       ( String          -- ^ Test name
       , a               -- ^ Filepath within the working directory
       , UTF8.ByteString -- ^ Symbol to search for
       , ServerResponse  -- ^ Expected response
       )
withFile file =
  fmap (\(name, sym, response) -> (name, file, sym, response))

withDirAndFile
  :: WorkingDirectory    -- ^ Working directory under testDataDir
  -> BaseName            -- ^ Filepath within the working directory
  -> TestSet
       ( String          -- ^ Test name
       , UTF8.ByteString -- ^ Symbol to search for
       , ServerResponse  -- ^ Expected response
       )
  -> TestSet ServerTest
withDirAndFile dir file =
  withWorkingDir dir . fmap (\(name, sym, response) -> (name, file, sym, response))

testData :: TestSet ServerTest
testData = GroupTest "server tests"
  [ AtomicTest ServerTest
      { stTestName         = "single module"
      , stWorkingDirectory = ShallowDir "0000single_module"
      , stFile             = "SingleModule.hs"
      , stSymbol           = "foo"
      , stExpectedResponse = Known "SingleModule.hs" 11 "Function"
      }
  , GroupTest "imports"
      [ withDirAndFile (ShallowDir "0001module_with_imports") "ModuleWithImports.hs" $
          GroupTest "vanilla"
            [ GroupTest "wildcard import" $ map mkQualUnqualTest
                [ ("name #1"
                  , "foo"
                  , Just "Imported1.foo"
                  , Known "Imported1.hs" 13 "Function"
                  )
                , ("name #2"
                  , "bar"
                  , Just "Imported1.bar"
                  , Known "Imported1.hs" 16 "Function"
                  )
                , ("operator"
                  , "$$"
                  , Just "Imported1.$$"
                  , Known "Imported1.hs" 19 "Operator"
                  )
                , ("type name"
                  , ":$$:"
                  , Just "Imported1.:$$:"
                  , Known "Imported1.hs" 22 "Type"
                  )
                , ("constructor name"
                  , ":$$$:"
                  , Just "Imported1.:$$$:"
                  , Known "Imported1.hs" 23 "Constructor"
                  )
                , ( "local def"
                  , "baz"
                  , Nothing
                  , Known "ModuleWithImports.hs" 16 "Function"
                  )
                ]
            , GroupTest "explicit import list" $ map mkQualUnqualTest
                [ ( "imported name"
                  , "foo2"
                  , Just "Imported2.foo2"
                  , Known "Imported2.hs" 13 "Function"
                  )
                , ( "not imported name"
                  , "bar2"
                  , Just "Imported2.bar2"
                  , NotFound
                  )
                , ( "imported operator"
                  , "$$*"
                  , Just "Imported2.$$*"
                  , Known "Imported2.hs" 19 "Operator"
                  )
                , ( "imported type name"
                  , ":$$*:"
                  , Just "Imported2.:$$*:"
                  , Known "Imported2.hs" 22 "Type"
                  )
                , ( "imported constructor name"
                  , ":$$$*:"
                  , Just "Imported2.:$$$*:"
                  , Known "Imported2.hs" 23 "Constructor"
                  )
                ]
            ]
      -- test extraction and subsequent parsing of multiline import list
      , withDirAndFile (ShallowDir "0001module_with_imports") "ModuleWithMultilineImportList.hs" $
          group "multiline import list"
            [ ("import #1"
              , "foo"
              , Known "Imported1.hs" 13 "Function"
              )
            , ("import #2"
              , "bar"
              , Known "Imported1.hs" 16 "Function"
              )
            , ( "import #3"
              , "foo2"
              , Known "Imported2.hs" 13 "Function"
              )
            , ( "import #4"
              , "bar2"
              , Known "Imported2.hs" 16 "Function"
              )
            , ( "local def in presence of wildcard import"
              , "baz"
              , Known "ModuleWithMultilineImportList.hs" 21 "Function"
              )
            ]
      , withDirAndFile (ShallowDir "0001module_with_imports") "ModuleWithQualifiedImport.hs" $
          group "qualified import with alias"
            [ ("Imp.foo"
              , "Imp.foo"
              , Known "Imported1.hs" 13 "Function"
              )
            , ("Imp.bar"
              , "Imp.bar"
              , Known "Imported1.hs" 16 "Function"
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
              , Known "ModuleWithQualifiedImport.hs" 13 "Function"
              )
            ]
      , withDirAndFile (ShallowDir "0001module_with_imports") "ModuleWithQualifiedImportNoAlias.hs" $
          group "qualified import without alias"
            [ ("Imported1.foo"
              , "Imported1.foo"
              , Known "Imported1.hs" 13 "Function"
              )
            , ("Imported1.bar"
              , "Imported1.bar"
              , Known "Imported1.hs" 16 "Function"
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
              , Known "ModuleWithQualifiedImportNoAlias.hs" 13 "Function"
              )
            ]
      , withDirAndFile (ShallowDir "0001module_with_imports") "ModuleWithImportsAndHiding.hs" $
          group "hiding"
            [ ("wildcard import #1"
              , "foo"
              , Known "Imported1.hs" 13 "Function"
              )
            , ("wildcard import #2"
              , "bar"
              , Known "Imported1.hs" 16 "Function"
              )
            , ( "import list - not hidden name"
              , "foo2"
              , Known "Imported2.hs" 13 "Function"
              )
            , ( "import list - hidden name"
              , "bar2"
              , NotFound
              )
            , ( "local def in presence of wildcard import"
              , "baz"
              , Known "ModuleWithImportsAndHiding.hs" 14 "Function"
              )
            ]
      , withDirAndFile (ShallowDir "0001module_with_imports") "ModuleWithEmptyImportList.hs" $
          group "empty import list"
            [ ("wildcard import #1"
              , "foo"
              , NotFound
              )
            , ("wildcard import #2"
              , "bar"
              , NotFound
              )
            , ( "import list - imported name"
              , "foo2"
              , Known "Imported2.hs" 13 "Function"
              )
            , ( "import list - not imported name"
              , "bar2"
              , NotFound
              )
            , ( "local def in presence of wildcard import"
              , "baz"
              , Known "ModuleWithEmptyImportList.hs" 14 "Function"
              )
            ]
      ]
  , GroupTest "export list"
      [ withDirAndFile (ShallowDir "0002export_lists") "ModuleWithImportsThatHaveExportsList.hs" $
          group "vanilla export list"
            [ ( "import module with export list #1"
              , "foo"
              , Known "ModuleWithExportList.hs" 11 "Function"
              )
            , ( "import module with export list #2"
              , "bar"
              , Known "ModuleWithExportList.hs" 14 "Function"
              )
            , ( "import module with export list #3"
              , "baz"
              , NotFound
              )
            , ( "import module with multiline export list #1"
              , "foo2"
              , Known "ModuleWithMultilineExportList.hs" 15 "Function"
              )
            , ( "import module with multiline export list #2"
              , "bar2"
              , Known "ModuleWithMultilineExportList.hs" 18 "Function"
              )
            , ( "import module with multiline export list #3"
              , "baz2"
              , NotFound
              )
            ]
      , withDirAndFile (ShallowDir "0002export_lists") "ModuleWithImportsThatHaveExportsList.hs" $
          group "wildcard export list"
            [ ( "import exported name"
              , "Foo"
              , Known "ModuleWithWildcardExport.hs" 14 "Type"
              )
            , ( "import wildcard-exported name #1"
              , "Bar"
              , Known "ModuleWithWildcardExport.hs" 14 "Constructor"
              )
            , ( "import wildcard-exported name #2"
              , "Baz"
              , Known "ModuleWithWildcardExport.hs" 15 "Constructor"
              )
            , ( "import wildcard-exported name #3"
              , "getBar"
              , Known "ModuleWithWildcardExport.hs" 14 "Function"
              )
            , ( "import wildcard-exported name #4"
              , "getBaz"
              , Known "ModuleWithWildcardExport.hs" 15 "Function"
              )
            ]
      , withDirAndFile (ShallowDir "0002export_lists") "ModuleWithImportsThatHaveExportsList.hs" $
          group "explicit export list"
            [ ( "import exported name"
              , "Foo2"
              , Known "ModuleWithExplicitExport.hs" 14 "Type"
              )
            , ( "import explicitly exported name #1"
              , "Bar2"
              , Known "ModuleWithExplicitExport.hs" 14 "Constructor"
              )
            , ( "import explicitly exported name #2"
              , "Baz2"
              , NotFound
              )
            , ( "import explicitly exported name #3"
              , "getBar2"
              , Known "ModuleWithExplicitExport.hs" 14 "Function"
              )
            , ( "import explicitly exported name #4"
              , "getBaz2"
              , NotFound
              )
            ]
      , withDirAndFile (ShallowDir "0002export_lists") "ModuleWithImportsThatHaveReexports.hs" $
          group "reexport"
            [ ( "import non-exported name"
              , "baz"
              , NotFound
              )
            , ( "import re-exported name without qualification #1"
              , "foo"
              , Known "ModuleWithExportList.hs" 11 "Function"
              )
            , ( "import re-exported name without qualification #2"
              , "bar"
              , Known "ModuleWithExportList.hs" 14 "Function"
              )
            , ( "import re-exported name without qualification #3"
              , "foo2"
              , Known "ModuleWithMultilineExportList.hs" 15 "Function"
              )
            , ( "import re-exported name with qualification"
              , "bar2"
              , Known "ModuleWithMultilineExportList.hs" 18 "Function"
              )
            ]
      ]
  , withDirAndFile (ShallowDir "0003module_header_detection") "ModuleWithCommentsResemblingModuleHeader.hs" $
      group "module header detection"
        [ ( "name defined locally"
          , "foo"
          , Known "ModuleWithCommentsResemblingModuleHeader.hs" 11 "Function"
          )
        , ( "imported name"
          , "bar"
          , Known "EmptyModule.hs" 3 "Function"
          )
        ]
  , withDirAndFile (ShallowDir "0004typeclass_export_associated_types") "MainModule.hs" $
      group "typeclass export"
        [ ( "name defined locally"
          , "foo"
          , Known "MainModule.hs" 14 "Function"
          )
        , ( "typeclass member function"
          , "wrap"
          , Known "ModuleWithTypeclass.hs" 20 "Function"
          )
        , ( "associated public type family"
          , "TestFam"
          , Known "ModuleWithTypeclass.hs" 18 "Family"
          )
        , ( "imported constructor of public associated type"
          , "IntBox"
          , Known "ModuleWithTypeclass.hs" 26 "Constructor"
          )
        , ( "imported field accessor of public associated type"
          , "unIntBox"
          , Known "ModuleWithTypeclass.hs" 27 "Function"
          )
        , ( "associated private type family"
          , "PrivateFam"
          , Known "ModuleWithTypeclass.hs" 19 "Family"
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
  , withWorkingDir (ShallowDir "0005import_cycle") $
      GroupTest "import cycle"
        [ GroupTest "wildcard export lists"
            [ withFile "A.hs" $
                group "A.hs"
                  [ ( "type defined locally in A"
                    , "TA"
                    , Known "A.hs" 14 "Type"
                    )
                  , ( "function defined locally in A"
                    , "f"
                    , Known "A.hs" 16 "Function"
                    )
                  , ( "type name imported into A"
                    , "TB"
                    , Known "B.hs" 15 "Type"
                    )
                  , ( "function imported into A"
                    , "g"
                    , Known "B.hs" 17 "Function"
                    )
                  ]
            , withFile "B.hs" $
                group "B.hs"
                  [ ( "type defined locally in B"
                    , "TB"
                    , Known "B.hs" 15 "Type"
                    )
                  , ( "function defined locally in B"
                    , "g"
                    , Known "B.hs" 17 "Function"
                    )
                  , ( "type name imported into B"
                    , "TA"
                    , Known "A.hs-boot" 4 "Type"
                    )
                  , ( "function imported into B"
                    , "f"
                    , NotFound
                    )
                  ]
            ]
        , GroupTest "explicit export lists"
            [ withFile "AWithExportList.hs" $
                group "AWithExportList.hs"
                  [ ( "type defined locally in AWithExportList"
                    , "TA"
                    , Known "AWithExportList.hs" 14 "Type"
                    )
                  , ( "function defined locally in AWithExportList"
                    , "f"
                    , Known "AWithExportList.hs" 16 "Function"
                    )
                  , ( "type name imported into AWithExportList"
                    , "TB"
                    , Known "BWithExportList.hs" 15 "Type"
                    )
                  , ( "function imported into AWithExportList"
                    , "g"
                    , Known "BWithExportList.hs" 17 "Function"
                    )
                  ]
            , withFile "BWithExportList.hs" $
                group "BWithExportList.hs"
                  [ ( "type defined locally in BWithExportList"
                    , "TB"
                    , Known "BWithExportList.hs" 15 "Type"
                    )
                  , ( "function defined locally in BWithExportList"
                    , "g"
                    , Known "BWithExportList.hs" 17 "Function"
                    )
                  , ( "type name imported into BWithExportList"
                    , "TA"
                    , Known "AWithExportList.hs-boot" 4 "Type"
                    )
                  , ( "function imported into BWithExportList"
                    , "f"
                    , NotFound
                    )
                  ]
            ]
        ]
  , withWorkingDir (ShallowDir "0006export_pattern_with_type_ghc8.0") $
      GroupTest "export pattern along with type"
        [ withFile file $
            group groupName
              [ ( "Exported type"
                , "FooTyp"
                , Known "ModuleThatExportsPattern.hs" 14 "Type"
                )
              , ( "Exported constructor 1"
                , "Foo"
                , Known "ModuleThatExportsPattern.hs" 15 "Constructor"
                )
              , ( "Exported constructor 2"
                , "Bar"
                , Known "ModuleThatExportsPattern.hs" 16 "Constructor"
                )
              , ( "Non-existent constructor"
                , "Baz"
                , NotFound
                )
              , ( "Exported pattern 1"
                , "Foo'"
                , Known "ModuleThatExportsPattern.hs" 18 "Pattern"
                )
              , ( "Exported pattern 2"
                , "Baz'"
                , Known "ModuleThatExportsPattern.hs" 19 "Pattern"
                )
              ]
        | (file, groupName) <-
            [ ("ModuleWithoutImportList.hs", "import without import list")
            , ("ModuleWithWildcardImportList.hs", "import with wildcard import list")
            , ("ModuleWithSpecificImportList.hs", "import with specific import list")
            ]
        ]
  , withWorkingDir (ShallowDir "0007resolvable_import_cycle") $
      GroupTest "Resolvable import cycle"
        [ withFile "A.hs" $
          group "A imports B with import list"
            [ ( "Local function 1"
              , "foo"
              , Known "A.hs" 14 "Function"
              )
            , ( "Local function 2"
              , "bar"
              , Known "A.hs" 17 "Function"
              )
            , ( "Local function 3"
              , "baz"
              , Known "A.hs" 20 "Function"
              )
             , ( "Imported visible type 1"
              , "FooTyp"
              , Known "B.hs" 14 "Type"
              )
            , ( "Imported visible constructor"
              , "Foo"
              , Known "B.hs" 14 "Constructor"
              )
            , ( "Imported visible type 2"
              , "BarTyp"
              , Known "B.hs" 16 "Type"
              )
            , ( "Imported hidden constructor"
              , "Bar"
              , NotFound
              )
            , ( "Non-imported type"
              , "BazTyp"
              , NotFound
              )
            , ( "Constructor of non-imported type"
              , "Baz"
              , NotFound
              )
            ]
        , withFile "B.hs" $
          group "B imports A with import list"
            [ ( "Local type 1"
              , "FooTyp"
              , Known "B.hs" 14 "Type"
              )
            , ( "Local constructor 1"
              , "Foo"
              , Known "B.hs" 14 "Constructor"
              )
            , ( "Local type 2"
              , "BarTyp"
              , Known "B.hs" 16 "Type"
              )
            , ( "Local constructor 2"
              , "Bar"
              , Known "B.hs" 16 "Constructor"
              )
            , ( "Local type 3"
              , "BazTyp"
              , Known "B.hs" 18 "Type"
              )
            , ( "Local constructor 3"
              , "Baz"
              , Known "B.hs" 18 "Constructor"
              )
            , ( "Imported function 1"
              , "foo"
              , Known "A.hs" 14 "Function"
              )
            , ( "Imported function 2"
              , "bar"
              , Known "A.hs" 17 "Function"
              )
            , ( "Not imported function"
              , "baz"
              , NotFound
              )
            ]
        , withFile "C.hs" $
          group "C imports D with import list"
            [ ( "Local function 1"
              , "foo"
              , Known "C.hs" 14 "Function"
              )
            , ( "Local function 2"
              , "bar"
              , Known "C.hs" 17 "Function"
              )
            , ( "Local function 3"
              , "baz"
              , Known "C.hs" 20 "Function"
              )
             , ( "Imported visible type 1"
              , "FooTyp"
              , Known "D.hs" 14 "Type"
              )
            , ( "Imported visible constructor"
              , "Foo"
              , Known "D.hs" 14 "Constructor"
              )
            , ( "Imported visible type 2"
              , "BarTyp"
              , Known "D.hs" 16 "Type"
              )
            , ( "Imported hidden constructor"
              , "Bar"
              , NotFound
              )
            , ( "Non-imported type"
              , "BazTyp"
              , NotFound
              )
            , ( "Constructor of non-imported type"
              , "Baz"
              , NotFound
              )
            ]
        , withFile "D.hs" $
          group "D imports C without import list"
            [ ( "Local type 1"
              , "FooTyp"
              , Known "D.hs" 14 "Type"
              )
            , ( "Local constructor 1"
              , "Foo"
              , Known "D.hs" 14 "Constructor"
              )
            , ( "Local type 2"
              , "BarTyp"
              , Known "D.hs" 16 "Type"
              )
            , ( "Local constructor 2"
              , "Bar"
              , Known "D.hs" 16 "Constructor"
              )
            , ( "Local type 3"
              , "BazTyp"
              , Known "D.hs" 18 "Type"
              )
            , ( "Local constructor 3"
              , "Baz"
              , Known "D.hs" 18 "Constructor"
              )
            , ( "Imported function 1"
              , "foo"
              , Known "C.hs" 14 "Function"
              )
            , ( "Imported function 2"
              , "bar"
              , Known "C.hs" 17 "Function"
              )
            , ( "Imported function 3"
              , "baz"
              , Known "C.hs" 20 "Function"
              )
            ]

        , withFile "E.hs" $
          group "E without export list imports F with import list"
            [ ( "Local function 1"
              , "foo"
              , Known "E.hs" 14 "Function"
              )
            , ( "Local function 2"
              , "bar"
              , Known "E.hs" 17 "Function"
              )
            , ( "Local function 3"
              , "baz"
              , Known "E.hs" 20 "Function"
              )
             , ( "Imported visible type 1"
              , "FooTyp"
              , Known "F.hs" 14 "Type"
              )
            , ( "Imported visible constructor"
              , "Foo"
              , Known "F.hs" 14 "Constructor"
              )
            , ( "Imported visible type 2"
              , "BarTyp"
              , Known "F.hs" 16 "Type"
              )
            , ( "Imported hidden constructor"
              , "Bar"
              , NotFound
              )
            , ( "Non-imported type"
              , "BazTyp"
              , NotFound
              )
            , ( "Constructor of non-imported type"
              , "Baz"
              , NotFound
              )
            ]
        , withFile "F.hs" $
          group "F imports E without import list"
            [ ( "Local type 1"
              , "FooTyp"
              , Known "F.hs" 14 "Type"
              )
            , ( "Local constructor 1"
              , "Foo"
              , Known "F.hs" 14 "Constructor"
              )
            , ( "Local type 2"
              , "BarTyp"
              , Known "F.hs" 16 "Type"
              )
            , ( "Local constructor 2"
              , "Bar"
              , Known "F.hs" 16 "Constructor"
              )
            , ( "Local type 3"
              , "BazTyp"
              , Known "F.hs" 18 "Type"
              )
            , ( "Local constructor 3"
              , "Baz"
              , Known "F.hs" 18 "Constructor"
              )
            , ( "Imported function 1"
              , "foo"
              , Known "E.hs" 14 "Function"
              )
            , ( "Imported function 2"
              , "bar"
              , Known "E.hs" 17 "Function"
              )
            , ( "Imported function 3"
              , "baz"
              , Known "E.hs" 20 "Function"
              )
            ]

        , withFile "G.hs" $
          group "G without export list imports H with import list"
            [ ( "Local function 1"
              , "foo"
              , Known "G.hs" 14 "Function"
              )
            , ( "Local function 2"
              , "bar"
              , Known "G.hs" 17 "Function"
              )
            , ( "Local function 3"
              , "baz"
              , Known "G.hs" 20 "Function"
              )
             , ( "Imported visible type 1"
              , "FooTyp"
              , Known "H.hs" 14 "Type"
              )
            , ( "Imported visible constructor 1"
              , "Foo"
              , Known "H.hs" 14 "Constructor"
              )
            , ( "Imported visible type 2"
              , "BarTyp"
              , Known "H.hs" 16 "Type"
              )
            , ( "Imported visible constructor 2"
              , "Bar"
              , Known "H.hs" 16 "Constructor"
              )
            , ( "Non-imported type"
              , "BazTyp"
              , NotFound
              )
            , ( "Constructor of non-imported type"
              , "Baz"
              , NotFound
              )
            ]
        , withFile "H.hs" $
          group "H imports G with import list"
            [ ( "Local type 1"
              , "FooTyp"
              , Known "H.hs" 14 "Type"
              )
            , ( "Local constructor 1"
              , "Foo"
              , Known "H.hs" 14 "Constructor"
              )
            , ( "Local type 2"
              , "BarTyp"
              , Known "H.hs" 16 "Type"
              )
            , ( "Local constructor 2"
              , "Bar"
              , Known "H.hs" 16 "Constructor"
              )
            , ( "Local type 3"
              , "BazTyp"
              , Known "H.hs" 18 "Type"
              )
            , ( "Local constructor 3"
              , "Baz"
              , Known "H.hs" 18 "Constructor"
              )
            , ( "Imported function 1"
              , "foo"
              , Known "G.hs" 14 "Function"
              )
            , ( "Imported function 2"
              , "bar"
              , Known "G.hs" 17 "Function"
              )
            , ( "Not imported function"
              , "baz"
              , NotFound
              )
            ]
        ]
  , withDirAndFile (ShallowDir "0008module_reexport") "ModuleWithImportsThatHaveModuleReexports.hs" $
      group "Module reexport"
        [ ( "Import non-exported & non-reexported name"
          , "baz"
          , NotFound
          )
        , ( "Import name exported via module that reexports itself"
          , "test"
          , Known "ModuleWithModuleReexport.hs" 15 "Function"
          )
        , ( "Name imported through module with vanilla module reexport #1"
          , "foo"
          , Known "Module1.hs" 12 "Function"
          )
        , ( "Name imported through module with vanilla module reexport #2"
          , "bar"
          , Known "Module1.hs" 16 "Function"
          )
        , ( "Name imported through module reexporting with alias #1"
          , "foo2"
          , Known "Module2.hs" 13 "Function"
          )
        , ( "Name imported through module reexporting with alias #2"
          , "bar2"
          , Known "Module2.hs" 18 "Function"
          )
        , ( "Private non-exported name"
          , "baz2"
          , NotFound
          )
        , ( "Name imported through qualified module reexporting #1"
          , "foo3"
          , NotFound
          )
        , ( "Name imported through qualified module reexporting #2"
          , "bar3"
          , NotFound
          )
        ]
  , withDirAndFile (ShallowDir "0009empty_export_list_is_wildcard") "MainModule.hs" $
      group "Empty export list is treated as export all wildcard"
        [ ( "Non-exported name #1"
          , "Foo"
          , Known "ModuleWithEmptyExportList.hs" 11 "Type"
          )
        , ( "Non-exported name #2"
          , "Bar"
          , Known "ModuleWithEmptyExportList.hs" 12 "Constructor"
          )
        , ( "Non-exported name #3"
          , "Baz"
          , Known "ModuleWithEmptyExportList.hs" 15 "Type"
          )
        , ( "Non-exported name #4"
          , "Quux"
          , Known "ModuleWithEmptyExportList.hs" 16 "Constructor"
          )
        , ( "Non-exported name #5"
          , "frob"
          , Known "ModuleWithEmptyExportList.hs" 20 "Function"
          )
        , ( "Non-existing name"
          , "frobnicate"
          , NotFound
          )
        ]
  , GroupTest "Import of module that defines some entities via macro"
    [ withDirAndFile (ShallowDir "0010exported_name_defined_via_macro") groupModule $
        group groupName
          [ ( "Type defined via macro #1"
            , "ViaMacroWithWildcardChildren"
            , Known "Definitions.hs" 12 "Type"
            )
          , ( "Wildcard-exported constructor defined via macro #1.1"
            , "MWWC1"
            , NotFound
            )
          , ( "Wildcard-exported constructor defined via macro #1.2"
            , "MWWC2"
            , NotFound
            )

          , ( "Type defined via macro #2"
            , "ViaMacroWithExplicitChildren"
            , Known "Definitions.hs" 13 "Type"
            )
          , ( "Unexported constructor defined via macro #2.1"
            , "MWEC1"
            , NotFound
            )
          , ( "Explicitly exported constructor defined via macro #2.2"
            , "MWEC2"
            , Known "Definitions.hs" 13 "Constructor"
            )

          , ( "Type defined via macro #3"
            , "ViaMacroNoChildren"
            , Known "Definitions.hs" 14 "Type"
            )
          , ( "Unexported constructor defined via macro #3.1"
            , "MNC1"
            , NotFound
            )
          , ( "Unexported constructor defined via macro #3.2"
            , "MNC2"
            , NotFound
            )

          , ( "Function defined via macro"
            , "viaMacro"
            , Known "Definitions.hs" 15 "Function"
            )

          , ( "Vanilla type #1"
            , "ViaDefWithWildcardChildren"
            , Known "Definitions.hs" 34 "Type"
            )
          , ( "Vanilla wildcard-exported constructor #1"
            , "DWWC1"
            , Known "Definitions.hs" 35 "Constructor"
            )
          , ( "Vanilla wildcard-exported constructor #2"
            , "DWWC2"
            , Known "Definitions.hs" 36 "Constructor"
            )

          , ( "Vanilla type #2"
            , "ViaDefWithExplicitChildren"
            , Known "Definitions.hs" 39 "Type"
            )
          , ( "Vanilla unexported constructor #2.1"
            , "DWEC1"
            , NotFound
            )
          , ( "Vanilla explicitly exported constructor"
            , "DWEC2"
            , Known "Definitions.hs" 41 "Constructor"
            )

          , ( "Vanilla type #3"
            , "ViaDefNoChildren"
            , Known "Definitions.hs" 44 "Type"
            )
          , ( "Vanilla unexported constructor #3.1"
            , "DNC1"
            , NotFound
            )
          , ( "Vanilla unexported constructor #3.2"
            , "DNC2"
            , NotFound
            )

          , ( "Vanilla function"
            , "viaDef"
            , Known "Definitions.hs" 51 "Function"
            )
          ]
    | (groupName, groupModule) <-
      [ ("Direct import", "ImportDirectly.hs")
      , ("Via reexport", "ImportViaReexport.hs")
      ]
    ]
  ]

tests :: TestTree
tests =
  withResource
    (getNumCapabilities >>= \caps -> newPortPool caps (defaultPort + 1))
    (const (pure ()))
    (\pool -> makeTestTree pool testData)
  where
    makeTestTree :: IO PortPool -> TestSet ServerTest -> TestTree
    makeTestTree pool = go
      where
        go (GroupTest name xs)     = testGroup name $ map go xs
        go (AtomicTest serverTest) = mkFindSymbolTest pool serverTest

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

reportErr :: MonadBase IO m => Doc Void -> m a
reportErr = liftBase . throwIO . ErrorCall . displayDocString

withConnection
  :: forall m a. (MonadMask m, MonadBaseControl IO m, MonadFS m)
  => IO PortPool
  -> TagsServerConf
  -> (ServerConnection -> m a)
  -> m a
withConnection pool conf f = do
  -- Try to connect to a server. If attempt succeeds then server is running
  -- and there's no need to run server ourselves.
  existing <- tryConnect defaultPort
  case existing of
    Right existing' ->
      f (ExistingServer existing') `finally` liftBase (closeConnection existing')
    Left  _reason   -> startLocalServer
  where
    startLocalServer :: m a
    startLocalServer = do
      pool' <- liftBase pool
      withPort pool' $ \port -> do
        serv <- runExceptT $ mkLogCollectingServer conf port
        case serv of
          Left err -> reportErr $
            "Failed to start local server:" ## pretty err
          Right serv' -> do
            waitUntilStart serv'
            conn <- tryConnect port
            case conn of
              Left err    -> reportErr $
                "Failed to connect to a locally started server:" ## ppShow err
              Right conn' -> do
                let server = LocalServer serv' conn'
                f server `finally` liftBase (closeConnection server)

    tryConnect :: PortNumber -> m (Either IOException TCP)
    tryConnect port =
      liftBase (Right <$> tcpClient "localhost" port) `catch` (pure . Left)

mkFindSymbolTest
  :: IO PortPool
  -> ServerTest
  -> TestTree
mkFindSymbolTest pool ServerTest{stTestName, stWorkingDirectory, stFile, stSymbol, stExpectedResponse} =
 testCase stTestName $ do
    conf <- mkTestsConfig stWorkingDirectory
    withConnection pool conf $ \conn -> do
      let dir  = case stWorkingDirectory of
            ShallowDir   x -> x
            RecursiveDir x -> x
          path = pathFragmentToUTF8 $ testDataDir </> dir </> stFile
      r    <- call conn "haskell-tags-server" "find" [ BinaryTerm path
                                                     , BinaryTerm stSymbol
                                                     ]
      logs <- case conn of
                ExistingServer _   -> pure mempty
                LocalServer serv _ -> do
                  logs <- getLogs serv
                  pure $ "Logs:" ## PP.indent 2 (PP.vcat logs)
      case r of
        Left err  ->
          assertFailure $ displayDocString $ ppShow err ## logs
        Right res -> do
          actual <- relativizePathsInResponse res
          let msg = docFromString $ "expected: " ++ show expected ++ "\n but got: " ++ show actual
          if responseType actual == responseType expected
          then
            unless (actual == expected) $
              assertFailure $ displayDocString $ msg ## logs
          else
            assertFailure $ displayDocString $
              case extractResponseError actual of
                Nothing   ->
                  msg ## logs
                Just msg' ->
                  "Error from server:" ## PP.nest 2 (docFromByteString msg') ## logs
          where
            expected = responseToTerm stExpectedResponse

responseType :: Term -> Maybe String
responseType (TupleTerm (AtomTerm x : _)) = Just x
responseType _                            = Nothing

extractResponseError :: Term -> Maybe UTF8.ByteString
extractResponseError (TupleTerm [AtomTerm "error", BinaryTerm msg]) = Just msg
extractResponseError _                                              = Nothing

relativizePathsInResponse :: forall m. MonadBase IO m => Term -> m Term
relativizePathsInResponse term =
  case term of
    TupleTerm [a@(AtomTerm "loc_known"), loc] -> do
      loc' <- fixLoc loc
      pure $ TupleTerm [a, loc']
    TupleTerm [a@(AtomTerm "loc_ambiguous"), ListTerm locs] -> do
      locs' <- traverse fixLoc locs
      pure $ TupleTerm [a, ListTerm locs']
    x -> pure x
  where
    fixLoc :: Term -> m Term
    fixLoc (TupleTerm [BinaryTerm path, line, typ]) = do
      path' <- toFilename path
      pure $ TupleTerm [BinaryTerm path', line, typ]
    fixLoc x = error $ "invalid symbol location term: " ++ show x
    toFilename :: UTF8.ByteString -> m UTF8.ByteString
    toFilename = fmap (baseNameToUTF8 . takeFileName) . fullPathFromUTF8

baseNameToUTF8 :: BaseName -> UTF8.ByteString
baseNameToUTF8 = pathFragmentToUTF8 . unBaseName

_fullPathToUTF8 :: FullPath -> UTF8.ByteString
_fullPathToUTF8 = C8.fromStrict . TE.encodeUtf8 . unFullPath

fullPathFromUTF8 :: MonadBase IO m => UTF8.ByteString -> m FullPath
fullPathFromUTF8 = mkFullPath . TE.decodeUtf8 . C8.toStrict

pathFragmentToUTF8 :: PathFragment -> UTF8.ByteString
pathFragmentToUTF8 = C8.fromStrict . TE.encodeUtf8 . unPathFragment

