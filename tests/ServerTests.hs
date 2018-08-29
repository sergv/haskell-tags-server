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
import Control.Exception
import Control.Monad
import Control.Monad.Base
import Control.Monad.Except
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Ext
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

mkTestsConfig :: MonadBase IO m => PathFragment -> Set FullPath -> m TagsServerConf
mkTestsConfig srcDir trees = liftBase $ do
  srcDir' <- mkFullPath $ testDataDir </> srcDir
  pure defaultTagsServerConf
    { tsconfSearchDirs   = (tsconfSearchDirs defaultTagsServerConf)
        { shallowPaths   = S.singleton srcDir'
        , recursivePaths = trees
        }
    , tsconfEagerTagging = False
    }

type SymbolType = String

-- | Type that encodes all possible BERT responses.
data BertResponse =
    Known BaseName Int SymbolType
  | Ambiguous [(BaseName, Int, SymbolType)]
  | NotFound
  deriving (Eq, Ord, Show)

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
    mkSymbol :: (BaseName, Int, SymbolType) -> Term
    mkSymbol (filename, line, typ) = TupleTerm
      [ BinaryTerm $ baseNameToUTF8 filename
      , IntTerm line
      , AtomTerm typ
      ]

data ServerTest = ServerTest
  { stTestName         :: String
  , stWorkingDirectory :: Directory
  , stFile             :: BaseName
  , stSymbol           :: UTF8.ByteString
  , stExpectedResponse :: BertResponse
  } deriving (Eq, Ord, Show)

newtype Directory = Directory { unDirectory :: PathFragment }
  deriving (Eq, Ord, Show)

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
  :: Directory           -- ^ Working directory under testDataDir
  -> TestSet
       ( String          -- ^ Test name
       , BaseName        -- ^ Filepath within the working directory
       , UTF8.ByteString -- ^ Symbol to search for
       , BertResponse    -- ^ Expected response
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
       , BertResponse    -- ^ Expected response
       )
  -> TestSet
       ( String          -- ^ Test name
       , a               -- ^ Filepath within the working directory
       , UTF8.ByteString -- ^ Symbol to search for
       , BertResponse    -- ^ Expected response
       )
withFile file =
  fmap (\(name, sym, response) -> (name, file, sym, response))

withDirAndFile
  :: Directory           -- ^ Working directory under testDataDir
  -> BaseName            -- ^ Filepath within the working directory
  -> TestSet
       ( String          -- ^ Test name
       , UTF8.ByteString -- ^ Symbol to search for
       , BertResponse    -- ^ Expected response
       )
  -> TestSet ServerTest
withDirAndFile dir file =
  withWorkingDir dir . fmap (\(name, sym, response) -> (name, file, sym, response))

testData :: TestSet ServerTest
testData = GroupTest "server tests"
  [ AtomicTest ServerTest
      { stTestName         = "single module"
      , stWorkingDirectory = Directory "0000single_module"
      , stFile             = "SingleModule.hs"
      , stSymbol           = "foo"
      , stExpectedResponse = Known "SingleModule.hs" 16 "Function"
      }
  , GroupTest "imports"
      [ withDirAndFile (Directory "0001module_with_imports") "ModuleWithImports.hs" $
          GroupTest "vanilla"
            [ GroupTest "wildcard import" $ map mkQualUnqualTest
                [ ("name #1"
                  , "foo"
                  , Just "Imported1.foo"
                  , Known "Imported1.hs" 18 "Function"
                  )
                , ("name #2"
                  , "bar"
                  , Just "Imported1.bar"
                  , Known "Imported1.hs" 21 "Function"
                  )
                , ("operator"
                  , "$$"
                  , Just "Imported1.$$"
                  , Known "Imported1.hs" 24 "Operator"
                  )
                , ("type name"
                  , ":$$:"
                  , Just "Imported1.:$$:"
                  , Known "Imported1.hs" 27 "Type"
                  )
                , ("constructor name"
                  , ":$$$:"
                  , Just "Imported1.:$$$:"
                  , Known "Imported1.hs" 28 "Constructor"
                  )
                , ( "local def"
                  , "baz"
                  , Nothing
                  , Known "ModuleWithImports.hs" 21 "Function"
                  )
                ]
            , GroupTest "explicit import list" $ map mkQualUnqualTest
                [ ( "imported name"
                  , "foo2"
                  , Just "Imported2.foo2"
                  , Known "Imported2.hs" 18 "Function"
                  )
                , ( "not imported name"
                  , "bar2"
                  , Just "Imported2.bar2"
                  , NotFound
                  )
                , ( "imported operator"
                  , "$$*"
                  , Just "Imported2.$$*"
                  , Known "Imported2.hs" 24 "Operator"
                  )
                , ( "imported type name"
                  , ":$$*:"
                  , Just "Imported2.:$$*:"
                  , Known "Imported2.hs" 27 "Type"
                  )
                , ( "imported constructor name"
                  , ":$$$*:"
                  , Just "Imported2.:$$$*:"
                  , Known "Imported2.hs" 28 "Constructor"
                  )
                ]
            ]
      -- test extraction and subsequent parsing of multiline import list
      , withDirAndFile (Directory "0001module_with_imports") "ModuleWithMultilineImportList.hs" $
          group "multiline import list"
            [ ("import #1"
              , "foo"
              , Known "Imported1.hs" 18 "Function"
              )
            , ("import #2"
              , "bar"
              , Known "Imported1.hs" 21 "Function"
              )
            , ( "import #3"
              , "foo2"
              , Known "Imported2.hs" 18 "Function"
              )
            , ( "import #4"
              , "bar2"
              , Known "Imported2.hs" 21 "Function"
              )
            , ( "local def in presence of wildcard import"
              , "baz"
              , Known "ModuleWithMultilineImportList.hs" 26 "Function"
              )
            ]
      , withDirAndFile (Directory "0001module_with_imports") "ModuleWithQualifiedImport.hs" $
          group "qualified import with alias"
            [ ("Imp.foo"
              , "Imp.foo"
              , Known "Imported1.hs" 18 "Function"
              )
            , ("Imp.bar"
              , "Imp.bar"
              , Known "Imported1.hs" 21 "Function"
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
      , withDirAndFile (Directory "0001module_with_imports") "ModuleWithQualifiedImportNoAlias.hs" $
          group "qualified import without alias"
            [ ("Imported1.foo"
              , "Imported1.foo"
              , Known "Imported1.hs" 18 "Function"
              )
            , ("Imported1.bar"
              , "Imported1.bar"
              , Known "Imported1.hs" 21 "Function"
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
      , withDirAndFile (Directory "0001module_with_imports") "ModuleWithImportsAndHiding.hs" $
          group "hiding"
            [ ("wildcard import #1"
              , "foo"
              , Known "Imported1.hs" 18 "Function"
              )
            , ("wildcard import #2"
              , "bar"
              , Known "Imported1.hs" 21 "Function"
              )
            , ( "import list - not hidden name"
              , "foo2"
              , Known "Imported2.hs" 18 "Function"
              )
            , ( "import list - hidden name"
              , "bar2"
              , NotFound
              )
            , ( "local def in presence of wildcard import"
              , "baz"
              , Known "ModuleWithImportsAndHiding.hs" 19 "Function"
              )
            ]
      , withDirAndFile (Directory "0001module_with_imports") "ModuleWithEmptyImportList.hs" $
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
              , Known "Imported2.hs" 18 "Function"
              )
            , ( "import list - not imported name"
              , "bar2"
              , NotFound
              )
            , ( "local def in presence of wildcard import"
              , "baz"
              , Known "ModuleWithEmptyImportList.hs" 19 "Function"
              )
            ]
      ]
  , GroupTest "export list"
      [ withDirAndFile (Directory "0002export_lists") "ModuleWithImportsThatHaveExportsList.hs" $
          group "vanilla export list"
            [ ( "import module with export list #1"
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
            , ( "import module with multiline export list #1"
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
      , withDirAndFile (Directory "0002export_lists") "ModuleWithImportsThatHaveExportsList.hs" $
          group "wildcard export list"
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
      , withDirAndFile (Directory "0002export_lists") "ModuleWithImportsThatHaveExportsList.hs" $
          group "explicit export list"
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
      , withDirAndFile (Directory "0002export_lists") "ModuleWithImportsThatHaveReexports.hs" $
          group "reexport"
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
      ]
  , withDirAndFile (Directory "0003module_header_detection") "ModuleWithCommentsResemblingModuleHeader.hs" $
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
  , withDirAndFile (Directory "0004typeclass_export_associated_types") "MainModule.hs" $
      group "typeclass export"
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
          , Known "ModuleWithTypeclass.hs" 31 "Constructor"
          )
        , ( "imported field accessor of public associated type"
          , "unIntBox"
          , Known "ModuleWithTypeclass.hs" 32 "Function"
          )
        , ( "associated private type family"
          , "PrivateFam"
          , Known "ModuleWithTypeclass.hs" 24 "Family"
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
  , withWorkingDir (Directory "0005import_cycle") $
      GroupTest "import cycle"
        [ GroupTest "wildcard export lists"
            [ withFile "A.hs" $
                group "A.hs"
                  [ ( "type defined locally in A"
                    , "TA"
                    , Known "A.hs" 19 "Type"
                    )
                  , ( "function defined locally in A"
                    , "f"
                    , Known "A.hs" 21 "Function"
                    )
                  , ( "type name imported into A"
                    , "TB"
                    , Known "B.hs" 20 "Type"
                    )
                  , ( "function imported into A"
                    , "g"
                    , Known "B.hs" 22 "Function"
                    )
                  ]
            , withFile "B.hs" $
                group "B.hs"
                  [ ( "type defined locally in B"
                    , "TB"
                    , Known "B.hs" 20 "Type"
                    )
                  , ( "function defined locally in B"
                    , "g"
                    , Known "B.hs" 22 "Function"
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
                    , Known "AWithExportList.hs" 19 "Type"
                    )
                  , ( "function defined locally in AWithExportList"
                    , "f"
                    , Known "AWithExportList.hs" 21 "Function"
                    )
                  , ( "type name imported into AWithExportList"
                    , "TB"
                    , Known "BWithExportList.hs" 20 "Type"
                    )
                  , ( "function imported into AWithExportList"
                    , "g"
                    , Known "BWithExportList.hs" 22 "Function"
                    )
                  ]
            , withFile "BWithExportList.hs" $
                group "BWithExportList.hs"
                  [ ( "type defined locally in BWithExportList"
                    , "TB"
                    , Known "BWithExportList.hs" 20 "Type"
                    )
                  , ( "function defined locally in BWithExportList"
                    , "g"
                    , Known "BWithExportList.hs" 22 "Function"
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
  , withWorkingDir (Directory "0006export_pattern_with_type_ghc8.0") $
      GroupTest "export pattern along with type"
        [ withFile file $
            group groupName
              [ ( "Exported type"
                , "FooTyp"
                , Known "ModuleThatExportsPattern.hs" 19 "Type"
                )
              , ( "Exported constructor 1"
                , "Foo"
                , Known "ModuleThatExportsPattern.hs" 20 "Constructor"
                )
              , ( "Exported constructor 2"
                , "Bar"
                , Known "ModuleThatExportsPattern.hs" 21 "Constructor"
                )
              , ( "Non-existent constructor"
                , "Baz"
                , NotFound
                )
              , ( "Exported pattern 1"
                , "Foo'"
                , Known "ModuleThatExportsPattern.hs" 23 "Pattern"
                )
              , ( "Exported pattern 2"
                , "Baz'"
                , Known "ModuleThatExportsPattern.hs" 24 "Pattern"
                )
              ]
        | (file, groupName) <-
            [ ("ModuleWithoutImportList.hs", "import without import list")
            , ("ModuleWithWildcardImportList.hs", "import with wildcard import list")
            , ("ModuleWithSpecificImportList.hs", "import with specific import list")
            ]
        ]
  , withWorkingDir (Directory "0007resolvable_import_cycle") $
      GroupTest "Resolvable import cycle"
        [ withFile "A.hs" $
          group "A imports B with import list"
            [ ( "Local function 1"
              , "foo"
              , Known "A.hs" 19 "Function"
              )
            , ( "Local function 2"
              , "bar"
              , Known "A.hs" 22 "Function"
              )
            , ( "Local function 3"
              , "baz"
              , Known "A.hs" 25 "Function"
              )
             , ( "Imported visible type 1"
              , "FooTyp"
              , Known "B.hs" 19 "Type"
              )
            , ( "Imported visible constructor"
              , "Foo"
              , Known "B.hs" 19 "Constructor"
              )
            , ( "Imported visible type 2"
              , "BarTyp"
              , Known "B.hs" 21 "Type"
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
              , Known "B.hs" 19 "Type"
              )
            , ( "Local constructor 1"
              , "Foo"
              , Known "B.hs" 19 "Constructor"
              )
            , ( "Local type 2"
              , "BarTyp"
              , Known "B.hs" 21 "Type"
              )
            , ( "Local constructor 2"
              , "Bar"
              , Known "B.hs" 21 "Constructor"
              )
            , ( "Local type 3"
              , "BazTyp"
              , Known "B.hs" 23 "Type"
              )
            , ( "Local constructor 3"
              , "Baz"
              , Known "B.hs" 23 "Constructor"
              )
            , ( "Imported function 1"
              , "foo"
              , Known "A.hs" 19 "Function"
              )
            , ( "Imported function 2"
              , "bar"
              , Known "A.hs" 22 "Function"
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
              , Known "C.hs" 19 "Function"
              )
            , ( "Local function 2"
              , "bar"
              , Known "C.hs" 22 "Function"
              )
            , ( "Local function 3"
              , "baz"
              , Known "C.hs" 25 "Function"
              )
             , ( "Imported visible type 1"
              , "FooTyp"
              , Known "D.hs" 19 "Type"
              )
            , ( "Imported visible constructor"
              , "Foo"
              , Known "D.hs" 19 "Constructor"
              )
            , ( "Imported visible type 2"
              , "BarTyp"
              , Known "D.hs" 21 "Type"
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
              , Known "D.hs" 19 "Type"
              )
            , ( "Local constructor 1"
              , "Foo"
              , Known "D.hs" 19 "Constructor"
              )
            , ( "Local type 2"
              , "BarTyp"
              , Known "D.hs" 21 "Type"
              )
            , ( "Local constructor 2"
              , "Bar"
              , Known "D.hs" 21 "Constructor"
              )
            , ( "Local type 3"
              , "BazTyp"
              , Known "D.hs" 23 "Type"
              )
            , ( "Local constructor 3"
              , "Baz"
              , Known "D.hs" 23 "Constructor"
              )
            , ( "Imported function 1"
              , "foo"
              , Known "C.hs" 19 "Function"
              )
            , ( "Imported function 2"
              , "bar"
              , Known "C.hs" 22 "Function"
              )
            , ( "Imported function 3"
              , "baz"
              , Known "C.hs" 25 "Function"
              )
            ]

        , withFile "E.hs" $
          group "E without export list imports F with import list"
            [ ( "Local function 1"
              , "foo"
              , Known "E.hs" 19 "Function"
              )
            , ( "Local function 2"
              , "bar"
              , Known "E.hs" 22 "Function"
              )
            , ( "Local function 3"
              , "baz"
              , Known "E.hs" 25 "Function"
              )
             , ( "Imported visible type 1"
              , "FooTyp"
              , Known "F.hs" 19 "Type"
              )
            , ( "Imported visible constructor"
              , "Foo"
              , Known "F.hs" 19 "Constructor"
              )
            , ( "Imported visible type 2"
              , "BarTyp"
              , Known "F.hs" 21 "Type"
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
              , Known "F.hs" 19 "Type"
              )
            , ( "Local constructor 1"
              , "Foo"
              , Known "F.hs" 19 "Constructor"
              )
            , ( "Local type 2"
              , "BarTyp"
              , Known "F.hs" 21 "Type"
              )
            , ( "Local constructor 2"
              , "Bar"
              , Known "F.hs" 21 "Constructor"
              )
            , ( "Local type 3"
              , "BazTyp"
              , Known "F.hs" 23 "Type"
              )
            , ( "Local constructor 3"
              , "Baz"
              , Known "F.hs" 23 "Constructor"
              )
            , ( "Imported function 1"
              , "foo"
              , Known "E.hs" 19 "Function"
              )
            , ( "Imported function 2"
              , "bar"
              , Known "E.hs" 22 "Function"
              )
            , ( "Imported function 3"
              , "baz"
              , Known "E.hs" 25 "Function"
              )
            ]

        , withFile "G.hs" $
          group "G without export list imports H with import list"
            [ ( "Local function 1"
              , "foo"
              , Known "G.hs" 19 "Function"
              )
            , ( "Local function 2"
              , "bar"
              , Known "G.hs" 22 "Function"
              )
            , ( "Local function 3"
              , "baz"
              , Known "G.hs" 25 "Function"
              )
             , ( "Imported visible type 1"
              , "FooTyp"
              , Known "H.hs" 19 "Type"
              )
            , ( "Imported visible constructor 1"
              , "Foo"
              , Known "H.hs" 19 "Constructor"
              )
            , ( "Imported visible type 2"
              , "BarTyp"
              , Known "H.hs" 21 "Type"
              )
            , ( "Imported visible constructor 2"
              , "Bar"
              , Known "H.hs" 21 "Constructor"
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
              , Known "H.hs" 19 "Type"
              )
            , ( "Local constructor 1"
              , "Foo"
              , Known "H.hs" 19 "Constructor"
              )
            , ( "Local type 2"
              , "BarTyp"
              , Known "H.hs" 21 "Type"
              )
            , ( "Local constructor 2"
              , "Bar"
              , Known "H.hs" 21 "Constructor"
              )
            , ( "Local type 3"
              , "BazTyp"
              , Known "H.hs" 23 "Type"
              )
            , ( "Local constructor 3"
              , "Baz"
              , Known "H.hs" 23 "Constructor"
              )
            , ( "Imported function 1"
              , "foo"
              , Known "G.hs" 19 "Function"
              )
            , ( "Imported function 2"
              , "bar"
              , Known "G.hs" 22 "Function"
              )
            , ( "Not imported function"
              , "baz"
              , NotFound
              )
            ]
        ]
  , withDirAndFile (Directory "0008module_reexport") "ModuleWithImportsThatHaveModuleReexports.hs" $
      group "Module reexport"
        [ ( "Import non-exported & non-reexported name"
          , "baz"
          , NotFound
          )
        , ( "Import name exported via module that reexports itself"
          , "test"
          , Known "ModuleWithModuleReexport.hs" 20 "Function"
          )
        , ( "Name imported through module with vanilla module reexport #1"
          , "foo"
          , Known "Module1.hs" 17 "Function"
          )
        , ( "Name imported through module with vanilla module reexport #2"
          , "bar"
          , Known "Module1.hs" 21 "Function"
          )
        , ( "Name imported through module reexporting with alias #1"
          , "foo2"
          , Known "Module2.hs" 18 "Function"
          )
        , ( "Name imported through module reexporting with alias #2"
          , "bar2"
          , Known "Module2.hs" 23 "Function"
          )
        , ( "Private non-exported name"
          , "baz2"
          , NotFound
          )
        , ( "Name imported through qualiifed module reexporting #1"
          , "foo3"
          , NotFound
          )
        , ( "Name imported through qualiifed module reexporting #2"
          , "bar3"
          , NotFound
          )
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
    makeTestTree pool (GroupTest name xs) = testGroup name $ map (makeTestTree pool) xs
    makeTestTree pool (AtomicTest serverTest) = makeTest pool serverTest

    makeTest :: IO PortPool -> ServerTest -> TestTree
    makeTest pool test@ServerTest{stWorkingDirectory} =
      withResource (connect pool (unDirectory stWorkingDirectory) mempty) closeConnection $ \getConn ->
        mkFindSymbolTest getConn test

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

connect :: IO PortPool -> PathFragment -> Set FullPath -> IO ServerConnection
connect pool sourceDir dirTree =
  -- Try to connect to a server. If attempt succeeds then server is running
  -- and there's no need to run server ourselves.
  tryConnect defaultPort >>= either (const startLocalServer) (pure . ExistingServer)
  where
    startLocalServer :: IO ServerConnection
    startLocalServer = do
      pool' <- pool
      withPort pool' $ \port -> do
        conf <- mkTestsConfig sourceDir dirTree
        serv <- runExceptT $ mkLogCollectingServer conf port
        case serv of
          Left err -> throwIO $ ErrorCall $ displayDocString $
            "Failed to start local server:" <> PP.line <> pretty err
          Right serv' -> do
            waitUntilStart serv'
            conn <- tryConnect port
            case conn of
              Left err    -> reportErr $
                "Failed to connect to locally started server\n" ++ show err
              Right conn' ->
                pure $ LocalServer serv' conn'

    tryConnect :: PortNumber -> IO (Either IOException TCP)
    tryConnect port =
      (Right <$> tcpClient "localhost" port) `catch` (pure . Left)

mkFindSymbolTest
  :: IO ServerConnection
  -> ServerTest
  -> TestTree
mkFindSymbolTest getConn ServerTest{stTestName, stWorkingDirectory, stFile, stSymbol, stExpectedResponse} =
  testCase stTestName $ do
    conn <- getConn
    let path = pathFragmentToUTF8 $ testDataDir </> unDirectory stWorkingDirectory </> stFile
    r    <- call conn "haskell-tags-server" "find" [ BinaryTerm path
                                                   , BinaryTerm stSymbol
                                                   ]
    logs <- case conn of
              ExistingServer _   -> pure mempty
              LocalServer serv _ -> do
                logs <- getLogs serv
                pure $ "Logs:" <> PP.line <> PP.indent 2 (PP.vcat logs)
    case r of
      Left err  ->
        assertFailure $ displayDocString $ ppShow err <> PP.line <> logs
      Right res -> do
        actual <- relativizeFilepaths res
        let msg = docFromString $ "expected: " ++ show expected ++ "\n but got: " ++ show actual
        if responseType actual == responseType expected
        then
          unless (actual == expected) $
            assertFailure $ displayDocString $ msg <> PP.line <> ppShow logs
        else
          assertFailure $ displayDocString $
            case extractResponseError actual of
              Nothing  ->
                msg <> PP.line <> logs
              Just msg ->
                "Error from server:" <> PP.line <> PP.nest 2 (docFromByteString msg) <> PP.line <> logs
        where
          expected = responseToTerm stExpectedResponse

responseType :: Term -> Maybe String
responseType (TupleTerm (AtomTerm x : _)) = Just x
responseType _                            = Nothing

extractResponseError :: Term -> Maybe UTF8.ByteString
extractResponseError (TupleTerm [AtomTerm "error", BinaryTerm msg]) = Just msg
extractResponseError _                                              = Nothing

relativizeFilepaths :: forall m. MonadBase IO m => Term -> m Term
relativizeFilepaths term =
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

