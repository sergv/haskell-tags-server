----------------------------------------------------------------------------
-- |
-- Module      :  ServerTests
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ServerTests (tests) where

import Control.Concurrent
import Control.Exception (IOException, throwIO, ErrorCall(..))
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.ErrorExcept
import Control.Monad.Except
import Control.Monad.Trans.Control

import qualified Data.ByteString.Lazy.Char8 as CL8
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
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

import Control.Monad.Filesystem
import Data.ErrorMessage
import Data.Path
import Data.Symbols
import Haskell.Language.Server.BERT
import Haskell.Language.Server.Tags
import Haskell.Language.Server.Tags.Types (NameResolutionStrictness(..))
import PortPool

import ServerTests.LogCollectingServer

-- | Directory with test projects.
testDataDir :: PathFragment
testDataDir = "test-data"

s :: Text -> UnqualifiedSymbolName
s str = fromMaybe err . mkUnqualifiedSymbolName . mkSymbolName $ str
  where
    err = error $ "Invalid unqualified symbol: " ++ T.unpack str

known :: Text -> PathFragment -> Int -> SymbolType -> ServerResponse
known sym file line typ = Known (s sym) file line typ

mkTestsConfig
  :: (MonadBase IO m, MonadError ErrorMessage m)
  => NameResolutionStrictness
  -> WorkingDirectory
  -> m (SearchCfg, TagsServerConf)
mkTestsConfig tsconfNameResolution srcDir = do
  searchDirs <- case srcDir of
    ShallowDir   dir -> do
      dir' <- mkFullPath $ testDataDir </> dir
      pure mempty { scShallowPaths = S.singleton dir' }
    RecursiveDir dir -> do
      dir' <- mkFullPath $ testDataDir </> dir
      pure mempty { scRecursivePaths = S.singleton dir' }
    RecursiveWithIgnored dir globs -> do
      dir' <- mkFullPath $ testDataDir </> dir
      pure mempty
        { scRecursivePaths = S.singleton dir'
        , scIgnoredGlobs   = S.fromList globs
        }
  let conf = defaultTagsServerConf
        { tsconfEagerTagging   = False
        , tsconfNameResolution
        }
  pure (searchDirs, conf)

type SymbolType = String

-- | Type that encodes all possible BERT responses.
data ServerResponse =
    Known UnqualifiedSymbolName PathFragment Int SymbolType
  | Ambiguous [(UnqualifiedSymbolName, PathFragment, Int, SymbolType)]
  | NotFound
  deriving (Eq, Ord, Show)

responseToTerm :: ServerResponse -> Term
responseToTerm resp =
  case resp of
    Known symName filename line typ ->
      TupleTerm [AtomTerm "loc_known", mkSymbol (symName, filename, line, typ)]
    Ambiguous xs ->
      TupleTerm [AtomTerm "loc_ambiguous", ListTerm (map mkSymbol xs)]
    NotFound ->
      AtomTerm "not_found"
  where
    mkSymbol :: (UnqualifiedSymbolName, PathFragment, Int, SymbolType) -> Term
    mkSymbol (symName, filename, line, typ) = TupleTerm
      [ BinaryTerm $ CL8.fromStrict $ TE.encodeUtf8 $ unqualSymNameText symName
      , BinaryTerm $ pathFragmentToUTF8 filename
      , IntTerm line
      , AtomTerm typ
      ]

data WorkingDirectory =
    ShallowDir PathFragment
  | RecursiveDir PathFragment
  | RecursiveWithIgnored PathFragment [Text]
  deriving (Eq, Ord, Show)

data ServerTest = ServerTest
  { stTestName                 :: String
  , stNameResolutionStrictness :: NameResolutionStrictness
  , stWorkingDirectory         :: WorkingDirectory
  , stFile                     :: PathFragment
  , stSymbol                   :: UTF8.ByteString
  , stExpectedResponse         :: ServerResponse
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
  :: NameResolutionStrictness
  -> WorkingDirectory     -- ^ Working directory under testDataDir
  -> TestSet
       ( String          -- ^ Test name
       , PathFragment    -- ^ Filepath within the working directory
       , UTF8.ByteString -- ^ Symbol to search for
       , ServerResponse  -- ^ Expected response
       )
  -> TestSet ServerTest
withWorkingDir mode dir =
  fmap $ \(name, file, sym, response) -> ServerTest
    { stTestName                 = name
    , stNameResolutionStrictness = mode
    , stWorkingDirectory         = dir
    , stFile                     = file
    , stSymbol                   = sym
    , stExpectedResponse         = response
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
  :: NameResolutionStrictness
  -> WorkingDirectory    -- ^ Working directory under testDataDir
  -> PathFragment        -- ^ Filepath within the working directory
  -> TestSet
       ( String          -- ^ Test name
       , UTF8.ByteString -- ^ Symbol to search for
       , ServerResponse  -- ^ Expected response
       )
  -> TestSet ServerTest
withDirAndFile mode dir file =
  withWorkingDir mode dir . fmap (\(name, sym, response) -> (name, file, sym, response))

testData :: TestSet ServerTest
testData = GroupTest "server tests"
  [ AtomicTest ServerTest
      { stTestName                 = "single module"
      , stNameResolutionStrictness = NameResolutionStrict
      , stWorkingDirectory         = ShallowDir "0000single_module"
      , stFile                     = "SingleModule.hs"
      , stSymbol                   = "foo"
      , stExpectedResponse         = known "foo" "SingleModule.hs" 11 "Function"
      }
  , GroupTest "imports"
      [ withDirAndFile NameResolutionStrict (ShallowDir "0001module_with_imports") "ModuleWithImports.hs" $
          GroupTest "vanilla"
            [ GroupTest "wildcard import" $ map mkQualUnqualTest
                [ ("name #1"
                  , "foo"
                  , Just "Imported1.foo"
                  , known "foo" "Imported1.hs" 13 "Function"
                  )
                , ("name #2"
                  , "bar"
                  , Just "Imported1.bar"
                  , known "bar" "Imported1.hs" 16 "Function"
                  )
                , ("operator"
                  , "$$"
                  , Just "Imported1.$$"
                  , known "$$" "Imported1.hs" 19 "Operator"
                  )
                , ("type name"
                  , ":$$:"
                  , Just "Imported1.:$$:"
                  , known ":$$:" "Imported1.hs" 22 "Type"
                  )
                , ("constructor name"
                  , ":$$$:"
                  , Just "Imported1.:$$$:"
                  , known ":$$$:" "Imported1.hs" 23 "Constructor"
                  )
                , ( "local def"
                  , "baz"
                  , Nothing
                  , known "baz" "ModuleWithImports.hs" 16 "Function"
                  )
                ]
            , GroupTest "explicit import list" $ map mkQualUnqualTest
                [ ( "imported name"
                  , "foo2"
                  , Just "Imported2.foo2"
                  , known "foo2" "Imported2.hs" 13 "Function"
                  )
                , ( "not imported name"
                  , "bar2"
                  , Just "Imported2.bar2"
                  , NotFound
                  )
                , ( "imported operator"
                  , "$$*"
                  , Just "Imported2.$$*"
                  , known "$$*" "Imported2.hs" 19 "Operator"
                  )
                , ( "imported type name"
                  , ":$$*:"
                  , Just "Imported2.:$$*:"
                  , known ":$$*:" "Imported2.hs" 22 "Type"
                  )
                , ( "imported constructor name"
                  , ":$$$*:"
                  , Just "Imported2.:$$$*:"
                  , known ":$$$*:" "Imported2.hs" 23 "Constructor"
                  )
                ]
            ]
      -- test extraction and subsequent parsing of multiline import list
      , withDirAndFile NameResolutionStrict (ShallowDir "0001module_with_imports") "ModuleWithMultilineImportList.hs" $
          group "multiline import list"
            [ ("import #1"
              , "foo"
              , known "foo" "Imported1.hs" 13 "Function"
              )
            , ("import #2"
              , "bar"
              , known "bar" "Imported1.hs" 16 "Function"
              )
            , ( "import #3"
              , "foo2"
              , known "foo2" "Imported2.hs" 13 "Function"
              )
            , ( "import #4"
              , "bar2"
              , known "bar2" "Imported2.hs" 16 "Function"
              )
            , ( "local def in presence of wildcard import"
              , "baz"
              , known "baz" "ModuleWithMultilineImportList.hs" 21 "Function"
              )
            ]
      , withDirAndFile NameResolutionStrict (ShallowDir "0001module_with_imports") "ModuleWithQualifiedImport.hs" $
          group "qualified import with alias"
            [ ("Imp.foo"
              , "Imp.foo"
              , known "foo" "Imported1.hs" 13 "Function"
              )
            , ("Imp.bar"
              , "Imp.bar"
              , known "bar" "Imported1.hs" 16 "Function"
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
              , known "baz" "ModuleWithQualifiedImport.hs" 13 "Function"
              )
            ]
      , withDirAndFile NameResolutionStrict (ShallowDir "0001module_with_imports") "ModuleWithQualifiedImportNoAlias.hs" $
          group "qualified import without alias"
            [ ("Imported1.foo"
              , "Imported1.foo"
              , known "foo" "Imported1.hs" 13 "Function"
              )
            , ("Imported1.bar"
              , "Imported1.bar"
              , known "bar" "Imported1.hs" 16 "Function"
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
              , known "baz" "ModuleWithQualifiedImportNoAlias.hs" 13 "Function"
              )
            ]
      , withDirAndFile NameResolutionStrict (ShallowDir "0001module_with_imports") "ModuleWithImportsAndHiding.hs" $
          group "hiding"
            [ ("wildcard import #1"
              , "foo"
              , known "foo" "Imported1.hs" 13 "Function"
              )
            , ("wildcard import #2"
              , "bar"
              , known "bar" "Imported1.hs" 16 "Function"
              )
            , ( "import list - not hidden name"
              , "foo2"
              , known "foo2" "Imported2.hs" 13 "Function"
              )
            , ( "import list - hidden name"
              , "bar2"
              , NotFound
              )
            , ( "local def in presence of wildcard import"
              , "baz"
              , known "baz" "ModuleWithImportsAndHiding.hs" 14 "Function"
              )
            ]
      , withDirAndFile NameResolutionStrict (ShallowDir "0001module_with_imports") "ModuleWithEmptyImportList.hs" $
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
              , known "foo2" "Imported2.hs" 13 "Function"
              )
            , ( "import list - not imported name"
              , "bar2"
              , NotFound
              )
            , ( "local def in presence of wildcard import"
              , "baz"
              , known "baz" "ModuleWithEmptyImportList.hs" 14 "Function"
              )
            ]
      ]
  , GroupTest "export list"
      [ withDirAndFile NameResolutionStrict (ShallowDir "0002export_lists") "ModuleWithImportsThatHaveExportsList.hs" $
          group "vanilla export list"
            [ ( "import module with export list #1"
              , "foo"
              , known "foo" "ModuleWithExportList.hs" 11 "Function"
              )
            , ( "import module with export list #2"
              , "bar"
              , known "bar" "ModuleWithExportList.hs" 14 "Function"
              )
            , ( "import module with export list #3"
              , "baz"
              , NotFound
              )
            , ( "import module with multiline export list #1"
              , "foo2"
              , known "foo2" "ModuleWithMultilineExportList.hs" 15 "Function"
              )
            , ( "import module with multiline export list #2"
              , "bar2"
              , known "bar2" "ModuleWithMultilineExportList.hs" 18 "Function"
              )
            , ( "import module with multiline export list #3"
              , "baz2"
              , NotFound
              )
            ]
      , withDirAndFile NameResolutionStrict (ShallowDir "0002export_lists") "ModuleWithImportsThatHaveExportsList.hs" $
          group "wildcard export list"
            [ ( "import exported name"
              , "Foo"
              , known "Foo" "ModuleWithWildcardExport.hs" 14 "Type"
              )
            , ( "import wildcard-exported name #1"
              , "Bar"
              , known "Bar" "ModuleWithWildcardExport.hs" 14 "Constructor"
              )
            , ( "import wildcard-exported name #2"
              , "Baz"
              , known "Baz" "ModuleWithWildcardExport.hs" 15 "Constructor"
              )
            , ( "import wildcard-exported name #3"
              , "getBar"
              , known "getBar" "ModuleWithWildcardExport.hs" 14 "Function"
              )
            , ( "import wildcard-exported name #4"
              , "getBaz"
              , known "getBaz" "ModuleWithWildcardExport.hs" 15 "Function"
              )
            ]
      , withDirAndFile NameResolutionStrict (ShallowDir "0002export_lists") "ModuleWithImportsThatHaveExportsList.hs" $
          group "explicit export list"
            [ ( "import exported name"
              , "Foo2"
              , known "Foo2" "ModuleWithExplicitExport.hs" 14 "Type"
              )
            , ( "import explicitly exported name #1"
              , "Bar2"
              , known "Bar2" "ModuleWithExplicitExport.hs" 14 "Constructor"
              )
            , ( "import explicitly exported name #2"
              , "Baz2"
              , NotFound
              )
            , ( "import explicitly exported name #3"
              , "getBar2"
              , known "getBar2" "ModuleWithExplicitExport.hs" 14 "Function"
              )
            , ( "import explicitly exported name #4"
              , "getBaz2"
              , NotFound
              )
            ]
      , withDirAndFile NameResolutionStrict (ShallowDir "0002export_lists") "ModuleWithImportsThatHaveReexports.hs" $
          group "reexport"
            [ ( "import non-exported name"
              , "baz"
              , NotFound
              )
            , ( "import re-exported name without qualification #1"
              , "foo"
              , known "foo" "ModuleWithExportList.hs" 11 "Function"
              )
            , ( "import re-exported name without qualification #2"
              , "bar"
              , known "bar" "ModuleWithExportList.hs" 14 "Function"
              )
            , ( "import re-exported name without qualification #3"
              , "foo2"
              , known "foo2" "ModuleWithMultilineExportList.hs" 15 "Function"
              )
            , ( "import re-exported name with qualification"
              , "bar2"
              , known "bar2" "ModuleWithMultilineExportList.hs" 18 "Function"
              )
            ]
      ]
  , withDirAndFile NameResolutionStrict (ShallowDir "0003module_header_detection") "ModuleWithCommentsResemblingModuleHeader.hs" $
      group "module header detection"
        [ ( "name defined locally"
          , "foo"
          , known "foo" "ModuleWithCommentsResemblingModuleHeader.hs" 11 "Function"
          )
        , ( "imported name"
          , "bar"
          , known "bar" "EmptyModule.hs" 3 "Function"
          )
        ]
  , withDirAndFile NameResolutionStrict (ShallowDir "0004typeclass_export_associated_types") "MainModule.hs" $
      group "typeclass export"
        [ ( "name defined locally"
          , "foo"
          , known "foo" "MainModule.hs" 14 "Function"
          )
        , ( "typeclass member function"
          , "wrap"
          , known "wrap" "ModuleWithTypeclass.hs" 20 "Function"
          )
        , ( "associated public type family"
          , "TestFam"
          , known "TestFam" "ModuleWithTypeclass.hs" 18 "Family"
          )
        , ( "imported constructor of public associated type"
          , "IntBox"
          , known "IntBox" "ModuleWithTypeclass.hs" 26 "Constructor"
          )
        , ( "imported field accessor of public associated type"
          , "unIntBox"
          , known "unIntBox" "ModuleWithTypeclass.hs" 27 "Function"
          )
        , ( "associated private type family"
          , "PrivateFam"
          , known "PrivateFam" "ModuleWithTypeclass.hs" 19 "Family"
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
  , withWorkingDir NameResolutionStrict (ShallowDir "0005import_cycle") $
      GroupTest "import cycle"
        [ GroupTest "wildcard export lists"
            [ withFile "A.hs" $
                group "A.hs"
                  [ ( "type defined locally in A"
                    , "TA"
                    , known "TA" "A.hs" 14 "Type"
                    )
                  , ( "function defined locally in A"
                    , "f"
                    , known "f" "A.hs" 16 "Function"
                    )
                  , ( "type name imported into A"
                    , "TB"
                    , known "TB" "B.hs" 15 "Type"
                    )
                  , ( "function imported into A"
                    , "g"
                    , known "g" "B.hs" 17 "Function"
                    )
                  ]
            , withFile "B.hs" $
                group "B.hs"
                  [ ( "type defined locally in B"
                    , "TB"
                    , known "TB" "B.hs" 15 "Type"
                    )
                  , ( "function defined locally in B"
                    , "g"
                    , known "g" "B.hs" 17 "Function"
                    )
                  , ( "type name imported into B"
                    , "TA"
                    , known "TA" "A.hs-boot" 4 "Type"
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
                    , known "TA" "AWithExportList.hs" 14 "Type"
                    )
                  , ( "function defined locally in AWithExportList"
                    , "f"
                    , known "f" "AWithExportList.hs" 16 "Function"
                    )
                  , ( "type name imported into AWithExportList"
                    , "TB"
                    , known "TB" "BWithExportList.hs" 15 "Type"
                    )
                  , ( "function imported into AWithExportList"
                    , "g"
                    , known "g" "BWithExportList.hs" 17 "Function"
                    )
                  ]
            , withFile "BWithExportList.hs" $
                group "BWithExportList.hs"
                  [ ( "type defined locally in BWithExportList"
                    , "TB"
                    , known "TB" "BWithExportList.hs" 15 "Type"
                    )
                  , ( "function defined locally in BWithExportList"
                    , "g"
                    , known "g" "BWithExportList.hs" 17 "Function"
                    )
                  , ( "type name imported into BWithExportList"
                    , "TA"
                    , known "TA" "AWithExportList.hs-boot" 4 "Type"
                    )
                  , ( "function imported into BWithExportList"
                    , "f"
                    , NotFound
                    )
                  ]
            ]
        ]
  , withWorkingDir NameResolutionStrict (ShallowDir "0006export_pattern_with_type_ghc8.0") $
      GroupTest "export pattern along with type"
        [ withFile file $
            group groupName
              [ ( "Exported type"
                , "FooTyp"
                , known "FooTyp" "ModuleThatExportsPattern.hs" 14 "Type"
                )
              , ( "Exported constructor 1"
                , "Foo"
                , known "Foo" "ModuleThatExportsPattern.hs" 15 "Constructor"
                )
              , ( "Exported constructor 2"
                , "Bar"
                , known "Bar" "ModuleThatExportsPattern.hs" 16 "Constructor"
                )
              , ( "Non-existent constructor"
                , "Baz"
                , NotFound
                )
              , ( "Exported pattern 1"
                , "Foo'"
                , known "Foo'" "ModuleThatExportsPattern.hs" 18 "Pattern"
                )
              , ( "Exported pattern 2"
                , "Baz'"
                , known "Baz'" "ModuleThatExportsPattern.hs" 19 "Pattern"
                )
              ]
        | (file, groupName) <-
            [ ("ModuleWithoutImportList.hs", "import without import list")
            , ("ModuleWithWildcardImportList.hs", "import with wildcard import list")
            , ("ModuleWithSpecificImportList.hs", "import with specific import list")
            ]
        ]
  , withWorkingDir NameResolutionStrict (ShallowDir "0007resolvable_import_cycle") $
      GroupTest "Resolvable import cycle"
        [ withFile "A.hs" $
          group "A imports B with import list"
            [ ( "Local function 1"
              , "foo"
              , known "foo" "A.hs" 14 "Function"
              )
            , ( "Local function 2"
              , "bar"
              , known "bar" "A.hs" 17 "Function"
              )
            , ( "Local function 3"
              , "baz"
              , known "baz" "A.hs" 20 "Function"
              )
             , ( "Imported visible type 1"
              , "FooTyp"
              , known "FooTyp" "B.hs" 14 "Type"
              )
            , ( "Imported visible constructor"
              , "Foo"
              , known "Foo" "B.hs" 14 "Constructor"
              )
            , ( "Imported visible type 2"
              , "BarTyp"
              , known "BarTyp" "B.hs" 16 "Type"
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
              , known "FooTyp" "B.hs" 14 "Type"
              )
            , ( "Local constructor 1"
              , "Foo"
              , known "Foo" "B.hs" 14 "Constructor"
              )
            , ( "Local type 2"
              , "BarTyp"
              , known "BarTyp" "B.hs" 16 "Type"
              )
            , ( "Local constructor 2"
              , "Bar"
              , known "Bar" "B.hs" 16 "Constructor"
              )
            , ( "Local type 3"
              , "BazTyp"
              , known "BazTyp" "B.hs" 18 "Type"
              )
            , ( "Local constructor 3"
              , "Baz"
              , known "Baz" "B.hs" 18 "Constructor"
              )
            , ( "Imported function 1"
              , "foo"
              , known "foo" "A.hs" 14 "Function"
              )
            , ( "Imported function 2"
              , "bar"
              , known "bar" "A.hs" 17 "Function"
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
              , known "foo" "C.hs" 14 "Function"
              )
            , ( "Local function 2"
              , "bar"
              , known "bar" "C.hs" 17 "Function"
              )
            , ( "Local function 3"
              , "baz"
              , known "baz" "C.hs" 20 "Function"
              )
             , ( "Imported visible type 1"
              , "FooTyp"
              , known "FooTyp" "D.hs" 14 "Type"
              )
            , ( "Imported visible constructor"
              , "Foo"
              , known "Foo" "D.hs" 14 "Constructor"
              )
            , ( "Imported visible type 2"
              , "BarTyp"
              , known "BarTyp" "D.hs" 16 "Type"
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
              , known "FooTyp" "D.hs" 14 "Type"
              )
            , ( "Local constructor 1"
              , "Foo"
              , known "Foo" "D.hs" 14 "Constructor"
              )
            , ( "Local type 2"
              , "BarTyp"
              , known "BarTyp" "D.hs" 16 "Type"
              )
            , ( "Local constructor 2"
              , "Bar"
              , known "Bar" "D.hs" 16 "Constructor"
              )
            , ( "Local type 3"
              , "BazTyp"
              , known "BazTyp" "D.hs" 18 "Type"
              )
            , ( "Local constructor 3"
              , "Baz"
              , known "Baz" "D.hs" 18 "Constructor"
              )
            , ( "Imported function 1"
              , "foo"
              , known "foo" "C.hs" 14 "Function"
              )
            , ( "Imported function 2"
              , "bar"
              , known "bar" "C.hs" 17 "Function"
              )
            , ( "Imported function 3"
              , "baz"
              , known "baz" "C.hs" 20 "Function"
              )
            ]

        , withFile "E.hs" $
          group "E without export list imports F with import list"
            [ ( "Local function 1"
              , "foo"
              , known "foo" "E.hs" 14 "Function"
              )
            , ( "Local function 2"
              , "bar"
              , known "bar" "E.hs" 17 "Function"
              )
            , ( "Local function 3"
              , "baz"
              , known "baz" "E.hs" 20 "Function"
              )
             , ( "Imported visible type 1"
              , "FooTyp"
              , known "FooTyp" "F.hs" 14 "Type"
              )
            , ( "Imported visible constructor"
              , "Foo"
              , known "Foo" "F.hs" 14 "Constructor"
              )
            , ( "Imported visible type 2"
              , "BarTyp"
              , known "BarTyp" "F.hs" 16 "Type"
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
              , known "FooTyp" "F.hs" 14 "Type"
              )
            , ( "Local constructor 1"
              , "Foo"
              , known "Foo" "F.hs" 14 "Constructor"
              )
            , ( "Local type 2"
              , "BarTyp"
              , known "BarTyp" "F.hs" 16 "Type"
              )
            , ( "Local constructor 2"
              , "Bar"
              , known "Bar" "F.hs" 16 "Constructor"
              )
            , ( "Local type 3"
              , "BazTyp"
              , known "BazTyp" "F.hs" 18 "Type"
              )
            , ( "Local constructor 3"
              , "Baz"
              , known "Baz" "F.hs" 18 "Constructor"
              )
            , ( "Imported function 1"
              , "foo"
              , known "foo" "E.hs" 14 "Function"
              )
            , ( "Imported function 2"
              , "bar"
              , known "bar" "E.hs" 17 "Function"
              )
            , ( "Imported function 3"
              , "baz"
              , known "baz" "E.hs" 20 "Function"
              )
            ]

        , withFile "G.hs" $
          group "G without export list imports H with import list"
            [ ( "Local function 1"
              , "foo"
              , known "foo" "G.hs" 14 "Function"
              )
            , ( "Local function 2"
              , "bar"
              , known "bar" "G.hs" 17 "Function"
              )
            , ( "Local function 3"
              , "baz"
              , known "baz" "G.hs" 20 "Function"
              )
             , ( "Imported visible type 1"
              , "FooTyp"
              , known "FooTyp" "H.hs" 14 "Type"
              )
            , ( "Imported visible constructor 1"
              , "Foo"
              , known "Foo" "H.hs" 14 "Constructor"
              )
            , ( "Imported visible type 2"
              , "BarTyp"
              , known "BarTyp" "H.hs" 16 "Type"
              )
            , ( "Imported visible constructor 2"
              , "Bar"
              , known "Bar" "H.hs" 16 "Constructor"
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
              , known "FooTyp" "H.hs" 14 "Type"
              )
            , ( "Local constructor 1"
              , "Foo"
              , known "Foo" "H.hs" 14 "Constructor"
              )
            , ( "Local type 2"
              , "BarTyp"
              , known "BarTyp" "H.hs" 16 "Type"
              )
            , ( "Local constructor 2"
              , "Bar"
              , known "Bar" "H.hs" 16 "Constructor"
              )
            , ( "Local type 3"
              , "BazTyp"
              , known "BazTyp" "H.hs" 18 "Type"
              )
            , ( "Local constructor 3"
              , "Baz"
              , known "Baz" "H.hs" 18 "Constructor"
              )
            , ( "Imported function 1"
              , "foo"
              , known "foo" "G.hs" 14 "Function"
              )
            , ( "Imported function 2"
              , "bar"
              , known "bar" "G.hs" 17 "Function"
              )
            , ( "Not imported function"
              , "baz"
              , NotFound
              )
            ]
        ]
  , withDirAndFile NameResolutionStrict (ShallowDir "0008module_reexport") "ModuleWithImportsThatHaveModuleReexports.hs" $
      group "Module reexport"
        [ ( "Import non-exported & non-reexported name"
          , "baz"
          , NotFound
          )
        , ( "Import name exported via module that reexports itself"
          , "test"
          , known "test" "ModuleWithModuleReexport.hs" 15 "Function"
          )
        , ( "Name imported through module with vanilla module reexport #1"
          , "foo"
          , known "foo" "Module1.hs" 12 "Function"
          )
        , ( "Name imported through module with vanilla module reexport #2"
          , "bar"
          , known "bar" "Module1.hs" 16 "Function"
          )
        , ( "Name imported through module reexporting with alias #1"
          , "foo2"
          , known "foo2" "Module2.hs" 13 "Function"
          )
        , ( "Name imported through module reexporting with alias #2"
          , "bar2"
          , known "bar2" "Module2.hs" 18 "Function"
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
  , withDirAndFile NameResolutionStrict (ShallowDir "0009empty_export_list_is_wildcard") "MainModule.hs" $
      group "Empty export list is treated as export all wildcard"
        [ ( "Non-exported name #1"
          , "Foo"
          , known "Foo" "ModuleWithEmptyExportList.hs" 11 "Type"
          )
        , ( "Non-exported name #2"
          , "Bar"
          , known "Bar" "ModuleWithEmptyExportList.hs" 12 "Constructor"
          )
        , ( "Non-exported name #3"
          , "Baz"
          , known "Baz" "ModuleWithEmptyExportList.hs" 15 "Type"
          )
        , ( "Non-exported name #4"
          , "Quux"
          , known "Quux" "ModuleWithEmptyExportList.hs" 16 "Constructor"
          )
        , ( "Non-exported name #5"
          , "frob"
          , known "frob" "ModuleWithEmptyExportList.hs" 20 "Function"
          )
        , ( "Non-existing name"
          , "frobnicate"
          , NotFound
          )
        ]
  , withWorkingDir NameResolutionLax (ShallowDir "0010exported_name_defined_via_macro") $
    GroupTest "Import of module that defines some entities via macro"
      [ withFile groupModule $
        group groupName
          [ ( "Type defined via macro #1"
            , "ViaMacroWithWildcardChildren"
            , known "ViaMacroWithWildcardChildren" "Definitions.hs" 12 "Type"
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
            , known "ViaMacroWithExplicitChildren" "Definitions.hs" 13 "Type"
            )
          , ( "Unexported constructor defined via macro #2.1"
            , "MWEC1"
            , NotFound
            )
          , ( "Explicitly exported constructor defined via macro #2.2"
            , "MWEC2"
            , known "MWEC2" "Definitions.hs" 13 "Constructor"
            )

          , ( "Type defined via macro #3"
            , "ViaMacroNoChildren"
            , known "ViaMacroNoChildren" "Definitions.hs" 14 "Type"
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
            , known "viaMacro" "Definitions.hs" 15 "Function"
            )

          , ( "Vanilla type #1"
            , "ViaDefWithWildcardChildren"
            , known "ViaDefWithWildcardChildren" "Definitions.hs" 34 "Type"
            )
          , ( "Vanilla wildcard-exported constructor #1"
            , "DWWC1"
            , known "DWWC1" "Definitions.hs" 35 "Constructor"
            )
          , ( "Vanilla wildcard-exported constructor #2"
            , "DWWC2"
            , known "DWWC2" "Definitions.hs" 36 "Constructor"
            )

          , ( "Vanilla type #2"
            , "ViaDefWithExplicitChildren"
            , known "ViaDefWithExplicitChildren" "Definitions.hs" 39 "Type"
            )
          , ( "Vanilla unexported constructor #2.1"
            , "DWEC1"
            , NotFound
            )
          , ( "Vanilla explicitly exported constructor"
            , "DWEC2"
            , known "DWEC2" "Definitions.hs" 41 "Constructor"
            )

          , ( "Vanilla type #3"
            , "ViaDefNoChildren"
            , known "ViaDefNoChildren" "Definitions.hs" 44 "Type"
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
            , known "viaDef" "Definitions.hs" 51 "Function"
            )
          ]
      | (groupName, groupModule) <-
        [ ("Direct import", "ImportDirectly.hs")
        , ("Via reexport", "ImportViaReexport.hs")
        ]
      ]
  , withDirAndFile NameResolutionStrict (RecursiveDir "0011hide_constructor_named_as_type") "MainModule.hs" $
      group "When constructor has the same name as its type then only type will be found"
        [ ( "Same name - record"
          , "FooMatching"
          , known "FooMatching" "deps/DependencyMatchingConstructorsTypes.hs" 13 "Type"
          )
        , ( "Same name - newtype"
          , "BarMatching"
          , known "BarMatching" "deps/DependencyMatchingConstructorsTypes.hs" 15 "Type"
          )
        , ( "Same name - newtype accessor"
          , "unBarMatching"
          , known "unBarMatching" "deps/DependencyMatchingConstructorsTypes.hs" 16 "Function"
          )
        , ( "Same name - data with alternatives"
          , "BazMatching"
          , known "BazMatching" "deps/DependencyMatchingConstructorsTypes.hs" 18 "Type"
          )
        , ( "Same name - GADT with alternatives"
          , "QuuxMatching"
          , known "QuuxMatching" "deps/DependencyMatchingConstructorsTypes.hs" 22 "Type"
          )
        , ( "Different name - GADT with alternatives #1"
          , "QuuxInt"
          , known "QuuxInt" "deps/DependencyMatchingConstructorsTypes.hs" 24 "Constructor"
          )

        , ( "Shifted name - record"
          , "FooShifted"
          , Ambiguous
              [ (s "FooShifted", "deps/DependencyShiftedConstructorsTypes.hs", 14, "Type")
              , (s "FooShifted", "deps/DependencyShiftedConstructorsTypes.hs", 17, "Constructor")
              ]
          )
        , ( "Shifted name - newtype"
          , "BarShifted"
          , Ambiguous
              [ (s "BarShifted", "deps/DependencyShiftedConstructorsTypes.hs", 16, "Type")
              , (s "BarShifted", "deps/DependencyShiftedConstructorsTypes.hs", 21, "Constructor")
              ]
          )
        , ( "Shifted name - newtype accessor"
          , "unBarShifted"
          , known "unBarShifted" "deps/DependencyShiftedConstructorsTypes.hs" 17 "Function"
          )
        , ( "Shifted name - data with alternatives"
          , "BazShifted"
          , Ambiguous
              [ (s "BazShifted", "deps/DependencyShiftedConstructorsTypes.hs", 19, "Type")
              , (s "BazShifted", "deps/DependencyShiftedConstructorsTypes.hs", 24, "Constructor")
              ]
          )
        , ( "Shifted name - GADT with alternatives"
          , "QuuxShifted"
          , Ambiguous
              [ (s "QuuxShifted", "deps/DependencyShiftedConstructorsTypes.hs", 14, "Constructor")
              , (s "QuuxShifted", "deps/DependencyShiftedConstructorsTypes.hs", 23, "Type")
              ]
          )
        , ( "Different name - GADT with alternatives #2"
          , "QuuxDouble"
          , known "QuuxDouble" "deps/DependencyShiftedConstructorsTypes.hs" 25 "Constructor"
          )
        ]
  , withWorkingDir NameResolutionStrict (RecursiveDir "0012resolve_reexport_import_cycles") $
    GroupTest "Resolve import cycles caused by module that reexports stuff"
      [ withFile "import1NoListImport2WithListChildrenWildcardsReexportModule/ImportReexports.hs" $
        group "Import1 - NoList Import2 - WithListChildrenWildcards ReexportModule - ImportReexports"
          [ (CL8.unpack sym, sym, response)
          | (sym, response) <-
            [ ("FooA",       known "FooA"   "ANoExportList.hs" 15 "Type")
            , ("FooA1",      known "FooA1"  "ANoExportList.hs" 15 "Constructor")
            , ("fooA1",      known "fooA1"  "ANoExportList.hs" 16 "Function")
            , ("fooA2",      known "fooA2"  "ANoExportList.hs" 17 "Function")
            , ("BarA",       known "BarA"   "ANoExportList.hs" 20 "Type")
            , ("BarA1",      known "BarA1"  "ANoExportList.hs" 21 "Constructor")
            , ("unBarA",     known "unBarA" "ANoExportList.hs" 22 "Function")
            , ("BazAP",      known "BazAP"  "ANoExportList.hs" 24 "Pattern")
            , ("quuxA",      known "quuxA"  "ANoExportList.hs" 27 "Function")
            , ("FrobAP",     known "FrobAP" "ANoExportList.hs" 30 "Pattern")
            , ("QuuxA",      known "QuuxA"  "ANoExportList.hs" 33 "Type")
            , ("QuuxA1",     known "QuuxA1" "ANoExportList.hs" 34 "Constructor")
            , ("QuuxA2",     known "QuuxA2" "ANoExportList.hs" 35 "Constructor")
            , ("QuuxAP",     known "QuuxAP" "ANoExportList.hs" 37 "Pattern")
            , ("derivedA",   NotFound)

            , ("FooB",       known "FooB"     "BWildcardExportListWithChildren.hs" 25 "Type")
            , ("FooB1",      known "FooB1"    "BWildcardExportListWithChildren.hs" 25 "Constructor")
            , ("fooB1",      known "fooB1"    "BWildcardExportListWithChildren.hs" 26 "Function")
            , ("fooB2",      known "fooB2"    "BWildcardExportListWithChildren.hs" 27 "Function")
            , ("BarB",       known "BarB"     "BWildcardExportListWithChildren.hs" 30 "Type")
            , ("BarB1",      known "BarB1"    "BWildcardExportListWithChildren.hs" 31 "Constructor")
            , ("unBarB",     known "unBarB"   "BWildcardExportListWithChildren.hs" 32 "Function")
            , ("BazBP",      known "BazBP"    "BWildcardExportListWithChildren.hs" 34 "Pattern")
            , ("quuxB",      known "quuxB"    "BWildcardExportListWithChildren.hs" 37 "Function")
            , ("FrobBP",     known "FrobBP"   "BWildcardExportListWithChildren.hs" 40 "Pattern")
            , ("QuuxB",      known "QuuxB"    "BWildcardExportListWithChildren.hs" 43 "Type")
            , ("QuuxB1",     known "QuuxB1"   "BWildcardExportListWithChildren.hs" 44 "Constructor")
            , ("QuuxB2",     known "QuuxB2"   "BWildcardExportListWithChildren.hs" 45 "Constructor")
            , ("QuuxBP",     known "QuuxBP"   "BWildcardExportListWithChildren.hs" 47 "Pattern")
            , ("derivedB",   known "derivedB" "BWildcardExportListWithChildren.hs" 22 "Function")

            , ("FooC",       known "FooC"     "CWildcardExportListWithChildrenPlusSome.hs" 22 "Type")
            , ("FooC1",      known "FooC1"    "CWildcardExportListWithChildrenPlusSome.hs" 22 "Constructor")
            , ("fooC1",      known "fooC1"    "CWildcardExportListWithChildrenPlusSome.hs" 23 "Function")
            , ("fooC2",      known "fooC2"    "CWildcardExportListWithChildrenPlusSome.hs" 24 "Function")
            , ("BarC",       known "BarC"     "CWildcardExportListWithChildrenPlusSome.hs" 27 "Type")
            , ("BarC1",      known "BarC1"    "CWildcardExportListWithChildrenPlusSome.hs" 28 "Constructor")
            , ("unBarC",     known "unBarC"   "CWildcardExportListWithChildrenPlusSome.hs" 29 "Function")
            , ("BazCP",      known "BazCP"    "CWildcardExportListWithChildrenPlusSome.hs" 31 "Pattern")
            , ("quuxC",      known "quuxC"    "CWildcardExportListWithChildrenPlusSome.hs" 34 "Function")
            , ("FrobCP",     known "FrobCP"   "CWildcardExportListWithChildrenPlusSome.hs" 37 "Pattern")
            , ("QuuxC",      known "QuuxC"    "CWildcardExportListWithChildrenPlusSome.hs" 40 "Type")
            , ("QuuxC1",     known "QuuxC1"   "CWildcardExportListWithChildrenPlusSome.hs" 41 "Constructor")
            , ("QuuxC2",     known "QuuxC2"   "CWildcardExportListWithChildrenPlusSome.hs" 42 "Constructor")
            , ("QuuxCP",     known "QuuxCP"   "CWildcardExportListWithChildrenPlusSome.hs" 44 "Pattern")
            , ("derivedC",   known "derivedC" "CWildcardExportListWithChildrenPlusSome.hs" 19 "Function")

            , ("FooD",       known "FooD"     "DSpecificExportListWithChildren.hs" 26 "Type")
            , ("FooD1",      known "FooD1"    "DSpecificExportListWithChildren.hs" 26 "Constructor")
            , ("fooD1",      NotFound)
            , ("fooD2",      NotFound)
            , ("BarD",       known "BarD"     "DSpecificExportListWithChildren.hs" 31 "Type")
            , ("BarD1",      known "BarD1"    "DSpecificExportListWithChildren.hs" 32 "Constructor")
            , ("unBarD",     known "unBarD"   "DSpecificExportListWithChildren.hs" 33 "Function")
            , ("BazDP",      known "BazDP"    "DSpecificExportListWithChildren.hs" 35 "Pattern")
            , ("quuxD",      known "quuxD"    "DSpecificExportListWithChildren.hs" 38 "Function")
            , ("FrobDP",     known "FrobDP"   "DSpecificExportListWithChildren.hs" 41 "Pattern")
            , ("QuuxD",      known "QuuxD"    "DSpecificExportListWithChildren.hs" 44 "Type")
            , ("QuuxD1",     NotFound)
            , ("QuuxD2",     known "QuuxD2"   "DSpecificExportListWithChildren.hs" 46 "Constructor")
            , ("QuuxDP",     known "QuuxDP"   "DSpecificExportListWithChildren.hs" 48 "Pattern")
            , ("derivedD",   known "derivedD" "DSpecificExportListWithChildren.hs" 22 "Function")

            , ("FooE",       known "FooE"     "ESpecificExportListWithChildrenPlusSome.hs" 22 "Type")
            , ("FooE1",      known "FooE1"    "ESpecificExportListWithChildrenPlusSome.hs" 22 "Constructor")
            , ("fooE1",      NotFound)
            , ("fooE2",      NotFound)
            , ("BarE",       known "BarE"     "ESpecificExportListWithChildrenPlusSome.hs" 27 "Type")
            , ("BarE1",      known "BarE1"    "ESpecificExportListWithChildrenPlusSome.hs" 28 "Constructor")
            , ("unBarE",     known "unBarE"   "ESpecificExportListWithChildrenPlusSome.hs" 29 "Function")
            , ("BazEP",      known "BazEP"    "ESpecificExportListWithChildrenPlusSome.hs" 31 "Pattern")
            , ("quuxE",      known "quuxE"    "ESpecificExportListWithChildrenPlusSome.hs" 34 "Function")
            , ("FrobEP",     known "FrobEP"   "ESpecificExportListWithChildrenPlusSome.hs" 37 "Pattern")
            , ("QuuxE",      known "QuuxE"    "ESpecificExportListWithChildrenPlusSome.hs" 40 "Type")
            , ("QuuxE1",     NotFound)
            , ("QuuxE2",     known "QuuxE2"   "ESpecificExportListWithChildrenPlusSome.hs" 42 "Constructor")
            , ("QuuxEP",     known "QuuxEP"   "ESpecificExportListWithChildrenPlusSome.hs" 44 "Pattern")
            , ("derivedE",   known "derivedE" "ESpecificExportListWithChildrenPlusSome.hs" 19 "Function")

            , ("reexportsFunc", known "reexportsFunc" "import1NoListImport2WithListChildrenWildcardsReexportModule/Reexports.hs" 27 "Function")
            , ("ReexportType",  known "ReexportType"  "import1NoListImport2WithListChildrenWildcardsReexportModule/Reexports.hs" 30 "Type")
            , ("ReexportC1",    known "ReexportC1"    "import1NoListImport2WithListChildrenWildcardsReexportModule/Reexports.hs" 31 "Constructor")
            , ("ReexportC2",    known "ReexportC2"    "import1NoListImport2WithListChildrenWildcardsReexportModule/Reexports.hs" 32 "Constructor")
            , ("commonFunc"
              , Ambiguous $ map (\(sym, file, n) -> (sym, file, n, "Function"))
                [ (s "commonFunc", "ANoExportList.hs", 40)
                , (s "commonFunc", "BWildcardExportListWithChildren.hs", 50)
                , (s "commonFunc", "CWildcardExportListWithChildrenPlusSome.hs", 47)
                , (s "commonFunc", "DSpecificExportListWithChildren.hs", 51)
                , (s "commonFunc", "ESpecificExportListWithChildrenPlusSome.hs", 47)
                ]
              )
            ]
          ]
      , withFile "import1NoListImport2WithListChildrenWildcardsReexportModule/ImportCausesImportCycle.hs" $
        group "Import1 - NoList Import2 - WithListChildrenWildcards ReexportModule - ImportCausesImportCycle"
          [ (CL8.unpack sym, sym, response)
          | (sym, response) <-
            [ ("foo", known "foo" "import1NoListImport2WithListChildrenWildcardsReexportModule/CausesImportCycle.hs" 17 "Function")
            ]
          ]
      ]
  , withDirAndFile NameResolutionStrict (RecursiveDir "0013module_imports_same_name") "MainModule.hs" $
      group "Some dependent module imports another module with the same name but different location"
        [ (CL8.unpack sym, sym, response)
        | (sym, response) <-
          [ ("foo",       known "foo" "module1/Dependency.hs" 16 "Function")
          , ("bar",       known "bar" "module2/Dependency.hs" 11 "Function")
          , ("ambiguous"
            , Ambiguous $ map (\(sym, file, n) -> (sym, file, n, "Function"))
              [ (s "ambiguous", "module1/Dependency.hs", 13)
              , (s "ambiguous", "module2/Dependency.hs", 14)
              ]
            )
          ]
        ]
  , withDirAndFile NameResolutionStrict (RecursiveDir "0014module_imports_same_name_multiple_occurrences") "MainModule.hs" $
      group "Several dependent modules imports another module with the same name but different location"
        [ (CL8.unpack sym, sym, response)
        | (sym, response) <-
          [ ("foo",       known "foo" "module1/Dependency.hs" 16 "Function")
          , ("foofoo",    known "foofoo" "module1/Foo.hs"        15 "Function")
          , ("bar",       known "bar" "module2/Dependency.hs" 11 "Function")
          , ("frob",      known "frob" "module3/Dependency.hs" 18 "Function")
          , ("ambiguous"
            , Ambiguous $ map (\(sym, file, n) -> (sym, file, n, "Function"))
              [ (s "ambiguous", "module1/Dependency.hs", 13)
              , (s "ambiguous", "module2/Dependency.hs", 14)
              , (s "ambiguous", "module3/Dependency.hs", 15)
              ]
            )
          ]
        ]
  , withDirAndFile NameResolutionStrict (ShallowDir "0015reexport_via_implicit_qualifier") "Main.hs" $
      group "Reexport name via implicit module qualifier"
        [ (CL8.unpack sym, sym, response)
        | (sym, response) <-
          [ ("foo",       known "foo" "Source.hs" 11 "Function")
          , ("Bar",       known "Bar" "Source.hs" 14 "Type")
          , ("unBar",     NotFound)
          , ("lookup",    known "lookup" "ModuleWithReexport.hs" 17 "Function")
          ]
        ]
  , withDirAndFile NameResolutionLax (ShallowDir "0016reexport_of_missing_module") "Main.hs" $
      group "Reexport of missing module"
        [ (CL8.unpack sym, sym, response)
        | (sym, response) <-
          [ ("foo",       known "foo" "Source.hs" 11 "Function")
          , ("Bar",       known "Bar" "Source.hs" 14 "Type")
          , ("unBar",     NotFound)
          , ("lookup",    known "lookup" "ModuleWithReexport.hs" 21 "Function")
          -- These names come from missing modules and should default
          -- to the export list of the module that refers to them.
          , ("foo2",      known "foo2" "ModuleWithReexport.hs" 13 "Function")
          , ("Quux",      known "Quux" "ModuleWithReexport.hs" 14 "Type")
          ]
        ]
  , withDirAndFile NameResolutionLax (ShallowDir "0017ignored_dirs_and_files") "Main.hs" $
      group "Shallow dir does not find dependency in subdirectory"
        [ (CL8.unpack sym, sym, response)
        | (sym, response) <-
          [ ("foo",       NotFound)
          , ("bar",       NotFound)
          ]
        ]
  , withDirAndFile NameResolutionLax (RecursiveDir "0017ignored_dirs_and_files") "Main.hs" $
      group "Recursive dir does find dependency in subdirectory"
        [ (CL8.unpack sym, sym, response)
        | (sym, response) <-
          [ ("foo",       known "foo" "dep/Dependency.hs" 12 "Function")
          , ("bar",       NotFound)
          ]
        ]
  , withDirAndFile NameResolutionLax (RecursiveWithIgnored "0017ignored_dirs_and_files" ["*/dep/*"]) "Main.hs" $
      group "Recursive dir with ignored glob that matches directory only"
        [ (CL8.unpack sym, sym, response)
        | (sym, response) <-
          [ ("foo",       NotFound)
          , ("bar",       NotFound)
          ]
        ]
  , withDirAndFile NameResolutionLax (RecursiveWithIgnored "0017ignored_dirs_and_files" ["*/dep*"]) "Main.hs" $
      group "Recursive dir with ignored glob that matches directory and its contents"
        [ (CL8.unpack sym, sym, response)
        | (sym, response) <-
          [ ("foo",       NotFound)
          , ("bar",       NotFound)
          ]
        ]
  , withDirAndFile NameResolutionLax (RecursiveWithIgnored "0017ignored_dirs_and_files" ["*"]) "Main.hs" $
      group "Recursive dir with ignored glob that matches everything"
        [ (CL8.unpack sym, sym, response)
        | (sym, response) <-
          [ ("foo",       NotFound)
          , ("bar",       NotFound)
          ]
        ]
  , withDirAndFile NameResolutionLax (RecursiveWithIgnored "0017ignored_dirs_and_files" ["*.hs"]) "Main.hs" $
      group "Recursive dir with ignored glob that matches all files"
        [ (CL8.unpack sym, sym, response)
        | (sym, response) <-
          [ ("foo",       NotFound)
          , ("bar",       NotFound)
          ]
        ]
  , withDirAndFile NameResolutionLax (RecursiveDir "0018definitions_between_import_and_foreign_import") "Main.hs" $
      group "Recursive dir with ignored glob that matches all files"
        [ (CL8.unpack sym, sym, response)
        | (sym, response) <-
          [ ("foo",       NotFound)
          , ("bar",       known "bar" "Dependency1.hs" 19 "Function")
          , ("test",      known "test" "Dependency1.hs" 22 "Function")
          , ("baz",       known "baz" "Dependency1.hs" 24 "Function")
          , ("quux",      known "quux" "Dependency2.hs" 11 "Function")
          ]
        ]
  ]

tests :: TestTree
tests =
  withResource
    (getNumCapabilities >>= \caps -> newPortPool (2 * caps) (defaultPort + 1))
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
  -> SearchCfg
  -> TagsServerConf
  -> (ServerConnection -> m a)
  -> m a
withConnection pool searchDirs conf f = do
  -- -- Try to connect to a server. If attempt succeeds then server is running
  -- -- and there's no need to run server ourselves.
  -- existing <- tryConnect defaultPort
  -- case existing of
  --   Right existing' ->
  --     f (ExistingServer existing') `finally` liftBase (closeConnection existing')
  --   Left  _reason   -> startLocalServer
  startLocalServer
  where
    startLocalServer :: m a
    startLocalServer = do
      pool' <- liftBase pool
      withPort pool' $ \port -> do
        serv <- runErrorExceptT $ mkLogCollectingServer searchDirs conf port
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
mkFindSymbolTest pool ServerTest{stTestName, stNameResolutionStrictness, stWorkingDirectory, stFile, stSymbol, stExpectedResponse} =
 testCase stTestName $ do
    result <- runErrorExceptT $ do
      (searchDirs, conf) <- mkTestsConfig stNameResolutionStrictness stWorkingDirectory
      withConnection pool searchDirs conf $ \conn -> do
        let dir  = case stWorkingDirectory of
              ShallowDir   x           -> testDataDir </> x
              RecursiveDir x           -> testDataDir </> x
              RecursiveWithIgnored x _ -> testDataDir </> x
            path = pathFragmentToUTF8 $ dir </> stFile
        r    <- liftBase $
          call conn "haskell-tags-server" "find"
            [IntTerm 1337, TupleTerm [BinaryTerm path, BinaryTerm stSymbol, AtomTerm "local"]]
        logs <- case conn of
          ExistingServer _   -> pure mempty
          LocalServer serv _ -> do
            logs <- liftBase $ getLogs serv
            pure $ "Logs:" ## PP.indent 2 (PP.vcat logs)
        case r of
          Left err  -> assertFailure' $ ppShow err ## logs
          Right res ->
            case res of
              TupleTerm [callId, res'] -> do
                case callId of
                  IntTerm 1337 -> pure ()
                  unexpected   -> assertFailure' $
                    "Unexpected call id was returned:" <+> ppShow unexpected
                case res' of
                  TupleTerm [AtomTerm "error", err] -> assertFailure' $
                    "Unexpected error from tags server:" <+> ppShow err
                  TupleTerm [AtomTerm "ok", res''] -> do
                    dir'   <- mkFullPath dir
                    actual <- relativizePathsInResponse dir' res''
                    let msg = docFromString $ "expected: " ++ show expected ++ "\n but got: " ++ show actual
                    if responseType actual == responseType expected
                    then
                      unless (actual == expected) $
                        assertFailure' $ msg ## logs
                    else
                      assertFailure' $
                        case extractResponseError actual of
                          Nothing   -> msg ## logs
                          Just msg' -> "Error from server:" ##
                            PP.nest 2 (docFromByteString msg') ## logs
                  unexpected -> assertFailure' $
                    "Unexpected response from tags server:" <+> ppShow unexpected
              invalid -> assertFailure' $
                "Invalid repsonse:" <+> ppShow invalid
            where
              expected = responseToTerm stExpectedResponse
    case result of
      Right () -> pure ()
      Left err -> assertFailure' $ "Failure:" ## pretty err
  where
    assertFailure' :: MonadBase IO m => Doc ann -> m a
    assertFailure' = liftBase . assertFailure . displayDocString

responseType :: Term -> Maybe String
responseType (TupleTerm (AtomTerm x : _)) = Just x
responseType _                            = Nothing

extractResponseError :: Term -> Maybe UTF8.ByteString
extractResponseError (TupleTerm [AtomTerm "error", BinaryTerm msg]) = Just msg
extractResponseError _                                              = Nothing

relativizePathsInResponse
  :: forall m. (MonadBase IO m, MonadError ErrorMessage m)
  => FullPath 'Dir -> Term -> m Term
relativizePathsInResponse rootDir term =
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
    fixLoc (TupleTerm [symName, BinaryTerm path, line, typ]) = do
      path' <- relativise path
      pure $ TupleTerm [symName, BinaryTerm path', line, typ]
    fixLoc x = error $ "invalid symbol location term: " ++ show x
    relativise :: UTF8.ByteString -> m UTF8.ByteString
    relativise = fmap (either mkRel mkRel) . fullPathFromUTF8
    mkRel
      :: MakeRelative (FullPath 'Dir) (FullPath a) PathFragment
      => FullPath a -> UTF8.ByteString
    mkRel = pathFragmentToUTF8 . makeRelative rootDir

-- _baseNameToUTF8 :: BaseName -> UTF8.ByteString
-- _baseNameToUTF8 = pathFragmentToUTF8 . unBaseName

-- _fullPathToUTF8 :: FullPath -> UTF8.ByteString
-- _fullPathToUTF8 = CL8.fromStrict . TE.encodeUtf8 . unFullPath

fullPathFromUTF8
  :: (MonadBase IO m, MonadError ErrorMessage m)
  => UTF8.ByteString
  -> m (Either (FullPath 'File) (FullPath 'Dir))
fullPathFromUTF8 = mkSomeFullPath . TE.decodeUtf8 . CL8.toStrict

pathFragmentToUTF8 :: PathFragment -> UTF8.ByteString
pathFragmentToUTF8 = CL8.fromStrict . TE.encodeUtf8 . unPathFragment
