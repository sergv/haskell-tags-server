----------------------------------------------------------------------------
-- |
-- Module      :  Server.Tags.AnalyzeHeaderTests
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Friday, 23 September 2016
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskell.Language.Server.Tags.AnalyzeHeaderTests (tests) where

import Control.Monad.Except
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.PrettyPrint.Leijen.Text as PP

import FastTags (tokenizeInput)
import Token (Token)


import Control.Monad.Logging.DiscardLogs
import qualified Data.KeyMap as KM
import Data.Symbols
import Haskell.Language.Server.Tags.AnalyzeHeader
import Haskell.Language.Server.Tags.Types
import Text.PrettyPrint.Leijen.Text.Utils

simpleHeaderTest :: TestTree
simpleHeaderTest = doTest "Simple header"
  "module Foo where"
  ModuleHeader
    { mhModName          = mkModuleName "Foo"
    , mhExports          = Nothing
    , mhImportQualifiers = mempty
    , mhImports          = mempty
    }

moduleWithUnqualifiedImportTest :: TestTree
moduleWithUnqualifiedImportTest = doTest "Module with unqualified import"
  "module ModuleWithUnqualifiedImport where\n\
  \import Imported1"
  ModuleHeader
    { mhModName          = mkModuleName "ModuleWithUnqualifiedImport"
    , mhExports          = Nothing
    , mhImportQualifiers = mempty
    , mhImports          = M.fromList
        [ ( mkModuleName "Imported1"
          , neSingleton ImportSpec
              { ispecModuleName    = mkModuleName "Imported1"
              , ispecQualification = Unqualified
              , ispecImportList    = Nothing
              }
          )
        ]
    }

moduleWithUnqualifiedImportAndEmptyImportListTest :: TestTree
moduleWithUnqualifiedImportAndEmptyImportListTest = doTest "Module with unqualified import and empty import list"
  "module Test where\n\
  \import Imported1 ()"
  ModuleHeader
    { mhModName          = mkModuleName "Test"
    , mhExports          = Nothing
    , mhImportQualifiers = mempty
    , mhImports          = M.fromList
        [ ( mkModuleName "Imported1"
          , neSingleton ImportSpec
              { ispecModuleName    = mkModuleName "Imported1"
              , ispecQualification = Unqualified
              , ispecImportList    = Just $ Imported mempty
              }
          )
        ]
    }

moduleWithUnqualifiedImportAndEmptyHiddenImportListTest :: TestTree
moduleWithUnqualifiedImportAndEmptyHiddenImportListTest = doTest "Module with unqualified import and empty hidden import list"
  "module Test where\n\
  \import Imported1 hiding ()"
  ModuleHeader
    { mhModName          = mkModuleName "Test"
    , mhExports          = Nothing
    , mhImportQualifiers = mempty
    , mhImports          = M.fromList
        [ ( mkModuleName "Imported1"
          , neSingleton ImportSpec
              { ispecModuleName    = mkModuleName "Imported1"
              , ispecQualification = Unqualified
              , ispecImportList    = Just $ Hidden mempty
              }
          )
        ]
    }

moduleWithUnqualifiedImportAndSingletonImportListTest :: TestTree
moduleWithUnqualifiedImportAndSingletonImportListTest = doTest "Module with unqualified import and singleton import list"
  "module Test where\n\
  \import Imported1 (foo)"
  ModuleHeader
    { mhModName          = mkModuleName "Test"
    , mhExports          = Nothing
    , mhImportQualifiers = mempty
    , mhImports          = M.fromList
        [ ( mkModuleName "Imported1"
          , neSingleton ImportSpec
              { ispecModuleName    = mkModuleName "Imported1"
              , ispecQualification = Unqualified
              , ispecImportList    = Just $ Imported $ KM.fromList
                  [ EntryWithChildren (mkUnqualifiedSymbolName' "foo") Nothing
                  ]
              }
          )
        ]
    }

moduleWithUnqualifiedImportAndNonemptyImportListTest :: TestTree
moduleWithUnqualifiedImportAndNonemptyImportListTest = doTest "Module with unqualified import and nonempty import list"
  "module Test where\n\
  \import Imported1 (foo, bar, baz)"
  ModuleHeader
    { mhModName          = mkModuleName "Test"
    , mhExports          = Nothing
    , mhImportQualifiers = mempty
    , mhImports          = M.fromList
        [ ( mkModuleName "Imported1"
          , neSingleton ImportSpec
              { ispecModuleName    = mkModuleName "Imported1"
              , ispecQualification = Unqualified
              , ispecImportList    = Just $ Imported $ KM.fromList
                  [ EntryWithChildren (mkUnqualifiedSymbolName' "foo") Nothing
                  , EntryWithChildren (mkUnqualifiedSymbolName' "bar") Nothing
                  , EntryWithChildren (mkUnqualifiedSymbolName' "baz") Nothing
                  ]
              }
          )
        ]
    }

moduleWithUnqualifiedImportAndNonemptyImportListWithDifferentVisibilitiesTest :: TestTree
moduleWithUnqualifiedImportAndNonemptyImportListWithDifferentVisibilitiesTest = doTest "Module with unqualified import and nonempty import list with different visibilities"
  "module Test where\n\
  \import Imported1 (foo, Bar(..), Baz(Quux, Fizz))"
  ModuleHeader
    { mhModName          = mkModuleName "Test"
    , mhExports          = Nothing
    , mhImportQualifiers = mempty
    , mhImports          = M.fromList
        [ ( mkModuleName "Imported1"
          , neSingleton ImportSpec
              { ispecModuleName    = mkModuleName "Imported1"
              , ispecQualification = Unqualified
              , ispecImportList    = Just $ Imported $ KM.fromList
                  [ EntryWithChildren (mkUnqualifiedSymbolName' "foo") Nothing
                  , EntryWithChildren (mkUnqualifiedSymbolName' "Bar") $ Just VisibleAllChildren
                  , EntryWithChildren (mkUnqualifiedSymbolName' "Baz") $ Just $ VisibleSpecificChildren $ S.fromList
                      [ mkUnqualifiedSymbolName' "Quux"
                      , mkUnqualifiedSymbolName' "Fizz"
                      ]
                  ]
              }
          )
        ]
    }

moduleWithQualifiedImportTest :: TestTree
moduleWithQualifiedImportTest = doTest "Module with qualified import"
  "module ModuleWithQualifiedImport where\n\
  \import qualified Imported1"
  ModuleHeader
    { mhModName          = mkModuleName "ModuleWithQualifiedImport"
    , mhExports          = Nothing
    , mhImportQualifiers = M.fromList
        [ ( mkImportQualifier $ mkModuleName "Imported1"
          , neSingleton $ mkModuleName "Imported1"
          )
        ]
    , mhImports          = M.fromList
        [ ( mkModuleName "Imported1"
          , neSingleton ImportSpec
              { ispecModuleName    = mkModuleName "Imported1"
              , ispecQualification =
                  Qualified $ mkImportQualifier $ mkModuleName "Imported1"
              , ispecImportList    = Nothing
              }
          )
        ]
    }

moduleWithQualifiedImportAndAliasTest :: TestTree
moduleWithQualifiedImportAndAliasTest = doTest "Module with qualified import and alias"
  "module ModuleWithQualifiedImportAndAlias where\n\
  \import qualified Imported1 as Imp"
  ModuleHeader
    { mhModName          = mkModuleName "ModuleWithQualifiedImportAndAlias"
    , mhExports          = Nothing
    , mhImportQualifiers = M.fromList
        [ ( mkImportQualifier $ mkModuleName "Imp"
          , neSingleton $ mkModuleName "Imported1"
          )
        ]
    , mhImports          = M.fromList
        [ ( mkModuleName "Imported1"
          , neSingleton ImportSpec
              { ispecModuleName    = mkModuleName "Imported1"
              , ispecQualification =
                  Qualified $ mkImportQualifier $ mkModuleName "Imp"
              , ispecImportList    = Nothing
              }
          )
        ]
    }

moduleWithImportAndAliasTest :: TestTree
moduleWithImportAndAliasTest = doTest "Module with import and alias"
  "module ModuleWithImportAndAlias where\n\
  \import Imported1 as Imp"
  ModuleHeader
    { mhModName          = mkModuleName "ModuleWithImportAndAlias"
    , mhExports          = Nothing
    , mhImportQualifiers = M.fromList
        [ ( mkImportQualifier $ mkModuleName "Imp"
          , neSingleton $ mkModuleName "Imported1"
          )
        ]
    , mhImports          = M.fromList
        [ ( mkModuleName "Imported1"
          , neSingleton ImportSpec
              { ispecModuleName    = mkModuleName "Imported1"
              , ispecQualification =
                  BothQualifiedAndUnqualified $ mkImportQualifier $ mkModuleName "Imp"
              , ispecImportList    = Nothing
              }
          )
        ]
    }

moduleWithImportAndAliasAndHidingImportListTest :: TestTree
moduleWithImportAndAliasAndHidingImportListTest = doTest "Module with import, alias and hiding import list"
  "module ModuleWithImportAndAliasAandHidingImportList where\n\
  \import Imported1 as Imp hiding (Foo(..), bar, Quux(Baz))"
  ModuleHeader
    { mhModName          = mkModuleName "ModuleWithImportAndAliasAandHidingImportList"
    , mhExports          = Nothing
    , mhImportQualifiers = M.fromList
        [ ( mkImportQualifier $ mkModuleName "Imp"
          , neSingleton $ mkModuleName "Imported1"
          )
        ]
    , mhImports          = M.fromList
        [ ( mkModuleName "Imported1"
          , neSingleton ImportSpec
              { ispecModuleName    = mkModuleName "Imported1"
              , ispecQualification =
                  BothQualifiedAndUnqualified $ mkImportQualifier $ mkModuleName "Imp"
              , ispecImportList    = Just $ Hidden $ KM.fromList
                  [ EntryWithChildren (mkUnqualifiedSymbolName' "Foo") $ Just VisibleAllChildren
                  , EntryWithChildren (mkUnqualifiedSymbolName' "bar") Nothing
                  , EntryWithChildren (mkUnqualifiedSymbolName' "Quux") $ Just $ VisibleSpecificChildren $ S.fromList
                      [ mkUnqualifiedSymbolName' "Baz"
                      ]
                  ]
              }
          )
        ]
    }

moduleWithEmptyExportsTest :: TestTree
moduleWithEmptyExportsTest = doTest "Module with empty exports"
  "module ModuleWithEmptyExport () where"
  ModuleHeader
    { mhModName          = mkModuleName "ModuleWithEmptyExport"
    , mhExports          = Just ModuleExports
        { meExportedEntries    = mempty
        , meReexports          = mempty
        , meHasWildcardExports = False
        }
    , mhImportQualifiers = mempty
    , mhImports          = mempty
    }

moduleWithExportsTest :: TestTree
moduleWithExportsTest = doTest "Module exports"
  "module ModuleWithExport (foo, Bar(..), Baz(Quux, Fizz), pattern Pat, module Frob) where"
  ModuleHeader
    { mhModName          = mkModuleName "ModuleWithExport"
    , mhExports          = Just ModuleExports
        { meExportedEntries    = KM.fromList
            [ EntryWithChildren (mkSymbolName "foo") Nothing
            , EntryWithChildren (mkSymbolName "Bar") $ Just VisibleAllChildren
            , EntryWithChildren (mkSymbolName "Baz") $ Just $ VisibleSpecificChildren $ S.fromList
                [ mkUnqualifiedSymbolName' "Quux"
                , mkUnqualifiedSymbolName' "Fizz"
                ]
            , EntryWithChildren (mkSymbolName "Pat") Nothing
            ]
        , meReexports          = S.singleton $ mkModuleName "Frob"
        , meHasWildcardExports = True
        }
    , mhImportQualifiers = mempty
    , mhImports          = mempty
    }

moduleWithMultilineExportsTest :: TestTree
moduleWithMultilineExportsTest = doTest "Module with peculiarly indented export list"
  "module ModuleWithExport\n\
  \ (\n\
  \   foo\n\
  \ , \n\
  \  Bar\n\
  \    (..)\n\
  \ ,        Baz(\n\
  \  Quux\n\
  \    ,\n\
  \   Fizz \n\
  \  )\n\
  \    ,\n\
  \  pattern\n\
  \     Pat\n\
  \  , \n\
  \    module \n\
  \  Frob\n\
  \   ) \n\
  \   where"
  ModuleHeader
    { mhModName          = mkModuleName "ModuleWithExport"
    , mhExports          = Just ModuleExports
        { meExportedEntries    = KM.fromList
            [ EntryWithChildren (mkSymbolName "foo") Nothing
            , EntryWithChildren (mkSymbolName "Bar") $ Just VisibleAllChildren
            , EntryWithChildren (mkSymbolName "Baz") $ Just $ VisibleSpecificChildren $ S.fromList
                [ mkUnqualifiedSymbolName' "Quux"
                , mkUnqualifiedSymbolName' "Fizz"
                ]
            , EntryWithChildren (mkSymbolName "Pat") Nothing
            ]
        , meReexports          = S.singleton $ mkModuleName "Frob"
        , meHasWildcardExports = True
        }
    , mhImportQualifiers = mempty
    , mhImports          = mempty
    }

tests :: TestTree
tests = testGroup "Header analysis tests"
  [ simpleHeaderTest
  , testGroup "imports"
    [ moduleWithUnqualifiedImportTest
    , moduleWithUnqualifiedImportAndEmptyImportListTest
    , moduleWithUnqualifiedImportAndEmptyHiddenImportListTest
    , moduleWithUnqualifiedImportAndSingletonImportListTest
    , moduleWithUnqualifiedImportAndNonemptyImportListTest
    , moduleWithUnqualifiedImportAndNonemptyImportListWithDifferentVisibilitiesTest
    , moduleWithQualifiedImportTest
    , moduleWithQualifiedImportAndAliasTest
    , moduleWithImportAndAliasTest
    , moduleWithImportAndAliasAndHidingImportListTest
    ]
  , testGroup "exports"
    [ moduleWithEmptyExportsTest
    , moduleWithExportsTest
    , moduleWithMultilineExportsTest
    ]
  ]

doTest :: String -> T.Text -> ModuleHeader -> TestTree
doTest name input expectedHeader =
  testCase name $
    case runIdentity $ runDiscardLogsT $ runExceptT $ analyzeHeader =<< tokens of
      Left msg               -> assertFailure $ displayDocString msg
      Right (Nothing, _)     -> assertFailure $ displayDocString $
        "No header detected, but was expecting header" PP.<$> pretty expectedHeader
      Right (Just header, _) ->
        unless (header == expectedHeader) $
          assertFailure $ displayDocString $ ppDict "Headers are different"
            [ name :-> ppAlist ["Actual" :-> x, "Expected" :-> y]
            | (name, different, x, y) <-
              [ ( "ModuleName"
                , mhModName header /= mhModName expectedHeader
                , pretty $ mhModName header
                , pretty $ mhModName expectedHeader
                )
              , ( "Exports"
                , mhExports header /= mhExports expectedHeader
                , pretty $ mhExports header
                , pretty $ mhExports expectedHeader
                )
              , ( "ImportQualifiers"
                , mhImportQualifiers header /= mhImportQualifiers expectedHeader
                , ppMap $ ppNE <$> mhImportQualifiers header
                , ppMap $ ppNE <$> mhImportQualifiers expectedHeader
                )
              , ( "Imports"
                , mhImports header /= mhImports expectedHeader
                , ppMap $ ppNE <$> mhImports header
                , ppMap $ ppNE <$> mhImports expectedHeader
                )
              ]
            , different
            ]
  where
    tokens :: forall m. (MonadError Doc m) => m [Token]
    tokens = either (throwError . docFromString) return $ tokenizeInput "test" False input

mkUnqualifiedSymbolName' :: T.Text -> UnqualifiedSymbolName
mkUnqualifiedSymbolName' name =
  fromMaybe err $ mkUnqualifiedSymbolName $ mkSymbolName name
  where
    err = error $ "invalid unqualified symbol name: " ++ show name


neSingleton :: a -> NonEmpty a
neSingleton x = x :| []
