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
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskell.Language.Server.Tags.AnalyzeHeaderTests (tests) where

import Control.Arrow
import Control.Monad.Except
import Control.Monad.Writer
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.PrettyPrint.Leijen.Text as PP

import FastTags (tokenizeInput)
import Token (Token)

import Control.Monad.Logging.Simple
import qualified Data.KeyMap as KM
import qualified Data.SubkeyMap as SubkeyMap
import Data.Symbols
import Haskell.Language.Server.Tags.AnalyzeHeader
import Haskell.Language.Server.Tags.Types
import Text.PrettyPrint.Leijen.Text.Utils

import TestUtils
import Haskell.Language.Server.Tags.AnalyzeHeaderTests.Regressions

type Test = TestCase T.Text UnresolvedModuleHeader

simpleHeaderTest :: Test
simpleHeaderTest = TestCase
  { testName       = "Simple header"
  , input          =
      "module Foo where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Foo"
      , mhExports          = Nothing
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithUnqualifiedImportTest :: Test
moduleWithUnqualifiedImportTest = TestCase
  { testName       = "Module with unqualified import"
  , input          =
      "module ModuleWithUnqualifiedImport where\n\
      \import Imported1"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithUnqualifiedImport"
      , mhExports          = Nothing
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = Nothing
              }
          ]
      }
  }

moduleWithUnqualifiedImportWithSourceTest :: Test
moduleWithUnqualifiedImportWithSourceTest = TestCase
  { testName       = "Module with unqualified import with {-# SOURCE #-}"
  , input          =
      "module ModuleWithUnqualifiedImport where\n\
      \import {-# SOURCE #-} Imported1"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithUnqualifiedImport"
      , mhExports          = Nothing
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = HsBootModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = Nothing
              }
          ]
      }
  }

moduleWithUnqualifiedSafeImportTest :: Test
moduleWithUnqualifiedSafeImportTest = TestCase
  { testName       = "Module with unqualified safe import"
  , input          =
      "module ModuleWithUnqualifiedImport where\n\
      \import safe Imported1"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithUnqualifiedImport"
      , mhExports          = Nothing
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = Nothing
              }
          ]
      }
  }

moduleWithUnqualifiedImportAndEmptyImportListTest :: Test
moduleWithUnqualifiedImportAndEmptyImportListTest = TestCase
  { testName       = "Module with unqualified import and empty import list"
  , input          =
      "module Test where\n\
      \import Imported1 ()"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = Nothing
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = Just ImportList
                  { ilEntries       = mempty
                  , ilImportType    = Imported
                  , ilImportedNames = ()
                  }
              }
          ]
      }
  }

moduleWithUnqualifiedImportAndEmptyHiddenImportListTest :: Test
moduleWithUnqualifiedImportAndEmptyHiddenImportListTest = TestCase
  { testName       = "Module with unqualified import and empty hidden import list"
  , input          =
      "module Test where\n\
      \import Imported1 hiding ()"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = Nothing
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = Just ImportList
                  { ilEntries       = mempty
                  , ilImportType    = Hidden
                  , ilImportedNames = ()
                  }
              }
          ]
      }
  }

moduleWithUnqualifiedImportAndSingletonImportListTest :: Test
moduleWithUnqualifiedImportAndSingletonImportListTest = TestCase
  { testName       = "Module with unqualified import and singleton import list"
  , input          =
      "module Test where\n\
      \import Imported1 (foo)"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = Nothing
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = Just ImportList
                  { ilEntries       = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "foo"
                          , entryChildrenVisibility = Nothing
                          }
                      ]
                  , ilImportType    = Imported
                  , ilImportedNames = ()
                  }
              }
          ]
      }
  }

moduleWithUnqualifiedImportAndNonemptyImportListTest :: Test
moduleWithUnqualifiedImportAndNonemptyImportListTest = TestCase
  { testName       = "Module with unqualified import and nonempty import list"
  , input          =
      "module Test where\n\
      \import Imported1 (foo, bar, baz)"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = Nothing
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = Just ImportList
                  { ilEntries       = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "foo"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "bar"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "baz"
                          , entryChildrenVisibility = Nothing
                          }
                      ]
                  , ilImportType    = Imported
                  , ilImportedNames = ()
                  }
              }
          ]
      }
  }

moduleWithUnqualifiedImportAndNonemptyImportListWithDifferentVisibilitiesTest :: Test
moduleWithUnqualifiedImportAndNonemptyImportListWithDifferentVisibilitiesTest = TestCase
  { testName       =
      "Module with unqualified import and nonempty import list with different visibilities"
  , input          =
      "module Test where\n\
      \import Imported1 (foo, Bar(..), Baz(Quux, Fizz), type Typ, type (++), (:$:), (:$$:)(..), (:$$*:)((:$$$*:), (:$$$**:)))"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = Nothing
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = Just ImportList
                  { ilEntries       = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "foo"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Bar"
                          , entryChildrenVisibility = Just VisibleAllChildren
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Baz"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ S.fromList
                              [ mkUnqualSymName "Quux"
                              , mkUnqualSymName "Fizz"
                              ]
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Typ"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "++"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ":$:"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ":$$:"
                          , entryChildrenVisibility = Just VisibleAllChildren
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ":$$*:"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ S.fromList
                              [ mkUnqualSymName ":$$$*:"
                              , mkUnqualSymName ":$$$**:"
                              ]
                          }
                      ]
                  , ilImportType    = Imported
                  , ilImportedNames = ()
                  }
              }
          ]
      }
  }

moduleWithQualifiedImportTest :: Test
moduleWithQualifiedImportTest = TestCase
  { testName       = "Module with qualified import"
  , input          =
      "module ModuleWithQualifiedImport where\n\
      \import qualified Imported1"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithQualifiedImport"
      , mhExports          = Nothing
      , mhImportQualifiers = M.fromList
          [ ( mkImportQualifier $ mkModuleName "Imported1"
            , neSingleton $ mkModuleName "Imported1"
            )
          ]
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification =
                  Qualified $ mkImportQualifier $ mkModuleName "Imported1"
              , ispecImportList    = Nothing
              }
          ]
      }
  }


moduleWithQualifiedSafeAndPackageImportTest :: Test
moduleWithQualifiedSafeAndPackageImportTest = TestCase
  { testName       = "Module with qualified safe import with package import"
  , input          =
      "module ModuleWithQualifiedImport where\n\
      \import safe qualified \"foobar\" Imported1"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithQualifiedImport"
      , mhExports          = Nothing
      , mhImportQualifiers = M.fromList
          [ ( mkImportQualifier $ mkModuleName "Imported1"
            , neSingleton $ mkModuleName "Imported1"
            )
          ]
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification =
                  Qualified $ mkImportQualifier $ mkModuleName "Imported1"
              , ispecImportList    = Nothing
              }
          ]
      }
  }

moduleWithQualifiedImportAndAliasTest :: Test
moduleWithQualifiedImportAndAliasTest = TestCase
  { testName       = "Module with qualified import and alias"
  , input          =
      "module ModuleWithQualifiedImportAndAlias where\n\
      \import qualified Imported1 as Imp"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithQualifiedImportAndAlias"
      , mhExports          = Nothing
      , mhImportQualifiers = M.fromList
          [ ( mkImportQualifier $ mkModuleName "Imp"
            , neSingleton $ mkModuleName "Imported1"
            )
          ]
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification =
                  Qualified $ mkImportQualifier $ mkModuleName "Imp"
              , ispecImportList    = Nothing
              }
          ]
      }
  }

moduleWithImportAndAliasTest :: Test
moduleWithImportAndAliasTest = TestCase
  { testName       = "Module with import and alias"
  , input          =
      "module ModuleWithImportAndAlias where\n\
      \import Imported1 as Imp"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithImportAndAlias"
      , mhExports          = Nothing
      , mhImportQualifiers = M.fromList
          [ ( mkImportQualifier $ mkModuleName "Imp"
            , neSingleton $ mkModuleName "Imported1"
            )
          ]
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification =
                  BothQualifiedAndUnqualified $ mkImportQualifier $ mkModuleName "Imp"
              , ispecImportList    = Nothing
              }
          ]
      }
  }

moduleWithImportAndAliasAndHidingImportListTest :: Test
moduleWithImportAndAliasAndHidingImportListTest = TestCase
  { testName       = "Module with import, alias and hiding import list"
  , input          =
      "module ModuleWithImportAndAliasAandHidingImportList where\n\
      \import Imported1 as Imp hiding (Foo(..), bar, Quux(Baz))"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithImportAndAliasAandHidingImportList"
      , mhExports          = Nothing
      , mhImportQualifiers = M.fromList
          [ ( mkImportQualifier $ mkModuleName "Imp"
            , neSingleton $ mkModuleName "Imported1"
            )
          ]
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification =
                  BothQualifiedAndUnqualified $ mkImportQualifier $ mkModuleName "Imp"
              , ispecImportList    = Just ImportList
                  { ilEntries       = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "Foo"
                          , entryChildrenVisibility = Just VisibleAllChildren
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "bar"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Quux"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ S.fromList
                              [ mkUnqualSymName "Baz"
                              ]
                          }
                      ]
                  , ilImportType    = Hidden
                  , ilImportedNames = ()
                  }
              }
          ]
      }
  }

-- Operators with these names have standalone tokens for them after lexing,
-- so it's important to account for them during header recognition.
moduleWithImportOfSpeciallyNamedOperators :: Test
moduleWithImportOfSpeciallyNamedOperators = TestCase
  { testName       = "Module with import of operators with special names"
  , input          =
      "module ModuleWithImportOfSpeciallyNamedOperators where\n\
      \import Imported1 ((.), (!), (~), (.+.))"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithImportOfSpeciallyNamedOperators"
      , mhExports          = Nothing
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = Just ImportList
                  { ilEntries       = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "."
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "!"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "~"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ".+."
                          , entryChildrenVisibility = Nothing
                          }
                      ]
                  , ilImportType    = Imported
                  , ilImportedNames = ()
                  }
              }
          ]
      }
  }

moduleWithAmbigousImportList :: Test
moduleWithAmbigousImportList = TestCase
  { testName = "Module with ambigous import list"
  , input =
      "module Test where\n\
      \import Imported1 (Foo (:$$:)(..) Bar (:$$$:)(X) Baz (:?:) (:+:)((:++:)))"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = Nothing
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = Just ImportList
                  { ilEntries       = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "Foo"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ":$$:"
                          , entryChildrenVisibility = Just VisibleAllChildren
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Bar"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ":$$$:"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ S.fromList
                              [ mkUnqualSymName "X"
                              ]
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Baz"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ":?:"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ":+:"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ S.fromList
                              [ mkUnqualSymName ":++:"
                              ]
                          }
                      ]
                  , ilImportType    = Imported
                  , ilImportedNames = ()
                  }
              }
          ]
      }
  }

moduleWithImportListWithoutCommas :: Test
moduleWithImportListWithoutCommas = TestCase
  { testName = "Module with import list without commas"
  , input          =
      "module Test where\n\
      \import Imported1 (foo Bar(..) Baz(Quux, Fizz) (:$:) (:$$:)(..) (:$$*:)((:$$$*:), (:$$$**:)))"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = Nothing
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = Just ImportList
                  { ilEntries       = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "foo"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Bar"
                          , entryChildrenVisibility = Just VisibleAllChildren
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Baz"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ S.fromList
                              [ mkUnqualSymName "Quux"
                              , mkUnqualSymName "Fizz"
                              ]
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ":$:"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ":$$:"
                          , entryChildrenVisibility = Just VisibleAllChildren
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ":$$*:"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ S.fromList
                              [ mkUnqualSymName ":$$$*:"
                              , mkUnqualSymName ":$$$**:"
                              ]
                          }
                      ]
                  , ilImportType    = Imported
                  , ilImportedNames = ()
                  }
              }
          ]
      }
  }

moduleWithImportsThatHaveChildrenListWithoutCommas :: Test
moduleWithImportsThatHaveChildrenListWithoutCommas = TestCase
  { testName = "Module with children list without commas"
  , input          =
      "module Test where\n\
      \import Imported1 (Baz(Quux, Fizz), (:$$*:)((:$$$*:) (:$$$**:)))"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = Nothing
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = Just ImportList
                  { ilEntries       = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "Baz"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ S.fromList
                              [ mkUnqualSymName "Quux"
                              , mkUnqualSymName "Fizz"
                              ]
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ":$$*:"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ S.fromList
                              [ mkUnqualSymName ":$$$*:"
                              , mkUnqualSymName ":$$$**:"
                              ]
                          }
                      ]
                  , ilImportType    = Imported
                  , ilImportedNames = ()
                  }
              }
          ]
      }
  }

moduleWithEmptyExportsTest :: Test
moduleWithEmptyExportsTest = TestCase
  { testName       = "Module with empty exports"
  , input          =
      "module ModuleWithEmptyExport () where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithEmptyExport"
      , mhExports          = Just ModuleExports
          { meExportedEntries    = mempty
          , meReexports          = mempty
          , meHasWildcardExports = False
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithQuaifiedExportsTest :: Test
moduleWithQuaifiedExportsTest = TestCase
  { testName       = "Module with qualified exports"
  , input          =
      "module ModuleWithEmptyExport (Foo.bar, Baz.Quux(..)) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithEmptyExport"
      , mhExports          = Just ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = mkSymbolName "Foo.bar"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "Baz.Quux"
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = True
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithExportsTest :: Test
moduleWithExportsTest = TestCase
  { testName       = "Module exports"
  , input          =
      "module ModuleWithExport (foo, Bar(..), Baz(Quux, Fizz), pattern Pat, pattern (!), pattern (:!:), module Frob, type Typ, type (++), (:$:), (:$$:)(..), (:$$*:)((:$$$*:), (:$$$**:))) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = Just ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = mkSymbolName "foo"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "Bar"
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "Baz"
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ S.fromList
                      [ mkUnqualSymName "Quux"
                      , mkUnqualSymName "Fizz"
                      ]
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "Pat"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "!"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName ":!:"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "Typ"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "++"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName ":$:"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName ":$$:"
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName ":$$*:"
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ S.fromList
                      [ mkUnqualSymName ":$$$*:"
                      , mkUnqualSymName ":$$$**:"
                      ]
                  }
              ]
          , meReexports          = S.singleton $ mkModuleName "Frob"
          , meHasWildcardExports = True
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithMultilineExportsTest :: Test
moduleWithMultilineExportsTest = TestCase
  { testName       = "Module with peculiarly indented export list"
  , input          =
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
      \\n\
      \  , pattern     (!)       , \n\
      \             pattern      \n\
      \        (:!:)   \n\
      \  , \n\
      \    module \n\
      \  Frob\n\
      \     ,     \n\
      \         (      :$:     )             , \n\
      \      (   \n\
      \   :$$:      \n\
      \    )      (  ..    ) \n\
      \      ,       (  \n\
      \       :$$*:   )   (  \n\
      \         (:$$$*:)  \n\
      \ , (:$$$**:)   )        \n\
      \   ) \n\
      \   where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = Just ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = mkSymbolName "foo"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "Bar"
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "Baz"
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ S.fromList
                      [ mkUnqualSymName "Quux"
                      , mkUnqualSymName "Fizz"
                      ]
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "Pat"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "!"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName ":!:"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName ":$:"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName ":$$:"
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName ":$$*:"
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ S.fromList
                      [ mkUnqualSymName ":$$$*:"
                      , mkUnqualSymName ":$$$**:"
                      ]
                  }
              ]
          , meReexports          = S.singleton $ mkModuleName "Frob"
          , meHasWildcardExports = True
          }
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.empty
      }
  }

moduleWithExportsOfSpeciallyNamedOperators :: Test
moduleWithExportsOfSpeciallyNamedOperators = TestCase
  { testName       = "Export of operators with special names"
  , input          =
      "module ModuleWithExport ((.), (!), (~), (.+.), (Test..||.)) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = Just ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = mkSymbolName "."
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "!"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "~"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName ".+."
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "Test..||."
                  , entryChildrenVisibility = Nothing
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = False
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithExportListWithoutCommas :: Test
moduleWithExportListWithoutCommas = TestCase
  { testName       = "Module with export list without commas"
  , input          =
      "module ModuleWithExport (foo Bar(..) Baz(Quux, Fizz) pattern Pat pattern (!) pattern (:!:) module Frob (:$:) (:$$:)(..) module Bazzz (:$$*:)((:$$$*:), (:$$$**:)) module Quuxxx) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = Just ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = mkSymbolName "foo"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "Bar"
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "Baz"
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ S.fromList
                      [ mkUnqualSymName "Quux"
                      , mkUnqualSymName "Fizz"
                      ]
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "Pat"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "!"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName ":!:"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName ":$:"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName ":$$:"
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName ":$$*:"
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ S.fromList
                      [ mkUnqualSymName ":$$$*:"
                      , mkUnqualSymName ":$$$**:"
                      ]
                  }
              ]
          , meReexports          = S.fromList
              [ mkModuleName "Frob"
              , mkModuleName "Bazzz"
              , mkModuleName "Quuxxx"
              ]
          , meHasWildcardExports = True
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithExportListWithoutCommasAndStructuresAfterNameWithoutChildrenTest :: Test
moduleWithExportListWithoutCommasAndStructuresAfterNameWithoutChildrenTest = TestCase
  { testName       = "Module with export list without commas and structures after name without children"
  , input          =
      "module ModuleWithExport (foo module Foo (++) module Bar baz pattern Baz quux type Quux pattern Pat module Patterns) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = Just ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = mkSymbolName "foo"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "++"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "baz"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "Baz"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "quux"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "Quux"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "Pat"
                  , entryChildrenVisibility = Nothing
                  }
              ]
          , meReexports          = S.fromList
              [ mkModuleName "Foo"
              , mkModuleName "Bar"
              , mkModuleName "Patterns"
              ]
          , meHasWildcardExports = False
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithExportsThatHaveChildrenListWithoutCommas :: Test
moduleWithExportsThatHaveChildrenListWithoutCommas = TestCase
  { testName       = "Module with exports that have children list without commas"
  , input          =
      "module ModuleWithExport (Bar(..), Baz(Quux Fizz), (:$:), (:$$*:)((:$$$*:) (:$$$**:))) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = Just ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = mkSymbolName "Bar"
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "Baz"
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ S.fromList
                      [ mkUnqualSymName "Quux"
                      , mkUnqualSymName "Fizz"
                      ]
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName ":$:"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName ":$$*:"
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ S.fromList
                      [ mkUnqualSymName ":$$$*:"
                      , mkUnqualSymName ":$$$**:"
                      ]
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = True
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }


importRegressionTests :: TestTree
importRegressionTests = testGroup "tests that caused problems before"
  [ doTest aesonHeaderTest
  , doTest unixCompatHeaderTest
  ]

tests :: TestTree
tests = testGroup "Header analysis tests"
  [ doTest simpleHeaderTest
  , testGroup "imports"
    [ doTest moduleWithUnqualifiedImportTest
    , doTest moduleWithUnqualifiedImportWithSourceTest
    , doTest moduleWithUnqualifiedSafeImportTest
    , doTest moduleWithUnqualifiedImportAndEmptyImportListTest
    , doTest moduleWithUnqualifiedImportAndEmptyHiddenImportListTest
    , doTest moduleWithUnqualifiedImportAndSingletonImportListTest
    , doTest moduleWithUnqualifiedImportAndNonemptyImportListTest
    , doTest moduleWithUnqualifiedImportAndNonemptyImportListWithDifferentVisibilitiesTest
    , doTest moduleWithQualifiedImportTest
    , doTest moduleWithQualifiedSafeAndPackageImportTest
    , doTest moduleWithQualifiedImportAndAliasTest
    , doTest moduleWithImportAndAliasTest
    , doTest moduleWithImportAndAliasAndHidingImportListTest
    , doTest moduleWithImportOfSpeciallyNamedOperators
    , testGroup "malformed import lists"
        [ doTest moduleWithAmbigousImportList
        , doTest moduleWithImportListWithoutCommas
        , doTest moduleWithImportsThatHaveChildrenListWithoutCommas
        ]
    , importRegressionTests
    ]
  , testGroup "exports"
    [ doTest moduleWithEmptyExportsTest
    , doTest moduleWithQuaifiedExportsTest
    , doTest moduleWithExportsTest
    , doTest moduleWithMultilineExportsTest
    , doTest moduleWithExportsOfSpeciallyNamedOperators
    , testGroup "malformed export lists"
        [ doTest moduleWithExportListWithoutCommas
        , doTest moduleWithExportsThatHaveChildrenListWithoutCommas
        , doTest moduleWithExportListWithoutCommasAndStructuresAfterNameWithoutChildrenTest
        ]
    ]
  ]

doTest :: Test -> TestTree
doTest TestCase{testName, input, expectedResult} =
  testCase testName $ do
    let (res, logs) = runWriter $ runSimpleLoggerT (Just (Custom (tell . (:[])))) Debug $ runExceptT $ analyzeHeader =<< tokens
        logsDoc     = "Logs:" PP.<$> PP.indent 2 (PP.vcat logs)
    case res of
      Left msg               -> assertFailure $ displayDocString $ msg PP.<$> logsDoc
      Right (Nothing, _)     -> assertFailure $ displayDocString $
        "No header detected, but was expecting header" PP.<$> pretty expectedResult PP.<$> logsDoc
      Right (Just header, _) -> do
        let msg = ppDict "Headers are different"
              [ name :-> ppAlist ["Actual" :-> x, "Expected" :-> y]
              | (name, different, x, y) <-
                [ ( "ModuleName"
                  , mhModName header /= mhModName expectedResult
                  , pretty $ mhModName header
                  , pretty $ mhModName expectedResult
                  )
                , ( "Exports"
                  , mhExports header /= mhExports expectedResult
                  , pretty $ mhExports header
                  , pretty $ mhExports expectedResult
                  )
                , ( "ImportQualifiers"
                  , mhImportQualifiers header /= mhImportQualifiers expectedResult
                  , ppMap $ ppNE <$> mhImportQualifiers header
                  , ppMap $ ppNE <$> mhImportQualifiers expectedResult
                  )
                , ( "Imports"
                  , mhImports header /= mhImports expectedResult
                  , ppSubkeyMap $ ppNE <$> mhImports header
                  , ppSubkeyMap $ ppNE <$> mhImports expectedResult
                  )
                ]
              , different
              ]
        unless (header == expectedResult) $
          assertFailure $ displayDocString $ msg PP.<$> logsDoc
  where
    tokens :: forall m. (MonadError Doc m) => m [Token]
    tokens = either (throwError . docFromString) return $ tokenizeInput "test" False input
