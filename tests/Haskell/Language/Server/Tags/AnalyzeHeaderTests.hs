----------------------------------------------------------------------------
-- |
-- Module      :  Server.Tags.AnalyzeHeaderTests
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Friday, 23 September 2016
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskell.Language.Server.Tags.AnalyzeHeaderTests (tests) where

import Control.Arrow
import Control.Monad.Except (throwError)
import Control.Monad.Except.Ext
import Control.Monad.Writer
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Ext
import Test.Tasty
import Test.Tasty.HUnit

import Haskell.Language.Lexer (tokenize)
import Haskell.Language.Lexer.FastTags (Token)

import Control.Monad.Logging.Simple
import Data.ErrorMessage
import qualified Data.KeyMap as KM
import qualified Data.SubkeyMap as SubkeyMap
import Data.Symbols
import Haskell.Language.Server.Tags.AnalyzeHeader
import Haskell.Language.Server.Tags.Types

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
  { testName       = "Unqualified import"
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
              , ispecImportedNames = ()
              }
          ]
      }
  }

moduleWithUnqualifiedSourceImportTest :: Test
moduleWithUnqualifiedSourceImportTest = TestCase
  { testName       = "Unqualified import with {-# SOURCE #-}"
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
              , ispecImportedNames = ()
              }
          ]
      }
  }

moduleWithUnqualifiedSafeImportTest :: Test
moduleWithUnqualifiedSafeImportTest = TestCase
  { testName       = "Unqualified safe import"
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
              , ispecImportedNames = ()
              }
          ]
      }
  }

moduleWithPatternImportTest :: Test
moduleWithPatternImportTest = TestCase
  { testName       = "Pattern import"
  , input          =
      "module ModuleWithPatternImport where\n\
      \import Imported1 (pattern Pat)"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithPatternImport"
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
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "Pat"
                          , entryChildrenVisibility = Nothing
                          }
                      ]
                  , ilImportType = Imported
                  }
              , ispecImportedNames = ()
              }
          ]
      }
  }

moduleWithUnqualifiedImportAndEmptyImportListTest :: Test
moduleWithUnqualifiedImportAndEmptyImportListTest = TestCase
  { testName       = "Unqualified import and empty import list"
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
                  { ilEntries    = mempty
                  , ilImportType = Imported
                  }
              , ispecImportedNames = ()
              }
          ]
      }
  }

moduleWithUnqualifiedImportAndEmptyHiddenImportListTest :: Test
moduleWithUnqualifiedImportAndEmptyHiddenImportListTest = TestCase
  { testName       = "Unqualified import and empty hidden import list"
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
                  { ilEntries    = mempty
                  , ilImportType = Hidden
                  }
              , ispecImportedNames = ()
              }
          ]
      }
  }

moduleWithUnqualifiedImportAndSingletonImportListTest :: Test
moduleWithUnqualifiedImportAndSingletonImportListTest = TestCase
  { testName       = "Unqualified import and singleton import list"
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
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "foo"
                          , entryChildrenVisibility = Nothing
                          }
                      ]
                  , ilImportType = Imported
                  }
              , ispecImportedNames = ()
              }
          ]
      }
  }

moduleWithUnqualifiedImportAndNonemptyImportListTest :: Test
moduleWithUnqualifiedImportAndNonemptyImportListTest = TestCase
  { testName       = "Unqualified import and nonempty import list"
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
                  { ilEntries    = KM.fromList
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
                  , ilImportType = Imported
                  }
              , ispecImportedNames = ()
              }
          ]
      }
  }

moduleWithUnqualifiedImportAndNonemptyImportListWithDifferentVisibilitiesTest :: Test
moduleWithUnqualifiedImportAndNonemptyImportListWithDifferentVisibilitiesTest = TestCase
  { testName       =
      "Unqualified import and nonempty import list with different visibilities"
  , input          =
      "module Test where\n\
      \import Imported1 (foo, Bar(..), Baz(Quux, Fizz), type Typ, type (++), pattern Pat, pattern (:++), (:$:), (:$$:)(..), (:$$*:)((:$$$*:), (:$$$**:)))"
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
                  { ilEntries    = KM.fromList
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
                          { entryName               = mkUnqualSymName "Pat"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ":++"
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
                  , ilImportType = Imported
                  }
              , ispecImportedNames = ()
              }
          ]
      }
  }

moduleWithQualifiedImportTest :: Test
moduleWithQualifiedImportTest = TestCase
  { testName       = "Qualified import"
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
              , ispecImportedNames = ()
              }
          ]
      }
  }


moduleWithQualifiedSafeAndPackageImportTest :: Test
moduleWithQualifiedSafeAndPackageImportTest = TestCase
  { testName       = "Qualified safe import with package import"
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
              , ispecImportedNames = ()
              }
          ]
      }
  }

moduleWithQualifiedImportAndAliasTest :: Test
moduleWithQualifiedImportAndAliasTest = TestCase
  { testName       = "Qualified import and alias"
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
              , ispecImportedNames = ()
              }
          ]
      }
  }

moduleWithImportAndAliasTest :: Test
moduleWithImportAndAliasTest = TestCase
  { testName       = "Import and alias"
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
              , ispecImportedNames = ()
              }
          ]
      }
  }

moduleWithImportAndAliasAndHidingImportListTest :: Test
moduleWithImportAndAliasAndHidingImportListTest = TestCase
  { testName       = "Import, alias and hiding import list"
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
                  { ilEntries    = KM.fromList
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
                  , ilImportType = Hidden
                  }
              , ispecImportedNames = ()
              }
          ]
      }
  }

-- Operators with these names have standalone tokens for them after lexing,
-- so it's important to account for them during header recognition.
moduleWithImportOfSpeciallyNamedOperatorsTest :: Test
moduleWithImportOfSpeciallyNamedOperatorsTest = TestCase
  { testName       = "import of operators with special names"
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
                  { ilEntries    = KM.fromList
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
                  , ilImportType = Imported
                  }
              , ispecImportedNames = ()
              }
          ]
      }
  }

moduleWithImportOfPatternFuncTest :: Test
moduleWithImportOfPatternFuncTest = TestCase
  { testName       = "Import of \"pattern\" function"
  , input          =
      "module Test where\n\
      \import Imported1 (pattern)"
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
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "pattern"
                          , entryChildrenVisibility = Nothing
                          }
                      ]
                  , ilImportType = Imported
                  }
              , ispecImportedNames = ()
              }
          ]
      }
  }

moduleWithImportOfManyFuncsAndPatternFuncTest :: Test
moduleWithImportOfManyFuncsAndPatternFuncTest = TestCase
  { testName       = "Import of several functions, including \"pattern\" function"
  , input          =
      "module Test where\n\
      \import Imported1 (Foo(..), pattern, (++), Bar)"
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
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "Foo"
                          , entryChildrenVisibility = Just VisibleAllChildren
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "pattern"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "++"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Bar"
                          , entryChildrenVisibility = Nothing
                          }
                      ]
                  , ilImportType = Imported
                  }
              , ispecImportedNames = ()
              }
          ]
      }
  }

moduleWithoutCommasAndPatternFuncImportBeforeOperator :: Test
moduleWithoutCommasAndPatternFuncImportBeforeOperator = TestCase
  { testName       = "Module without commas in import list and import of pattern function before operator function"
  , input          =
      "module Test where\n\
      \import Imported1 (Foo(..) pattern (++) Bar)"
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
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "Foo"
                          , entryChildrenVisibility = Just VisibleAllChildren
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "pattern"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "++"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Bar"
                          , entryChildrenVisibility = Nothing
                          }
                      ]
                  , ilImportType = Imported
                  }
              , ispecImportedNames = ()
              }
          ]
      }
  }

moduleWithoutCommasAndPatternFuncImportBeforeConstructorWithChildren :: Test
moduleWithoutCommasAndPatternFuncImportBeforeConstructorWithChildren = TestCase
  { testName       = "Module without commas in import list and import of pattern function before constructor with children"
  , input          =
      "module Test where\n\
      \import Imported1 (pattern Foo(..) (++) Bar)"
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
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "Foo"
                          , entryChildrenVisibility = Just VisibleAllChildren
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "pattern"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "++"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Bar"
                          , entryChildrenVisibility = Nothing
                          }
                      ]
                  , ilImportType = Imported
                  }
              , ispecImportedNames = ()
              }
          ]
      }
  }

moduleWithoutCommasAndPatternFuncImportBeforeOperatorConstructorWithChildren :: Test
moduleWithoutCommasAndPatternFuncImportBeforeOperatorConstructorWithChildren = TestCase
  { testName       = "Module without commas in import list and import of pattern function before operator constructor with children"
  , input          =
      "module Test where\n\
      \import Imported1 (pattern (:++)(..) (++) Bar)"
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
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName ":++"
                          , entryChildrenVisibility = Just VisibleAllChildren
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "pattern"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "++"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Bar"
                          , entryChildrenVisibility = Nothing
                          }
                      ]
                  , ilImportType = Imported
                  }
              , ispecImportedNames = ()
              }
          ]
      }
  }

moduleWithoutCommasAndSeveralPatternImports :: Test
moduleWithoutCommasAndSeveralPatternImports = TestCase
  { testName       = "Module without commas in import list"
  , input          =
      "module Test where\n\
      \import Imported1 (Foo(..) pattern (:++) Bar pattern Baz)"
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
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "Foo"
                          , entryChildrenVisibility = Just VisibleAllChildren
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ":++"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Bar"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Baz"
                          , entryChildrenVisibility = Nothing
                          }
                      ]
                  , ilImportType = Imported
                  }
              , ispecImportedNames = ()
              }
          ]
      }
  }

moduleWithAmbigousImportList :: Test
moduleWithAmbigousImportList = TestCase
  { testName       = "Ambigous import list"
  , input          =
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
                  { ilEntries    = KM.fromList
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
                  , ilImportType = Imported
                  }
              , ispecImportedNames = ()
              }
          ]
      }
  }

moduleWithImportListWithoutCommas :: Test
moduleWithImportListWithoutCommas = TestCase
  { testName       = "Import list without commas"
  , input          =
      "module Test where\n\
      \import Imported1 (foo Bar(..) Baz(Quux, Fizz) (:$:) (:$$:)(..) (:$$*:)((:$$$*:), (:$$$**:)) pattern Pat pattern (:++))"
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
                  { ilEntries    = KM.fromList
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
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Pat"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ":++"
                          , entryChildrenVisibility = Nothing
                          }
                      ]
                  , ilImportType = Imported
                  }
              , ispecImportedNames = ()
              }
          ]
      }
  }

moduleWithImportsThatHaveChildrenListWithoutCommas :: Test
moduleWithImportsThatHaveChildrenListWithoutCommas = TestCase
  { testName       = "Import list where children list has no commas"
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
                  { ilEntries    = KM.fromList
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
                  , ilImportType = Imported
                  }
              , ispecImportedNames = ()
              }
          ]
      }
  }

moduleWithUnbalancedParensInImportList :: Test
moduleWithUnbalancedParensInImportList = TestCase
  { testName       = "Module with unbalanced parens in import list"
  , input          =
      "module Test where \n\
      \import Mod\n\
      \#if FOO\n\
      \  ( Foo(X, Y)\n\
      \#else\n\
      \  ( Foo\n\
      \#endif\n\
      \  , Bar\n\
      \  )"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = Nothing
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Mod"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = Just ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "Foo"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ S.fromList
                              [ mkUnqualSymName "X"
                              , mkUnqualSymName "Y"
                              ]
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Foo"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Bar"
                          , entryChildrenVisibility = Nothing
                          }
                      ]
                  , ilImportType = Imported
                  }
              , ispecImportedNames = ()
              }
          ]
      }
  }

moduleWithUnbalancedParensInImportChildrenList :: Test
moduleWithUnbalancedParensInImportChildrenList = TestCase
  { testName       = "Module with unbalanced parens in import children list"
  , input          =
      "module Test where \n\
      \import Mod\n\
      \  ( Foo\n\
      \#if FOO\n\
      \    ( X\n\
      \#else\n\
      \    ( Z\n\
      \#endif\n\
      \    , Y\n\
      \    )\n\
      \  , Bar\n\
      \  )"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = Nothing
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Mod"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = Just ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "Foo"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ S.fromList
                              [ mkUnqualSymName "X"
                              , mkUnqualSymName "Y"
                              , mkUnqualSymName "Z"
                              ]
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Bar"
                          , entryChildrenVisibility = Nothing
                          }
                      ]
                  , ilImportType = Imported
                  }
              , ispecImportedNames = ()
              }
          ]
      }
  }


moduleWithEmptyExportsTest :: Test
moduleWithEmptyExportsTest = TestCase
  { testName       = "Empty exports"
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
  { testName       = "Qualified exports"
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
      "module ModuleWithExport (foo, Bar(..), Baz(Quux, Fizz), Frob(.., Frob', Frob''), pattern Pat, pattern (:!:), module Frob, type Typ, type (++), (:$:), (:$$:)(..), (:$$*:)((:$$$*:), (:$$$**:))) where"
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
                  { entryName               = mkSymbolName "Frob"
                  , entryChildrenVisibility = Just $ VisibleAllChildrenPlusSome $ S.fromList
                      [ mkUnqualSymName "Frob'"
                      , mkUnqualSymName "Frob''"
                      ]
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "Pat"
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
  { testName       = "Peculiarly indented export list"
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
      \\n\
      \                   Frob         \n\
      \                     (       \n\
      \                ..  \n\
      \             ,                \n\
      \       Frob'      \n\
      \             ,                  \n\
      \        Frob''            \n\
      \    )                                       \n\
      \              , \n\
      \\n\
      \  pattern\n\
      \     Pat\n\
      \\n\
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
                  { entryName               = mkSymbolName "Frob"
                  , entryChildrenVisibility = Just $ VisibleAllChildrenPlusSome $ S.fromList
                      [ mkUnqualSymName "Frob'"
                      , mkUnqualSymName "Frob''"
                      ]
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "Pat"
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

moduleWithExportsOfSpeciallyNamedOperatorsTest :: Test
moduleWithExportsOfSpeciallyNamedOperatorsTest = TestCase
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

moduleStarExports :: Test
moduleStarExports = TestCase
  { testName       = "Star exports"
  , input          =
      "module Data.Kind ( Type, Constraint, type (*), type (★) ) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Data.Kind"
      , mhExports          = Just ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = mkSymbolName name
                  , entryChildrenVisibility = Nothing
                  }
              | name <- ["Type", "Constraint", "*", "★"]
              ]
          , meReexports          = mempty
          , meHasWildcardExports = False
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithExportOfPatternFuncTest :: Test
moduleWithExportOfPatternFuncTest = TestCase
  { testName       = "Export of \"pattern\" function"
  , input          =
      "module ModuleWithExport (pattern) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = Just ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = mkSymbolName "pattern"
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

moduleWithExportOfManyFuncsAndPatternFuncTest :: Test
moduleWithExportOfManyFuncsAndPatternFuncTest = TestCase
  { testName       = "Export of several functions, including \"pattern\" function"
  , input          =
      "module ModuleWithExport (Foo(..), pattern, (++), Bar) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = Just ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = mkSymbolName "Foo"
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "pattern"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "++"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "Bar"
                  , entryChildrenVisibility = Nothing
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = True
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithoutCommasAndPatternFuncExportBeforeOperator :: Test
moduleWithoutCommasAndPatternFuncExportBeforeOperator = TestCase
  { testName       =
      "Module without commas in export list and export of pattern function before operator function"
  , input          =
      "module ModuleWithExport (Foo(..) pattern (++) Bar) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = Just ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = mkSymbolName "Foo"
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "pattern"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "++"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "Bar"
                  , entryChildrenVisibility = Nothing
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = True
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithoutCommasAndPatternFuncExportBeforeConstructorWithChildren :: Test
moduleWithoutCommasAndPatternFuncExportBeforeConstructorWithChildren = TestCase
  { testName       =
      "Module without commas in export list and export of pattern function before constructor with children"
  , input          =
      "module ModuleWithExport (pattern Foo(..) (++) Bar) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = Just ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = mkSymbolName "Foo"
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "pattern"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "++"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "Bar"
                  , entryChildrenVisibility = Nothing
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = True
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithoutCommasAndPatternFuncExportBeforeOperatorConstructorWithChildren :: Test
moduleWithoutCommasAndPatternFuncExportBeforeOperatorConstructorWithChildren = TestCase
  { testName       =
      "Module without commas in export list and export of pattern function before operator constructor with children"
  , input          =
      "module ModuleWithExport (pattern (:++)(..) (++) Bar) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = Just ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = mkSymbolName ":++"
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "pattern"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "++"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "Bar"
                  , entryChildrenVisibility = Nothing
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = True
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithoutCommasAndSeveralPatternExports :: Test
moduleWithoutCommasAndSeveralPatternExports = TestCase
  { testName       =
      "Export of several functions without commas, including \"pattern\" function"
  , input          =
      "module ModuleWithExport (Foo(..) pattern (:++) Bar pattern Baz) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = Just ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = mkSymbolName "Foo"
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName ":++"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "Bar"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "Baz"
                  , entryChildrenVisibility = Nothing
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = True
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }
moduleWithExportListWithoutCommasTest :: Test
moduleWithExportListWithoutCommasTest = TestCase
  { testName       = "Export list without commas"
  , input          =
      "module ModuleWithExport (foo Bar(..) Baz(Quux, Fizz) pattern Pat pattern (:!:) module Frob (:$:) (:$$:)(..) module Bazzz (:$$*:)((:$$$*:), (:$$$**:)) module Quuxxx) where"
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
              [ mkModuleName name
              | name <- ["Frob", "Bazzz", "Quuxxx"]
              ]
          , meHasWildcardExports = True
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithExportListWithoutCommasAndStructuresAfterNameWithoutChildrenTest :: Test
moduleWithExportListWithoutCommasAndStructuresAfterNameWithoutChildrenTest = TestCase
  { testName       = "Export list without commas and structures after name without children"
  , input          =
      "module ModuleWithExport (foo module Foo (++) module Bar baz pattern Baz quux type Quux pattern Pat module Patterns) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = Just ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = mkSymbolName name
                  , entryChildrenVisibility = Nothing
                  }
              | name <- ["foo", "++", "baz", "Baz", "quux", "Quux", "Pat"]
              ]
          , meReexports          = S.fromList
              [ mkModuleName name
              | name <- ["Foo", "Bar", "Patterns"]
              ]
          , meHasWildcardExports = False
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithUnbalancedParensInExportList :: Test
moduleWithUnbalancedParensInExportList = TestCase
  { testName       = "Module with unbalanced parens in export list"
  , input          =
      "module Test\n\
      \#if FOO\n\
      \  ( Foo(X, Y)\n\
      \#else\n\
      \  ( Foo\n\
      \#endif\n\
      \  , Bar\n\
      \  ) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = Just ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = mkSymbolName "Foo"
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ S.fromList
                      [ mkUnqualSymName "X"
                      , mkUnqualSymName "Y"
                      ]
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "Foo"
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "Bar"
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

moduleWithUnbalancedParensInExportChildrenList :: Test
moduleWithUnbalancedParensInExportChildrenList = TestCase
  { testName       = "Module with unbalanced parens in export children list"
  , input          =
      "module Test\n\
      \  ( Foo\n\
      \#if FOO\n\
      \      ( X\n\
      \#else\n\
      \      ( Z\n\
      \#endif\n\
      \      , Y\n\
      \      )\n\
      \  , Bar\n\
      \  ) where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = Just ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = mkSymbolName "Foo"
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ S.fromList
                      [ mkUnqualSymName "X"
                      , mkUnqualSymName "Y"
                      , mkUnqualSymName "Z"
                      ]
                  }
              , EntryWithChildren
                  { entryName               = mkSymbolName "Bar"
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


moduleWithExportsThatHaveChildrenListWithoutCommasTest :: Test
moduleWithExportsThatHaveChildrenListWithoutCommasTest = TestCase
  { testName       = "Exports that have children list without commas"
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
    , doTest moduleWithUnqualifiedSourceImportTest
    , doTest moduleWithUnqualifiedSafeImportTest
    , doTest moduleWithPatternImportTest
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
    , doTest moduleWithImportOfSpeciallyNamedOperatorsTest
    , testGroup "pattern as a function name"
        [ doTest moduleWithImportOfPatternFuncTest
        , doTest moduleWithImportOfManyFuncsAndPatternFuncTest
        , doTest moduleWithoutCommasAndPatternFuncImportBeforeOperator
        , doTest moduleWithoutCommasAndPatternFuncImportBeforeConstructorWithChildren
        , doTest moduleWithoutCommasAndPatternFuncImportBeforeOperatorConstructorWithChildren
        , doTest moduleWithoutCommasAndSeveralPatternImports
        ]
    , testGroup "malformed import lists"
        [ doTest moduleWithAmbigousImportList
        , doTest moduleWithImportListWithoutCommas
        , doTest moduleWithImportsThatHaveChildrenListWithoutCommas
        , doTest moduleWithUnbalancedParensInImportList
        , doTest moduleWithUnbalancedParensInImportChildrenList
        ]
    , importRegressionTests
    ]
  , testGroup "exports"
    [ doTest moduleWithEmptyExportsTest
    , doTest moduleWithQuaifiedExportsTest
    , doTest moduleWithExportsTest
    , doTest moduleWithMultilineExportsTest
    , doTest moduleWithExportsOfSpeciallyNamedOperatorsTest
    , doTest moduleStarExports
    , testGroup "pattern as a function name"
        [ doTest moduleWithExportOfPatternFuncTest
        , doTest moduleWithExportOfManyFuncsAndPatternFuncTest
        , doTest moduleWithoutCommasAndPatternFuncExportBeforeOperator
        , doTest moduleWithoutCommasAndPatternFuncExportBeforeConstructorWithChildren
        , doTest moduleWithoutCommasAndPatternFuncExportBeforeOperatorConstructorWithChildren
        , doTest moduleWithoutCommasAndSeveralPatternExports
        ]
    , testGroup "malformed export lists"
        [ doTest moduleWithExportListWithoutCommasTest
        , doTest moduleWithExportsThatHaveChildrenListWithoutCommasTest
        , doTest moduleWithExportListWithoutCommasAndStructuresAfterNameWithoutChildrenTest
        , doTest moduleWithUnbalancedParensInExportList
        , doTest moduleWithUnbalancedParensInExportChildrenList
        ]
    ]
  ]

doTest :: HasCallStack => Test -> TestTree
doTest TestCase{testName, input, expectedResult} =
  testCase testName $ do
    let (res, logs) = runWriter $ runSimpleLoggerT (Just (Custom (tell . (:[])))) Debug $ runExceptT $ analyzeHeader =<< tokens
        logsDoc     = "Logs, size " <> pretty (length logs) <> ":" <> PP.line <> PP.indent 2 (PP.vcat logs)
    case res of
      Left msg               -> assertFailure $ displayDocString $ pretty msg <> PP.line <> logsDoc
      Right (Nothing, _)     -> assertFailure $ displayDocString $
        "No header detected, but was expecting header" <> PP.line <> pretty expectedResult <> PP.line <> logsDoc
      Right (Just header, _) -> do
        let msg = ppDictHeader "Headers are different" $
              ("Input" :-> PP.dquotes (pretty input)) :
              [ name :-> ppDictAssocList ["Actual" :-> x, "Expected" :-> y]
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
                  , ppMapWith pretty ppNE $ mhImportQualifiers header
                  , ppMapWith pretty ppNE $ mhImportQualifiers expectedResult
                  )
                , ( "Imports"
                  , mhImports header /= mhImports expectedResult
                  , ppSubkeyMapWith pretty pretty ppNE $ mhImports header
                  , ppSubkeyMapWith pretty pretty ppNE $ mhImports expectedResult
                  )
                ]
              , different
              ]
        unless (header == expectedResult) $
          assertFailure $ displayDocString $ msg <> PP.line <> logsDoc
  where
    tokens :: forall m. MonadError ErrorMessage m => m [Token]
    tokens = either throwError pure $ tokenize "test.hs" input
