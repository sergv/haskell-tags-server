----------------------------------------------------------------------------
-- |
-- Module      :  Server.Tags.AnalyzeHeaderTests
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Friday, 23 September 2016
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE MultilineStrings  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskell.Language.Server.Tags.AnalyzeHeaderTests (tests) where

import Control.Arrow
import Control.Monad
import Control.Monad.ErrorExcept
import Control.Monad.Writer

import Data.Foldable
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Semigroup (Any(..))
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Prettyprinter qualified as PP
import Prettyprinter.Ext
import Test.Tasty
import Test.Tasty.HUnit

import Haskell.Language.Lexer (tokenize, modeFromFilename)
import Haskell.Language.Lexer.Types (Pos, ServerToken, Line(..), Type(..))

import Control.Monad.Logging.Simple
import Data.GenericDiff
import Data.KeyMap qualified as KM
import Data.Path
import Data.SubkeyMap qualified as SubkeyMap
import Data.Symbols
import Haskell.Language.Server.Tags.AnalyzeHeader
import Haskell.Language.Server.Tags.Types.Imports
import Haskell.Language.Server.Tags.Types.Modules

import TestUtils
import Haskell.Language.Server.Tags.AnalyzeHeaderTests.Regressions

type Test = TestCase T.Text ModuleHeader

filename :: FullPath 'File
filename = "/foo/bar/test.hs"

pt :: Int -> Type -> PosAndType
pt n = PosAndType filename (Line n)

simpleHeaderTest :: Test
simpleHeaderTest = TestCase
  { testName       = "Simple header"
  , input          =
      "module Foo where"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Foo"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithUnqualifiedImportTest :: Test
moduleWithUnqualifiedImportTest = TestCase
  { testName       = "Unqualified import"
  , input          =
      """
      module ModuleWithUnqualifiedImport where
      import Imported1
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithUnqualifiedImport"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = NoImportList
              }
          ]
      }
  }

moduleWithUnqualifiedSourceImportTest :: Test
moduleWithUnqualifiedSourceImportTest = TestCase
  { testName       = "Unqualified import with {-# SOURCE #-}"
  , input          =
      """
      module ModuleWithUnqualifiedImport where
      import {-# SOURCE #-} Imported1
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithUnqualifiedImport"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = HsBootModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = NoImportList
              }
          ]
      }
  }

moduleWithUnqualifiedSafeImportTest :: Test
moduleWithUnqualifiedSafeImportTest = TestCase
  { testName       = "Unqualified safe import"
  , input          =
      """
      module ModuleWithUnqualifiedImport where
      import safe Imported1
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithUnqualifiedImport"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = NoImportList
              }
          ]
      }
  }

moduleWithPatternImportTest :: Test
moduleWithPatternImportTest = TestCase
  { testName       = "Pattern import"
  , input          =
      """
      module ModuleWithPatternImport where
      import Imported1 (pattern Pat)
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithPatternImport"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "Pat"
                          , entryChildrenVisibility = Nothing
                          }
                      ]
                  , ilImportType = Imported
                  }
              }
          ]
      }
  }

moduleWithUnqualifiedImportAndEmptyImportListTest :: Test
moduleWithUnqualifiedImportAndEmptyImportListTest = TestCase
  { testName       = "Unqualified import and empty import list"
  , input          =
      """
      module Test where
      import Imported1 ()
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = mempty
                  , ilImportType = Imported
                  }
              }
          ]
      }
  }

moduleWithUnqualifiedImportAndEmptyHiddenImportListTest :: Test
moduleWithUnqualifiedImportAndEmptyHiddenImportListTest = TestCase
  { testName       = "Unqualified import and empty hidden import list"
  , input          =
      """
      module Test where
      import Imported1 hiding ()
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = mempty
                  , ilImportType = Hidden
                  }
              }
          ]
      }
  }

moduleWithUnqualifiedImportAndSingletonImportListTest :: Test
moduleWithUnqualifiedImportAndSingletonImportListTest = TestCase
  { testName       = "Unqualified import and singleton import list"
  , input          =
      """
      module Test where
      import Imported1 (foo)
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "foo"
                          , entryChildrenVisibility = Nothing
                          }
                      ]
                  , ilImportType = Imported
                  }
              }
          ]
      }
  }

moduleWithUnqualifiedImportAndNonemptyImportListTest :: Test
moduleWithUnqualifiedImportAndNonemptyImportListTest = TestCase
  { testName       = "Unqualified import and nonempty import list"
  , input          =
      """
      module Test where
      import Imported1 (foo, bar, baz)
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
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
              }
          ]
      }
  }

moduleWithUnqualifiedImportAndNonemptyImportListWithDifferentVisibilitiesTest :: Test
moduleWithUnqualifiedImportAndNonemptyImportListWithDifferentVisibilitiesTest = TestCase
  { testName       =
      "Unqualified import and nonempty import list with different visibilities"
  , input          =
      """
      module Test where
      import Imported1 (foo, Bar(..), Baz(Quux, Fizz), type Typ, type (++), pattern Pat, pattern (:++), (:$:), (:$$:)(..), (:$$*:)((:$$$*:), (:$$$**:)))
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
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
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromSet (const ()) $ S.fromList
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
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromSet (const ()) $ S.fromList
                              [ mkUnqualSymName ":$$$*:"
                              , mkUnqualSymName ":$$$**:"
                              ]
                          }
                      ]
                  , ilImportType = Imported
                  }
              }
          ]
      }
  }

moduleWithPreQualifiedImportTest :: Test
moduleWithPreQualifiedImportTest = TestCase
  { testName       = "Qualified import prefix"
  , input          =
      """
      module ModuleWithQualifiedImport where
      import qualified Imported1
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithQualifiedImport"
      , mhExports          = NoExports
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
              , ispecImportList    = NoImportList
              }
          ]
      }
  }

moduleWithPostQualifiedImportTest :: Test
moduleWithPostQualifiedImportTest = TestCase
  { testName       = "Qualified import postfix"
  , input          =
      """
      module ModuleWithQualifiedImport where
      import Imported1 qualified
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithQualifiedImport"
      , mhExports          = NoExports
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
              , ispecImportList    = NoImportList
              }
          ]
      }
  }

moduleWithQualifiedSafeAndPackageImportTest :: Test
moduleWithQualifiedSafeAndPackageImportTest = TestCase
  { testName       = "Qualified safe import with package import"
  , input          =
      """
      module ModuleWithQualifiedImport where
      import safe qualified "foobar" Imported1
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithQualifiedImport"
      , mhExports          = NoExports
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
              , ispecImportList    = NoImportList
              }
          ]
      }
  }

moduleWithPreQualifiedImportAndAliasTest :: Test
moduleWithPreQualifiedImportAndAliasTest = TestCase
  { testName       = "Prefix qualified import and alias"
  , input          =
      """
      module ModuleWithQualifiedImportAndAlias where
      import qualified Imported1 as Imp
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithQualifiedImportAndAlias"
      , mhExports          = NoExports
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
              , ispecImportList    = NoImportList
              }
          ]
      }
  }

moduleWithPostQualifiedImportAndAliasTest :: Test
moduleWithPostQualifiedImportAndAliasTest = TestCase
  { testName       = "Postfix qualified import and alias"
  , input          =
      """
      module ModuleWithQualifiedImportAndAlias where
      import Imported1 qualified as Imp
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithQualifiedImportAndAlias"
      , mhExports          = NoExports
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
              , ispecImportList    = NoImportList
              }
          ]
      }
  }

moduleWithImportAndAliasTest :: Test
moduleWithImportAndAliasTest = TestCase
  { testName       = "Import and alias"
  , input          =
      """
      module ModuleWithImportAndAlias where
      import Imported1 as Imp
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithImportAndAlias"
      , mhExports          = NoExports
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
              , ispecImportList    = NoImportList
              }
          ]
      }
  }

moduleWithImportAndAliasAndHidingImportListTest :: Test
moduleWithImportAndAliasAndHidingImportListTest = TestCase
  { testName       = "Import, alias and hiding import list"
  , input          =
      """
      module ModuleWithImportAndAliasAandHidingImportList where
      import Imported1 as Imp hiding (Foo(..), bar, Quux(Baz))
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithImportAndAliasAandHidingImportList"
      , mhExports          = NoExports
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
              , ispecImportList    = SpecificImports ImportList
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
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromSet (const ()) $ S.fromList
                              [ mkUnqualSymName "Baz"
                              ]
                          }
                      ]
                  , ilImportType = Hidden
                  }
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
      """
      module ModuleWithImportOfSpeciallyNamedOperators where
      import Imported1 ((.), (!), (~), (.+.))
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithImportOfSpeciallyNamedOperators"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
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
              }
          ]
      }
  }

moduleWithMultipleImports :: Test
moduleWithMultipleImports = TestCase
  { testName       = "Module multiple imports"
  , input          =
      """
      module Test where
      import Mod1
      import Mod2 as Foo

      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = M.fromList
        [ (mkImportQualifier $ mkModuleName "Foo", neSingleton $ mkModuleName "Mod2")
        ]
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Mod1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = NoImportList
              }
          , neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Mod2"
                  }
              , ispecQualification =
                BothQualifiedAndUnqualified $ mkImportQualifier $ mkModuleName "Foo"
              , ispecImportList    = NoImportList
              }
          ]
      }
  }

mkModuleWithImportsAfterDefinitionTest :: String -> T.Text -> Test
mkModuleWithImportsAfterDefinitionTest testName thing = TestCase
  { testName
  , input          =
      """
      module Test where
      import Quux.Mod1


      """
      <> thing <> " :: a -> a\n"
      <> thing <> " x = x\n\n"
      <> "import Mod2 as Foo\n"
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = M.fromList
        [ (mkImportQualifier $ mkModuleName "Foo", neSingleton $ mkModuleName "Mod2")
        ]
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Quux.Mod1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = NoImportList
              }
          , neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Mod2"
                  }
              , ispecQualification =
                BothQualifiedAndUnqualified $ mkImportQualifier $ mkModuleName "Foo"
              , ispecImportList    = NoImportList
              }
          ]
      }
  }

moduleWithImportsAfterFunctionDefinition :: Test
moduleWithImportsAfterFunctionDefinition =
  mkModuleWithImportsAfterDefinitionTest
    "Module imports after function definition"
    "foo"

moduleWithImportsAfterOperatorDefinition :: Test
moduleWithImportsAfterOperatorDefinition =
  mkModuleWithImportsAfterDefinitionTest
    "Module imports after operator definition"
    "(+++)"

moduleWithImportsAfterExclamationMarkOperatorDefinition :: Test
moduleWithImportsAfterExclamationMarkOperatorDefinition =
  mkModuleWithImportsAfterDefinitionTest
    "Module imports after (!) operator definition"
    "(!)"

moduleWithImportsAfterDotOperatorDefinition :: Test
moduleWithImportsAfterDotOperatorDefinition =
  mkModuleWithImportsAfterDefinitionTest
    "Module imports after (.) operator definition"
    "(.)"

-- No reasonable way to make work this work. Just check that we don't crash
-- and attempt something remotely sensible.
moduleWithParensInImportList1 :: Test
moduleWithParensInImportList1 = TestCase
  { testName       = "Import of \"pattern\" function 1"
  , input          =
      """
      module Test where
      import Foo
      #ifdef FOO
        ( foo
      #else
        ( bar
      #endif
        )
      import Bar

      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Foo"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "foo"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromSet (const ()) $ S.fromList
                              [ mkUnqualSymName "bar"
                              ]
                          }
                      ]
                  , ilImportType = Imported
                  }
              }
          , neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Bar"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = NoImportList
              }
          ]
      }
  }

-- No reasonable way to make work this work. Just check that we don't crash
-- and attempt something remotely sensible.
moduleWithParensInImportList2 :: Test
moduleWithParensInImportList2 = TestCase
  { testName       = "Import of \"pattern\" function 2"
  , input          =
      """
      module Test where
      import Foo
      #ifdef FOO
        ( foo
        , bar
      #else
        ( baz
        , quux
      #endif
        , fizz
        )
      import Bar

      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Foo"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "foo"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "bar"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromSet (const ()) $ S.fromList
                              [ mkUnqualSymName name
                              | name <- ["baz", "quux", "fizz"]
                              ]
                          }
                      ]
                  , ilImportType = Imported
                  }
              }
          , neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Bar"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = NoImportList
              }
          ]
      }
  }

moduleWithMultilinePreprocessor :: Test
moduleWithMultilinePreprocessor = TestCase
  { testName       = "Multiline preprocessor"
  , input          =
      """
      module Test where
      import Foo
        ( foo
      #if defined(FOO) \\
        && !BAR
        , bar
      #else
        , baz
      #endif
        , quux
        )
      import Bar

      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Foo"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName name
                          , entryChildrenVisibility = Nothing
                          }
                      | name <- ["foo", "bar", "baz", "quux"]
                      ]
                  , ilImportType = Imported
                  }
              }
          , neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Bar"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = NoImportList
              }
          ]
      }
  }

moduleWithDefineInImportList :: Test
moduleWithDefineInImportList = TestCase
  { testName       = "Module define in import list"
  , input          =
      """
      module Test where
      import Mod
        ( Foo
      #define FOO
        , Bar
      #ifndef BAR
      #define BAR
      #endif
        , Frob1(..)
      #define FOO
        , Frob2(
      #define FOO
              Baz1
      #define FOO
              ,
      #define FOO
      #define FOO
              Baz2
      #define FOO
            )
        )
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Mod"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "Foo"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Bar"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Frob1"
                          , entryChildrenVisibility = Just VisibleAllChildren
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "Frob2"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromSet (const ()) $ S.fromList
                              [ mkUnqualSymName "Baz1"
                              , mkUnqualSymName "Baz2"
                              ]
                          }
                      ]
                  , ilImportType = Imported
                  }
              }
          ]
      }
  }

moduleImportNamespaces :: Test
moduleImportNamespaces = TestCase
  { testName       = "Import with explicit namespaces"
  , input          =
      """
      module ModuleWithImport where
      import Foo
        ( foo
        , type Typ
        , type (++)
        , data Typ2
        , data (:**)
        , C(type (#))
        )
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithImport"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Foo"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "foo"
                          , entryChildrenVisibility = Nothing
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
                          { entryName               = mkUnqualSymName "Typ2"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ":**"
                          , entryChildrenVisibility = Nothing
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName "C"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromSet (const ()) $ S.fromList
                              [ mkUnqualSymName "#"
                              ]
                          }
                      ]
                  , ilImportType = Imported
                  }
              }
          ]
      }
  }

moduleWithImportOfPatternFuncTest :: Test
moduleWithImportOfPatternFuncTest = TestCase
  { testName       = "Import of \"pattern\" function"
  , input          =
      """
      module Test where
      import Imported1 (pattern)
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "pattern"
                          , entryChildrenVisibility = Nothing
                          }
                      ]
                  , ilImportType = Imported
                  }
              }
          ]
      }
  }

moduleWithImportOfManyFuncsAndPatternFuncTest :: Test
moduleWithImportOfManyFuncsAndPatternFuncTest = TestCase
  { testName       = "Import of several functions, including \"pattern\" function"
  , input          =
      """
      module Test where
      import Imported1 (Foo(..), pattern, (++), Bar)
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
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
              }
          ]
      }
  }

moduleWithoutCommasAndPatternFuncImportBeforeOperator :: Test
moduleWithoutCommasAndPatternFuncImportBeforeOperator = TestCase
  { testName       = "Module without commas in import list and import of pattern function before operator function"
  , input          =
      """
      module Test where
      import Imported1 (Foo(..) pattern (++) Bar)
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
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
              }
          ]
      }
  }

moduleWithoutCommasAndPatternFuncImportBeforeConstructorWithChildren :: Test
moduleWithoutCommasAndPatternFuncImportBeforeConstructorWithChildren = TestCase
  { testName       = "Module without commas in import list and import of pattern function before constructor with children"
  , input          =
      """
      module Test where
      import Imported1 (pattern Foo(..) (++) Bar)
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
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
              }
          ]
      }
  }

moduleWithoutCommasAndPatternFuncImportBeforeOperatorConstructorWithChildren :: Test
moduleWithoutCommasAndPatternFuncImportBeforeOperatorConstructorWithChildren = TestCase
  { testName       = "Module without commas in import list and import of pattern function before operator constructor with children"
  , input          =
      """
      module Test where
      import Imported1 (pattern (:++)(..) (++) Bar)
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
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
              }
          ]
      }
  }

moduleWithoutCommasAndSeveralPatternImports :: Test
moduleWithoutCommasAndSeveralPatternImports = TestCase
  { testName       = "Module without commas in import list"
  , input          =
      """
      module Test where
      import Imported1 (Foo(..) pattern (:++) Bar pattern Baz)
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
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
              }
          ]
      }
  }

moduleWithAmbigousImportList :: Test
moduleWithAmbigousImportList = TestCase
  { testName       = "Ambigous import list"
  , input          =
      """
      module Test where
      import Imported1 (Foo (:$$:)(..) Bar (:$$$:)(X) Baz (:?:) (:+:)((:++:)))
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
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
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromSet (const ()) $ S.fromList
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
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromSet (const ()) $ S.fromList
                              [ mkUnqualSymName ":++:"
                              ]
                          }
                      ]
                  , ilImportType = Imported
                  }
              }
          ]
      }
  }

moduleWithImportListWithoutCommas :: Test
moduleWithImportListWithoutCommas = TestCase
  { testName       = "Import list without commas"
  , input          =
      """
      module Test where
      import Imported1 (foo Bar(..) Baz(Quux, Fizz) (:$:) (:$$:)(..) (:$$*:)((:$$$*:), (:$$$**:)) pattern Pat pattern (:++))
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
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
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromSet (const ()) $ S.fromList
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
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromSet (const ()) $ S.fromList
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
              }
          ]
      }
  }

moduleWithImportsThatHaveChildrenListWithoutCommas :: Test
moduleWithImportsThatHaveChildrenListWithoutCommas = TestCase
  { testName       = "Import list where children list has no commas"
  , input          =
      """
      module Test where
      import Imported1 (Baz(Quux, Fizz), (:$$*:)((:$$$*:) (:$$$**:)))
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Imported1"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "Baz"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromSet (const ()) $ S.fromList
                              [ mkUnqualSymName "Quux"
                              , mkUnqualSymName "Fizz"
                              ]
                          }
                      , EntryWithChildren
                          { entryName               = mkUnqualSymName ":$$*:"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromSet (const ()) $ S.fromList
                              [ mkUnqualSymName ":$$$*:"
                              , mkUnqualSymName ":$$$**:"
                              ]
                          }
                      ]
                  , ilImportType = Imported
                  }
              }
          ]
      }
  }

moduleWithUnbalancedParensInImportList :: Test
moduleWithUnbalancedParensInImportList = TestCase
  { testName       = "Module with unbalanced parens in import list"
  , input          =
      """
      module Test where
      import Mod
      #if FOO
        ( Foo(X, Y)
      #else
        ( Foo
      #endif
        , Bar
        )
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Mod"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "Foo"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromSet (const ()) $ S.fromList
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
              }
          ]
      }
  }

moduleWithUnbalancedParensInImportChildrenList :: Test
moduleWithUnbalancedParensInImportChildrenList = TestCase
  { testName       = "Module with unbalanced parens in import children list"
  , input          =
      """
      module Test where
      import Mod
        ( Foo
      #if FOO
          ( X
      #else
          ( Z
      #endif
          , Y
          )
        , Bar
        )
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Mod"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = SpecificImports ImportList
                  { ilEntries    = KM.fromList
                      [ EntryWithChildren
                          { entryName               = mkUnqualSymName "Foo"
                          , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromSet (const ()) $ S.fromList
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
              }
          ]
      }
  }

moduleWithSingleHSC2HSDirectiveInImportList :: Test
moduleWithSingleHSC2HSDirectiveInImportList = TestCase
  { testName       = "Module with single hsc2hs directive in import list"
  , input          =
      """
      module Test where
      import Mod
        ( #{type int64_t}
        )
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Mod"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = AssumedWildcardImportList
              }
          ]
      }
  }

moduleWithSomeHSC2HSDirectivesInImportList1 :: Test
moduleWithSomeHSC2HSDirectivesInImportList1 = TestCase
  { testName       = "Module with some hsc2hs directives in import list #1"
  , input          =
      """
      module Test where
      import Mod
        ( Foo
        , #{type int64_t}
        , Bar
        , #{type baz_t}
        )
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Mod"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = AssumedWildcardImportList
              }
          ]
      }
  }

moduleWithSomeHSC2HSDirectivesInImportList2 :: Test
moduleWithSomeHSC2HSDirectivesInImportList2 = TestCase
  { testName       = "Module with some hsc2hs directives in import list #2"
  , input          =
      """
      module Test where
      import Mod
        ( Foo
        , #{type int64_t}
        , #{type baz_t}
        , Bar
        )
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Mod"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = AssumedWildcardImportList
              }
          ]
      }
  }

moduleWithSomeHSC2HSDirectivesInImportList3 :: Test
moduleWithSomeHSC2HSDirectivesInImportList3 = TestCase
  { testName       = "Module with some hsc2hs directives in import list #3"
  , input          =
      """
      module Test where
      import Mod
        ( pattern Foo
        , #{type int64_t}
        , pattern Bar
        , Frob(..)
        , #{type baz_t}
        )
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = NoExports
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.fromList $ map (ispecImportKey . NE.head &&& id)
          [ neSingleton ImportSpec
              { ispecImportKey     = ImportKey
                  { ikImportTarget = VanillaModule
                  , ikModuleName   = mkModuleName "Mod"
                  }
              , ispecQualification = Unqualified
              , ispecImportList    = AssumedWildcardImportList
              }
          ]
      }
  }

moduleWithEmptyExportsTest :: Test
moduleWithEmptyExportsTest = TestCase
  { testName       = "Empty exports"
  , input          =
      """
      module ModuleWithEmptyExport () where
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithEmptyExport"
      , mhExports          = EmptyExports
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithQuaifiedExportsTest :: Test
moduleWithQuaifiedExportsTest = TestCase
  { testName       = "Qualified exports"
  , input          =
      """
      module ModuleWithEmptyExport (Foo.bar, Baz.Quux(..)) where
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithEmptyExport"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "Foo.bar", pt 1 Function)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Baz.Quux", pt 1 Type)
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = Any True
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithExportsTest :: Test
moduleWithExportsTest = TestCase
  { testName       = "Module exports"
  , input          =
      """
      module ModuleWithExport (foo, Bar(..), Baz(Quux, Fizz, wat, (??)), Frob(.., Frob', Frob''), pattern Pat, pattern (:!:), module Frob, type Typ, type (++), (:$:), (:$$:)(..), (:$$*:)((:$$$*:), (:$$$**:))) where
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "foo", pt 1 Function)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Bar", pt 1 Type)
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Baz", pt 1 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName "Quux", pt 1 Constructor)
                      , (mkUnqualSymName "Fizz", pt 1 Constructor)
                      , (mkUnqualSymName "wat",  pt 1 Function)
                      , (mkUnqualSymName "??",   pt 1 Operator)
                      ]
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Frob", pt 1 Type)
                  , entryChildrenVisibility = Just $ VisibleAllChildrenPlusSome $ M.fromList
                      [ (mkUnqualSymName "Frob'", pt 1 Constructor)
                      , (mkUnqualSymName "Frob''", pt 1 Constructor)
                      ]
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Pat", pt 1 Pattern)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":!:", pt 1 Pattern)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Typ", pt 1 Family)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "++", pt 1 Family)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":$:", pt 1 Type)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":$$:", pt 1 Type)
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":$$*:", pt 1 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName ":$$$*:",  pt 1 Constructor)
                      , (mkUnqualSymName ":$$$**:", pt 1 Constructor)
                      ]
                  }
              ]
          , meReexports          = S.singleton $ mkModuleName "Frob"
          , meHasWildcardExports = Any True
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithMultilineExportsTest :: Test
moduleWithMultilineExportsTest = TestCase
  { testName       = "Peculiarly indented export list"
  , input          =
      """
      module ModuleWithExport
       (
         foo
       ,
        Bar
          (..)
       ,        Baz(
        Quux
          ,
         Fizz
        )
          ,

                         Frob
                           (
                      ..
                   ,
             Frob'
                   ,
              Frob''
          )
                    ,

        pattern
           Pat

                   pattern
              (:!:)
        ,
          module
        Frob
           ,
               (      :$:     )             ,
            (
         :$$:
          )      (  ..    )
            ,       (
             :$$*:   )   (
               (:$$$*:)
       , (:$$$**:)   )
         )
         where
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "foo", pt 3 Function)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Bar", pt 5 Type)
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Baz", pt 7 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName "Quux", pt 8 Constructor)
                      , (mkUnqualSymName "Fizz", pt 10 Constructor)
                      ]
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Frob", pt 14 Type)
                  , entryChildrenVisibility = Just $ VisibleAllChildrenPlusSome $ M.fromList
                      [ (mkUnqualSymName "Frob'",   pt 18 Constructor)
                      , (mkUnqualSymName "Frob''",  pt 20 Constructor)
                      ]
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Pat", pt 25 Pattern)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":!:", pt 28 Pattern)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":$:", pt 33 Type)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":$$:", pt 35 Type)
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":$$*:", pt 38 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName ":$$$*:",  pt 39 Constructor)
                      , (mkUnqualSymName ":$$$**:", pt 40 Constructor)
                      ]
                  }
              ]
          , meReexports          = S.singleton $ mkModuleName "Frob"
          , meHasWildcardExports = Any True
          }
      , mhImportQualifiers = mempty
      , mhImports          = SubkeyMap.empty
      }
  }

moduleWithExportsOfSpeciallyNamedOperatorsTest :: Test
moduleWithExportsOfSpeciallyNamedOperatorsTest = TestCase
  { testName       = "Export of operators with special names"
  , input          =
      """
      module ModuleWithExport ((.), (!), (~), (.+.), (Test..||.)) where
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName ".", pt 1 Operator)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "!", pt 1 Operator)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "~", pt 1 Operator)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ".+.", pt 1 Operator)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Test..||.", pt 1 Operator)
                  , entryChildrenVisibility = Nothing
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = Any False
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleStarExports :: Test
moduleStarExports = TestCase
  { testName       = "Star exports"
  , input          =
      """
      module Data.Kind ( Type, Constraint, type (*), type (★) ) where
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Data.Kind"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName name, pt 1 typ)
                  , entryChildrenVisibility = Nothing
                  }
              | (name, typ) <-
                [ ("Type", Type)
                , ("Constraint", Type)
                , ("*", Family)
                , ("★", Family)
                ]
              ]
          , meReexports          = mempty
          , meHasWildcardExports = Any False
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithTypeExportsTest1 :: Test
moduleWithTypeExportsTest1 = TestCase
  { testName       = "Exports type children"
  , input          =
      """
      module ModuleWithTypeExports
        ( Foo
        , Bar(type Baz)
        )
        where
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithTypeExports"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "Foo", pt 2 Type)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Bar", pt 3 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName "Baz", pt 3 Family)
                      ]
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = Any False
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithTypeExportsTest2 :: Test
moduleWithTypeExportsTest2 = TestCase
  { testName       = "Export type operator children"
  , input          =
      """
      module ModuleWithTypeOpExports
        ( (+)
        , (**)(type (!!))
        , (##)(type (###))
        )
        where
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithTypeOpExports"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "+", pt 2 Operator)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "**", pt 3 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName "!!", pt 3 Family)
                      ]
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "##", pt 4 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName "###", pt 4 Family)
                      ]
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = Any False
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithQualifiedOperatorChildrenExportTest :: Test
moduleWithQualifiedOperatorChildrenExportTest = TestCase
  { testName       = "Export qualified children operator"
  , input          =
      """
      module ModuleWithQualifiedOperatorChildrenExports
        ( (Foo.+)
        , Foo.Bar((Foo.<><>))
        )
        where
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithQualifiedOperatorChildrenExports"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "Foo.+", pt 2 Operator)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Foo.Bar", pt 3 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName "<><>", pt 3 Operator)
                      ]
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = Any False
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithDefineInExportList :: Test
moduleWithDefineInExportList = TestCase
  { testName       = "Module with define in export list"
  , input          =
      """
      module ModuleWithExport
        ( foo
        , Bar(..)
      #define FOO
        , Baz(Quux, Fizz,
      #define BAZ
          wat, (??))
        , Frob(.., Frob', Frob'')
        , pattern Pat
      #ifndef BAR
      #define BAR
      #endif
        , pattern (:!:)
        , module Frob, type Typ
        , type (++)
        , (:$:)
        , (:$$:)(..)
        , (:$$*:)((:$$$*:)
        , (:$$$**:))
        ) where
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "foo", pt 2 Function)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Bar", pt 3 Type)
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Baz", pt 5 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName "Quux", pt 5 Constructor)
                      , (mkUnqualSymName "Fizz", pt 5 Constructor)
                      , (mkUnqualSymName "wat",  pt 7 Function)
                      , (mkUnqualSymName "??",   pt 7 Operator)
                      ]
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Frob", pt 8 Type)
                  , entryChildrenVisibility = Just $ VisibleAllChildrenPlusSome $ M.fromList
                      [ (mkUnqualSymName "Frob'", pt 8 Constructor)
                      , (mkUnqualSymName "Frob''", pt 8 Constructor)
                      ]
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Pat", pt 9 Pattern)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":!:", pt 13 Pattern)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Typ", pt 14 Family)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "++", pt 15 Family)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":$:", pt 16 Type)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":$$:", pt 17 Type)
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":$$*:", pt 18 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName ":$$$*:",  pt 18 Constructor)
                      , (mkUnqualSymName ":$$$**:", pt 19 Constructor)
                      ]
                  }
              ]
          , meReexports          = S.singleton $ mkModuleName "Frob"
          , meHasWildcardExports = Any True
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleExportNamespaces :: Test
moduleExportNamespaces = TestCase
  { testName       = "Export with explicit namespaces"
  , input          =
      """
      module ModuleWithExport
        ( foo
        , type Typ
        , type (++)
        , data Typ2
        , data (:**)
        , C(type (#))
        ) where
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "foo", pt 2 Function)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Typ", pt 3 Family)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "++", pt 4 Family)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Typ2", pt 5 Constructor)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":**", pt 6 Constructor)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "C", pt 7 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName "#", pt 7 Family)
                      ]
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = Any False
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithExportOfPatternFuncTest :: Test
moduleWithExportOfPatternFuncTest = TestCase
  { testName       = "Export of \"pattern\" function"
  , input          =
      """
      module ModuleWithExport (pattern) where
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "pattern", pt 1 Function)
                  , entryChildrenVisibility = Nothing
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = Any False
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithExportOfManyFuncsAndPatternFuncTest :: Test
moduleWithExportOfManyFuncsAndPatternFuncTest = TestCase
  { testName       = "Export of several functions, including \"pattern\" function"
  , input          =
      """
      module ModuleWithExport (Foo(..), pattern, (++), Bar) where
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "Foo", pt 1 Type)
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "pattern", pt 1 Function)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "++", pt 1 Operator)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Bar", pt 1 Type)
                  , entryChildrenVisibility = Nothing
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = Any True
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
      """
      module ModuleWithExport (Foo(..) pattern (++) Bar) where
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "Foo", pt 1 Type)
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "pattern", pt 1 Function)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "++", pt 1 Operator)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Bar", pt 1 Type)
                  , entryChildrenVisibility = Nothing
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = Any True
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
      """
      module ModuleWithExport (pattern Foo(..) (++) Bar) where
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "Foo", pt 1 Type)
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "pattern", pt 1 Function)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "++", pt 1 Operator)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Bar", pt 1 Type)
                  , entryChildrenVisibility = Nothing
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = Any True
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
      """
      module ModuleWithExport (pattern (:++)(..) (++) Bar) where
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName ":++", pt 1 Type)
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "pattern", pt 1 Function)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "++", pt 1 Operator)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Bar", pt 1 Type)
                  , entryChildrenVisibility = Nothing
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = Any True
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
      """
      module ModuleWithExport (Foo(..) pattern (:++) Bar pattern Baz) where
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "Foo", pt 1 Type)
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":++", pt 1 Pattern)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Bar", pt 1 Type)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Baz", pt 1 Pattern)
                  , entryChildrenVisibility = Nothing
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = Any True
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }
moduleWithExportListWithoutCommasTest :: Test
moduleWithExportListWithoutCommasTest = TestCase
  { testName       = "Export list without commas"
  , input          =
      """
      module ModuleWithExport (foo Bar(..) Baz(Quux, Fizz) pattern Pat pattern (:!:) module Frob (:$:) (:$$:)(..) module Bazzz (:$$*:)((:$$$*:), (:$$$**:)) module Quuxxx) where
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "foo", pt 1 Function)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Bar", pt 1 Type)
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Baz", pt 1 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName "Quux", pt 1 Constructor)
                      , (mkUnqualSymName "Fizz", pt 1 Constructor)
                      ]
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Pat", pt 1 Pattern)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":!:", pt 1 Pattern)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":$:", pt 1 Type)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":$$:", pt 1 Type)
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":$$*:", pt 1 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName ":$$$*:",  pt 1 Constructor)
                      , (mkUnqualSymName ":$$$**:", pt 1 Constructor)
                      ]
                  }
              ]
          , meReexports          = S.fromList
              [ mkModuleName name
              | name <- ["Frob", "Bazzz", "Quuxxx"]
              ]
          , meHasWildcardExports = Any True
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithExportListWithoutCommasAndStructuresAfterNameWithoutChildrenTest :: Test
moduleWithExportListWithoutCommasAndStructuresAfterNameWithoutChildrenTest = TestCase
  { testName       = "Export list without commas and structures after name without children"
  , input          =
      """
      module ModuleWithExport (foo module Foo (++) module Bar baz pattern Baz quux type Quux pattern Pat module Patterns) where
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName name, pt 1 typ)
                  , entryChildrenVisibility = Nothing
                  }
              | (name, typ) <-
                [ ("foo",  Function)
                , ("++",   Operator)
                , ("baz",  Function)
                , ("Baz",  Pattern)
                , ("quux", Function)
                , ("Quux", Family)
                , ("Pat",  Pattern)
                ]
              ]
          , meReexports          = S.fromList
              [ mkModuleName name
              | name <- ["Foo", "Bar", "Patterns"]
              ]
          , meHasWildcardExports = Any False
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithUnbalancedParensInExportList :: Test
moduleWithUnbalancedParensInExportList = TestCase
  { testName       = "Module with unbalanced parens in export list"
  , input          =
      """
      module Test
      #if FOO
        ( Foo(X, Y)
      #else
        ( Foo
      #endif
        , Bar
        ) where
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "Foo", pt 3 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName "X", pt 3 Constructor)
                      , (mkUnqualSymName "Y", pt 3 Constructor)
                      ]
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Foo", pt 5 Type)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Bar", pt 7 Type)
                  , entryChildrenVisibility = Nothing
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = Any False
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithUnbalancedParensInExportChildrenList :: Test
moduleWithUnbalancedParensInExportChildrenList = TestCase
  { testName       = "Module with unbalanced parens in export children list"
  , input          =
      """
      module Test
        ( Foo
      #if FOO
            ( X
      #else
            ( Z
      #endif
            , Y
            )
        , Bar
        ) where
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "Foo", pt 2 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName "X", pt 4 Constructor)
                      , (mkUnqualSymName "Y", pt 8 Constructor)
                      , (mkUnqualSymName "Z", pt 6 Constructor)
                      ]
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Bar", pt 10 Type)
                  , entryChildrenVisibility = Nothing
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = Any False
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithDuplicateModuleNameTest :: Test
moduleWithDuplicateModuleNameTest = TestCase
  { testName       = "Module duplicate module name"
  , input          =
      """
      #if FOO
      module Test
      #else
      module Test
      #endif
        ( Foo( X, Y, Z)
        , Bar
        ) where
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "Foo", pt 6 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName "X", pt 6 Constructor)
                      , (mkUnqualSymName "Y", pt 6 Constructor)
                      , (mkUnqualSymName "Z", pt 6 Constructor)
                      ]
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Bar", pt 7 Type)
                  , entryChildrenVisibility = Nothing
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = Any False
          }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
  }

moduleWithExportsThatHaveChildrenListWithoutCommasTest :: Test
moduleWithExportsThatHaveChildrenListWithoutCommasTest = TestCase
  { testName       = "Exports that have children list without commas"
  , input          =
      """
      module ModuleWithExport (Bar(..), Baz(Quux Fizz), (:$:), (:$$*:)((:$$$*:) (:$$$**:))) where
      """
  , expectedResult = ModuleHeader
      { mhModName          = mkModuleName "ModuleWithExport"
      , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KM.fromList
              [ EntryWithChildren
                  { entryName               = (mkSymbolName "Bar", pt 1 Type)
                  , entryChildrenVisibility = Just VisibleAllChildren
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName "Baz", pt 1 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName "Quux", pt 1 Constructor)
                      , (mkUnqualSymName "Fizz", pt 1 Constructor)
                      ]
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":$:", pt 1 Type)
                  , entryChildrenVisibility = Nothing
                  }
              , EntryWithChildren
                  { entryName               = (mkSymbolName ":$$*:", pt 1 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName ":$$$*:",  pt 1 Constructor)
                      , (mkUnqualSymName ":$$$**:", pt 1 Constructor)
                      ]
                  }
              ]
          , meReexports          = mempty
          , meHasWildcardExports = Any True
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
    , doTest moduleWithPreQualifiedImportTest
    , doTest moduleWithPostQualifiedImportTest
    , doTest moduleWithQualifiedSafeAndPackageImportTest
    , doTest moduleWithPreQualifiedImportAndAliasTest
    , doTest moduleWithPostQualifiedImportAndAliasTest
    , doTest moduleWithImportAndAliasTest
    , doTest moduleWithImportAndAliasAndHidingImportListTest
    , doTest moduleWithImportOfSpeciallyNamedOperatorsTest
    , doTest moduleWithMultipleImports
    , doTest moduleWithImportsAfterFunctionDefinition
    , doTest moduleWithImportsAfterOperatorDefinition
    , doTest moduleWithImportsAfterExclamationMarkOperatorDefinition
    , doTest moduleWithImportsAfterDotOperatorDefinition
    , doTest moduleWithParensInImportList1
    , doTest moduleWithParensInImportList2
    , doTest moduleWithMultilinePreprocessor
    , doTest moduleWithDefineInImportList
    , doTest moduleImportNamespaces
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
        , doTest moduleWithSingleHSC2HSDirectiveInImportList
        , doTest moduleWithSomeHSC2HSDirectivesInImportList1
        , doTest moduleWithSomeHSC2HSDirectivesInImportList2
        , doTest moduleWithSomeHSC2HSDirectivesInImportList3
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
    , doTest moduleWithTypeExportsTest1
    , doTest moduleWithTypeExportsTest2
    , doTest moduleWithQualifiedOperatorChildrenExportTest
    , doTest moduleWithDefineInExportList
    , doTest moduleExportNamespaces
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
        , doTest moduleWithDuplicateModuleNameTest
        ]
    ]
  ]

doTest :: HasCallStack => Test -> TestTree
doTest TestCase{testName, input, expectedResult} =
  testCase testName $ do
    (res, logs) <- runWriterT $ runSimpleLoggerT (Just (Custom (tell . (:[])))) Debug $ runErrorExceptT $ do
      (tokens :: [Pos ServerToken]) <-
        case tokenize (modeFromFilename filename) $ TE.encodeUtf8 input of
          Left err -> liftIO $ assertFailure $ renderStringWide $ "Failed to get tokens:" ## pretty err
          Right xs -> pure xs
      analyzeHeader Nothing filename tokens
    let logsDoc = "Logs, size " <> pretty (length logs) <> ":" ## PP.indent 2 (PP.vcat logs)
    case res of
      Left msg          -> assertFailure $ renderStringWide $ pretty msg ## logsDoc
      Right (header, _) -> do
        let header'         = normalizeHeader $ normalizeHeader header
            expectedResult' = normalizeHeader expectedResult
            msg             = ppDictHeader "Headers are different" $
              ("Input" :-> PP.dquotes (pretty input)) :
              [ ppDifference diff
              | diff <- toList $ genericDiff $ ActualExpected header' expectedResult'
              ]
        unless (header' == expectedResult') $
          assertFailure $ renderStringWide $ msg ## logsDoc

normalizeHeader :: ModuleHeader -> ModuleHeader
normalizeHeader mh = mh
  { mhImportQualifiers = fmap NE.sort $ mhImportQualifiers mh
  , mhImports          = fmap NE.sort $ mhImports mh
  }
