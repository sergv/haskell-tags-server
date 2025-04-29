-- |
-- Module:     Haskell.Language.ModuleTests
-- Copyright:  (c) Sergey Vinokurov 2025
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE MultilineStrings  #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Haskell.Language.ModuleTests (tests) where

import Prelude hiding (mod)

import Control.Monad
import Control.Monad.ErrorExcept
import Control.Monad.Writer
import Data.Foldable
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Prettyprinter qualified as PP
import Prettyprinter.Ext
import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Logging.Simple
import Data.ErrorMessage
import Data.GenericDiff
import Data.KeyMap qualified as KeyMap
import Data.Map.NonEmpty (NonEmptyMap)
import Data.Map.NonEmpty qualified as NEMap
import Data.Path
import Data.SubkeyMap qualified as SubkeyMap
import Data.SymbolMap qualified as SymbolMap
import Data.Symbols
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.Clock
import FasterRicherTags.Types
import Haskell.Language.Server.Tags.LoadModule (loadModuleFromSource)
import Haskell.Language.Server.Tags.Types.Imports
import Haskell.Language.Server.Tags.Types.Modules as Mods

import TestUtils

type Test = TestCase Text (NonEmptyMap ModuleName UnresolvedModule)

filename :: FullPath 'File
filename = "/foo/bar/test.hs"

instance Pretty UTCTime where
  pretty = ppUTCTimeISO8601

pt :: Int -> Type -> PosAndType
pt n = PosAndType filename (Line n)

mkSingleton :: UnresolvedModule -> NonEmptyMap ModuleName UnresolvedModule
mkSingleton mod = NEMap.singleton (mhModName (modHeader mod)) mod

defaultModHeader :: ModuleHeader
defaultModHeader = ModuleHeader
  { mhModName          = mkModuleName "Foo"
  , mhExports          = NoExports
  , mhImportQualifiers = mempty
  , mhImports          = mempty
  }

zeroTime :: UTCTime
zeroTime = UTCTime
  { utctDay     = fromOrdinalDate 2000 1
  , utctDayTime = 0
  }

defaltMod :: UnresolvedModule
defaltMod = Mods.Module
  { modHeader           = defaultModHeader
  , modAllSymbols       = mempty
  , modFile             = filename
  , modLastModified     = zeroTime
  , modAllExportedNames = ()
  , modIsDirty          = False
  }

emptyModuleTest :: Test
emptyModuleTest = TestCase
  { testName       = "Empty module"
  , input          =
      "module Foo where"
  , expectedResult = mkSingleton $ defaltMod
    { modHeader = defaultModHeader
      { mhModName          = mkModuleName "Foo"
      , mhExports          = NoExports
      }
    }
  }

simpleModuleTest :: Test
simpleModuleTest = TestCase
  { testName       = "Simple regular module 1"
  , input          =
      """
      module Foo where

      import Bar (xyz)

      foo :: Int -> Int
      foo = id
      """
  , expectedResult = mkSingleton $ defaltMod
    { modHeader     = defaultModHeader
      { mhModName = mkModuleName "Foo"
      , mhExports = NoExports
      , mhImports = SubkeyMap.fromList
        [ let key = ImportKey VanillaModule (mkModuleName "Bar") in
          ( key
          , NE.singleton $ ImportSpec key Unqualified $ SpecificImports $ ImportList
              { ilImportType = Imported
              , ilEntries    = KeyMap.fromList
                [ EntryWithChildren (mkSymName "xyz") Nothing
                ]
              }
          )
        ]
      }
    , modAllSymbols = SymbolMap.fromList
        [ mkResolvedSymbolFromParts filename (Line 5) (mkSymName "foo") Function Nothing
        ]
    }
  }

simpleModuleWithIf0Test1 :: Test
simpleModuleWithIf0Test1 = TestCase
  { testName       = "Simple regular module with ‘#if 0’ 1"
  , input          =
      """
      module Foo where

      import Bar (xyz)

      #if 0
      import Baz (xyz2)

      foo :: Int -> Int
      foo = id
      #endif

      bar :: Int -> Int
      bar = id
      """
  , expectedResult = mkSingleton $ defaltMod
    { modHeader     = defaultModHeader
      { mhModName = mkModuleName "Foo"
      , mhExports = NoExports
      , mhImports = SubkeyMap.fromList
        [ let key = ImportKey VanillaModule (mkModuleName "Bar") in
          ( key
          , NE.singleton $ ImportSpec key Unqualified $ SpecificImports $ ImportList
              { ilImportType = Imported
              , ilEntries    = KeyMap.fromList
                [ EntryWithChildren (mkSymName "xyz") Nothing
                ]
              }
          )
        ]
      }
    , modAllSymbols = SymbolMap.fromList
        [ mkResolvedSymbolFromParts filename (Line 12) (mkSymName "bar") Function Nothing
        ]
    }
  }

simpleModuleWithIf0Test2 :: Test
simpleModuleWithIf0Test2 = TestCase
  { testName       = "Simple regular module with ‘#if 0’ 2"
  , input          =
      """
      module Foo where

      import Bar (xyz)

      #if defined(FOO)
      baz :: Int -> Int
      baz = id

      #if 0
      import Baz (xyz2)

      foo :: Int -> Int
      foo = id
      #endif
      #endif

      bar :: Int -> Int
      bar = id
      """
  , expectedResult = mkSingleton $ defaltMod
    { modHeader     = defaultModHeader
      { mhModName = mkModuleName "Foo"
      , mhExports = NoExports
      , mhImports = SubkeyMap.fromList
        [ let key = ImportKey VanillaModule (mkModuleName "Bar") in
          ( key
          , NE.singleton $ ImportSpec key Unqualified $ SpecificImports $ ImportList
              { ilImportType = Imported
              , ilEntries    = KeyMap.fromList
                [ EntryWithChildren (mkSymName "xyz") Nothing
                ]
              }
          )
        ]
      }
    , modAllSymbols = SymbolMap.fromList
        [ mkResolvedSymbolFromParts filename (Line 17) (mkSymName "bar") Function Nothing
        , mkResolvedSymbolFromParts filename (Line 6) (mkSymName "baz") Function Nothing
        ]
    }
  }

recordFieldsTest :: Test
recordFieldsTest = TestCase
  { testName       = "Record fields"
  , input          =
      """
      module Foo where

      foo :: Int -> Int
      foo = id

      data Foo = Foo
        { bar :: Int
        , baz :: Double
        }
      """
  , expectedResult = mkSingleton $ defaltMod
    { modHeader     = defaultModHeader
      { mhModName = mkModuleName "Foo"
      , mhExports = NoExports
      }
    , modAllSymbols = SymbolMap.fromList
        [ mkResolvedSymbolFromParts filename (Line 3) (mkSymName "foo") Function Nothing
        , mkResolvedSymbolFromParts filename (Line 6) (mkSymName "Foo") Constructor (Just (ParentTag "Foo" Type))
        , mkResolvedSymbolFromParts filename (Line 6) (mkSymName "Foo") Type Nothing
        , mkResolvedSymbolFromParts filename (Line 7) (mkSymName "bar") Function (Just (ParentTag "Foo" Type))
        , mkResolvedSymbolFromParts filename (Line 8) (mkSymName "baz") Function (Just (ParentTag "Foo" Type))
        ]
    }
  }

preprocessorInImportListsIsNotLost :: Test
preprocessorInImportListsIsNotLost = TestCase
  { testName       = "Preprocessor in import list is not lost"
  , input          =
      """
      module Foo where

      import Bar
        ( xyz1
      #define FOO
        , xyz2
        )

      foo :: Int -> Int
      foo = id
      """
  , expectedResult = mkSingleton $ defaltMod
    { modHeader     = defaultModHeader
      { mhModName = mkModuleName "Foo"
      , mhExports = NoExports
      , mhImports = SubkeyMap.fromList
        [ let key = ImportKey VanillaModule (mkModuleName "Bar") in
          ( key
          , NE.singleton $ ImportSpec key Unqualified $ SpecificImports $ ImportList
              { ilImportType = Imported
              , ilEntries    = KeyMap.fromList
                [ EntryWithChildren (mkSymName "xyz1") Nothing
                , EntryWithChildren (mkSymName "xyz2") Nothing
                ]
              }
          )
        ]
      }
    , modAllSymbols = SymbolMap.fromList
        [ mkResolvedSymbolFromParts filename (Line 5) (mkSymName "FOO") Define Nothing
        , mkResolvedSymbolFromParts filename (Line 9) (mkSymName "foo") Function Nothing
        ]
    }
  }

preprocessorOverExportList :: Test
preprocessorOverExportList = TestCase
  { testName       = "Preprocessor over export list"
  , input          =
      """
      module Foo
      #if !defined(FOO)
        ( xyz1
        , xyz2
        ) where

      import Bar
      #else
        ( ) where
      #endif

      foo :: Int -> Int
      foo = id
      """
  , expectedResult = mkSingleton $ defaltMod
    { modHeader     = defaultModHeader
      { mhModName = mkModuleName "Foo"
      , mhExports = SpecificExports $ ModuleExports
        { meReexports          = mempty
        , meHasWildcardExports = False
        , meExportedEntries    = KeyMap.fromList
            [ EntryWithChildren (mkSymbolName "xyz1", PosAndType filename (Line 3) Function) Nothing
            , EntryWithChildren (mkSymbolName "xyz2", PosAndType filename (Line 4) Function) Nothing
            ]
        }
      , mhImports = SubkeyMap.fromList
        [ let key = ImportKey VanillaModule (mkModuleName "Bar") in
          ( key
          , NE.singleton $ ImportSpec key Unqualified NoImportList
          )
        ]
      }
    , modAllSymbols = SymbolMap.fromList
        [ mkResolvedSymbolFromParts filename (Line 12) (mkSymName "foo") Function Nothing
        ]
    }
  }

preprocessorOverWholeModule :: Test
preprocessorOverWholeModule = TestCase
  { testName       = "Preprocessor over whole module"
  , input          =
      """
      module Foo
      #if defined(FOO)
        where

      import Bar ()
      #else
        ( xyz1
        , xyz2
        ) where

      import Baz

      foo :: Int -> Int
      foo = id
      #endif
      """
  , expectedResult = mkSingleton $ defaltMod
    { modHeader     = defaultModHeader
      { mhModName = mkModuleName "Foo"
      , mhExports = NoExportsWithSomeGuaranteed $ ModuleExports
        { meReexports          = mempty
        , meHasWildcardExports = False
        , meExportedEntries    = KeyMap.fromList
            [ EntryWithChildren (mkSymbolName "xyz1", PosAndType filename (Line 7) Function) Nothing
            , EntryWithChildren (mkSymbolName "xyz2", PosAndType filename (Line 8) Function) Nothing
            ]
        }
      , mhImports = SubkeyMap.fromList
        [ let key = ImportKey VanillaModule (mkModuleName "Bar") in
          ( key
          , NE.singleton $ ImportSpec key Unqualified $ SpecificImports $ ImportList
              { ilImportType = Imported
              , ilEntries    = mempty
              }
          )
        , let key = ImportKey VanillaModule (mkModuleName "Baz") in
          ( key
          , NE.singleton $ ImportSpec key Unqualified NoImportList
          )
        ]
      }
    , modAllSymbols = SymbolMap.fromList
        [ mkResolvedSymbolFromParts filename (Line 13) (mkSymName "foo") Function Nothing
        ]
    }
  }

moduleWithDisabledSectionTest1 :: Test
moduleWithDisabledSectionTest1 = TestCase
  { testName       = "Module header with a part guarded by #if 0"
  , input          =
      """
      module Test
        (
          Foo( X, Y, Z)
      #if 0
        , Bar
      #endif
        , Baz
        ) where
      """
  , expectedResult = mkSingleton $ defaltMod
    { modHeader =
        ModuleHeader
          { mhModName          = mkModuleName "Test"
          , mhExports          = SpecificExports ModuleExports
            { meExportedEntries    = KeyMap.fromList
                [ EntryWithChildren
                  { entryName               = (mkSymbolName "Foo", pt 3 Type)
                  , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                      [ (mkUnqualSymName "X", pt 3 Constructor)
                      , (mkUnqualSymName "Y", pt 3 Constructor)
                      , (mkUnqualSymName "Z", pt 3 Constructor)
                      ]
                  }
                , EntryWithChildren
                  { entryName               = (mkSymbolName "Baz", pt 7 Type)
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
  }

moduleWithDisabledSectionTest2 :: Test
moduleWithDisabledSectionTest2 = TestCase
  { testName       = "Module header with a part guarded by a multiline #if 0"
  , input          =
      """
      module Test
        (
          Foo( X, Y, Z)
      #if  \\
                 0
        , Bar
      #endif
        , Baz
        ) where
      """
  , expectedResult = mkSingleton $ defaltMod
      { modHeader = ModuleHeader
        { mhModName          = mkModuleName "Test"
        , mhExports          = SpecificExports ModuleExports
          { meExportedEntries    = KeyMap.fromList
              [ EntryWithChildren
                { entryName               = (mkSymbolName "Foo", pt 3 Type)
                , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                    [ (mkUnqualSymName "X", pt 3 Constructor)
                    , (mkUnqualSymName "Y", pt 3 Constructor)
                    , (mkUnqualSymName "Z", pt 3 Constructor)
                    ]
                }
              , EntryWithChildren
                { entryName               = (mkSymbolName "Baz", pt 8 Type)
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
  }

moduleWithDisabledAndEnabledSectionsTest :: Test
moduleWithDisabledAndEnabledSectionsTest = TestCase
  { testName       = "Module header with a part guarded by #if 0 and some parts guarded by #if <nonzero>"
  , input          =
      """
      module Test
        (
          Foo( X, Y, Z)
      #if 0
        , Bar
      #endif
        , Baz
      #if 10
        , Quux
      #endif
      #if 01
        , Fizz
      #endif
      #if 101
        , Buzz
      #endif
        ) where
      """
  , expectedResult = mkSingleton $ defaltMod
    { modHeader = ModuleHeader
      { mhModName          = mkModuleName "Test"
      , mhExports          = SpecificExports ModuleExports
        { meExportedEntries    = KeyMap.fromList
            [ EntryWithChildren
              { entryName               = (mkSymbolName "Foo", pt 3 Type)
              , entryChildrenVisibility = Just $ VisibleSpecificChildren $ M.fromList
                  [ (mkUnqualSymName "X", pt 3 Constructor)
                  , (mkUnqualSymName "Y", pt 3 Constructor)
                  , (mkUnqualSymName "Z", pt 3 Constructor)
                  ]
              }
            , EntryWithChildren
              { entryName               = (mkSymbolName "Baz", pt 7 Type)
              , entryChildrenVisibility = Nothing
              }
            , EntryWithChildren
              { entryName               = (mkSymbolName "Quux", pt 9 Type)
              , entryChildrenVisibility = Nothing
              }
            , EntryWithChildren
              { entryName               = (mkSymbolName "Fizz", pt 12 Type)
              , entryChildrenVisibility = Nothing
              }
            , EntryWithChildren
              { entryName               = (mkSymbolName "Buzz", pt 15 Type)
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
  }

mkSymName
  :: HasCallStack
  => Text
  -> UnqualifiedSymbolName
mkSymName str
  = fromMaybe (error $ "Invalid symbol name: " ++ show str)
  $ mkUnqualifiedSymbolName
  $ mkSymbolName str

tests :: TestTree
tests = testGroup "Whole module tests"
  [ doTest emptyModuleTest
  , doTest simpleModuleTest
  , doTest simpleModuleWithIf0Test1
  , doTest simpleModuleWithIf0Test2
  , doTest recordFieldsTest
  , doTest preprocessorInImportListsIsNotLost
  , doTest preprocessorOverExportList
  , doTest preprocessorOverWholeModule
  , testGroup "exports"
    [ doTest moduleWithDisabledSectionTest1
    , doTest moduleWithDisabledSectionTest2
    , doTest moduleWithDisabledAndEnabledSectionsTest
    ]
  ]

doTest :: HasCallStack => Test -> TestTree
doTest TestCase{testName, input, expectedResult = expectedResult :: NonEmptyMap ModuleName UnresolvedModule} =
  testCase testName $ do
    (res :: Either ErrorMessage (NonEmptyMap ModuleName UnresolvedModule), logs) <-
      runWriterT $ runSimpleLoggerT (Just (Custom (tell . (:[])))) Debug $ runErrorExceptT $
        loadModuleFromSource Nothing zeroTime filename $ TE.encodeUtf8 input
    let logsDoc = "Logs, size " <> pretty (length logs) <> ":" ## PP.indent 2 (PP.vcat logs)
    case res of
      Left  msg -> assertFailure $ renderStringWide $ pretty msg ## logsDoc
      Right mod -> do
        let msg = ppDictHeader "Modules are different" $
              ("Input" :-> PP.dquotes (pretty input)) :
              [ ppDifference diff
              | diff <- toList $ genericDiff $ ActualExpected mod expectedResult
              ]
        unless (mod == expectedResult) $
          assertFailure $ renderStringWide $ msg ## logsDoc

