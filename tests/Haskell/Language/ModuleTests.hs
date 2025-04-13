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

type Test = TestCase Text UnresolvedModule

filename :: FullPath 'File
filename = "/foo/bar/test.hs"

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
  , expectedResult = defaltMod
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
  , expectedResult = defaltMod
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

simpleModuleWithIf0Test :: Test
simpleModuleWithIf0Test = TestCase
  { testName       = "Simple regular module with #if 0"
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
  , expectedResult = defaltMod
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
  , expectedResult = defaltMod
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
  , expectedResult = defaltMod
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
  , expectedResult = defaltMod
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
          , NE.singleton $ ImportSpec key Unqualified AssumedWildcardImportList
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
  , expectedResult = defaltMod
    { modHeader     = defaultModHeader
      { mhModName = mkModuleName "Foo"
      , mhExports = SpecificExports $ ModuleExports
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
  , doTest simpleModuleWithIf0Test
  , doTest recordFieldsTest
  , doTest preprocessorInImportListsIsNotLost
  , doTest preprocessorOverExportList
  , doTest preprocessorOverWholeModule
  ]

instance Pretty UTCTime where
  pretty = ppUTCTimeISO8601

doTest :: HasCallStack => Test -> TestTree
doTest TestCase{testName, input, expectedResult = expectedResult :: UnresolvedModule} =
  testCase testName $ do
    (res :: Either ErrorMessage UnresolvedModule, logs) <-
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
