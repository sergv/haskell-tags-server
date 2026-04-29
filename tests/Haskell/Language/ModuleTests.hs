-- |
-- Module:     Haskell.Language.ModuleTests
-- Copyright:  (c) Sergey Vinokurov 2025
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE MultilineStrings  #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskell.Language.ModuleTests (tests) where

import Prelude hiding (mod)

import Control.Monad
import Control.Monad.ErrorExcept
import Control.Monad.Writer
import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Void (Void)
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
import Data.Semigroup (Any(..))
import Data.SubkeyMap qualified as SubkeyMap
import Data.SymbolMap qualified as SymbolMap
import Data.Symbols
import Haskell.Language.Lexer.Types (LitMode(..))
import Haskell.Language.Server.Tags.LoadModule (loadModuleFromSource)
import Haskell.Language.Server.Tags.Types.Imports
import Haskell.Language.Server.Tags.Types.Modules as Mods
import Haskell.Language.Tags.Types

import TestUtils

type Test = TestCase (Text, LitMode Void) (NonEmptyMap ModuleName (NonEmpty UnresolvedModule))

filename :: FullPath 'File
filename = "/foo/bar/test.hs"

pt :: Int -> Type -> PosAndType
pt n = PosAndType filename (Line n)

mkSingleton :: UnresolvedModule -> NonEmptyMap ModuleName (NonEmpty UnresolvedModule)
mkSingleton mod = NEMap.singleton (mhModName (modHeader mod)) $ NE.singleton mod

mkSingleton' :: NonEmpty UnresolvedModule -> NonEmptyMap ModuleName (NonEmpty UnresolvedModule)
mkSingleton' mods@(m :| _) = NEMap.singleton (mhModName (modHeader m)) $ mods

defaultModHeader :: ModuleHeader
defaultModHeader = ModuleHeader
  { mhModName          = mkModuleName "Main"
  , mhExports          = NoExports
  , mhImportQualifiers = mempty
  , mhImports          = mempty
  }

defaultMod :: UnresolvedModule
defaultMod = Mods.Module
  { modHeader           = defaultModHeader
  , modAllSymbols       = mempty
  , modFile             = filename
  , modAllExportedNames = ()
  , modIsDirty          = False
  }

emptyModuleTest :: Test
emptyModuleTest = TestCase
  { testName       = "Empty module"
  , input          = (, LitVanilla) $
      "module Foo where"
  , expectedResult = mkSingleton $ defaultMod
    { modHeader = defaultModHeader
      { mhModName = mkModuleName "Foo"
      }
    }
  }

simpleModuleTest :: Test
simpleModuleTest = TestCase
  { testName       = "Simple regular module 1"
  , input          = (, LitVanilla) $
      """
      module Foo where

      import Bar (xyz)

      foo :: Int -> Int
      foo = id
      """
  , expectedResult = mkSingleton $ defaultMod
    { modHeader     = defaultModHeader
      { mhModName = mkModuleName "Foo"
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
  , input          = (, LitVanilla) $
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
  , expectedResult = mkSingleton $ defaultMod
    { modHeader     = defaultModHeader
      { mhModName = mkModuleName "Foo"
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
  , input          = (, LitVanilla) $
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
  , expectedResult = mkSingleton $ defaultMod
    { modHeader     = defaultModHeader
      { mhModName = mkModuleName "Foo"
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
  , input          = (, LitVanilla) $
      """
      module Foo where

      foo :: Int -> Int
      foo = id

      data Foo = Foo
        { bar :: Int
        , baz :: Double
        }
      """
  , expectedResult = mkSingleton $ defaultMod
    { modHeader     = defaultModHeader
      { mhModName = mkModuleName "Foo"
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
  , input          = (, LitVanilla) $
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
  , expectedResult = mkSingleton $ defaultMod
    { modHeader     = defaultModHeader
      { mhModName = mkModuleName "Foo"
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
  , input          = (, LitVanilla) $
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
  , expectedResult = mkSingleton' $ nePair
      ( defaultMod
          { modHeader     = defaultModHeader
              { mhModName = mkModuleName "Foo"
              , mhExports = EmptyExports
              , mhImports = mempty
              }
          , modAllSymbols = SymbolMap.fromList
              [ mkResolvedSymbolFromParts filename (Line 12) (mkSymName "foo") Function Nothing
              ]
          }
      , defaultMod
          { modHeader     = defaultModHeader
              { mhModName = mkModuleName "Foo"
              , mhExports = SpecificExports $ ModuleExports
                  { meReexports          = mempty
                  , meHasWildcardExports = Any False
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
      )
  }

preprocessorOverWholeModule :: Test
preprocessorOverWholeModule = TestCase
  { testName       = "Preprocessor over whole module"
  , input          = (, LitVanilla) $
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
  , expectedResult = mkSingleton' $ nePair
      ( defaultMod
          { modHeader     = defaultModHeader
              { mhModName = mkModuleName "Foo"
              , mhExports = SpecificExports $ ModuleExports
                  { meReexports          = mempty
                  , meHasWildcardExports = Any False
                  , meExportedEntries    = KeyMap.fromList
                      [ EntryWithChildren (mkSymbolName "xyz1", PosAndType filename (Line 7) Function) Nothing
                      , EntryWithChildren (mkSymbolName "xyz2", PosAndType filename (Line 8) Function) Nothing
                      ]
                  }
              , mhImports = uncurry SubkeyMap.singleton $
                  let key = ImportKey VanillaModule (mkModuleName "Baz") in
                  ( key
                  , NE.singleton $ ImportSpec key Unqualified NoImportList
                  )

              }
          , modAllSymbols = SymbolMap.fromList
              [ mkResolvedSymbolFromParts filename (Line 13) (mkSymName "foo") Function Nothing
              ]
          }
      , defaultMod
          { modHeader     = defaultModHeader
              { mhModName = mkModuleName "Foo"
              , mhExports = NoExports
              , mhImports = uncurry SubkeyMap.singleton $
                  let key = ImportKey VanillaModule (mkModuleName "Bar") in
                  ( key
                  , NE.singleton $ ImportSpec key Unqualified $ SpecificImports $ ImportList
                      { ilImportType = Imported
                        , ilEntries    = mempty
                        }
                    )

              }
          , modAllSymbols = mempty
          }
      )
  }

includeWithCStyleComment :: Test
includeWithCStyleComment = TestCase
  { testName       = "#include with trailing C-style comment"
  , input          = (, LitVanilla) $
      """
      import Foo
      #include <foo.h> /* just for testing */
      import Bar

      foo :: Int -> Int
      foo x = x
      """
  , expectedResult = mkSingleton $ defaultMod
    { modHeader = defaultModHeader
      { mhImports = SubkeyMap.fromList
          [ let key = ImportKey VanillaModule (mkModuleName name) in
            ( key
            , NE.singleton $ ImportSpec key Unqualified NoImportList
            )
          | name <- ["Foo", "Bar"]
          ]
      }
    , modAllSymbols = SymbolMap.fromList
        [ mkResolvedSymbolFromParts filename (Line 5) (mkSymName "foo") Function Nothing
        ]
    }
  }

moduleWithDisabledSectionTest1 :: Test
moduleWithDisabledSectionTest1 = TestCase
  { testName       = "Module header with a part guarded by #if 0"
  , input          = (, LitVanilla) $
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
  , expectedResult = mkSingleton $ defaultMod
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
            , meHasWildcardExports = Any False
            }
          , mhImportQualifiers = mempty
          , mhImports          = mempty
          }
    }
  }

moduleWithDisabledSectionTest2 :: Test
moduleWithDisabledSectionTest2 = TestCase
  { testName       = "Module header with a part guarded by a multiline #if 0"
  , input          = (, LitVanilla) $
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
  , expectedResult = mkSingleton $ defaultMod
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
          , meHasWildcardExports = Any False
          }
        , mhImportQualifiers = mempty
        , mhImports          = mempty
        }
      }
  }

moduleWithDisabledAndEnabledSectionsTest :: Test
moduleWithDisabledAndEnabledSectionsTest = TestCase
  { testName       = "Module header with a part guarded by #if 0 and some parts guarded by #if <nonzero>"
  , input          = (, LitVanilla) $
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
  , expectedResult = mkSingleton $ defaultMod
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
        , meHasWildcardExports = Any False
        }
      , mhImportQualifiers = mempty
      , mhImports          = mempty
      }
    }
  }

doubleDefine1 :: Test
doubleDefine1 = TestCase
  { testName       = "Double define 1"
  , input          = (, LitVanilla) $
      """
      #define FOO 1
      #define BAR 2

      foo :: a -> a
      foo x = x
      """
  , expectedResult = mkSingleton $ defaultMod
    { modHeader = defaultModHeader
    , modAllSymbols = SymbolMap.fromList
        [ mkResolvedSymbolFromParts filename (Line 1) (mkSymName "FOO") Define Nothing
        , mkResolvedSymbolFromParts filename (Line 2) (mkSymName "BAR") Define Nothing
        , mkResolvedSymbolFromParts filename (Line 4) (mkSymName "foo") Function Nothing
        ]
    }
  }

doubleDefine2 :: Test
doubleDefine2 = TestCase
  { testName       = "Double define 2"
  , input          = (, LitVanilla) $
      """
      #ifdef QUUX
      #define FOO 1
      #define BAR 2
      #endif

      foo :: a -> a
      foo x = x
      """
  , expectedResult = mkSingleton $ defaultMod
    { modHeader = defaultModHeader
    , modAllSymbols = SymbolMap.fromList
        [ mkResolvedSymbolFromParts filename (Line 2) (mkSymName "FOO") Define Nothing
        , mkResolvedSymbolFromParts filename (Line 3) (mkSymName "BAR") Define Nothing
        , mkResolvedSymbolFromParts filename (Line 6) (mkSymName "foo") Function Nothing
        ]
    }
  }

doubleDefineInImportList :: Test
doubleDefineInImportList = TestCase
  { testName       = "Double define in import list"
  , input          = (, LitVanilla) $
      """
      import Foo
      #ifdef QUUX
      #define FOO 1
      #define BAR 2
      import Quux
      #endif
      import Bar

      foo :: a -> a
      foo x = x
      """
  , expectedResult = mkSingleton $ defaultMod
    { modHeader = defaultModHeader
      { mhImports = SubkeyMap.fromList
          [ let key = ImportKey VanillaModule (mkModuleName name) in
            ( key
            , NE.singleton $ ImportSpec key Unqualified NoImportList
            )
          | name <- ["Foo", "Bar", "Quux"]
          ]
      }
    , modAllSymbols = SymbolMap.fromList
        [ mkResolvedSymbolFromParts filename (Line 3) (mkSymName "FOO") Define Nothing
        , mkResolvedSymbolFromParts filename (Line 4) (mkSymName "BAR") Define Nothing
        , mkResolvedSymbolFromParts filename (Line 9) (mkSymName "foo") Function Nothing
        ]
    }
  }

defineBeforeModuleHeader :: Test
defineBeforeModuleHeader = TestCase
  { testName       = "Define before module header"
  , input          = (, LitVanilla) $
      """
      #define FOO 1

      module Foo where

      foo :: a -> a
      foo x = x
      """
  , expectedResult = mkSingleton $ defaultMod
    { modHeader = defaultModHeader
      { mhModName = mkModuleName "Foo"
      }
    , modAllSymbols = SymbolMap.fromList
        [ mkResolvedSymbolFromParts filename (Line 1) (mkSymName "FOO") Define Nothing
        , mkResolvedSymbolFromParts filename (Line 5) (mkSymName "foo") Function Nothing
        ]
    }
  }

alexPreprocessing :: Test
alexPreprocessing = TestCase
  { testName       = "Alex preprocessing"
  , input          = (, LitVanilla) $
      """
      {
      module Foo where

      import Bar

      }

      $asclarge = [A-Z]
      $ascsmall = [a-z]
      $dot      = [\\.]

      $ascident = [$ascsmall $asclarge]

      $ascsymbol = [\\!\\#\\$\\%\\&\\*\\+\\.\\/\\<\\=\\>\\?\\@\\\\\\^\\|\\-\\~\\:]

      $large = [$asclarge]
      $ident = [$ascident] # [$ascsymbol]

      @qualificationPrefix = ( $large $ident* $dot )*

      -- Vanilla tokens
      <0> {

      -- Newlines and comments (unfinished).
      <0> {

      @nl ">" $space*
        / { isLiterateEnabled' }
        { \\input len -> pure $! Newline $! countInputSpace input len }
      @nl
        / { shouldEndLiterateBird }
        { \\_ _   -> Newline 0 <$ endLiterate }
      @nl "\\end{code}"
        / { shouldEndLiterateLatex }
        { \\_ _   -> endLiterate' }

      [\\\\]? @nl $space*       { \\input len -> pure $! Newline $! len - countBackslashCR input - 1 }
      [\\-][\\-]+ ~[$symbol $nl] { \\_ _ -> dropUntilNL' }
      [\\-][\\-]+ / @nl         ;

      }

      @qualificationPrefix $ident+
        { \\input len -> pure $! T $! takeText input len }

      }

      {
      foo :: a -> a
      foo x = x
      }
      """
  , expectedResult = mkSingleton $ defaultMod
    { modHeader = defaultModHeader
      { mhModName = mkModuleName "Foo"
      , mhImports = SubkeyMap.fromList
        [ let key = ImportKey VanillaModule (mkModuleName name) in
          ( key
          , NE.singleton $ ImportSpec key Unqualified NoImportList
          )
        | name <- ["Bar"]
        ]
      }
    , modAllSymbols = SymbolMap.fromList
        [ mkResolvedSymbolFromParts filename (Line 49) (mkSymName "foo") Function Nothing
        ]
    }
  }

alexTests :: TestTree
alexTests = testGroup "Alex"
  [ testGroup "vanilla"
    [ doTest $ TestCase
      { testName = "1"
      , input = (, LitVanilla) $
          """
          {
          module AlexTest where

          import FooBar

          foobar :: Int -> Int
          foobar = (+ 1)

          }


          -- Can skip whitespace everywhere since it does not affect meaning in any
          -- state.
          <0, comment, qq, literate> $ws+ ;

          -- Literate Haskell support. 'literate' code handles all text except actual
          -- Haskell program text. It aims to strip all non-Haskell text.
          <literate> {
          $nl ">" $ws*
            { \\_ len -> (Newline $! len - 2) <$ startLiterateBird }
          $nl "\\begin{code}" @nl $space*
            { \\input len -> (Newline $! countInputSpace input len) <$ startLiterateLatex }
          $nl ;
          .
            { \\_ _ -> dropUntilNL' }
          }

          -- Vanilla tokens
          <0> {

          "#" @cpp_opt_ws ("{" (@cpp_ws | @nl)*)? "enum"
            { \\_ _ -> pure HSCEnum }
          "#" @cpp_opt_ws
            ( ($ascident # [e]) $ascident*
            | $ascident ($ascident # [n]) $ascident*
            | $ascident $ascident ($ascident # [u]) $ascident*
            | $ascident $ascident $ascident ($ascident # [m]) $ascident*
            | $ascident $ascident $ascident $ascident $ascident+
            )
            { \\_ _ -> pure HSCDirective }
          "#" @cpp_opt_ws "{" (@cpp_ws | @nl)*
            ( ($ascident # [e]) $ascident*
            | $ascident ($ascident # [n]) $ascident*
            | $ascident $ascident ($ascident # [u]) $ascident*
            | $ascident $ascident $ascident ($ascident # [m]) $ascident*
            | $ascident $ascident $ascident $ascident $ascident+
            )
            { \\_ _ -> pure HSCDirectiveBraced }
          }




          {
          foo :: Int -> Int
          foo x = x + x
          }
          """
      , expectedResult = mkSingleton $ defaultMod
        { modHeader     = defaultModHeader
          { mhModName = mkModuleName "AlexTest"
          , mhImports = SubkeyMap.fromList
            [ let key = ImportKey VanillaModule (mkModuleName "FooBar") in
              ( key
              , NE.singleton $ ImportSpec key Unqualified NoImportList
              )
            ]
          }
        , modAllSymbols = SymbolMap.fromList
            [ mkResolvedSymbolFromParts filename (Line 6) (mkSymName "foobar") Function Nothing
            , mkResolvedSymbolFromParts filename (Line 55) (mkSymName "foo") Function Nothing
            ]
        }
      }
    ]
  , testGroup "literate"
    [ doTest $ TestCase
      { testName = "1"
      , input = (, LitOutside) $
          """
          Very useful description 1
          Very useful description 2
          > {
          > module AlexTest where
          >
          > import FooBar
          >
          > foobar :: Int -> Int
          > foobar = (+ 1)
          >
          > }

          Useful description 1
          Useful description 2
          Useful description 3
          Useful description 4

          > -- Can skip whitespace everywhere since it does not affect meaning in any
          > -- state.
          > <0, comment, qq, literate> $ws+ ;
          >
          > -- Literate Haskell support. 'literate' code handles all text except actual
          > -- Haskell program text. It aims to strip all non-Haskell text.
          > <literate> {
          > $nl ">" $ws*
          >   { \\_ len -> (Newline $! len - 2) <$ startLiterateBird }
          > $nl "\\begin{code}" @nl $space*
          >   { \\input len -> (Newline $! countInputSpace input len) <$ startLiterateLatex }
          > $nl ;
          > .
          >   { \\_ _ -> dropUntilNL' }
          > }
          >
          Useful description 1
          Useful description 2
          Useful description 3
          Useful description 4
          > -- Vanilla tokens
          > <0> {
          >
          > "#" @cpp_opt_ws ("{" (@cpp_ws | @nl)*)? "enum"
          >   { \\_ _ -> pure HSCEnum }
          > "#" @cpp_opt_ws
          >   ( ($ascident # [e]) $ascident*
          >   | $ascident ($ascident # [n]) $ascident*
          >   | $ascident $ascident ($ascident # [u]) $ascident*
          >   | $ascident $ascident $ascident ($ascident # [m]) $ascident*
          >   | $ascident $ascident $ascident $ascident $ascident+
          >   )
          >   { \\_ _ -> pure HSCDirective }
          > "#" @cpp_opt_ws "{" (@cpp_ws | @nl)*
          >   ( ($ascident # [e]) $ascident*
          >   | $ascident ($ascident # [n]) $ascident*
          >   | $ascident $ascident ($ascident # [u]) $ascident*
          >   | $ascident $ascident $ascident ($ascident # [m]) $ascident*
          >   | $ascident $ascident $ascident $ascident $ascident+
          >   )
          >   { \\_ _ -> pure HSCDirectiveBraced }
          > }
          >
          >
          > {
          > foo :: Int -> Int
          > foo x = x + x
          > }
          >
          """
      , expectedResult = mkSingleton $ defaultMod
        { modHeader     = defaultModHeader
          { mhModName = mkModuleName "AlexTest"
          , mhImports = SubkeyMap.fromList
            [ let key = ImportKey VanillaModule (mkModuleName "FooBar") in
              ( key
              , NE.singleton $ ImportSpec key Unqualified NoImportList
              )
            ]
          }
        , modAllSymbols = SymbolMap.fromList
            [ mkResolvedSymbolFromParts filename (Line 8) (mkSymName "foobar") Function Nothing
            , mkResolvedSymbolFromParts filename (Line 63) (mkSymName "foo") Function Nothing
            ]
        }
      }
    , doTest $ TestCase
      { testName = "2"
      , input = (, LitOutside) $
          """
          Very useful description 1
          Very useful description 2
          \\begin{code}
          {
          module AlexTest where

          import FooBar

          foobar :: Int -> Int
          foobar = (+ 1)

          }
          \\end{code}

          Useful description 1
          Useful description 2
          Useful description 3
          Useful description 4

          \\begin{code}
          -- Can skip whitespace everywhere since it does not affect meaning in any
          -- state.
          <0, comment, qq, literate> $ws+ ;

          -- Literate Haskell support. 'literate' code handles all text except actual
          -- Haskell program text. It aims to strip all non-Haskell text.
          <literate> {
          $nl ">" $ws*
            { \\_ len -> (Newline $! len - 2) <$ startLiterateBird }
          $nl "\\begin{code}" @nl $space*
            { \\input len -> (Newline $! countInputSpace input len) <$ startLiterateLatex }
          $nl ;
          .
            { \\_ _ -> dropUntilNL' }
          }

          \\end{code}

          Useful description 1
          Useful description 2
          Useful description 3
          Useful description 4

          \\begin{code}
          -- Vanilla tokens
          <0> {

          "#" @cpp_opt_ws ("{" (@cpp_ws | @nl)*)? "enum"
            { \\_ _ -> pure HSCEnum }
          "#" @cpp_opt_ws
            ( ($ascident # [e]) $ascident*
            | $ascident ($ascident # [n]) $ascident*
            | $ascident $ascident ($ascident # [u]) $ascident*
            | $ascident $ascident $ascident ($ascident # [m]) $ascident*
            | $ascident $ascident $ascident $ascident $ascident+
            )
            { \\_ _ -> pure HSCDirective }
          "#" @cpp_opt_ws "{" (@cpp_ws | @nl)*
            ( ($ascident # [e]) $ascident*
            | $ascident ($ascident # [n]) $ascident*
            | $ascident $ascident ($ascident # [u]) $ascident*
            | $ascident $ascident $ascident ($ascident # [m]) $ascident*
            | $ascident $ascident $ascident $ascident $ascident+
            )
            { \\_ _ -> pure HSCDirectiveBraced }
          }


          {
          foo :: Int -> Int
          foo x = x + x
          }

          \\end{code}

          """
      , expectedResult = mkSingleton $ defaultMod
        { modHeader     = defaultModHeader
          { mhModName = mkModuleName "AlexTest"
          , mhImports = SubkeyMap.fromList
            [ let key = ImportKey VanillaModule (mkModuleName "FooBar") in
              ( key
              , NE.singleton $ ImportSpec key Unqualified NoImportList
              )
            ]
          }
        , modAllSymbols = SymbolMap.fromList
            [ mkResolvedSymbolFromParts filename (Line 9) (mkSymName "foobar") Function Nothing
            , mkResolvedSymbolFromParts filename (Line 70) (mkSymName "foo") Function Nothing
            ]
        }
      }
    ]
  ]

happyTests :: TestTree
happyTests = testGroup "Happy"
  [ testGroup "vanilla"
    [ doTest $ TestCase
      { testName = "1"
      , input = (, LitVanilla) $
          """
          {
          {-# OPTIONS_GHC -w #-}
          module AttrGrammarParser (agParser) where
          import ParseMonad
          import AttrGrammar
          }

          %name agParser
          %tokentype { AgToken }
          %token
            "{"     { AgTok_LBrace }
            "}"     { AgTok_RBrace }
            ";"     { AgTok_Semicolon }
            '{'       { AgTok_LBrace }
            '}'       { AgTok_RBrace }
            '::'      { AgTok_Semicolon }
            "="     { AgTok_Eq }
            where     { AgTok_Where }
            selfRef   { AgTok_SelfRef _ }
            subRef    { AgTok_SubRef _ }
            rightRef  { AgTok_RightmostRef _ }
            unknown   { AgTok_Unknown _ }

          %monad { P }
          %lexer { agLexer } { AgTok_EOF }

          %%

          agParser :: { [AgRule] }
            : rules                                      { $1 }

          rules :: { [AgRule] }
            : rule '::' rules                            { $1 : $3 }
            | rule                                       { $1 : [] }
            |                                            { [] }

          rule :: { AgRule }
            : selfRef  "=" code                        { SelfAssign (selfRefVal $1) $3 }
            | subRef   "=" code                        { SubAssign (subRefVal $1) $3 }
            | rightRef "=" code                        { RightmostAssign (rightRefVal $1) $3 }
            | where code                                 { Conditional $2 }

          code :: { [AgToken] }
            : '{' code0 '}' code                         { [$1] ++ $2 ++ [$3] ++ $4 }
            | "=" code                                 { $1 : $2 }
            | selfRef code                               { $1 : $2 }
            | subRef code                                { $1 : $2 }
            | rightRef code                              { $1 : $2 }
            | unknown code                               { $1 : $2 }
            |                                            { [] }

          code0 :: { [AgToken] }
            : "{" code0 "}" code0                    { [$1] ++ $2 ++ [$3] ++ $4 }
            | "=" code0                                { $1 : $2 }
            | '::' code0                                 { $1 : $2 }
            | selfRef code0                              { $1 : $2 }
            | subRef code0                               { $1 : $2 }
            | rightRef code                              { $1 : $2 }
            | unknown code0                              { $1 : $2 }
            |                                            { [] }

          {
          happyError :: P a
          happyError = fail ("Parse error\\n")

          test :: a -> a
          test x = x
          }

          """
      , expectedResult = mkSingleton $ defaultMod
        { modHeader     = defaultModHeader
          { mhModName = mkModuleName "AttrGrammarParser"
          , mhExports = SpecificExports $ ModuleExports
            { meReexports          = mempty
            , meHasWildcardExports = Any False
            , meExportedEntries    = KeyMap.fromList
                [ EntryWithChildren (mkSymbolName "agParser", PosAndType filename (Line 3) Function) Nothing
                ]
            }
          , mhImports = SubkeyMap.fromList
            [ let key = ImportKey VanillaModule (mkModuleName name) in
              ( key
              , NE.singleton $ ImportSpec key Unqualified NoImportList
              )
            | name <- ["ParseMonad", "AttrGrammar"]
            ]
          }
        , modAllSymbols = SymbolMap.fromList
            [ mkResolvedSymbolFromParts filename (Line 63) (mkSymName "happyError") Function Nothing
            , mkResolvedSymbolFromParts filename (Line 66) (mkSymName "test") Function Nothing
            ]
        }
      }
    ]
  , testGroup "literate"
    [ doTest $ TestCase
      { testName = "1"
      , input = (, LitOutside) $
          """
          This parser parses the contents of the attribute grammar
          into a list of rules.  A rule can either be an assignment
          to an attribute of the LHS (synthesized attribute), and
          assignment to an attribute of the RHS (an inherited attribute),
          or a conditional statement.

          > {
          > {-# OPTIONS_GHC -w #-}
          > module AttrGrammarParser (agParser) where
          > import ParseMonad
          > import AttrGrammar
          > }

          > %name agParser
          > %tokentype { AgToken }
          > %token
          >   "{"     { AgTok_LBrace }
          >   "}"     { AgTok_RBrace }
          >   ";"     { AgTok_Semicolon }
          >   "="     { AgTok_Eq }
          >   where     { AgTok_Where }
          >   selfRef   { AgTok_SelfRef _ }
          >   subRef    { AgTok_SubRef _ }
          >   rightRef  { AgTok_RightmostRef _ }
          >   unknown   { AgTok_Unknown _ }
          >
          > %monad { P }
          > %lexer { agLexer } { AgTok_EOF }

          > %%

          > agParser :: { [AgRule] }
          >   : rules                                      { $1 }

          > rules :: { [AgRule] }
          >   : rule ";" rules                           { $1 : $3 }
          >   | rule                                       { $1 : [] }
          >   |                                            { [] }

          > rule :: { AgRule }
          >   : selfRef  "=" code                        { SelfAssign (selfRefVal $1) $3 }
          >   | subRef   "=" code                        { SubAssign (subRefVal $1) $3 }
          >   | rightRef "=" code                        { RightmostAssign (rightRefVal $1) $3 }
          >   | where code                                 { Conditional $2 }

          > code :: { [AgToken] }
          >   : "{" code0 "}" code                     { [$1] ++ $2 ++ [$3] ++ $4 }
          >   | "=" code                                 { $1 : $2 }
          >   | selfRef code                               { $1 : $2 }
          >   | subRef code                                { $1 : $2 }
          >   | rightRef code                              { $1 : $2 }
          >   | unknown code                               { $1 : $2 }
          >   |                                            { [] }

          > code0 :: { [AgToken] }
          >   : "{" code0 "}" code0                    { [$1] ++ $2 ++ [$3] ++ $4 }
          >   | "=" code0                                { $1 : $2 }
          >   | ";" code0                                { $1 : $2 }
          >   | selfRef code0                              { $1 : $2 }
          >   | subRef code0                               { $1 : $2 }
          >   | rightRef code                              { $1 : $2 }
          >   | unknown code0                              { $1 : $2 }
          >   |                                            { [] }

          > {
          > happyError :: P a
          > happyError = fail ("Parse error\\n")
          >
          > test :: a -> a
          > test x = x
          > }

          """
      , expectedResult = mkSingleton $ defaultMod
        { modHeader     = defaultModHeader
          { mhModName = mkModuleName "AttrGrammarParser"
          , mhExports = SpecificExports $ ModuleExports
            { meReexports          = mempty
            , meHasWildcardExports = Any False
            , meExportedEntries    = KeyMap.fromList
                [ EntryWithChildren (mkSymbolName "agParser", PosAndType filename (Line 9) Function) Nothing
                ]
            }
          , mhImports = SubkeyMap.fromList
            [ let key = ImportKey VanillaModule (mkModuleName name) in
              ( key
              , NE.singleton $ ImportSpec key Unqualified NoImportList
              )
            | name <- ["ParseMonad", "AttrGrammar"]
            ]
          }
        , modAllSymbols = SymbolMap.fromList
            [ mkResolvedSymbolFromParts filename (Line 66) (mkSymName "happyError") Function Nothing
            , mkResolvedSymbolFromParts filename (Line 69) (mkSymName "test") Function Nothing
            ]
        }
      }
    , doTest $ TestCase
      { testName = "2"
      , input = (, LitOutside) $
          """
          This parser parses the contents of the attribute grammar
          into a list of rules.  A rule can either be an assignment
          to an attribute of the LHS (synthesized attribute), and
          assignment to an attribute of the RHS (an inherited attribute),
          or a conditional statement.

          \\begin{code}

          {
          {-# OPTIONS_GHC -w #-}
          module AttrGrammarParser (agParser) where
          import ParseMonad
          import AttrGrammar
          }

          \\end{code}

          \\begin{code}
          %name agParser
          %tokentype { AgToken }
          %token
            "{"     { AgTok_LBrace }
            "}"     { AgTok_RBrace }
            ";"     { AgTok_Semicolon }
            "="     { AgTok_Eq }
            where     { AgTok_Where }
            selfRef   { AgTok_SelfRef _ }
            subRef    { AgTok_SubRef _ }
            rightRef  { AgTok_RightmostRef _ }
            unknown   { AgTok_Unknown _ }

          %monad { P }
          %lexer { agLexer } { AgTok_EOF }
          \\end{code}

          \\begin{code}
          %%
          \\end{code}

          \\begin{code}
          agParser :: { [AgRule] }
            : rules                                      { $1 }
          \\end{code}

          \\begin{code}
          rules :: { [AgRule] }
            : rule ";" rules                           { $1 : $3 }
            | rule                                       { $1 : [] }
            |                                            { [] }
          \\end{code}

          \\begin{code}
          rule :: { AgRule }
            : selfRef  "=" code                        { SelfAssign (selfRefVal $1) $3 }
            | subRef   "=" code                        { SubAssign (subRefVal $1) $3 }
            | rightRef "=" code                        { RightmostAssign (rightRefVal $1) $3 }
            | where code                                 { Conditional $2 }
          \\end{code}

          \\begin{code}
          code :: { [AgToken] }
            : "{" code0 "}" code                     { [$1] ++ $2 ++ [$3] ++ $4 }
            | "=" code                                 { $1 : $2 }
            | selfRef code                               { $1 : $2 }
            | subRef code                                { $1 : $2 }
            | rightRef code                              { $1 : $2 }
            | unknown code                               { $1 : $2 }
            |                                            { [] }
          \\end{code}

          \\begin{code}
          code0 :: { [AgToken] }
            : "{" code0 "}" code0                    { [$1] ++ $2 ++ [$3] ++ $4 }
            | "=" code0                                { $1 : $2 }
            | ";" code0                                { $1 : $2 }
            | selfRef code0                              { $1 : $2 }
            | subRef code0                               { $1 : $2 }
            | rightRef code                              { $1 : $2 }
            | unknown code0                              { $1 : $2 }
            |                                            { [] }
          \\end{code}

          \\begin{code}
          {
          happyError :: P a
          happyError = fail ("Parse error\\n")

          test :: a -> a
          test x = x
          }
          \\end{code}

          """
      , expectedResult = mkSingleton $ defaultMod
        { modHeader     = defaultModHeader
          { mhModName = mkModuleName "AttrGrammarParser"
          , mhExports = SpecificExports $ ModuleExports
            { meReexports          = mempty
            , meHasWildcardExports = Any False
            , meExportedEntries    = KeyMap.fromList
                [ EntryWithChildren (mkSymbolName "agParser", PosAndType filename (Line 11) Function) Nothing
                ]
            }
          , mhImports = SubkeyMap.fromList
            [ let key = ImportKey VanillaModule (mkModuleName name) in
              ( key
              , NE.singleton $ ImportSpec key Unqualified NoImportList
              )
            | name <- ["ParseMonad", "AttrGrammar"]
            ]
          }
        , modAllSymbols = SymbolMap.fromList
            [ mkResolvedSymbolFromParts filename (Line 85) (mkSymName "happyError") Function Nothing
            , mkResolvedSymbolFromParts filename (Line 88) (mkSymName "test") Function Nothing
            ]
        }
      }
    ]
  ]

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
  , doTest includeWithCStyleComment
  , doTest doubleDefine1
  , doTest doubleDefine2
  , doTest doubleDefineInImportList
  , doTest defineBeforeModuleHeader
  , doTest alexPreprocessing
  , testGroup "exports"
    [ doTest moduleWithDisabledSectionTest1
    , doTest moduleWithDisabledSectionTest2
    , doTest moduleWithDisabledAndEnabledSectionsTest
    ]
  , alexTests
  , happyTests
  ]

doTest :: HasCallStack => Test -> TestTree
doTest TestCase{testName, input = (src, mode), expectedResult} =
  testCase testName $ do
    (res :: Either ErrorMessage (NonEmptyMap ModuleName (NonEmpty UnresolvedModule)), logs) <-
      runWriterT $ runSimpleLoggerT (Just (Custom (tell . (:[])))) Debug $ runErrorExceptT $
        loadModuleFromSource Nothing mode filename $ TE.encodeUtf8 src
    let logsDoc = "Logs, size " <> pretty (length logs) <> ":" ## PP.indent 2 (PP.vcat logs)
    case res of
      Left  msg -> assertFailure $ renderStringWide $ pretty msg ## logsDoc
      Right mod -> do
        let msg = ppDictHeader "Modules are different" $
              -- ("Input" :-> PP.dquotes (pretty src)) :
              ("Mode"  :-> pretty mode) :
              [ ppDifference diff
              | diff <- toList $ genericDiff $ ActualExpected mod expectedResult
              ]
        unless (mod == expectedResult) $
          assertFailure $ renderStringWide $ msg ## logsDoc

nePair :: (a, a) -> NonEmpty a
nePair (x, y) = x :| [y]
