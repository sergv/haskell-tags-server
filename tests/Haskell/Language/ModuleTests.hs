-- |
-- Module:     Haskell.Language.ModuleTests
-- Copyright:  (c) Sergey Vinokurov 2025
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Haskell.Language.ModuleTests (tests) where

import Prelude hiding (mod)

import Control.Monad
import Control.Monad.ErrorExcept
import Control.Monad.Writer
import Data.Foldable
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Prettyprinter qualified as PP
import Prettyprinter.Ext
import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Logging.Simple
import Data.ErrorMessage
import Data.GenericDiff
import Data.Path
import Data.Symbols
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.Clock
import Haskell.Language.Server.Tags.LoadModule (loadModuleFromSource)
import Haskell.Language.Server.Tags.Types.Modules

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
defaltMod = Module
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

tests :: TestTree
tests = testGroup "Whole module tests"
  [ doTest emptyModuleTest
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
