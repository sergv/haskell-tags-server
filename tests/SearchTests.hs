----------------------------------------------------------------------------
-- |
-- Module      :  SearchTests
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module SearchTests (tests) where

import Control.Monad
import Control.Monad.Base
import Control.Monad.ErrorExcept
import Control.Monad.Except
import Data.Path qualified as Path
import Data.Set qualified as S
import GHC.Stack
import Prettyprinter.Ext
import System.Directory.OsPath.Streaming
import System.OsPath qualified as OsPath
import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Logging.Simple (Destination, runSimpleLoggerT, Severity(..))
import Data.ErrorMessage
import Data.Path
import Data.Symbols
import Haskell.Language.Server.Tags.Search (findSymbolInFiles)
import Haskell.Language.Server.Tags.Types
import Haskell.Language.Tags.Types

import SearchTests.Data

-- | Directory with test projects.
testDataDir :: PathFragment
testDataDir = "test-data"

mkTestsConfig
  :: (MonadBase IO m, MonadError ErrorMessage m)
  => NameResolutionStrictness
  -> m TagsServerConf
mkTestsConfig tsconfNameResolution = do
  let conf = defaultTagsServerConf
        { tsconfEagerTagging   = False
        , tsconfNameResolution
        }
  pure conf

tests :: TestTree
tests = makeTestTree testData
  where
    makeTestTree :: HasCallStack => TestSet SearchTest -> TestTree
    makeTestTree = go
      where
        go (GroupTest name xs)     = testGroup name $ map go xs
        go (AtomicTest serverTest) = mkFindSymbolTest serverTest

mkFindSymbolTest
  :: HasCallStack
  => SearchTest
  -> TestTree
mkFindSymbolTest SearchTest{stTestName, stNameResolutionStrictness, stWorkingDirectory, stFile, stSymbol, stExpectedResponse} =
  testCase stTestName $ do
    result <- runErrorExceptT $ do
      conf <- mkTestsConfig stNameResolutionStrictness

      let dir :: PathFragment
          dir  = testDataDir </> unWorkingDirectory stWorkingDirectory
          path = unPathFragment $ dir </> stFile

      files <- liftBase $
        fmap (map ((pathFragmentToOsPath dir OsPath.</>) . fst) . filter ((== regularFile) . snd)) $
          getDirectoryContentsRecursive $
            pathFragmentToOsPath dir

      path' <- Path.mkFullPath path

      actual <- runSimpleLoggerT (Nothing @(Destination (ErrorExceptT ErrorMessage IO))) Debug $
        findSymbolInFiles
          conf
          files
          ScopeCurrentModule
          path'
          (mkSymbolName stSymbol)

      let mkTestResolvedSym sym p line typ parent = do
            fullExpectedPath <- Path.mkFullPath $ dir </> p
            pure $ mkResolvedSymbolFromParts fullExpectedPath (Line line) sym typ parent

      case stExpectedResponse of
        Known sym expectedPath line typ parent ->
          case S.toList actual of
            [actual'] -> do
              expected <- mkTestResolvedSym sym expectedPath line typ parent
              unless (actual' == expected) $
                assertFailure' $ ppDictHeader "Different results"
                  [ "actual"   --> actual'
                  , "expected" --> expected
                  ]
            _ ->
              assertFailure' $ "Expected single result, but got" ## ppSet actual
        Ambiguous expected -> do
          expected' <- S.fromList <$> traverse (\(a, b, c, d, e) -> mkTestResolvedSym a b c d e) expected
          unless (actual == expected') $
            assertFailure' $ ppDictHeader "Different results"
              [ "actual"   :-> ppSet actual
              , "expected" :-> ppSet expected'
              ]
        NotFound                                                           ->
          unless (S.null actual) $
            assertFailure' $ "Expected 'not found' result, but got" ## ppSet actual

    case result of
      Right () -> pure ()
      Left err -> assertFailure' $ "Failure:" ## pretty err
  where
    assertFailure' :: MonadBase IO m => Doc ann -> m a
    assertFailure' = liftBase . assertFailure . renderStringWide
