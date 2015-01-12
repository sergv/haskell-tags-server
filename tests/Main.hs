----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

module Main where

import Control.Concurrent
import Test.Tasty

import Server
import qualified ServerTests as ST

main :: IO ()
main = do
  -- forkIO (runServer ST.testsConfig)
  defaultMain $ testGroup "Tests" [ST.tests]
