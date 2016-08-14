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

module Main (main) where

import Test.Tasty

import qualified ServerTests as ST

main :: IO ()
main = do
  -- Try to connect to server. If attempt succeeds then server is running
  -- and there's no need to run server ourselves.
  defaultMain $ testGroup "Tests" [ST.tests]
