----------------------------------------------------------------------------
-- |
-- Module      :  E
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Sunday, 23 October 2016
----------------------------------------------------------------------------

module E where

import F (FooTyp(..), BarTyp(..))

foo :: Int -> Int
foo x = x + x

bar :: Int -> Int
bar x = x * x

baz :: Int -> Int
baz x = x ^ x

