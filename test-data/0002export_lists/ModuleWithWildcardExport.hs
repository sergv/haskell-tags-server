----------------------------------------------------------------------------
-- |
-- Module      :  ModuleWithWildcardExport
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

module ModuleWithWildcardExport
  ( Foo(..)
  )
where

data Foo = Bar { getBar :: Int }
         | Baz { getBaz :: Int }
  deriving (Eq)
