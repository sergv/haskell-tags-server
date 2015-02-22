----------------------------------------------------------------------------
-- |
-- Module      :  ModuleWithQualifiedImportNoAlias
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

module ModuleWithQualifiedImportNoAlias where

import qualified Imported1

baz :: a -> a
baz x = x
