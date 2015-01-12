----------------------------------------------------------------------------
-- |
-- Module      :  ModuleWithImports
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

module ModuleWithImports where

import Imported1
import Imported2 (baz2)

bar :: a -> a
bar x = x
