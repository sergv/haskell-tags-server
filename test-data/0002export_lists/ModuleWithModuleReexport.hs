----------------------------------------------------------------------------
-- |
-- Module      :  ModuleWithModuleReexport
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

module ModuleWithModuleReexport
  ( module ModuleWithUnqualifiedReexport )
where

import ModuleWithUnqualifiedReexport

