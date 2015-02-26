----------------------------------------------------------------------------
-- |
-- Module      :  ModuleWithQualifiedModuleReexport
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

module ModuleWithQualifiedModuleReexport
  ( module MyModule )
where

import qualified ModuleWithQualifiedReexport as MyModule

