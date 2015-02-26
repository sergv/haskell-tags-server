----------------------------------------------------------------------------
-- |
-- Module      :  ModuleWithImportsThatHaveModuleReexports
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

module ModuleWithImportsThatHaveModuleReexports where

import ModuleWithModuleReexport
-- this brings in no new names because that's what GHC does
import ModuleWithQualifiedModuleReexport

