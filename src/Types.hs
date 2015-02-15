----------------------------------------------------------------------------
-- |
-- Module      :  Types
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

module Types where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

import qualified Network.BERT.Transport as BERT
import FastTags (Pos(..), TagVal(..))


data ServerConfig s = ServerConfig
  { confSourceDirectories :: Set FilePath -- ^ directories with haskell files to index
  , confCabalDirectories  :: Set FilePath -- ^ directories with cabal packages to index
  , confLazyTagging       :: Bool       -- ^ whether to read and compute tags lazily
  , confServer            :: s
  }
  deriving (Show, Eq, Ord)

emptyServerConfig :: (BERT.Server s) => s -> ServerConfig s
emptyServerConfig serv = ServerConfig S.empty S.empty False serv

-- e.g. Foo, Foo.Bar
newtype ModuleName = ModuleName { getModuleName :: Text }
  deriving (Show, Eq, Ord)

type Symbol = Pos TagVal

newtype SymbolName = SymbolName { getSymbolName :: Text }
  deriving (Show, Eq, Ord)

data Module = Module
  { modImports          :: [ModuleName] -- ^ all imports of a given module, even qualified ones
  , modImportQualifiers :: Map ModuleName ModuleName -- ^ mapping from qualifiers to original module names
  , modExports          :: Map SymbolName Symbol
  , modAllSymbols       :: Map SymbolName Symbol
  , modSource           :: Text
  , modFile             :: FilePath
  , modLastModified     :: UTCTime
  }
  deriving (Show, Eq, Ord)

data ServerState = ServerState
  { stateModules :: Map ModuleName [Module]
  }
  deriving (Show, Eq, Ord)

emptyServerState :: ServerState
emptyServerState = ServerState M.empty

