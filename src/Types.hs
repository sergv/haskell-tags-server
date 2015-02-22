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

import Data.List.NonEmpty (NonEmpty)
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
  , confLazyTagging       :: Bool         -- ^ whether to read and compute tags lazily
                                          -- or read them all at once on server start
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

-- -- ^ Designates specific name to be imported/hidden from a module or a
-- -- datatype/class name to be imported/hidden along with all its
-- -- constructors&fields/member functions&associated type families.
-- data ImportThing =
--     ImportName SymbolName
--   | ImportAll SymbolName
--   deriving (Show, Eq, Ord)
--
-- -- ^ Specifies which names are imported or hidden from a module.
-- data ImportList =
--     Imported (NonEmpty ImportThing)
--   | Hidden (NonEmpty ImportThing)
--   deriving (Show, Eq, Ord)

data Qualification =
    Qualified ModuleName -- ^ qualifier
  | Unqualified
  | BothQualifiedAndUnqualified ModuleName -- ^ qualifier
  deriving (Show, Eq, Ord)

getQualifier :: Qualification -> Maybe ModuleName
getQualifier (Qualified q)                   = Just q
getQualifier Unqualified                     = Nothing
getQualifier (BothQualifiedAndUnqualified q) = Just q

importsUnqualifiedNames :: Qualification -> Bool
importsUnqualifiedNames (Qualified _)                   = False
importsUnqualifiedNames Unqualified                     = True
importsUnqualifiedNames (BothQualifiedAndUnqualified _) = True


data Module = Module
  { modImports          :: [(ModuleName, Qualification)]
    -- ^ all imports of a given module, even qualified ones and type of their import
    -- NB same module namae may be present several times with different qualifications
    -- because it may be imported several times.
  , modImportQualifiers :: Map ModuleName ModuleName
    -- ^ mapping from qualifiers to original module names
  , modExports          :: Map SymbolName Symbol
    -- ^ exports of a given module
  , modAllSymbols       :: Map SymbolName Symbol
    -- ^ all symbols defined in this module
  , modSource           :: Text
  , modFile             :: FilePath
  , modLastModified     :: UTCTime -- ^ time as reported by getModificationTime
  }
  deriving (Show, Eq, Ord)

data ServerState = ServerState
  { stateModules :: Map ModuleName [Module]
  }
  deriving (Show, Eq, Ord)

emptyServerState :: ServerState
emptyServerState = ServerState M.empty

