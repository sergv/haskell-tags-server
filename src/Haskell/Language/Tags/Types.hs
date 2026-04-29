-- |
-- Module:     Haskell.Language.Tags.Types
-- Copyright:  (c) Sergey Vinokurov 2026
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE DerivingVia #-}

module Haskell.Language.Tags.Types
  ( Pos(..)
  , Line(..)
  , increaseLine
  , Offset(..)
  , SrcPos(..)
  , ParentTag(..)
  , TagVal(..)
  , tagLine
  , Type(..)
  , Tag(..)
  , onTagVal
  , partitionTags
  , extractParent
  ) where

import Control.DeepSeq (rnf, NFData)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Prettyprinter (Doc)
import Prettyprinter.Generics

data Pos a = Pos
  { posOf :: {-# UNPACK #-} !SrcPos
  , valOf :: !a
  }
  deriving (Eq, Ord, Generic)
  deriving Pretty via PPGeneric (Pos a)

instance NFData a => NFData (Pos a)

instance Show a => Show (Pos a) where
  show (Pos pos val) = show pos ++ ":" ++ show val

newtype Line = Line { unLine :: Int }
  deriving (Show, Eq, Ord, NFData, Num, Pretty)

newtype Offset = Offset { unOffset :: Int }
  deriving (Show, Eq, Ord, NFData, Num, Pretty)

increaseLine :: Line -> Line
increaseLine (Line n) = Line $! n + 1

data SrcPos = SrcPos
  { posLine   :: {-# UNPACK #-} !Line
  , posOffset :: {-# UNPACK #-} !Offset
    -- | No need to keep prefix strict since most of the prefixes will not be
    -- used.
  , posPrefix :: Text
  , posSuffix :: Text
  } deriving (Eq, Ord)

instance NFData SrcPos where
  rnf (SrcPos w x y z) = w `seq` rnf x `seq` rnf y `seq` rnf z

instance Show SrcPos where
  show (SrcPos line offset prefix suffix) =
    show (unLine line) ++ ":" ++ show (unOffset offset) ++ prefix' ++ suffix'
    where
      prefix' = clean prefix
      suffix' = clean suffix
      clean s | T.null s  = ""
              | otherwise = ":/" ++ T.unpack s ++ "/"

instance Pretty SrcPos where
  pretty = pretty . unLine . posLine

data ParentTag = ParentTag
  { ptName :: !Text
  , ptType :: !Type
  }
  deriving (Eq, Ord, Show, Generic)
  deriving Pretty via PPGeneric ParentTag

instance NFData ParentTag

data TagVal = TagVal
  { tvName   :: !Text
  , tvType   :: !Type
  , tvParent :: !(Maybe ParentTag)
    -- ^ parent of this tag; parent can only be of type
    -- Class, Data or Family
  }
  deriving (Eq, Ord, Show, Generic)
  deriving Pretty via PPGeneric TagVal

instance NFData TagVal

tagLine :: Pos TagVal -> Line
tagLine = posLine . posOf

-- | The Ord instance is used to sort tags with the same name.  Given multiple
-- matches, vim will visit them in order, so this should be in the order of
-- interest.
--
-- We rely that Type < Constructor.
data Type
  = Function
  | Type
  | Constructor
  | Class
  | Module
  | Operator
  | Pattern
  | Family
  | Define -- ^ Preprocessor #define
  deriving (Eq, Ord, Show, Generic)
  deriving Pretty via PPGeneric Type

instance NFData Type

data Tag
  = Tag !(Pos TagVal)
  -- | Just like Tag, except these should be deduplicated by their TagVal,
  -- where the one with the lowest line number will be preferred.
  -- The idea seems to be that functions will emit a tag for both the
  -- signature and definition.  TODO seems like a hack, why not just
  -- deduplicate all tags?  And I think I do that now with dropAdjacent.
  | RepeatableTag !(Pos TagVal)
  | Warning !(Pos (Doc Void))
  deriving (Show)

onTagVal :: (Pos TagVal -> Pos TagVal) -> Tag -> Tag
onTagVal f (Tag t)           = Tag $ f t
onTagVal f (RepeatableTag t) = RepeatableTag $ f t
onTagVal _ w@(Warning _)     = w

-- | Partition Tag, RepeatableTag, and Warning.
partitionTags :: [Tag] -> ([Pos TagVal], [Pos TagVal], [Pos (Doc Void)])
partitionTags = go [] [] []
  where
    go tags repeats warns [] = (tags, repeats, reverse warns)
    go tags repeats warns (t:ts) = case t of
      Tag a           -> go (a:tags) repeats warns ts
      RepeatableTag a -> go tags (a:repeats) warns ts
      Warning a       -> go tags repeats (a:warns) ts

extractParent :: Tag -> Maybe ParentTag
extractParent (Tag (Pos _ TagVal{tvName, tvType}))           =
  Just $ ParentTag tvName tvType
extractParent (RepeatableTag (Pos _ TagVal{tvName, tvType})) =
  Just $ ParentTag tvName tvType
extractParent (Warning _)                                    =
  Nothing

