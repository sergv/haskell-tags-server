----------------------------------------------------------------------------
-- |
-- Module      :  Prettyprinter.Ext
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Tuesday, 30 August 2016
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings     #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Prettyprinter.Ext
  ( Pretty(..)
  , (<+>)
  , show'
  , show''
  , ppKeyMapWith
  , SubkeyMap.ppSubkeyMapWith
  , ppNEMap
  , ppMonoidalMapWith
  , docFromByteString

  , module Prettyprinter.Combinators
  , module Prettyprinter.Generics
  , module Prettyprinter.Show
  ) where

import Data.ByteString.Lazy.UTF8 qualified as UTF8
import Data.Foldable (toList)
import Data.Text qualified as T
import Data.Text.Encoding.Error qualified as TEE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Prettyprinter ((<+>))
import Prettyprinter qualified as PP
import Prettyprinter.Combinators
import Prettyprinter.Generics
import Prettyprinter.Show

import Data.KeyMap (KeyMap)
import Data.KeyMap qualified as KM
import Data.Map.NonEmpty (NonEmptyMap)
import Data.Map.NonEmpty qualified as NEMap
import Data.MonoidalMap (MonoidalMap)
import Data.MonoidalMap qualified as MM
import Data.SubkeyMap qualified as SubkeyMap

docFromByteString :: UTF8.ByteString -> Doc ann
docFromByteString = PP.pretty . TLE.decodeUtf8With TEE.lenientDecode

ppKeyMapWith
  :: Foldable f
  => (KM.Key a -> Doc ann)
  -> (a -> Doc ann)
  -> KeyMap f a
  -> Doc ann
ppKeyMapWith ppKey ppVal = ppAssocListWith ppKey (ppListWith ppVal . toList) . KM.toList

ppNEMap :: (Pretty k, Pretty v) => NonEmptyMap k v -> Doc ann
ppNEMap = ppAssocList . toList . NEMap.toNonEmpty

ppMonoidalMapWith
  :: (k -> Doc ann)
  -> (v -> Doc ann)
  -> MonoidalMap k v
  -> Doc ann
ppMonoidalMapWith k v = ppMapWith k v . MM.unMonoidalMap

show' :: Show a => a -> T.Text
show' = T.pack . show

show'' :: Show a => a -> TL.Text
show'' = TL.pack . show
