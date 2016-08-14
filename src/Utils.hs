----------------------------------------------------------------------------
-- |
-- Module      :  Utils
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Utils (Morphism(..), Iso(..)) where

import Data.Text (Text)
import qualified Data.Text as T

class Morphism a b where
  morph :: a -> b

class (Morphism a b, Morphism b a) => Iso a b

instance Morphism String String where
  morph = id

instance Morphism String Text where
  morph = T.pack

instance Morphism Text String where
  morph = T.unpack
