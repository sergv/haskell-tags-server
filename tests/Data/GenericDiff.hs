-- |
-- Module:     Data.GenericDiff
-- Copyright:  (c) Sergey Vinokurov 2025
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE PolyKinds         #-}

module Data.GenericDiff
  ( GenericDiff(..)
  , GGenericDiff(..)
  , FieldDiff(..)
  ) where

import Data.DList (DList)
import Data.DList qualified as DL
import Data.Kind
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import GHC.Generics
import GHC.TypeLits
import Prettyprinter

data FieldDiff = FieldDiff
  { ofName   :: Maybe Text
  , ofFirst  :: Doc Void
  , ofSecond :: Doc Void
  }

class GenericDiff (a :: Type) where
  genericDiff :: a -> a -> DList FieldDiff
  default genericDiff :: (Generic a, GGenericDiff (Rep a)) => a -> a -> DList FieldDiff
  genericDiff x y = ggenericDiff (from x) (from y)

instance GenericDiff ()

class GGenericDiff (f :: Type -> Type) where
  ggenericDiff :: f a -> f a -> DList FieldDiff

instance GGenericDiff V1 where
  ggenericDiff _ _ = DL.empty

instance GGenericDiff U1 where
  ggenericDiff U1 U1 = DL.empty

instance (Pretty a, Eq a) => GGenericDiff (K1 i a) where
  ggenericDiff (K1 x) (K1 y)
    | x == y    = DL.empty
    | otherwise = DL.singleton $ FieldDiff
      { ofName   = Nothing
      , ofFirst  = pretty x
      , ofSecond = pretty y
      }

instance GGenericDiff x => GGenericDiff (M1 S ('MetaSel 'Nothing a b c) x) where
  ggenericDiff (M1 x) (M1 y) = ggenericDiff x y

instance (KnownSymbol sym, GGenericDiff x) => GGenericDiff (M1 S ('MetaSel ('Just sym) a b c) x) where
  ggenericDiff (M1 x) (M1 y) =
    fmap (\z -> z { ofName = Just $ T.pack $ symbolVal (Proxy @sym) }) $
      ggenericDiff x y

instance GGenericDiff x => GGenericDiff (M1 C a x) where
  ggenericDiff (M1 x) (M1 y) = ggenericDiff x y

instance GGenericDiff x => GGenericDiff (M1 D a x) where
  ggenericDiff (M1 x) (M1 y) = ggenericDiff x y

instance (GGenericDiff f, GGenericDiff g) => GGenericDiff (f :*: g) where
  ggenericDiff (x1 :*: y1) (x2 :*: y2) =
    ggenericDiff x1 x2 <> ggenericDiff y1 y2
