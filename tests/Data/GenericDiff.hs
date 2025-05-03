-- |
-- Module:     Data.GenericDiff
-- Copyright:  (c) Sergey Vinokurov 2025
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-partial-fields   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Data.GenericDiff
  ( GenericDiff(..)
  , ActualExpected(..)
  , Location(..)

  , GGenericDiff(..)

  , Difference(..)
  , ppDifference
  ) where

import Data.Bifunctor
import Data.DList (DList)
import Data.DList qualified as DL
import Data.Foldable
import Data.Kind
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable
import Data.Void
import GHC.Generics
import GHC.TypeLits
import Prettyprinter
import Prettyprinter.Combinators
import Prettyprinter.Generics
import Prettyprinter.MetaDoc

import Data.KeyMap (KeyMap)
import Data.KeyMap qualified as KM
import Data.Map.NonEmpty (NonEmptyMap)
import Data.Map.NonEmpty qualified as NEMap
import Data.SubkeyMap (SubkeyMap)
import Data.SubkeyMap qualified as SM
import Data.SymbolMap (SymbolMap)
import Haskell.Language.Server.Tags.Types.Imports
import Haskell.Language.Server.Tags.Types.Modules

data ActualExpected a = ActualExpected a a
  deriving (Eq, Ord, Generic, Functor, Foldable, Traversable)

data Location = InField Text | InType TypeRep

data MapDifference = MapDifference
  { extraInActual   :: [(Doc Void, Doc Void)]
  , missingInActual :: [(Doc Void, Doc Void)]
  , differentValues :: [(Doc Void, NonEmpty Difference)]
  }

data SpecificDifference
  = DifferentValues (ActualExpected (Doc Void))
  -- | DifferentLengths (ActualExpected Int) (NonEmpty SpecificDifference)
  | DifferentElements (NonEmpty (Int, NonEmpty Difference)) (ActualExpected (Doc Void))
  | DifferentMaps MapDifference (ActualExpected (Doc Void))

data Difference
  = Difference (NonEmpty Location) SpecificDifference

addLoc :: Location -> Difference -> Difference
addLoc loc (Difference locs x) = Difference (NE.cons loc locs) x

ppDifference :: forall ann. Difference -> MapEntry Text (Doc ann)
ppDifference (Difference locs diff) =
  go locs
  where
    end :: Doc ann
    end = vacuous $ case diff of
      DifferentValues (ActualExpected actual expected) ->
        ppDictAssocList
          [ "Actual"   :-> actual
          , "Expected" :-> expected
          ]
      DifferentElements xs (ActualExpected actual expected) ->
        ppDictAssocList
          [ "Element differences" :->
              ppAssocListWithSep "->" pretty (ppDictAssocList . map ppDifference . toList) (toList xs)
          , "Actual"              :-> actual
          , "Expected"            :-> expected
          ]
      DifferentMaps MapDifference{extraInActual, missingInActual, differentValues} (ActualExpected actual expected) ->
        ppDictAssocList
          [ "Extra in actual"   :-> ppAssocListWithSep "->" id id extraInActual
          , "Missing in actual" :-> ppAssocListWithSep "->" id id missingInActual
          , "Different values"  :-> ppAssocListWithSep "->" id (ppDictAssocList . map ppDifference . toList) differentValues
          , "Actual"            :-> vacuous actual
          , "Expected"          :-> vacuous expected
          ]
    go :: NonEmpty Location -> MapEntry Text (Doc ann)
    go (x :| xs) =
      case x of
        InField name ->
          ("In field " <> name) :-> xs'
        InType name  ->
          ("In type " <> T.pack (show name)) :-> xs'
      where
        xs' = case xs of
          []     -> end
          y : ys -> ppDictAssocList [go $ y :| ys]

class GenericDiff (a :: Type) where
  genericDiff :: ActualExpected a -> DList Difference
  default genericDiff
    :: (Generic a, GGenericDiff (Rep a), PPGenericOverride a, Typeable a)
    => ActualExpected a
    -> DList Difference
  genericDiff ae@(ActualExpected x y) =
    ggenericDiff (typeOf x) (ppGenericOverrideDoc <$> ae) (from x) (from y)

instance {-# OVERLAPS #-} (Eq a, Typeable a, PPGenericOverride a) => GenericDiff a where
  genericDiff ae@(ActualExpected actual expected)
    | actual == expected = DL.empty
    | otherwise          =
      DL.singleton $ Difference (NE.singleton loc) $ DifferentValues $ ppGenericOverrideDoc <$> ae
    where
      loc = InType (typeOf actual)

class GGenericDiff (f :: Type -> Type) where
  ggenericDiff :: TypeRep -> ActualExpected (Doc Void) -> f a -> f a -> DList Difference

instance GGenericDiff x => GGenericDiff (M1 S ('MetaSel 'Nothing a b c) x) where
  ggenericDiff typ ae (M1 x) (M1 y) = ggenericDiff typ ae x y

instance (KnownSymbol selSym, GGenericDiff x, Typeable b) => GGenericDiff (M1 S ('MetaSel ('Just selSym) a b c) x) where
  ggenericDiff typ ae (M1 x) (M1 y) =
    fmap (addLoc (InField (T.pack $ symbolVal (Proxy @selSym)))) $
      ggenericDiff typ ae x y

instance GGenericDiff x => GGenericDiff (M1 C a x) where
  ggenericDiff typ ae (M1 x) (M1 y) = ggenericDiff typ ae x y

instance GGenericDiff x => GGenericDiff (M1 D a x) where
  ggenericDiff typ ae (M1 x) (M1 y) =
    fmap (addLoc (InType typ)) $
      ggenericDiff typ ae x y

instance GGenericDiff V1 where
  ggenericDiff _ _ _ _ = DL.empty

instance GGenericDiff U1 where
  ggenericDiff _ _ U1 U1 = DL.empty

instance GenericDiff a => GGenericDiff (K1 i a) where
  ggenericDiff _ _ (K1 x) (K1 y) = genericDiff (ActualExpected x y)


instance (GGenericDiff f, GGenericDiff g) => GGenericDiff (f :*: g) where
  ggenericDiff locs ae (x1 :*: y1) (x2 :*: y2) =
    ggenericDiff locs ae x1 x2 <> ggenericDiff locs ae y1 y2

instance (GGenericDiff f, GGenericDiff g) => GGenericDiff (f :+: g) where
  ggenericDiff typ ae (L1 x1) (L1 x2) = ggenericDiff typ ae x1 x2
  ggenericDiff typ ae (R1 y1) (R1 y2) = ggenericDiff typ ae y1 y2
  ggenericDiff typ ae _       _       =
    DL.singleton $ Difference (NE.singleton (InType typ)) $ DifferentValues ae

ppGenericOverrideDoc :: PPGenericOverride a => a -> Doc ann
ppGenericOverrideDoc = mdPayload . ppGenericOverride

instance (Eq a, Pretty a, GenericDiff a, Typeable a) => GenericDiff (Module a)
instance GenericDiff ModuleHeader
instance GenericDiff ModuleExports
instance (Eq a, Pretty a, GenericDiff a, Typeable a) => GenericDiff (ModuleExportSpec a)
instance GenericDiff SymbolMap

instance (Eq a, Pretty a, GenericDiff a, Typeable a, Eq b, Pretty b, GenericDiff b, Typeable b) => GenericDiff (EntryWithChildren a b)

neZipWith3 :: (a -> b -> c -> d) -> NonEmpty a -> NonEmpty b -> NonEmpty c -> NonEmpty d
neZipWith3 f as bs cs = NE.zipWith ($) (NE.zipWith f as bs) cs

instance (Eq a, Pretty a, GenericDiff a, Typeable a) => GenericDiff (NonEmpty a) where
  genericDiff ae@(ActualExpected actuals expecteds)
    | actuals == expecteds
    = DL.empty
    | NE.length actuals /= NE.length expecteds
    = DL.singleton $ Difference (NE.singleton loc) $ DifferentValues $ ppGenericOverrideDoc <$> ae
    | otherwise
    = case catMaybes $ L.zipWith3 f [0..] (toList actuals) (toList expecteds) of
        []     -> error "Values are different but genericDiff returned no differences"
        x : xs ->
          DL.singleton $ Difference (NE.singleton loc) $ DifferentElements (x :| xs) $ ppGenericOverrideDoc <$> ae
    where
      loc = InType (typeOf actuals)

      f n actual expected = case toList $ genericDiff (ActualExpected actual expected) of
        []     -> Nothing
        x : xs -> Just (n, x :| xs)

instance (GenericDiff (KM.Key a), KM.HasKey a, GenericDiff (f a), Eq (f a), Typeable f, Typeable a, PPGenericOverride (f a), PPGenericOverride (KM.Key a), PPGenericOverride a) => GenericDiff (KeyMap f a) where
  genericDiff ae@(ActualExpected actual _) = genericDiffMaps (InType (typeOf actual)) (KM.toMap <$> ae)

instance
  (Ord k, GenericDiff k, GenericDiff (SM.Subkey k), GenericDiff v, Eq k, Eq (SM.Subkey k), Eq v, PPGenericOverride k, PPGenericOverride (SM.Subkey k), PPGenericOverride v, Typeable k, Typeable v)
  => GenericDiff (SubkeyMap k v) where
  genericDiff ae@(ActualExpected actual _) = genericDiffMaps (InType (typeOf actual)) (SM.toMap <$> ae)

instance
  (Ord k, GenericDiff k, GenericDiff v, Eq v, Typeable k, Typeable v, PPGenericOverride k, PPGenericOverride v)
  => GenericDiff (Map k v) where
  genericDiff ae@(ActualExpected actual _) = genericDiffMaps (InType (typeOf actual)) ae

instance
  (Ord k, GenericDiff k, GenericDiff v, Eq v, Typeable k, Typeable v, PPGenericOverride k, PPGenericOverride v)
  => GenericDiff (NonEmptyMap k v) where
  genericDiff ae@(ActualExpected actual _) = genericDiffMaps (InType (typeOf actual)) (NEMap.toMap <$> ae)

genericDiffMaps
  :: forall k v. (Ord k, GenericDiff k, GenericDiff v, Eq v, PPGenericOverride k, PPGenericOverride v)
  => Location
  -> ActualExpected (Map k v)
  -> DList Difference
genericDiffMaps loc ae@(ActualExpected actual expected)
    | actual == expected = DL.empty
    | otherwise          =
      DL.singleton $ Difference (NE.singleton loc) $ DifferentMaps md $ ppGenericOverrideDoc <$> ae
    where

      md :: MapDifference
      md = MapDifference
        { extraInActual   = map (bimap ppGenericOverrideDoc ppGenericOverrideDoc) $ M.toList extra
        , missingInActual = map (bimap ppGenericOverrideDoc ppGenericOverrideDoc) $ M.toList missing
        , differentValues = diff
        }

      extra :: Map k v
      extra = M.difference actual expected

      missing :: Map k v
      missing = M.difference expected actual

      diff :: [(Doc Void, NonEmpty Difference)]
      diff
        = mapMaybe
            (\(key, diffs) -> case toList diffs of
              []     -> Nothing
              x : xs -> Just $ (key, x :| xs))
        $ map (bimap ppGenericOverrideDoc genericDiff)
        $ M.toList
        $ M.intersectionWith ActualExpected actual expected

