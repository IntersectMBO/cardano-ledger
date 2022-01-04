{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Group.GrpMap where

import Control.DeepSeq (NFData)
import Control.Lens (Iso', Lens', at, iso, set)
import Control.Lens.Indexed (FoldableWithIndex (..))
import Control.Monad (guard)
import Data.Functor.Const (Const (..))
import Data.Group (Abelian, Group (..))
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as MapLazy
import qualified Data.Map.Merge.Strict as Map
import Data.Monoid (All (..))
import qualified Data.Set as Set
import GHC.Generics (Generic)
import qualified GHC.TypeLits as GHC

-- TODO: groups/monoidal-containers/some other place?
newtype GrpMap k v = GrpMap {unGrpMap :: Map.Map k v}
  deriving (Foldable, Eq, Ord, Show, Generic)

instance (NFData k, NFData v) => NFData (GrpMap k v)

restrictKeysGrpMap :: Ord k => GrpMap k a -> Set.Set k -> GrpMap k a
restrictKeysGrpMap (GrpMap xs) = GrpMap . Map.restrictKeys xs

grpMapSingleton :: (Eq v, Monoid v) => k -> v -> GrpMap k v
grpMapSingleton k v
  | v == mempty = GrpMap Map.empty
  | otherwise = GrpMap $ Map.singleton k v

mkGrpMap :: (Eq v, Monoid v) => Map.Map k v -> GrpMap k v
mkGrpMap = GrpMap . Map.filter (mempty /=)

_GrpMap :: (Eq a, Monoid a) => Iso' (GrpMap k a) (Map.Map k a)
_GrpMap = iso unGrpMap mkGrpMap

grpMap :: (Ord k, Eq a, Monoid a) => k -> Lens' (GrpMap k a) a
grpMap k a2fb (GrpMap s) = (\y -> GrpMap $ set (at k) (y <$ guard (y /= mempty)) s) <$> a2fb (maybe mempty id $ Map.lookup k s)

instance GHC.TypeError ('GHC.Text "GrpMap is not a Functor, use mapGrpMap") => Functor (GrpMap k) where
  fmap = error "GrpMap is not a Functor, use mapGrpMap"

instance GHC.TypeError ('GHC.Text "GrpMap is not a Functor, use mapGrpMap") => Traversable (GrpMap k) where
  traverse = error "GrpMap is not Traversable, use traverseGrpMap"

mapGrpMap :: (Eq b, Monoid b) => (a -> b) -> GrpMap k a -> GrpMap k b
mapGrpMap f = GrpMap . Map.mapMaybe f' . unGrpMap
  where
    f' x = let y = f x in y <$ guard (y /= mempty)
{-# INLINE mapGrpMap #-}

zipWithGrpMap :: (Ord k, Eq c, Monoid a, Monoid b, Monoid c) => (a -> b -> c) -> GrpMap k a -> GrpMap k b -> GrpMap k c
zipWithGrpMap f = \(GrpMap xs) (GrpMap ys) ->
  GrpMap $
    Map.merge
      (Map.mapMaybeMissing $ \_ x -> f' x mempty)
      (Map.mapMaybeMissing $ \_ y -> f' mempty y)
      (Map.zipWithMaybeMatched $ \_ -> f')
      xs
      ys
  where
    f' x y = let z = f x y in z <$ guard (z /= mempty)
{-# INLINE zipWithGrpMap #-}

-- | laws: traverseGrpMap (f . const mempty) xs === pure mempty
traverseGrpMap :: (Eq b, Monoid b, Applicative m) => (a -> m b) -> GrpMap k a -> m (GrpMap k b)
traverseGrpMap f = fmap GrpMap . Map.traverseMaybeWithKey f' . unGrpMap
  where
    f' _ x = (\y -> y <$ guard (y /= mempty)) <$> f x
{-# INLINE traverseGrpMap #-}

instance FoldableWithIndex k (GrpMap k) where
  ifoldMap f = ifoldMap f . unGrpMap

instance (Ord k, Eq v, Monoid v) => Semigroup (GrpMap k v) where
  GrpMap xs <> GrpMap ys = GrpMap $ Map.merge Map.preserveMissing Map.preserveMissing (Map.zipWithMaybeMatched f) xs ys
    where
      f _ x y = let z = x <> y in z <$ guard (z /= mempty)
  {-# INLINE (<>) #-}

instance (Ord k, Eq v, Monoid v) => Monoid (GrpMap k v) where
  mempty = GrpMap Map.empty
  {-# INLINE mempty #-}

instance (Ord k, Eq v, Group v) => Group (GrpMap k v) where
  invert = GrpMap . fmap invert . unGrpMap
  {-# INLINE invert #-}
  GrpMap xs ~~ GrpMap ys = GrpMap $ Map.merge Map.preserveMissing (Map.mapMissing $ const invert) (Map.zipWithMaybeMatched f) xs ys
    where
      f _ x y = let z = x ~~ y in z <$ guard (z /= mempty)
  {-# INLINE (~~) #-}

instance (Ord k, Eq v, Abelian v) => Abelian (GrpMap k v)

pointWise' :: Ord k => v -> (v -> v -> Bool) -> Map.Map k v -> Map.Map k v -> Bool
pointWise' z p = \xs ys ->
  getAll $! getConst $
    MapLazy.mergeA
      (MapLazy.traverseMissing $ \_ x -> Const $ All $ p x z)
      (MapLazy.traverseMissing $ \_ y -> Const $ All $ p z y)
      (MapLazy.zipWithAMatched $ \_ x y -> Const $ All $ p x y)
      xs
      ys
{-# INLINE pointWise' #-}
