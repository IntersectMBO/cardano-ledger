{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Functor.PiecewiseConstant where

import Control.Applicative (liftA2)
import Control.Lens.Fold (Fold, folding)
import Control.Lens.Indexed (FoldableWithIndex (..), FunctorWithIndex (..), TraversableWithIndex (..))
import Data.Foldable (Foldable (..))
import Data.Functor.Identity (Identity (..))
import Data.Group
import Data.Map.Strict as Map
import Data.Monoid (Ap (..))

data PieceInterval a = PieceInterval
  { _pieceInterval_lowerBound :: Maybe a,
    _pieceInterval_upperBound :: Maybe a
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | a datatype which represents a finitely supported total function; consisting
-- of constant pieces of the form [lb,ub)
data PiecewiseConstantMap a b = MkPiecewiseConstantMap b (Map.Map a b)
  deriving (Show, Functor, Foldable, Traversable)

deriving via
  (Ap (PiecewiseConstantMap k) a)
  instance
    (Ord k, Semigroup a) =>
    Semigroup (PiecewiseConstantMap k a)

deriving via
  (Ap (PiecewiseConstantMap k) a)
  instance
    (Ord k, Monoid a) =>
    Monoid (PiecewiseConstantMap k a)

instance (Ord k, Group a) => Group (PiecewiseConstantMap k a) where
  invert = fmap invert
  (~~) = liftA2 (~~)

instance (Ord k, Abelian a) => Abelian (PiecewiseConstantMap k a)

instance (Ord k, Eq a) => Eq (PiecewiseConstantMap k a) where
  xs == ys = and $ liftA2 (==) xs ys

instance (Ord k, Ord a) => Ord (PiecewiseConstantMap k a) where
  compare xs ys = fold $ liftA2 compare xs ys

instance Ord k => Applicative (PiecewiseConstantMap k) where
  (<*>) = liftA2 id
  {-# INLINE (<*>) #-}
  pure x = MkPiecewiseConstantMap x Map.empty
  liftA2 f = \(MkPiecewiseConstantMap a0 as0) (MkPiecewiseConstantMap b0 bs0) ->
    MkPiecewiseConstantMap (f a0 b0) $ Map.fromAscList $ go a0 b0 (Map.toAscList as0) (Map.toAscList bs0)
    where
      go _ _ [] [] = []
      go _ b as@(_ : _) [] = (fmap . fmap) (flip f b) as
      go a _ [] bs@(_ : _) = (fmap . fmap) (f a) bs
      go a b as@((ak, av) : aRest) bs@((bk, bv) : bRest) = case compare ak bk of
        LT -> (ak, f av b) : go av b aRest bs
        EQ -> (ak, f av bv) : go av bv aRest bRest
        GT -> (bk, f a bv) : go a bv as bRest
  {-# INLINE liftA2 #-}

-- | Piecewise constant maps do not form a category; consider `id :: PiecewiseConstantMap Rational Rational` cannot be finitely supported.
composePiecewiseConstantMap ::
  Ord b =>
  PiecewiseConstantMap b c ->
  PiecewiseConstantMap a b ->
  PiecewiseConstantMap a c
composePiecewiseConstantMap = fmap . liftPiecewiseConstantMap

liftPiecewiseConstantMap :: Ord a => PiecewiseConstantMap a b -> a -> b
liftPiecewiseConstantMap (MkPiecewiseConstantMap x xs) = maybe x snd . flip Map.lookupLE xs

-- it's not quite possible to have a pointwise lens in a piecewise constant map
-- because the pieces can't be "thin" enough without something like Enum;
ixPiecewiseConstantMap ::
  forall k a. Ord k => k -> Fold (PiecewiseConstantMap k a) a
ixPiecewiseConstantMap k = folding (\s -> Identity $ liftPiecewiseConstantMap s k)
{-# INLINE ixPiecewiseConstantMap #-}

-- combine two PiecewiseConstantMaps such that the range matches the first map
-- for the part of it's domain < k, and the second for parts >= k
splicePiecewiseConstantMap ::
  Ord k =>
  k ->
  PiecewiseConstantMap k a ->
  PiecewiseConstantMap k a ->
  PiecewiseConstantMap k a
splicePiecewiseConstantMap k (MkPiecewiseConstantMap x xs) y@(MkPiecewiseConstantMap _ ys) =
  MkPiecewiseConstantMap x (Map.unions [lower, middle, upper])
  where
    (lower, _) = Map.split k xs
    (_, upper) = Map.split k ys
    middle = Map.singleton k $ liftPiecewiseConstantMap y k

instance Ord k => Monad (PiecewiseConstantMap k) where
  MkPiecewiseConstantMap x0 xs >>= f =
    ifoldl
      (\k ys x -> splicePiecewiseConstantMap k ys $ f x)
      (f x0)
      xs

instance Ord k => FunctorWithIndex (PieceInterval k) (PiecewiseConstantMap k)

instance Ord k => FoldableWithIndex (PieceInterval k) (PiecewiseConstantMap k) where
  ifoldMap f (MkPiecewiseConstantMap x0 xs) = case Map.toAscList xs of
    [] -> f (PieceInterval Nothing Nothing) x0
    ((k, x') : ks) -> f (PieceInterval Nothing (Just k)) x0 <> go k x' ks
    where
      go k x [] = f (PieceInterval (Just k) Nothing) x
      go k x ((k', x') : rest) = f (PieceInterval (Just k) (Just k')) x <> go k' x' rest

instance Ord k => TraversableWithIndex (PieceInterval k) (PiecewiseConstantMap k) where
  itraverse f (MkPiecewiseConstantMap x0 xs) = case Map.toAscList xs of
    [] -> MkPiecewiseConstantMap <$> f (PieceInterval Nothing Nothing) x0 <*> pure Map.empty
    ((k, x') : ks) -> MkPiecewiseConstantMap <$> f (PieceInterval Nothing (Just k)) x0 <*> (Map.fromAscList <$> go k x' ks)
    where
      go k x [] = (\y -> [(k, y)]) <$> f (PieceInterval (Just k) Nothing) x
      go k x ((k', x') : rest) = (:) <$> fmap ((,) k) (f (PieceInterval (Just k) (Just k')) x) <*> go k' x' rest
