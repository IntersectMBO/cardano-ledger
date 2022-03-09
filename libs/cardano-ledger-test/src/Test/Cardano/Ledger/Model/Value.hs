{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Model.Value where

import Cardano.Binary (ToCBOR (..))
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Compactible
import Cardano.Ledger.Val hiding (invert)
import Control.DeepSeq (NFData)
import Control.Lens (Lens')
import Control.Lens.Indexed (FoldableWithIndex (..))
import Control.Lens.Wrapped (_Wrapped)
import Data.Coerce (coerce)
import Data.Foldable (Foldable (..))
import Data.Group (Abelian, Group (..))
import Data.Group.GrpMap
  ( GrpMap (..),
    grpMap,
    grpMapSingleton,
    mkGrpMap,
    pointWise',
  )
import qualified Data.Map as Map
import Data.Monoid (Ap (..), Sum (..))
import Data.Typeable
import GHC.Generics (Generic)

newtype ModelValueF a = ModelValueF {unModelValueF :: (Coin, GrpMap a (Sum Integer))}
  deriving (Eq, Ord, Show, Generic)

instance Foldable ModelValueF where
  foldMap f (ModelValueF (_, GrpMap xs)) = foldMap f (Map.keysSet xs)
  toList (ModelValueF (_, GrpMap xs)) = Map.keys xs
  null (ModelValueF (_, GrpMap xs)) = null xs
  length (ModelValueF (_, GrpMap xs)) = length xs

mapModelValueF :: Ord b => (a -> b) -> ModelValueF a -> ModelValueF b
mapModelValueF f (ModelValueF (x, ys)) = ModelValueF (x, ifoldMap (\y -> grpMapSingleton (f y)) ys)

traverseModelValueF :: (Applicative m, Ord b) => (a -> m b) -> ModelValueF a -> m (ModelValueF b)
traverseModelValueF f (ModelValueF (x, ys)) =
  ModelValueF . (,) x <$> getAp (ifoldMap (\y q -> Ap $ grpMapSingleton <$> f y <*> pure q) ys)

instance Ord a => Semigroup (ModelValueF a) where
  ModelValueF x <> ModelValueF y = ModelValueF (x <> y)

instance Ord a => Monoid (ModelValueF a) where
  mempty = ModelValueF mempty

instance Ord a => Group (ModelValueF a) where
  invert (ModelValueF x) = ModelValueF (invert x)
  ModelValueF x ~~ ModelValueF y = ModelValueF (x ~~ y)
  pow (ModelValueF x) = ModelValueF . pow x

instance Ord a => Abelian (ModelValueF a)

instance Ord a => Val (ModelValueF a) where
  -- these operations are kinda sus; represented variables can (and do)
  -- represent unknown qty of ADA,
  coin (ModelValueF (c, _)) = c
  modifyCoin f (ModelValueF (c, x)) = ModelValueF (f c, x)

  (<×>) = flip pow
  inject x = ModelValueF (x, mempty)
  size (ModelValueF (_, v))
    -- see comment in Cardano.Ledger.Mary.Value/instance Val (Value)/size
    | v == mempty = 2
    | otherwise =
        fromIntegral $
          roundupBytesToWords (representationSize v)
            + repOverhead
    where
      repOverhead = 4 + adaWords + numberMulAssets
      adaWords = 1
      numberMulAssets = 1
      roundupBytesToWords b = quot (b + wordLength - 1) wordLength
      wordLength = 8

      -- we pick an upper bound of the size.  the real size of a mary value
      -- can be smaller.
      representationSize (GrpMap xs) = abcRegionSize + pidBlockSize + anameBlockSize
        where
          len = length v
          abcRegionSize = len * 12

          numPids = len -- at most len distinct Pids
          pidSize = 10
          pidBlockSize = numPids * pidSize

          assetNames = () <$ xs
          assetNameLengths = 32 <$ assetNames
          anameBlockSize = sum assetNameLengths

  pointwise f (ModelValueF (Coin x, y)) (ModelValueF (Coin x', y')) = f x x' && pointWise' 0 f (coerce y) (coerce y')

  isAdaOnly (ModelValueF (_, v)) = Map.null (unGrpMap v)
  isAdaOnlyCompact (ModelValueCompactF v) = isAdaOnly v
  injectCompact (CompactCoin w64) = ModelValueCompactF (inject (Coin (toInteger w64)))

instance NFData a => NFData (ModelValueF a)

getModelValueF :: ModelValueF a -> (Coin, Map.Map a Integer)
getModelValueF =
  (fmap . fmap) getSum
    . fmap unGrpMap
    . unModelValueF

mkModelValueF' :: Integral i => Coin -> Map.Map a i -> ModelValueF a
mkModelValueF' ada ma = ModelValueF (ada, mkGrpMap $ fmap fromIntegral ma)

atModelValueF :: Ord a => Maybe a -> Lens' (ModelValueF a) Integer
atModelValueF = \case
  Nothing -> \a2fb (ModelValueF (Coin a, rest)) ->
    ModelValueF . (,rest) . Coin <$> a2fb a
  Just idx -> \a2fb (ModelValueF (rest, a)) ->
    ModelValueF . (,) rest <$> (grpMap idx (_Wrapped a2fb) a)
{-# INLINE atModelValueF #-}

evalModelValue ::
  forall val m a.
  (Val val, Applicative m) =>
  (a -> m val) ->
  ModelValueF a ->
  m val
evalModelValue env (ModelValueF (c, as)) =
  (<+>) (inject c)
    <$> getAp (ifoldMap (\a (Sum n) -> Ap $ (<×>) n <$> env a) as)

-- No compacting, just an identitity with ModelValueF, since Compactible instance is needed
instance (Eq a, Show a, Typeable a) => Compactible (ModelValueF a) where
  newtype CompactForm (ModelValueF a) = ModelValueCompactF (ModelValueF a)
    deriving (Show, Eq)

  toCompact = Just . ModelValueCompactF
  fromCompact (ModelValueCompactF mvc) = mvc

instance Typeable a => ToCBOR (CompactForm (ModelValueF a)) where
  toCBOR = error "Unimplemented. Instance is only needed to satisfy Compactible constraints"
