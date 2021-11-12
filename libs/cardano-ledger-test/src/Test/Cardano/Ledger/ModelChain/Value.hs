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

module Test.Cardano.Ledger.ModelChain.Value where

import Cardano.Ledger.Coin
import Cardano.Ledger.Val hiding (invert)
import Control.DeepSeq
import Control.Lens (Lens')
import Control.Lens.Indexed (FoldableWithIndex (..), ifoldl)
import Control.Lens.Wrapped
import Control.Monad
import qualified Control.Monad.Except as Except
import Data.Coerce (coerce)
import Data.Functor.Identity
import Data.Group (Abelian, Group (..))
import Data.Group.GrpMap
import Data.Kind
import qualified Data.Map as Map
import Data.Monoid (Sum (..))
import GHC.Generics (Generic)

data ModelValueF (a :: Type) where
  ModelValue_Var :: a -> ModelValueF a
  ModelValue_Inject :: Coin -> ModelValueF a
  ModelValue_Scale :: Integer -> ModelValueF a -> ModelValueF a
  ModelValue_Add :: ModelValueF a -> ModelValueF a -> ModelValueF a
  ModelValue_Sub :: ModelValueF a -> ModelValueF a -> ModelValueF a
  deriving (Generic)

instance NFData a => NFData (ModelValueF a)

deriving instance Show a => Show (ModelValueF a)

deriving instance Eq a => Eq (ModelValueF a)

deriving instance Ord a => Ord (ModelValueF a)

deriving instance Functor ModelValueF

deriving instance Foldable ModelValueF

deriving instance Traversable ModelValueF

instance Applicative ModelValueF where
  pure = ModelValue_Var
  (<*>) = ap

instance Monad ModelValueF where
  xs >>= k = go xs
    where
      go = \case
        ModelValue_Var x -> k x
        ModelValue_Inject val -> ModelValue_Inject val
        ModelValue_Scale x ys -> ModelValue_Scale x (go ys)
        ModelValue_Add ys zs -> ModelValue_Add (go ys) (go zs)
        ModelValue_Sub ys zs -> ModelValue_Sub (go ys) (go zs)

data ModelValueError a
  = ValueUnderflow a a
  deriving (Show)

newtype ModelValueSimple a = ModelValueSimple {unModelValueSimple :: (Coin, GrpMap a (Sum Integer))}
  deriving (Eq, Ord, Show, Generic)

instance Ord a => Semigroup (ModelValueSimple a) where
  ModelValueSimple x <> ModelValueSimple y = ModelValueSimple (x <> y)

instance Ord a => Monoid (ModelValueSimple a) where
  mempty = ModelValueSimple mempty

instance Ord a => Group (ModelValueSimple a) where
  invert (ModelValueSimple x) = ModelValueSimple (invert x)
  ModelValueSimple x ~~ ModelValueSimple y = ModelValueSimple (x ~~ y)
  pow (ModelValueSimple x) = ModelValueSimple . pow x

instance Ord a => Abelian (ModelValueSimple a)

instance Ord a => Val (ModelValueSimple a) where
  -- these operations are kinda sus; represented variables can (and do)
  -- represent unknown qty of ADA, for the same reason that the question
  -- "How many prime factors does '15*y' have?", it has at least the factors of
  -- 15, but also, all of unknown factors of y.
  coin (ModelValueSimple (c, _)) = c
  modifyCoin f (ModelValueSimple (c, x)) = ModelValueSimple (f c, x)

  (<×>) = flip pow
  inject x = ModelValueSimple (x, mempty)
  size (ModelValueSimple (_, v))
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

  pointwise f (ModelValueSimple (Coin x, y)) (ModelValueSimple (Coin x', y')) = f x x' && pointWise' 0 f (coerce y) (coerce y')

instance NFData a => NFData (ModelValueSimple a)

-- evaluate a modelValue and compute the linear sum of all free variables.  Not
-- unlike a MA token, but
evalModelValueSimple ::
  Ord a =>
  ModelValueF a ->
  Either
    (ModelValueError (ModelValueSimple a))
    (ModelValueSimple a)
evalModelValueSimple =
  runIdentity
    . evalModelValue (\x -> Identity $ ModelValueSimple (mempty, GrpMap (Map.singleton x 1)))

getModelValueSimple :: ModelValueSimple a -> (Coin, Map.Map a Integer)
getModelValueSimple =
  (fmap . fmap) getSum
    . fmap unGrpMap
    . unModelValueSimple

mkModelValueF :: ModelValueSimple a -> ModelValueF a
mkModelValueF = uncurry mkModelValueF' . getModelValueSimple

mkModelValueF' :: Integral i => Coin -> Map.Map a i -> ModelValueF a
mkModelValueF' ada =
  ifoldl
    ( \v acc q ->
        if
            | q == 0 -> acc
            | q == 1 -> acc `ModelValue_Add` (ModelValue_Var v)
            | q > 0 -> acc `ModelValue_Add` ModelValue_Scale (fromIntegral q) (ModelValue_Var v)
            | otherwise {- q < 0 -} -> acc `ModelValue_Sub` ModelValue_Scale (fromIntegral (- q)) (ModelValue_Var v)
    )
    (ModelValue_Inject ada)

atModelValueSimple :: Ord a => Maybe a -> Lens' (ModelValueSimple a) Integer
atModelValueSimple = \case
  Nothing -> \a2fb (ModelValueSimple (Coin a, rest)) ->
    ModelValueSimple . (,rest) . Coin <$> a2fb a
  Just idx -> \a2fb (ModelValueSimple (rest, a)) ->
    ModelValueSimple . (,) rest <$> (grpMap idx (_Wrapped a2fb) a)
{-# INLINE atModelValueSimple #-}

evalModelValue ::
  forall val m a.
  (Val val, Monad m) =>
  (a -> m val) ->
  ModelValueF a ->
  m (Either (ModelValueError val) val)
evalModelValue env x = Except.runExceptT (go x)
  where
    go :: ModelValueF a -> Except.ExceptT (ModelValueError val) m val
    go (ModelValue_Var a) = Except.lift $ env a
    go (ModelValue_Inject c) = pure (inject c)
    go (ModelValue_Add a b) = (<+>) <$> go a <*> go b
    go (ModelValue_Scale n a) = (n <×>) <$> go a
    go (ModelValue_Sub a b) = do
      a' <- go a
      b' <- go b
      unless (pointwise (>=) a' b') $ Except.throwError $ ValueUnderflow a' b'
      pure $ a' <-> b'
