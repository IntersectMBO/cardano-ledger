{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module defines a generalised notion of a "value" - that is, something
-- with which we may quantify a transaction output.
module Cardano.Ledger.Val (
  Val (..),
  scale,
  invert,
  sumVal,
  adaOnly,
)
where

import Cardano.Ledger.Coin (Coin (..), CompactForm (..), DeltaCoin (..))
import Cardano.Ledger.Compactible (Compactible (..))
import Data.Coerce
import Data.Foldable (foldl')
import Data.Group (Abelian)

class
  ( Compactible t
  , Abelian t
  , Eq t
  ) =>
  Val t
  where
  -- | the value with nothing in it
  zero :: t
  zero = mempty

  -- | add two value
  (<+>) :: t -> t -> t
  x <+> y = x <> y

  -- | scale a value by an Integral constant
  (<×>) :: Integral i => i -> t -> t

  -- | subtract two values
  (<->) :: t -> t -> t
  x <-> y = x <+> ((-1 :: Integer) <×> y)

  -- | Is the argument zero?
  isZero :: t -> Bool
  isZero t = t == mempty

  -- | Get the ADA present in the value (since ADA is our "blessed" currency)
  coin :: t -> Coin

  -- | Create a value containing only this amount of ADA
  inject :: Coin -> t

  -- | modify the blessed Coin part of t
  modifyCoin :: (Coin -> Coin) -> t -> t

  size :: t -> Integer -- compute size of Val instance

  -- | used to compare values pointwise. Rather than using: (v1 <= v2) use: pointwise (<=) v1 v2
  -- | If a quantity is stored in only one of 'v1' or 'v2', we use 0 for the missing quantity.
  pointwise :: (Integer -> Integer -> Bool) -> t -> t -> Bool

  -- | Check if value contains only ADA. Must hold property:
  --
  -- > inject (coin v) == v
  isAdaOnly :: t -> Bool

  isAdaOnlyCompact :: CompactForm t -> Bool

  coinCompact :: CompactForm t -> CompactForm Coin

  injectCompact :: CompactForm Coin -> CompactForm t

  modifyCompactCoin :: (CompactForm Coin -> CompactForm Coin) -> CompactForm t -> CompactForm t

-- =============================================================
-- Synonyms with types fixed at (Val t). Makes calls easier
-- to read, and gives better error messages, when a mistake is made

infixl 6 <+>

infixl 6 <->

infixl 7 <×>

scale :: (Val t, Integral i) => i -> t -> t
scale i v = i <×> v

sumVal :: (Foldable t, Val v) => t v -> v
sumVal = foldl' (<+>) mempty

invert :: Val t => t -> t
invert x = (-1 :: Integer) <×> x

-- returns a Value containing only the coin (ada) tokens from the input Value
adaOnly :: Val v => v -> Bool
adaOnly v = (inject . coin) v == v
{-# DEPRECATED adaOnly "In favor of `isAdaOnly`" #-}

instance Val Coin where
  n <×> (Coin x) = Coin $ fromIntegral n * x
  coin = id
  inject = id
  size _ = 1
  modifyCoin f v = f v
  pointwise p (Coin x) (Coin y) = p x y
  isAdaOnly _ = True
  isAdaOnlyCompact _ = True
  coinCompact = id
  injectCompact = id
  modifyCompactCoin = ($)

instance Val DeltaCoin where
  n <×> (DeltaCoin x) = DeltaCoin $ fromIntegral n * x
  coin = coerce
  inject = coerce
  size _ = 1
  modifyCoin f v = coerce f v
  pointwise p (DeltaCoin x) (DeltaCoin y) = p x y
  isAdaOnly _ = True
  isAdaOnlyCompact _ = True
  coinCompact (CompactDeltaCoin cc) = CompactCoin cc
  injectCompact (CompactCoin cc) = CompactDeltaCoin cc
  modifyCompactCoin f (CompactDeltaCoin cc) =
    case f (CompactCoin cc) of
      CompactCoin cc' -> CompactDeltaCoin cc'
