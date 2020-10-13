{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

-- | This module defines a generalised notion of a "value" - that is, something
-- with which we may quantify a transaction output. Values will have Val instances
-- and be defined as data family ASSET instances.
module Cardano.Ledger.Val
  ( Val (..),
    scale,
    invert,
    sumVal,
    Asset (..),
    ASSET,
    Blessed (..),
  )
where

import Data.Group (Abelian)
import Data.Kind

-- ================================================================

class
  ( Abelian t,
    Eq t
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

-- =============================================================
-- Synonyms with types fixed at (Val t). Makes calls easier
-- to read, and gives better error messages, when a mistake is made

infixl 6 <+>

infixl 6 <->

infixl 7 <×>

scale :: (Val t, Integral i) => i -> t -> t
scale i v = i <×> v

sumVal :: (Foldable t, Val v) => t v -> v
sumVal xs = foldl (<+>) mempty xs

invert :: Val t => t -> t
invert x = (-1 :: Integer) <×> x

-- =====================================================================
-- Several instances of Val will be type family instances of ASSET

type Coin = ASSET 'Ada ()

data Asset = Ada | MultiAsset | G Asset

class Blessed (t::Asset) where
  prezero :: ASSET t era
  precoin:: ASSET t era -> Coin
  preinject :: Coin -> ASSET t era

data family ASSET (t :: Asset) :: Type -> Type