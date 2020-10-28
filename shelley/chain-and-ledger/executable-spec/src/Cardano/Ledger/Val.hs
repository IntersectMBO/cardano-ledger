{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
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

-- | This module defines a generalised notion of a "value" - that is, something
-- with which we may quantify a transaction output.
module Cardano.Ledger.Val
  ( Val (..),
    scale,
    invert,
    sumVal,
    scaledMinDeposit,
  )
where

import Data.Group (Abelian)
import Shelley.Spec.Ledger.Coin (Coin (..), DeltaCoin (..))

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

instance Val Coin where
  n <×> (Coin x) = Coin $ (fromIntegral n) * x
  coin = id
  inject = id
  size _ = 1
  modifyCoin f v = f v
  pointwise p (Coin x) (Coin y) = p x y

deriving via Coin instance Val DeltaCoin

{- The scaledMinDeposit calculation uses the minUTxOValue protocol parameter
(passed to it as Coin mv) as a specification of "the cost of
making a Shelley-sized UTxO entry", calculated here by "utxoEntrySizeWithoutVal + uint",
using the constants in the "where" clause.

In the case when a UTxO entry contains coins only (and the Shelley
UTxO entry format is used - we will extend this to be correct for other
UTxO formats shortly), the deposit should be exactly the minUTxOValue.
This is the "inject (coin v) == v" case.

Otherwise, we calculate the per-byte deposit by multiplying the minimum deposit (which is
for the number of Shelley UTxO-entry bytes) by the size of a Shelley UTxO entry.
This is the "(mv * (utxoEntrySizeWithoutVal + uint))" calculation.

We then calculate the total deposit required for making a UTxO entry with a Val-class
member v by dividing "(mv * (utxoEntrySizeWithoutVal + uint))" by the
estimated total size of the UTxO entry containing v, ie by
"(utxoEntrySizeWithoutVal + size v)".

See the formal specification for details.

-}

-- TODO : This scaling function is right for UTxO, not EUTxO
-- constants are temporary, the UTxO entry size calculation will be moved
scaledMinDeposit :: (Val v) => v -> Coin -> Coin
scaledMinDeposit v (Coin mv)
  | inject (coin v) == v = Coin mv -- without non-Coin assets, scaled deposit should be exactly minUTxOValue
  | otherwise = Coin $ fst $ quotRem (mv * (utxoEntrySizeWithoutVal + uint)) (utxoEntrySizeWithoutVal + size v) -- round down
  where
    -- address hash length is always same as Policy ID length
    addrHashLen :: Integer
    addrHashLen = 28

    smallArray :: Integer
    smallArray = 1

    hashLen :: Integer
    hashLen = 32

    uint :: Integer
    uint = 5

    hashObj :: Integer
    hashObj = 2 + hashLen

    addrHeader :: Integer
    addrHeader = 1

    address :: Integer
    address = 2 + addrHeader + 2 * addrHashLen

    -- input size
    inputSize :: Integer
    inputSize = smallArray + uint + hashObj

    -- size of output not including the Val (compute that part with vsize later)
    outputSizeWithoutVal :: Integer
    outputSizeWithoutVal = smallArray + address

    -- size of the UTxO entry (ie the space the scaled minUTxOValue deposit pays)
    utxoEntrySizeWithoutVal :: Integer
    utxoEntrySizeWithoutVal = inputSize + outputSizeWithoutVal
