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
    DecodeNonNegative (..),
    DecodeMint (..),
    EncodeMint (..),
    scaledMinDeposit,
  )
where

import Cardano.Binary (Decoder, Encoding, decodeWord64, toCBOR)
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Prelude (heapWordsUnpacked)
import Data.Group (Abelian)
import Shelley.Spec.Ledger.Coin (Coin (..), CompactForm (..), DeltaCoin (..))

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

-- =============================================================

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

-- This scaling function is right for UTxO, not EUTxO
--
scaledMinDeposit :: (Val v) => v -> Coin -> Coin
scaledMinDeposit v (Coin mv)
  | inject (coin v) == v = Coin mv -- without non-Coin assets, scaled deposit should be exactly minUTxOValue
  -- The calculation should represent this equation
  -- minValueParameter / coinUTxOSize = actualMinValue / valueUTxOSize
  -- actualMinValue = (minValueParameter / coinUTxOSize) * valueUTxOSize
  | otherwise = Coin $ max mv (adaPerUTxOWord * (utxoEntrySizeWithoutVal + size v))
  where
    -- lengths obtained from tracing on HeapWords of inputs and outputs
    -- obtained experimentally, and number used here
    -- units are Word64s
    txoutLenNoVal = 14
    txinLen = 7

    -- unpacked CompactCoin Word64 size in Word64s
    coinSize :: Integer
    coinSize = fromIntegral $ heapWordsUnpacked (CompactCoin 0)

    utxoEntrySizeWithoutVal :: Integer
    utxoEntrySizeWithoutVal = 6 + txoutLenNoVal + txinLen

    -- how much ada does a Word64 of UTxO space cost, calculated from minAdaValue PP
    -- round down
    adaPerUTxOWord :: Integer
    adaPerUTxOWord = quot mv (utxoEntrySizeWithoutVal + coinSize)

-- =============================================================

class DecodeNonNegative v where
  decodeNonNegative :: Decoder s v

instance DecodeNonNegative Coin where
  decodeNonNegative = Coin . fromIntegral <$> decodeWord64

instance (DecodeNonNegative a, Compactible a, Show a) => DecodeNonNegative (CompactForm a) where
  decodeNonNegative = do
    v <- decodeNonNegative
    maybe (fail $ "illegal value: " <> show v) pure (toCompact v)

-- =============================================================

class DecodeMint v where
  decodeMint :: Decoder s v

instance DecodeMint Coin where
  decodeMint = fail "cannot have coin in mint field"

-- =============================================================

class EncodeMint v where
  encodeMint :: v -> Encoding

instance EncodeMint Coin where
  --we expect nothing to be able to successfully decode this
  --this is an alternative to throwing an error at encoding
  encodeMint = toCBOR
