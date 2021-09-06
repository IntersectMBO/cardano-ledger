{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
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
    adaOnly,
    DecodeNonNegative (..),
    DecodeMint (..),
    EncodeMint (..),
  )
where

import Cardano.Binary (Decoder, Encoding, decodeWord64, toCBOR)
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Compactible (Compactible (..))
import Data.Foldable (foldl')
import Data.Group (Abelian)

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
sumVal = foldl' (<+>) mempty

invert :: Val t => t -> t
invert x = (-1 :: Integer) <×> x

-- returns a Value containing only the coin (ada) tokens from the input Value
adaOnly :: Val v => v -> Bool
adaOnly v = (inject . coin) v == v

instance Val Coin where
  n <×> (Coin x) = Coin $ fromIntegral n * x
  coin = id
  inject = id
  size _ = 1
  modifyCoin f v = f v
  pointwise p (Coin x) (Coin y) = p x y

deriving via Coin instance Val DeltaCoin

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
