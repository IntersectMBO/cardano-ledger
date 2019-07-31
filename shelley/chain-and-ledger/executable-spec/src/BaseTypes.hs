{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module BaseTypes
  ( FixedPoint
  , fpPrecision
  , fpEpsilon
  , UnitInterval
  , mkUnitInterval
  , intervalValue
  , interval0
  , interval1
  , Seed(..)
  , mkNonce
  , (⭒)
  ) where


import qualified Data.Fixed as FP (Fixed, HasResolution, resolution)
import           Data.Word (Word8)

import           Cardano.Binary (ToCBOR (toCBOR), encodeListLen)

data E34

instance FP.HasResolution E34 where
  resolution _ = (10::Integer)^(34::Integer)

type Digits34 = FP.Fixed E34

type FixedPoint = Digits34

fpPrecision :: FixedPoint
fpPrecision = (10::FixedPoint)^(34::Integer)

fpEpsilon :: FixedPoint
fpEpsilon = (10::FixedPoint)^(17::Integer)

-- | Type to represent a value in the unit interval [0; 1]
newtype UnitInterval = UnitInterval Rational
    deriving (Show, Ord, Eq, ToCBOR)

-- | Return a `UnitInterval` type if `r` is in [0; 1].
mkUnitInterval :: Rational -> Maybe UnitInterval
mkUnitInterval r = if r <= 1 && r >= 0 then Just $ UnitInterval r else Nothing

-- | Get rational value of `UnitInterval` type
intervalValue :: UnitInterval -> Rational
intervalValue (UnitInterval v) = v

interval0 :: UnitInterval
interval0 = UnitInterval 0

interval1 :: UnitInterval
interval1 = UnitInterval 1

-- | Tree like structure to represent nonce values and to support the binary
-- operation on values.
data Seed
  = Nonce Integer
  | NeutralSeed
  | SeedL
  | SeedEta
  | SeedOp Seed
           Seed
  deriving (Show, Eq, Ord)

instance ToCBOR Seed where
  toCBOR = \case
    Nonce nonce -> encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR nonce
    NeutralSeed -> encodeListLen 1 <> toCBOR (1 :: Word8)
    SeedL -> encodeListLen 1 <> toCBOR (2 :: Word8)
    SeedEta -> encodeListLen 1 <> toCBOR (3 :: Word8)
    SeedOp s1 s2 ->
      encodeListLen 3 <> toCBOR (4 :: Word8) <> toCBOR s1 <> toCBOR s2

(⭒) :: Seed -> Seed -> Seed
NeutralSeed ⭒ s = s
s ⭒ NeutralSeed = s
a ⭒ b = SeedOp a b

mkNonce :: Integer -> Seed
mkNonce = Nonce
