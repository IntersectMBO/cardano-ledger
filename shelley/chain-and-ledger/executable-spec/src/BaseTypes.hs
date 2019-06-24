{-# LANGUAGE EmptyDataDecls #-}

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
  , seedOp
  , neutralSeed
  , EEnt(..)
  ) where

import qualified Data.Map.Strict as Map

import qualified Data.Fixed as FP
import qualified Keys       as Keys

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
    deriving(Show, Ord, Eq)

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

seedOp :: Seed -> Seed -> Seed
seedOp NeutralSeed s = s
seedOp s NeutralSeed = s
seedOp a b = SeedOp a b

neutralSeed :: Seed
neutralSeed = NeutralSeed

mkNonce :: Integer -> Seed
mkNonce = Nonce

-- | Extra entropy
newtype EEnt = EEnt (Map.Map Keys.VKeyGenesis Seed)
  deriving (Show, Ord, Eq)
