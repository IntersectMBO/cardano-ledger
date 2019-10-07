{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module BaseTypes
  ( FixedPoint
  , fpPrecision
  , fpEpsilon
  , UnitInterval
  , mkUnitInterval
  , truncateUnitInterval
  , intervalValue
  , interval0
  , interval1
  , Seed(..)
  , Nonce(..)
  , mkNonce
  , (⭒)
  , (==>)
  ) where


import           Data.Coerce (coerce)
import           Data.Word (Word8)
import qualified Data.Fixed as FP (Fixed, HasResolution, resolution)
import           Data.Ratio (numerator, denominator, (%))
import           Numeric.Natural (Natural)
import           GHC.Generics (Generic)
import           Cardano.Binary (ToCBOR(..), encodeListLen)
import           Cardano.Crypto.Hash
import           Cardano.Prelude (NoUnexpectedThunks(..))

data E34

instance FP.HasResolution E34 where
  resolution _ = (10::Integer)^(34::Integer)

type Digits34 = FP.Fixed E34

type FixedPoint = Digits34

fpPrecision :: FixedPoint
fpPrecision = (10::FixedPoint)^(34::Integer)

fpEpsilon :: FixedPoint
fpEpsilon = (10::FixedPoint)^(17::Integer) / fpPrecision

-- | Type to represent a value in the unit interval [0; 1]
newtype UnitInterval = UnitInterval Rational
    deriving (Show, Ord, Eq, NoUnexpectedThunks, ToCBOR)

-- | Return a `UnitInterval` type if `r` is in [0; 1].
mkUnitInterval :: Rational -> Maybe UnitInterval
mkUnitInterval r = if r <= 1 && r >= 0 then Just $ UnitInterval r else Nothing

-- | Convert a rational to a `UnitInterval` by ignoring its integer part.
truncateUnitInterval :: Rational -> UnitInterval
truncateUnitInterval r = case (numerator r, denominator r) of
  (n, d) | n > d -> UnitInterval $ (n - d) % d
  _ -> UnitInterval r

-- | Get rational value of `UnitInterval` type
intervalValue :: UnitInterval -> Rational
intervalValue (UnitInterval v) = v

interval0 :: UnitInterval
interval0 = UnitInterval 0

interval1 :: UnitInterval
interval1 = UnitInterval 1

-- | Evolving nonce type.
data Nonce
  = Nonce (Hash SHA256 Nonce)
  | NeutralNonce -- ^ Identity element
  deriving (Eq, Generic, Ord, Show)

instance NoUnexpectedThunks Nonce

instance ToCBOR Nonce where
  toCBOR NeutralNonce = encodeListLen 1 <> toCBOR (0 :: Word8)
  toCBOR (Nonce n) = encodeListLen 2 <> toCBOR (1 :: Word8) <> toCBOR n

-- | Evolve the nonce
(⭒) :: Nonce -> Nonce -> Nonce
(Nonce a) ⭒ (Nonce b) = Nonce . coerce $ hash @SHA256 (getHash a <> getHash b)
x ⭒ NeutralNonce = x
NeutralNonce ⭒ x = x

-- | Make a nonce from a natural number
mkNonce :: Natural -> Nonce
mkNonce = Nonce . coerce . hash @SHA256

-- | Seed to the verifiable random function.
--
--   We do not expose the constructor to `Seed`. Instead, a `Seed` should be
--   created using `mkSeed` for a VRF calculation.
newtype Seed = Seed (Hash SHA256 Seed)
  deriving (Eq, Ord, Show, Generic, NoUnexpectedThunks, ToCBOR)

(==>) :: Bool -> Bool -> Bool
a ==> b = not a || b
infix 1 ==>
