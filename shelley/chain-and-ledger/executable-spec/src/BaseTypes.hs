{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module BaseTypes
  ( FixedPoint
  , (==>)
  , (⭒)
  , Nonce(..)
  , Seed(..)
  , UnitInterval(..)
  , fpEpsilon
  , fpPrecision
  , interval0
  , interval1
  , intervalValue
  , invalidKey
  , mkNonce
  , mkUnitInterval
  , truncateUnitInterval
  , CborSeq(..)
    -- * STS Base
  , Globals (..)
  , ShelleyBase
  ) where


import           Cardano.Binary (Decoder, DecoderError (..), FromCBOR (fromCBOR), ToCBOR (toCBOR),
                     decodeBreakOr, decodeListLen, decodeListLenOrIndef, decodeWord, encodeBreak,
                     encodeListLen, encodeListLenIndef, matchSize)
import           Cardano.Crypto.Hash
import           Cardano.Prelude (NoUnexpectedThunks (..), cborError)
import           Cardano.Slotting.EpochInfo
import           Control.Monad (replicateM)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Coerce (coerce)
import qualified Data.Fixed as FP (Fixed, HasResolution, resolution)
import           Data.Functor.Identity
import           Data.Ratio (denominator, numerator, (%))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import           Data.Word (Word64, Word8)
import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)

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
newtype UnitInterval = UnsafeUnitInterval Rational
    deriving (Show, Ord, Eq, NoUnexpectedThunks)

instance ToCBOR UnitInterval where
  toCBOR (UnsafeUnitInterval r) =
    (convert . numerator) r <>
    (convert. denominator) r
   where
    convert = toCBOR . fromInteger @Word64

-- | Return a `UnitInterval` type if `r` is in [0; 1].
mkUnitInterval :: Rational -> Maybe UnitInterval
mkUnitInterval r = if r <= 1 && r >= 0 then Just $ UnsafeUnitInterval r else Nothing

instance FromCBOR UnitInterval where
  fromCBOR = do
    n <- decodeWord
    d <- decodeWord
    case mkUnitInterval (toInteger n % toInteger d) of
      Nothing -> cborError ("not a unit interval value" :: String)
      Just u -> pure u

-- | Convert a rational to a `UnitInterval` by ignoring its integer part.
truncateUnitInterval :: Rational -> UnitInterval
truncateUnitInterval r = case (numerator r, denominator r) of
  (n, d) | n > d -> UnsafeUnitInterval $ (n - d) % d
  _ -> UnsafeUnitInterval r

-- | Get rational value of `UnitInterval` type
intervalValue :: UnitInterval -> Rational
intervalValue (UnsafeUnitInterval v) = v

interval0 :: UnitInterval
interval0 = UnsafeUnitInterval 0

interval1 :: UnitInterval
interval1 = UnsafeUnitInterval 1

-- | Evolving nonce type.
data Nonce
  = Nonce (Hash SHA256 Nonce)
  | NeutralNonce -- ^ Identity element
  deriving (Eq, Generic, Ord, Show)

instance NoUnexpectedThunks Nonce

instance ToCBOR Nonce where
  toCBOR NeutralNonce = encodeListLen 1 <> toCBOR (0 :: Word8)
  toCBOR (Nonce n) = encodeListLen 2 <> toCBOR (1 :: Word8) <> toCBOR n

invalidKey :: Word -> Decoder s a
invalidKey k = cborError $ DecoderErrorCustom "not a valid key:" (Text.pack $ show k)

instance FromCBOR Nonce where
  fromCBOR = do
    n <- decodeListLen
    decodeWord >>= \case
      0 -> do
        matchSize "NeutralNonce" 1 n
        pure NeutralNonce
      1 -> do
        matchSize "Nonce" 2 n
        Nonce <$> fromCBOR
      k -> invalidKey k

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

newtype CborSeq a = CborSeq { unwrapCborSeq :: Seq a }

instance ToCBOR a => ToCBOR (CborSeq a) where
  toCBOR (CborSeq xs) =
    let l = fromIntegral $ Seq.length xs
        contents = foldMap toCBOR xs
    in
    if l <= 23
    then encodeListLen l <> contents
    else encodeListLenIndef <> contents <> encodeBreak

instance FromCBOR a => FromCBOR (CborSeq a) where
  fromCBOR = CborSeq . Seq.fromList <$> do
    decodeListLenOrIndef >>= \case
      Just len -> replicateM len fromCBOR
      Nothing -> loop [] (not <$> decodeBreakOr) fromCBOR
    where
    loop acc condition action = condition >>= \case
      False -> pure acc
      True -> action >>= \v -> loop (v:acc) condition action


--------------------------------------------------------------------------------
-- Base monad for all STS systems
--------------------------------------------------------------------------------

data Globals = Globals
  { epochInfo :: EpochInfo Identity
  , slotsPerKESPeriod :: Word64
    -- | Number of slots before the end of the epoch at which we stop updating
    --   the candidate nonce for the next epoch.
    --
    --   This value is also used in a number of other places; for example,
    --   protocol updates must be submitted at least this many slots before an
    --   epoch boundary.
  , slotsPrior :: Word64
    -- | Number of slots after the beginning of an epoch when we may begin to
    --   distribute rewards.
  , startRewards :: Word64
    -- | Maximum number of blocks we are allowed to roll back
  , securityParameter :: Word64
    -- | Maximum number of KES iterations
  , maxKESEvo :: Word64
    -- | Quorum for update system votes and MIR certificates
  , quorum :: Word64
  }

type ShelleyBase = ReaderT Globals Identity
