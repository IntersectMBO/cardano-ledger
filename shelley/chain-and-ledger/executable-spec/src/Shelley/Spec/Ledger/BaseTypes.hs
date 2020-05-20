{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Shelley.Spec.Ledger.BaseTypes
  ( FixedPoint,
    (==>),
    (⭒),
    Nonce (..),
    Seed (..),
    UnitInterval (..),
    fpEpsilon,
    fpPrecision,
    interval0,
    interval1,
    intervalValue,
    invalidKey,
    mkNonce,
    mkUnitInterval,
    truncateUnitInterval,
    StrictMaybe (..),
    strictMaybeToMaybe,
    maybeToStrictMaybe,
    fromSMaybe,
    Url,
    urlToText,
    textToUrl,
    DnsName,
    dnsToText,
    textToDns,
    Port,
    portToWord16,
    ActiveSlotCoeff,
    mkActiveSlotCoeff,
    activeSlotVal,
    activeSlotLog,

    -- * STS Base
    Globals (..),
    ShelleyBase,
  )
where

import Cardano.Binary
  ( Decoder,
    DecoderError (..),
    FromCBOR (fromCBOR),
    ToCBOR (toCBOR),
    decodeListLen,
    decodeWord,
    encodeListLen,
    matchSize,
  )
import Cardano.Crypto.Hash
import Cardano.Prelude (NoUnexpectedThunks (..), cborError)
import Cardano.Slotting.EpochInfo
import Control.Monad.Trans.Reader (ReaderT)
import qualified Data.ByteString as BS
import Data.Coerce (coerce)
import qualified Data.Fixed as FP (Fixed, HasResolution, resolution)
import Data.Functor.Identity
import Data.Ratio ((%), denominator, numerator)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word16, Word64, Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.Serialization (rationalFromCBOR, rationalToCBOR)
import Shelley.Spec.NonIntegral (ln')

data E34

instance FP.HasResolution E34 where
  resolution _ = (10 :: Integer) ^ (34 :: Integer)

type Digits34 = FP.Fixed E34

type FixedPoint = Digits34

fpPrecision :: FixedPoint
fpPrecision = (10 :: FixedPoint) ^ (34 :: Integer)

fpEpsilon :: FixedPoint
fpEpsilon = (10 :: FixedPoint) ^ (17 :: Integer) / fpPrecision

-- | Type to represent a value in the unit interval [0; 1]
newtype UnitInterval = UnsafeUnitInterval Rational -- TODO: Fixed precision
  deriving (Show, Ord, Eq, NoUnexpectedThunks)

instance ToCBOR UnitInterval where
  toCBOR (UnsafeUnitInterval u) = rationalToCBOR u

instance FromCBOR UnitInterval where
  fromCBOR = do
    r <- rationalFromCBOR
    case mkUnitInterval r of
      Nothing -> cborError $ DecoderErrorCustom "UnitInterval" (Text.pack $ show r)
      Just u -> pure u

-- | Return a `UnitInterval` type if `r` is in [0; 1].
mkUnitInterval :: Rational -> Maybe UnitInterval
mkUnitInterval r = if r <= 1 && r >= 0 then Just $ UnsafeUnitInterval r else Nothing

-- | Convert a rational to a `UnitInterval` by ignoring its integer part.
truncateUnitInterval :: Rational -> UnitInterval
truncateUnitInterval (abs -> r) = case (numerator r, denominator r) of
  (n, d) | n > d -> UnsafeUnitInterval $ (n `mod` d) % d
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
  = Nonce !(Hash SHA256 Nonce)
  | -- | Identity element
    NeutralNonce
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

-- | Strict 'Maybe'.
--
-- TODO move to @cardano-prelude@
data StrictMaybe a
  = SNothing
  | SJust !a
  deriving (Eq, Ord, Show, Generic)

instance NoUnexpectedThunks a => NoUnexpectedThunks (StrictMaybe a)

instance Functor StrictMaybe where
  fmap _ SNothing = SNothing
  fmap f (SJust a) = SJust (f a)

instance Applicative StrictMaybe where
  pure = SJust

  SJust f <*> m = fmap f m
  SNothing <*> _m = SNothing

  SJust _m1 *> m2 = m2
  SNothing *> _m2 = SNothing

instance Monad StrictMaybe where
  SJust x >>= k = k x
  SNothing >>= _ = SNothing

  (>>) = (*>)

  return = SJust
  fail _ = SNothing

instance ToCBOR a => ToCBOR (StrictMaybe a) where
  toCBOR SNothing = encodeListLen 0
  toCBOR (SJust x) = encodeListLen 1 <> toCBOR x

instance FromCBOR a => FromCBOR (StrictMaybe a) where
  fromCBOR = do
    n <- decodeListLen
    case n of
      0 -> pure SNothing
      1 -> SJust <$> fromCBOR
      _ -> fail "unknown tag"

strictMaybeToMaybe :: StrictMaybe a -> Maybe a
strictMaybeToMaybe SNothing = Nothing
strictMaybeToMaybe (SJust x) = Just x

maybeToStrictMaybe :: Maybe a -> StrictMaybe a
maybeToStrictMaybe Nothing = SNothing
maybeToStrictMaybe (Just x) = SJust x

fromSMaybe :: a -> StrictMaybe a -> a
fromSMaybe d SNothing = d
fromSMaybe _ (SJust x) = x

--
-- Helper functions for text with a 64 byte bound
--

text64 :: Text -> Maybe Text
text64 t =
  if (BS.length . encodeUtf8) t <= 64
    then Just t
    else Nothing

text64FromCBOR :: Decoder s Text
text64FromCBOR = do
  t <- fromCBOR
  if (BS.length . encodeUtf8) t > 64
    then cborError $ DecoderErrorCustom "text exceeds 64 bytes:" t
    else pure t

--
-- Types used in the Stake Pool Relays
--

newtype Url = Url {urlToText :: Text}
  deriving (Eq, Ord, Generic, Show, ToCBOR, NoUnexpectedThunks)

textToUrl :: Text -> Maybe Url
textToUrl t = Url <$> text64 t

instance FromCBOR Url where
  fromCBOR = Url <$> text64FromCBOR

newtype DnsName = DnsName {dnsToText :: Text}
  deriving (Eq, Ord, Generic, Show, ToCBOR, NoUnexpectedThunks)

textToDns :: Text -> Maybe DnsName
textToDns t = DnsName <$> text64 t

instance FromCBOR DnsName where
  fromCBOR = DnsName <$> text64FromCBOR

newtype Port = Port {portToWord16 :: Word16}
  deriving (Eq, Ord, Num, Generic, Show, ToCBOR, FromCBOR, NoUnexpectedThunks)

--------------------------------------------------------------------------------
-- Active Slot Coefficent, named f in
-- "Ouroboros Praos: An adaptively-secure, semi-synchronous proof-of-stake protocol"
--------------------------------------------------------------------------------

data ActiveSlotCoeff = ActiveSlotCoeff
  { unActiveSlotVal :: !UnitInterval,
    unActiveSlotLog :: !Integer -- TODO mgudemann make this FixedPoint,
    -- currently a problem because of
    -- NoUnexpectedThunks instance for FixedPoint
  }
  deriving (Eq, Ord, Show, Generic)

instance NoUnexpectedThunks ActiveSlotCoeff

instance FromCBOR ActiveSlotCoeff where
  fromCBOR = do
    v <- fromCBOR
    pure $ mkActiveSlotCoeff v

instance ToCBOR ActiveSlotCoeff where
  toCBOR
    ( ActiveSlotCoeff
        { unActiveSlotVal = slotVal,
          unActiveSlotLog = _logVal
        }
      ) =
      toCBOR slotVal

mkActiveSlotCoeff :: UnitInterval -> ActiveSlotCoeff
mkActiveSlotCoeff v =
  ActiveSlotCoeff
    { unActiveSlotVal = v,
      unActiveSlotLog =
        if (intervalValue v) == 1
          then -- If the active slot coefficient is equal to one,
          -- then nearly every stake pool can produce a block every slot.
          -- In this degenerate case, where ln (1-f) is not defined,
          -- we set the unActiveSlotLog to zero.
            0
          else
            floor
              ( fpPrecision
                  * ( ln' $ (1 :: FixedPoint) - (fromRational $ intervalValue v)
                    )
              )
    }

activeSlotVal :: ActiveSlotCoeff -> UnitInterval
activeSlotVal = unActiveSlotVal

activeSlotLog :: ActiveSlotCoeff -> FixedPoint
activeSlotLog f = (fromIntegral $ unActiveSlotLog f) / fpPrecision

--------------------------------------------------------------------------------
-- Base monad for all STS systems
--------------------------------------------------------------------------------

data Globals = Globals
  { epochInfo :: !(EpochInfo Identity),
    slotsPerKESPeriod :: !Word64,
    -- | The window size in which our chosen chain growth property
    --   guarantees at least k blocks. From the paper
    --   "Ouroboros praos: An adaptively-secure, semi-synchronous proof-of-stake protocol".
    --   The 'stabilityWindow' constant is used in a number of places; for example,
    --   protocol updates must be submitted at least twice this many slots before an epoch boundary.
    stabilityWindow :: !Word64,
    -- | Number of slots before the end of the epoch at which we stop updating
    --   the candidate nonce for the next epoch.
    randomnessStabilisationWindow :: !Word64,
    -- | Maximum number of blocks we are allowed to roll back
    securityParameter :: !Word64,
    -- | Maximum number of KES iterations
    maxKESEvo :: !Word64,
    -- | Quorum for update system votes and MIR certificates
    quorum :: !Word64,
    -- | All blocks invalid after this protocol version
    maxMajorPV :: !Natural,
    -- | Maximum number of lovelace in the system
    maxLovelaceSupply :: !Word64,
    -- | Active Slot Coefficient, named f in
    -- "Ouroboros Praos: An adaptively-secure, semi-synchronous proof-of-stake protocol"
    activeSlotCoeff :: !ActiveSlotCoeff
  }
  deriving (Generic)

instance NoUnexpectedThunks Globals

type ShelleyBase = ReaderT Globals Identity
