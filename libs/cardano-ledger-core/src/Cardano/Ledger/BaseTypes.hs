{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Ledger.BaseTypes
  ( FixedPoint,
    (==>),
    (⭒),
    Network (..),
    networkToWord8,
    word8ToNetwork,
    Nonce (..),
    Seed (..),
    UnitInterval,
    PositiveUnitInterval,
    PositiveInterval,
    NonNegativeInterval,
    BoundedRational (..),
    boundedRationalFromCBOR,
    boundedRationalToCBOR,
    fpPrecision,
    promoteRatio,
    invalidKey,
    mkNonceFromOutputVRF,
    mkNonceFromNumber,
    Url,
    urlToText,
    textToUrl,
    DnsName,
    dnsToText,
    textToDns,
    Port (..),
    ActiveSlotCoeff,
    mkActiveSlotCoeff,
    activeSlotVal,
    activeSlotLog,
    module Data.Maybe.Strict,
    BlocksMade (..),

    -- * STS Base
    Globals (..),
    epochInfo,
    ShelleyBase,
  )
where

import Cardano.Binary
  ( Decoder,
    DecoderError (..),
    Encoding,
    FromCBOR (fromCBOR),
    ToCBOR (toCBOR),
    encodeListLen,
  )
import Cardano.Crypto.Hash
import Cardano.Crypto.Util (SignableRepresentation (..))
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.NonIntegral (ln')
import Cardano.Ledger.Serialization (decodeRecordSum, ratioFromCBOR, ratioToCBOR)
import Cardano.Prelude (NFData, cborError)
import Cardano.Slotting.EpochInfo
import Cardano.Slotting.Time (SystemStart)
import Control.Exception (throw)
import Control.Monad (when, (<=<))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Coders (invalidKey)
import Data.Default.Class (Default (def))
import qualified Data.Fixed as FP (Fixed, HasResolution, resolution)
import Data.Functor.Identity
import Data.Map.Strict (Map)
import Data.Maybe.Strict
import Data.Ratio (Ratio, denominator, numerator, (%))
import Data.Scientific (Scientific, base10Exponent, coefficient, normalize, scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable
import Data.Word (Word16, Word64, Word8)
import GHC.Exception.Type (Exception)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)
import Quiet

data E34

instance FP.HasResolution E34 where
  resolution _ = (10 :: Integer) ^ (34 :: Integer)

type Digits34 = FP.Fixed E34

type FixedPoint = Digits34

fpPrecision :: FixedPoint
fpPrecision = (10 :: FixedPoint) ^ (34 :: Integer)

-- | This is an internal type for representing rational numbers that are bounded on some
-- interval that is controlled by phantom type variable @b@ as well as by
-- the bounds of underlying type @a@.
newtype BoundedRatio b a = BoundedRatio (Ratio a)
  deriving (Eq, Generic)
  deriving newtype (Show, NoThunks, NFData)

-- Deriving Ord instance can lead to integer overflow. We must go through Rational.
instance Integral a => Ord (BoundedRatio b a) where
  compare (BoundedRatio a) (BoundedRatio b) = compare (promoteRatio a) (promoteRatio b)

promoteRatio :: Integral a => Ratio a -> Rational
promoteRatio r = toInteger (numerator r) % toInteger (denominator r)

-- | Type clases that allows conversion between `Rational` and some form of bounded
-- rational type. Bounds can be restricted by both the `Bounded` type class and underlyng
-- representation.
--
-- > maybe True (\br -> minBound <= br && br <= maxBound) . boundRational
--
-- Roundtrip properties must hold:
--
-- > \r -> maybe True ((r ==) . unboundRational) (boundRational r)
-- > \br -> Just br == boundRational (unboundRational br)
class Bounded r => BoundedRational r where
  -- | Returns `Nothing` when supplied value is not within bounds or when precision is
  -- too high to be represented by the underlying type
  --
  -- ===__Example__
  --
  -- >>> :set -XTypeApplications
  -- >>> import Data.Ratio
  -- >>> boundRational @UnitInterval $ 2 % 3
  -- Just (2 % 3)
  -- >>> boundRational @UnitInterval (-0.5)
  -- Nothing
  -- >>> boundRational @UnitInterval (1.5)
  -- Nothing
  -- >>> boundRational @UnitInterval 0
  -- Just (0 % 1)
  -- >>> boundRational @PositiveUnitInterval 0
  -- Nothing
  boundRational :: Rational -> Maybe r

  -- | Promote bounded rational type into the unbounded `Rational`.
  unboundRational :: r -> Rational

instance
  (Bounded (BoundedRatio b a), Bounded a, Integral a) =>
  BoundedRational (BoundedRatio b a)
  where
  boundRational = fromRationalBoundedRatio
  unboundRational = toRationalBoundedRatio

toRationalBoundedRatio :: Integral a => BoundedRatio b a -> Rational
toRationalBoundedRatio (BoundedRatio r) = promoteRatio r

fromRationalBoundedRatio ::
  forall b a.
  (Bounded (BoundedRatio b a), Bounded a, Integral a) =>
  Rational ->
  Maybe (BoundedRatio b a)
fromRationalBoundedRatio r
  | n < minVal || d < minVal || n > maxVal || d > maxVal = Nothing -- protect against overflow
  | otherwise = fromRatioBoundedRatio $ fromInteger n % fromInteger d
  where
    minVal = toInteger (minBound :: a)
    maxVal = toInteger (maxBound :: a)
    n = numerator r
    d = denominator r

-- | Convert to `BoundedRatio`, while checking the bounds. This function doesn't guard
-- against overflow, therefore use `fromRationalBoundedRatio . promoteRatio` instead
-- when in doubt.
fromRatioBoundedRatio ::
  forall b a.
  (Bounded (BoundedRatio b a), Integral a) =>
  Ratio a ->
  Maybe (BoundedRatio b a)
fromRatioBoundedRatio ratio
  | r < toRationalBoundedRatio lowerBound
      || r > toRationalBoundedRatio upperBound =
    Nothing -- ensure valid range
  | otherwise = Just $ BoundedRatio ratio
  where
    r = promoteRatio ratio
    lowerBound = minBound :: BoundedRatio b a
    upperBound = maxBound :: BoundedRatio b a

instance (ToCBOR a, Integral a, Bounded a, Typeable b, Typeable a) => ToCBOR (BoundedRatio b a) where
  toCBOR (BoundedRatio u) = ratioToCBOR u

instance
  (FromCBOR a, Bounded (BoundedRatio b a), Bounded a, Integral a, Typeable b, Typeable a, Show a) =>
  FromCBOR (BoundedRatio b a)
  where
  fromCBOR = do
    r <- ratioFromCBOR
    case fromRatioBoundedRatio r of
      Nothing ->
        cborError $ DecoderErrorCustom "BoundedRatio" (Text.pack $ show r)
      Just u -> pure u

-- TODO: Remove `boundedRationalToCBOR`/`boundedRationalFromCBOR` in favor of
-- serialization through `ToCBOR`/`FromCBOR` that relies on the @Tag 30@. This
-- is a backwards incompatible change and must be done when breaking
-- serialization changes can be introduced.

-- | Serialize `BoundedRational` type in the same way `Rational` is serialized.
boundedRationalToCBOR :: BoundedRational r => r -> Encoding
boundedRationalToCBOR = toCBOR . unboundRational

-- | Deserialize `BoundedRational` type using `Rational` deserialization and
-- fail when bounds are violated.
boundedRationalFromCBOR :: BoundedRational r => Decoder s r
boundedRationalFromCBOR = do
  r <- fromCBOR
  case boundRational r of
    Nothing ->
      cborError $ DecoderErrorCustom "BoundedRational" (Text.pack $ show r)
    Just u -> pure u

instance ToJSON (BoundedRatio b Word64) where
  toJSON = toJSON . toScientificBoundedRatioWord64WithRounding

toScientificBoundedRatioWord64WithRounding :: BoundedRatio b Word64 -> Scientific
toScientificBoundedRatioWord64WithRounding (toRationalBoundedRatio -> ur) =
  scientific q 0 + scientific ((r * scale) `quot` d) (negate exp10)
  where
    n = numerator ur
    d = denominator ur
    (q, r) = n `quotRem` d
    -- We need to reduce precision for numbers bigger than 1 in order to make them
    -- parsable without overflowing
    exp10 = 19 - min 19 (numDigits q)
    scale = 10 ^ exp10
    numDigits :: Integer -> Int
    numDigits = go 0
      where
        go ds 0 = ds
        go ds i = ds `seq` go (ds + 1) (i `quot` 10)

instance Bounded (BoundedRatio b Word64) => FromJSON (BoundedRatio b Word64) where
  parseJSON = either fail pure . fromScientificBoundedRatioWord64 <=< parseJSON

fromScientificBoundedRatioWord64 ::
  Bounded (BoundedRatio b Word64) =>
  Scientific ->
  Either String (BoundedRatio b Word64)
fromScientificBoundedRatioWord64 (normalize -> sci)
  | coeff < 0 = failWith "negative"
  | exp10 <= 0 = do
    when (exp10 < -19) $ failWith "too precise"
    fromRationalEither (coeff % (10 ^ negate exp10))
  | otherwise = do
    when (19 < exp10) $ failWith "too big"
    fromRationalEither (coeff * 10 ^ exp10 % 1)
  where
    coeff = coefficient sci
    exp10 = base10Exponent sci
    failWith :: String -> Either String a
    failWith msg = Left $ "Value is " ++ msg ++ ": " ++ show sci
    fromRationalEither =
      maybe (failWith "outside of bounds") Right . fromRationalBoundedRatio

-- | Type to represent a value in the interval [0; +∞)
newtype NonNegativeInterval
  = NonNegativeInterval (BoundedRatio NonNegativeInterval Word64)
  deriving (Ord, Eq, Generic)
  deriving newtype
    ( Show,
      Bounded,
      BoundedRational,
      ToCBOR,
      FromCBOR,
      ToJSON,
      FromJSON,
      NoThunks,
      NFData
    )

instance Bounded (BoundedRatio NonNegativeInterval Word64) where
  minBound = BoundedRatio (0 % 1)
  maxBound = BoundedRatio (maxBound % 1)

-- | Type to represent a value in the interval (0; +∞)
newtype PositiveInterval
  = PositiveInterval (BoundedRatio PositiveInterval Word64)
  deriving (Ord, Eq, Generic)
  deriving newtype
    ( Show,
      Bounded,
      BoundedRational,
      ToCBOR,
      FromCBOR,
      ToJSON,
      FromJSON,
      NoThunks,
      NFData
    )

instance Bounded (BoundedRatio PositiveInterval Word64) where
  minBound = positiveIntervalEpsilon
  maxBound = BoundedRatio (maxBound % 1)

-- | The smallest decimal value that can roundtrip JSON
positiveIntervalEpsilon :: BoundedRatio b Word64
positiveIntervalEpsilon = BoundedRatio (1 % 10 ^ (19 :: Int))

-- | Type to represent a value in the unit interval (0; 1]
newtype PositiveUnitInterval
  = PositiveUnitInterval (BoundedRatio PositiveUnitInterval Word64)
  deriving (Ord, Eq, Generic)
  deriving newtype
    ( Show,
      Bounded,
      BoundedRational,
      ToCBOR,
      FromCBOR,
      ToJSON,
      FromJSON,
      NoThunks,
      NFData
    )

instance Bounded (BoundedRatio PositiveUnitInterval Word64) where
  minBound = positiveIntervalEpsilon
  maxBound = BoundedRatio (1 % 1)

-- | Type to represent a value in the unit interval [0; 1]
newtype UnitInterval
  = UnitInterval (BoundedRatio UnitInterval Word64)
  deriving (Ord, Eq, Generic)
  deriving newtype
    ( Show,
      Bounded,
      BoundedRational,
      ToCBOR,
      FromCBOR,
      ToJSON,
      FromJSON,
      NoThunks,
      NFData
    )

instance Integral a => Bounded (BoundedRatio UnitInterval a) where
  minBound = BoundedRatio (0 % 1)
  maxBound = BoundedRatio (1 % 1)

instance Default UnitInterval where
  def = minBound

-- | Evolving nonce type.
data Nonce
  = Nonce !(Hash Blake2b_256 Nonce)
  | -- | Identity element
    NeutralNonce
  deriving (Eq, Generic, Ord, Show, NFData)

instance NoThunks Nonce

instance ToCBOR Nonce where
  toCBOR NeutralNonce = encodeListLen 1 <> toCBOR (0 :: Word8)
  toCBOR (Nonce n) = encodeListLen 2 <> toCBOR (1 :: Word8) <> toCBOR n

instance FromCBOR Nonce where
  fromCBOR = decodeRecordSum "Nonce" $
    \case
      0 -> pure (1, NeutralNonce)
      1 -> do
        x <- fromCBOR
        pure (2, Nonce x)
      k -> invalidKey k

deriving anyclass instance ToJSON Nonce

deriving anyclass instance FromJSON Nonce

-- | Evolve the nonce
(⭒) :: Nonce -> Nonce -> Nonce
Nonce a ⭒ Nonce b =
  Nonce . castHash $
    hashWith id (hashToBytes a <> hashToBytes b)
x ⭒ NeutralNonce = x
NeutralNonce ⭒ x = x

-- | Make a nonce from the VRF output bytes
mkNonceFromOutputVRF :: VRF.OutputVRF v -> Nonce
mkNonceFromOutputVRF =
  Nonce
    . (castHash :: Hash Blake2b_256 (VRF.OutputVRF v) -> Hash Blake2b_256 Nonce)
    . hashWith VRF.getOutputVRFBytes

-- | Make a nonce from a number.
mkNonceFromNumber :: Word64 -> Nonce
mkNonceFromNumber =
  Nonce
    . (castHash :: Hash Blake2b_256 Word64 -> Hash Blake2b_256 Nonce)
    . hashWith (BSL.toStrict . B.runPut . B.putWord64be)

-- | Seed to the verifiable random function.
--
--   We do not expose the constructor to `Seed`. Instead, a `Seed` should be
--   created using `mkSeed` for a VRF calculation.
newtype Seed = Seed (Hash Blake2b_256 Seed)
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (NoThunks, ToCBOR)

instance SignableRepresentation Seed where
  getSignableRepresentation (Seed x) = hashToBytes x

(==>) :: Bool -> Bool -> Bool
a ==> b = not a || b

infix 1 ==>

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
  deriving (Eq, Ord, Generic, Show)
  deriving newtype (ToCBOR, NFData, NoThunks, FromJSON, ToJSON)

textToUrl :: Text -> Maybe Url
textToUrl t = Url <$> text64 t

instance FromCBOR Url where
  fromCBOR = Url <$> text64FromCBOR

newtype DnsName = DnsName {dnsToText :: Text}
  deriving (Eq, Ord, Generic, Show)
  deriving newtype (ToCBOR, NoThunks, NFData, FromJSON, ToJSON)

textToDns :: Text -> Maybe DnsName
textToDns t = DnsName <$> text64 t

instance FromCBOR DnsName where
  fromCBOR = DnsName <$> text64FromCBOR

newtype Port = Port {portToWord16 :: Word16}
  deriving (Eq, Ord, Generic, Show)
  deriving newtype (Num, FromCBOR, ToCBOR, NFData, NoThunks, ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Active Slot Coefficent, named f in
-- "Ouroboros Praos: An adaptively-secure, semi-synchronous proof-of-stake protocol"
--------------------------------------------------------------------------------

data ActiveSlotCoeff = ActiveSlotCoeff
  { unActiveSlotVal :: !PositiveUnitInterval,
    unActiveSlotLog :: !Integer -- TODO mgudemann make this FixedPoint,
    -- currently a problem because of
    -- NoThunks instance for FixedPoint
  }
  deriving (Eq, Ord, Show, Generic)

instance NoThunks ActiveSlotCoeff

instance NFData ActiveSlotCoeff

instance FromCBOR ActiveSlotCoeff where
  fromCBOR = mkActiveSlotCoeff <$> fromCBOR

instance ToCBOR ActiveSlotCoeff where
  toCBOR
    ActiveSlotCoeff
      { unActiveSlotVal = slotVal,
        unActiveSlotLog = _logVal
      } =
      toCBOR slotVal

mkActiveSlotCoeff :: PositiveUnitInterval -> ActiveSlotCoeff
mkActiveSlotCoeff v =
  ActiveSlotCoeff
    { unActiveSlotVal = v,
      unActiveSlotLog =
        if v == maxBound
          then -- If the active slot coefficient is equal to one,
          -- then nearly every stake pool can produce a block every slot.
          -- In this degenerate case, where ln (1-f) is not defined,
          -- we set the unActiveSlotLog to zero.
            0
          else
            floor
              (fpPrecision * ln' ((1 :: FixedPoint) - fromRational (unboundRational v)))
    }

activeSlotVal :: ActiveSlotCoeff -> PositiveUnitInterval
activeSlotVal = unActiveSlotVal

activeSlotLog :: ActiveSlotCoeff -> FixedPoint
activeSlotLog f = fromIntegral (unActiveSlotLog f) / fpPrecision

--------------------------------------------------------------------------------
-- Base monad for all STS systems
--------------------------------------------------------------------------------

data Globals = Globals
  { epochInfoWithErr :: !(EpochInfo (Either Text)),
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
    activeSlotCoeff :: !ActiveSlotCoeff,
    -- | The network ID
    networkId :: !Network,
    -- | System start time
    systemStart :: !SystemStart
  }
  deriving (Show, Generic)

instance NoThunks Globals

type ShelleyBase = ReaderT Globals Identity

epochInfo :: Globals -> EpochInfo Identity
epochInfo = hoistEpochInfo (either (throw . EpochErr) pure) . epochInfoWithErr

newtype EpochErr = EpochErr Text

deriving instance Show EpochErr

instance Exception EpochErr

data Network
  = Testnet
  | Mainnet
  deriving (Eq, Ord, Enum, Bounded, Show, Generic, NFData, ToJSON, FromJSON, NoThunks)

networkToWord8 :: Network -> Word8
networkToWord8 = toEnum . fromEnum

word8ToNetwork :: Word8 -> Maybe Network
word8ToNetwork e
  | fromEnum e > fromEnum (maxBound :: Network) = Nothing
  | fromEnum e < fromEnum (minBound :: Network) = Nothing
  | otherwise = Just $ toEnum (fromEnum e)

instance ToCBOR Network where
  toCBOR = toCBOR . networkToWord8

instance FromCBOR Network where
  fromCBOR =
    word8ToNetwork <$> fromCBOR >>= \case
      Nothing -> cborError $ DecoderErrorCustom "Network" "Unknown network id"
      Just n -> pure n

-- | Blocks made
newtype BlocksMade crypto = BlocksMade
  { unBlocksMade :: Map (KeyHash 'StakePool crypto) Natural
  }
  deriving (Eq, Generic)
  deriving (Show) via Quiet (BlocksMade crypto)
  deriving newtype (NoThunks, NFData, ToJSON, FromJSON, ToCBOR, FromCBOR)
