{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
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
    unitScale,
    fromScientificUnitInterval,
    fpPrecision,
    unitIntervalToRational,
    unitIntervalFromRational,
    invalidKey,
    mkNonceFromOutputVRF,
    mkNonceFromNumber,
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
    Port (..),
    ActiveSlotCoeff,
    mkActiveSlotCoeff,
    activeSlotVal,
    activeSlotLog,

    -- * STS Base
    Globals (..),
    epochInfo,
    ShelleyBase,
  )
where

import Cardano.Binary
  ( Decoder,
    DecoderError (..),
    FromCBOR (fromCBOR),
    ToCBOR (toCBOR),
    encodeListLen,
  )
import Cardano.Crypto.Hash
import Cardano.Crypto.Util (SignableRepresentation (..))
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Serialization (decodeRecordSum, ratioFromCBOR, ratioToCBOR)
import Cardano.Prelude (NFData, cborError)
import Cardano.Slotting.EpochInfo
import Cardano.Slotting.Time (SystemStart)
import Control.Exception (throw)
import Control.Monad (when)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Coders (invalidKey)
import Data.Coerce
import Data.Default.Class (Default (def))
import qualified Data.Fixed as FP (Fixed, HasResolution, resolution)
import Data.Functor.Identity
import Data.Maybe.Strict
import Data.Proxy
import Data.Ratio (denominator, numerator, (%))
import Data.Scientific (Scientific, base10Exponent, coefficient, normalize)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word16, Word64, Word8)
import GHC.Exception.Type (Exception)
import GHC.Generics (Generic)
import GHC.TypeLits
import NoThunks.Class (NoThunks (..))
import Numeric.Decimal
import Numeric.Natural (Natural)
import Shelley.Spec.NonIntegral (ln')

data E34

instance FP.HasResolution E34 where
  resolution _ = (10 :: Integer) ^ (34 :: Integer)

type Digits34 = FP.Fixed E34

type FixedPoint = Digits34

fpPrecision :: FixedPoint
fpPrecision = (10 :: FixedPoint) ^ (34 :: Integer)

-- | Maximum precision possible for unit interval when backed by a 64bit
type UnitScale = 19

unitScale :: Int
unitScale = fromInteger (natVal (Proxy :: Proxy UnitScale))

newtype RawUnit
  = RawUnit Word64
  deriving newtype (Show, Eq, Ord, Num, Real, Enum, Integral, NFData)

instance Bounded RawUnit where
  minBound = 0
  maxBound = RawUnit (10 ^ unitScale)

-- | Type to represent a value in the unit interval [0; 1]
newtype UnitInterval = UnitInterval
  { unitDecimal :: Decimal RoundHalfEven UnitScale RawUnit
  }
  deriving (Generic)
  deriving newtype (Show, Ord, Eq, Bounded, NFData)

instance Default UnitInterval where
  def = minBound

instance NoThunks UnitInterval where
  noThunks ctx = noThunks ctx . coerce @_ @Word64
  wNoThunks ctx = wNoThunks ctx . coerce @_ @Word64

-- We are using CBOR serialization through `Ratio Word64` for hisotrical reasons. It would
-- be better to use Word64 directly: `toCBOR . coerce @_ @Word64`
instance ToCBOR UnitInterval where
  toCBOR = ratioToCBOR . (% coerce (maxBound :: RawUnit)) . coerce @_ @Word64

instance FromCBOR UnitInterval where
  fromCBOR = do
    r <- ratioFromCBOR
    let n = toInteger (numerator r :: Word64)
        d = toInteger (denominator r :: Word64)
    case unitIntervalFromRational (n % d) of
      Nothing -> cborError $ DecoderErrorCustom "UnitInterval" (Text.pack $ show r)
      Just u -> pure u

instance ToJSON UnitInterval where
  toJSON = toJSON . toScientificDecimal . unitDecimal

instance FromJSON UnitInterval where
  parseJSON v = do
    d <- parseJSON v
    either fail pure $ fromScientificUnitInterval d

-- | safe-decimal-0.2.1.0 has a fixed version of `fromScientificDecimalBounded` function
-- that makes both of the `when` checks and `normalize` call redundant.
fromScientificUnitInterval :: Scientific -> Either String UnitInterval
fromScientificUnitInterval (normalize -> num) = do
  when (coeff < 0) $ Left "Negative values aren't allowed - protect against underflow"
  when (coeff > toInteger (maxBound :: Word64) || exp10 < 0 || exp10 > unitScale) $
    Left "Precision is too large - protection against overflow"
  either (Left . show) (Right . UnitInterval) . fromScientificDecimalBounded $ num
  where
    coeff = coefficient num
    exp10 = negate (base10Exponent num)

unitIntervalToRational :: UnitInterval -> Rational
unitIntervalToRational = toRationalDecimal . unitDecimal

-- | Returns `Nothing` when supplied value is not in the [0, 1] range. When rational
-- cannot be represented as decimal exactly it will be rounded.
--
-- ===__Example__
--
-- >>> import Data.Ratio
-- >>> unitIntervalFromRational $ 2 % 3
-- Just 0.6666666666666666667
unitIntervalFromRational :: Rational -> Maybe UnitInterval
unitIntervalFromRational r = UnitInterval <$> fromRationalDecimalBoundedWithRounding r

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
  { unActiveSlotVal :: !UnitInterval,
    unActiveSlotLog :: !Integer -- TODO mgudemann make this FixedPoint,
    -- currently a problem because of
    -- NoThunks instance for FixedPoint
  }
  deriving (Eq, Ord, Show, Generic)

instance NoThunks ActiveSlotCoeff

instance NFData ActiveSlotCoeff

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
        if v == maxBound
          then -- If the active slot coefficient is equal to one,
          -- then nearly every stake pool can produce a block every slot.
          -- In this degenerate case, where ln (1-f) is not defined,
          -- we set the unActiveSlotLog to zero.
            0
          else
            floor
              ( fpPrecision
                  * ( ln' $ (1 :: FixedPoint) - (fromRational $ unitIntervalToRational v)
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
epochInfo = (hoistEpochInfo (either (throw . EpochErr) pure)) . epochInfoWithErr

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
