{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Ledger.BaseTypes (
  module Slotting,
  module NonZero,
  ProtVer (..),
  module Cardano.Ledger.Binary.Version,
  FixedPoint,
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
  positiveUnitIntervalRelaxToUnitInterval,
  positiveUnitIntervalRelaxToPositiveInterval,
  positiveIntervalRelaxToNonNegativeInterval,
  BoundedRational (..),
  fpPrecision,
  integralToBounded,
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
  kindObject,

  -- * Indices
  TxIx (..),
  txIxToInt,
  txIxFromIntegral,
  mkTxIxPartial,
  CertIx (..),
  certIxToInt,
  certIxFromIntegral,
  mkCertIxPartial,
  Anchor (..),
  AnchorData (..),

  -- * STS Base
  Globals (..),
  epochInfoPure,
  ShelleyBase,
  Relation (..),
  Mismatch (..),
  swapMismatch,
  unswapMismatch,

  -- * Injection
  Inject (..),
  positiveUnitIntervalNonZeroRational,

  -- * Aeson helpers
  KeyValuePairs (..),
  ToKeyValuePairs (..),
) where

import Cardano.Crypto.Hash
import Cardano.Crypto.Util (SignableRepresentation (..))
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.BaseTypes.NonZero as NonZero
import Cardano.Ledger.Binary (
  CBORGroup (..),
  DecCBOR (decCBOR),
  DecCBORGroup (..),
  Decoder,
  DecoderError (..),
  EncCBOR (encCBOR),
  EncCBORGroup (..),
  FromCBOR,
  ToCBOR,
  cborError,
  ifDecoderVersionAtLeast,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Binary.Plain (
  FromCBOR (..),
  ToCBOR (..),
  decodeRecordSum,
  decodeWord8,
  encodeListLen,
  invalidKey,
 )
import qualified Cardano.Ledger.Binary.Plain as Plain (decodeRationalWithTag, encodeRatioWithTag)
import Cardano.Ledger.Binary.Version
import Cardano.Ledger.Hashes (HashAnnotated, SafeHash, SafeToHash)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.NonIntegral (ln')
import Cardano.Slotting.Block as Slotting (BlockNo (..))
import Cardano.Slotting.EpochInfo (EpochInfo, hoistEpochInfo)
import Cardano.Slotting.Slot as Slotting (
  EpochInterval (..),
  EpochNo (..),
  EpochSize (..),
  SlotNo (..),
  WithOrigin (..),
  addEpochInterval,
  binOpEpochNo,
 )
import Cardano.Slotting.Time (SystemStart)
import Control.DeepSeq (NFData (rnf), rwhnf)
import Control.Exception (throw)
import Control.Monad (when, (<=<))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson (
  FromJSON (..),
  KeyValue,
  ToJSON (..),
  Value (..),
  object,
  pairs,
  withObject,
  (.:),
  (.=),
 )
import Data.Aeson.Types (Pair)
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Coerce (coerce)
import Data.Default (Default (def))
import qualified Data.Fixed as FP (Fixed, HasResolution, resolution)
import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Maybe.Strict
import Data.MemPack
import Data.Ratio (Ratio, denominator, numerator, (%))
import Data.Scientific (
  Scientific,
  base10Exponent,
  coefficient,
  fromRationalRepetendLimited,
  normalize,
 )
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Typeable)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Exception.Type (Exception)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)
import Quiet (Quiet (Quiet))
import System.Random.Stateful (Random, Uniform (..), UniformRange (..))
import Type.Reflection (typeRep)
#if MIN_VERSION_random(1,3,0)
import System.Random.Stateful (isInRangeOrd)
#endif

maxDecimalsWord64 :: Int
maxDecimalsWord64 = 19

data ProtVer = ProtVer {pvMajor :: !Version, pvMinor :: !Natural}
  deriving (Show, Eq, Generic, Ord, NFData)
  deriving (EncCBOR) via (CBORGroup ProtVer)
  deriving (DecCBOR) via (CBORGroup ProtVer)

instance ToCBOR ProtVer where
  toCBOR ProtVer {..} = toCBOR (pvMajor, pvMinor)

instance FromCBOR ProtVer where
  fromCBOR = uncurry ProtVer <$> fromCBOR

instance NoThunks ProtVer

instance ToJSON ProtVer where
  toJSON (ProtVer major minor) =
    object
      [ "major" .= getVersion64 major
      , "minor" .= minor
      ]

instance FromJSON ProtVer where
  parseJSON =
    withObject "ProtVer" $ \obj -> do
      pvMajor <- mkVersion64 =<< obj .: "major"
      pvMinor <- obj .: "minor"
      pure ProtVer {..}

instance EncCBORGroup ProtVer where
  encCBORGroup (ProtVer x y) = encCBOR x <> encCBOR y

  listLen _ = 2

instance DecCBORGroup ProtVer where
  decCBORGroup =
    ProtVer
      <$> decCBOR
      <*> ifDecoderVersionAtLeast
        (natVersion @12)
        (fromIntegral @Word32 @Natural <$> decCBOR @Word32)
        (decCBOR @Natural)

data E34

instance FP.HasResolution E34 where
  resolution _ = (10 :: Integer) ^ (34 :: Integer)

type Digits34 = FP.Fixed E34

type FixedPoint = Digits34

fpPrecision :: FixedPoint
fpPrecision = (10 :: FixedPoint) ^ (34 :: Integer)

integralToBounded ::
  forall i b m. (Integral i, Integral b, Bounded b, MonadFail m) => i -> m b
integralToBounded i
  | int < minInt =
      fail $ "Value " ++ show int ++ " less than expected minimum value: " ++ show minInt
  | int > maxInt =
      fail $ "Value " ++ show int ++ " greater than expected maximum value: " ++ show maxInt
  | otherwise = pure $ fromInteger int
  where
    int = toInteger i
    minInt = toInteger (minBound @b)
    maxInt = toInteger (maxBound @b)
{-# INLINE integralToBounded #-}

-- | This is an internal type for representing rational numbers that are bounded on some
-- interval that is controlled by phantom type variable @b@ as well as by
-- the bounds of underlying type @a@.
newtype BoundedRatio b a = BoundedRatio (Ratio a)
  deriving (Eq)
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
  (Bounded a, Bounded (BoundedRatio b a), Integral a) =>
  Ratio a ->
  Maybe (BoundedRatio b a)
fromRatioBoundedRatio ratio
  | r < unboundRational lowerBound
      || r > unboundRational upperBound =
      Nothing -- ensure valid range
  | otherwise = Just $ BoundedRatio ratio
  where
    r = promoteRatio ratio
    lowerBound = minBound :: BoundedRatio b a
    upperBound = maxBound :: BoundedRatio b a

instance
  (ToCBOR a, Integral a, Bounded a, Typeable b, Typeable (BoundedRatio b a)) =>
  ToCBOR (BoundedRatio b a)
  where
  toCBOR (BoundedRatio u) = Plain.encodeRatioWithTag toCBOR u

instance
  ( FromCBOR a
  , Bounded (BoundedRatio b a)
  , Bounded a
  , Integral a
  , Typeable b
  , Show a
  , Typeable (BoundedRatio b a)
  ) =>
  FromCBOR (BoundedRatio b a)
  where
  fromCBOR = do
    r <- Plain.decodeRationalWithTag
    case boundRational r of
      Nothing ->
        cborError $ DecoderErrorCustom "BoundedRatio" (Text.pack $ show r)
      Just u -> pure u

instance (ToCBOR (BoundedRatio b a), Typeable b, Typeable a) => EncCBOR (BoundedRatio b a)

instance
  (FromCBOR (BoundedRatio b a), Typeable b, Typeable a, Typeable (BoundedRatio b a)) =>
  DecCBOR (BoundedRatio b a)

instance Bounded (BoundedRatio b Word64) => ToJSON (BoundedRatio b Word64) where
  toJSON :: BoundedRatio b Word64 -> Value
  toJSON br = case fromRationalRepetendLimited maxDecimalsWord64 r of
    Right (s, Nothing) -> toJSON s
    _ -> toJSON r
    where
      r = unboundRational br

instance Bounded (BoundedRatio b Word64) => FromJSON (BoundedRatio b Word64) where
  parseJSON = \case
    rational@(Object _) -> parseWith fromRationalEither rational
    sci -> parseWith fromScientificBoundedRatioWord64 sci
    where
      parseWith f = either fail pure . f <=< parseJSON

fromScientificBoundedRatioWord64 ::
  Bounded (BoundedRatio b Word64) =>
  Scientific ->
  Either String (BoundedRatio b Word64)
fromScientificBoundedRatioWord64 (normalize -> sci)
  | coeff < 0 = failWith "negative" sci
  | exp10 <= 0 = do
      when (exp10 < -maxDecimalsWord64) $ failWith "too precise" sci
      fromRationalEither (coeff % (10 ^ negate exp10))
  | otherwise = do
      when (maxDecimalsWord64 < exp10) $ failWith "too big" sci
      fromRationalEither (coeff * 10 ^ exp10 % 1)
  where
    coeff = coefficient sci
    exp10 = base10Exponent sci

fromRationalEither ::
  Bounded (BoundedRatio b Word64) => Rational -> Either String (BoundedRatio b Word64)
fromRationalEither r = maybe (failWith "outside of bounds" r) Right $ boundRational r

failWith :: Show a => String -> a -> Either String b
failWith msg val = Left $ "Value is " <> msg <> ": " <> show val

-- | Type to represent a value in the interval [0; +∞)
newtype NonNegativeInterval
  = NonNegativeInterval (BoundedRatio NonNegativeInterval Word64)
  deriving (Ord, Eq, Generic)
  deriving newtype
    ( Show
    , Bounded
    , BoundedRational
    , EncCBOR
    , DecCBOR
    , ToJSON
    , FromJSON
    , NoThunks
    , NFData
    )

instance Bounded (BoundedRatio NonNegativeInterval Word64) where
  minBound = BoundedRatio (0 % 1)
  maxBound = BoundedRatio (maxBound % 1)

-- | Type to represent a value in the interval (0; +∞)
newtype PositiveInterval
  = PositiveInterval (BoundedRatio PositiveInterval Word64)
  deriving (Ord, Eq, Generic)
  deriving newtype
    ( Show
    , Bounded
    , BoundedRational
    , EncCBOR
    , DecCBOR
    , ToJSON
    , FromJSON
    , NoThunks
    , NFData
    )

instance Bounded (BoundedRatio PositiveInterval Word64) where
  minBound = positiveIntervalEpsilon
  maxBound = BoundedRatio (maxBound % 1)

-- | The smallest decimal value that can roundtrip JSON
positiveIntervalEpsilon :: BoundedRatio b Word64
positiveIntervalEpsilon = BoundedRatio (1 % 10 ^ (maxDecimalsWord64 :: Int))

-- | Type to represent a value in the unit interval (0; 1]
newtype PositiveUnitInterval
  = PositiveUnitInterval (BoundedRatio PositiveUnitInterval Word64)
  deriving (Ord, Eq, Generic)
  deriving newtype
    ( Show
    , Bounded
    , BoundedRational
    , EncCBOR
    , DecCBOR
    , ToCBOR
    , FromCBOR
    , ToJSON
    , FromJSON
    , NoThunks
    , NFData
    )

instance Bounded (BoundedRatio PositiveUnitInterval Word64) where
  minBound = positiveIntervalEpsilon
  maxBound = BoundedRatio (1 % 1)

positiveUnitIntervalRelaxToUnitInterval :: PositiveUnitInterval -> UnitInterval
positiveUnitIntervalRelaxToUnitInterval = coerce

positiveUnitIntervalRelaxToPositiveInterval :: PositiveUnitInterval -> PositiveInterval
positiveUnitIntervalRelaxToPositiveInterval = coerce

positiveIntervalRelaxToNonNegativeInterval :: PositiveInterval -> NonNegativeInterval
positiveIntervalRelaxToNonNegativeInterval = coerce

-- | Type to represent a value in the unit interval [0; 1]
newtype UnitInterval
  = UnitInterval (BoundedRatio UnitInterval Word64)
  deriving (Ord, Eq, Generic)
  deriving newtype
    ( Show
    , Bounded
    , BoundedRational
    , EncCBOR
    , DecCBOR
    , ToJSON
    , FromJSON
    , NoThunks
    , NFData
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

instance EncCBOR Nonce

instance DecCBOR Nonce

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

instance ToJSON Nonce where
  toJSON NeutralNonce = Null
  toJSON (Nonce n) = toJSON n

instance FromJSON Nonce where
  parseJSON Null = return NeutralNonce
  parseJSON x = Nonce <$> parseJSON x

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
newtype Seed = Seed (Hash Blake2b_256 Seed)
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (NoThunks, EncCBOR)

instance SignableRepresentation Seed where
  getSignableRepresentation (Seed x) = hashToBytes x

(==>) :: Bool -> Bool -> Bool
a ==> b = not a || b

infix 1 ==>

--
-- Helper functions for text with byte-length bounds
--

textSizeN :: MonadFail m => Int -> Text -> m Text
textSizeN n t =
  let len = BS.length (encodeUtf8 t)
   in if len <= n
        then pure t
        else
          fail $
            "Text exceeds "
              ++ show n
              ++ " bytes:"
              ++ show t
              ++ "\n  Got "
              ++ show len
              ++ " bytes instead.\n"

textDecCBOR :: Int -> Decoder s Text
textDecCBOR n = decCBOR >>= textSizeN n

-- |  Turn a Text into a Url, fail if the Text has more than 'n' Bytes
textToUrl :: MonadFail m => Int -> Text -> m Url
textToUrl n t = Url <$> textSizeN n t

-- |  Turn a Text into a DnsName, fail if the Text has more than 'n' Bytes
textToDns :: MonadFail m => Int -> Text -> m DnsName
textToDns n t = DnsName <$> textSizeN n t

--
-- Types used in the Stake Pool Relays
--

newtype Url = Url {urlToText :: Text}
  deriving (Eq, Ord, Generic, Show)
  deriving newtype (EncCBOR, NFData, NoThunks, FromJSON, ToJSON)

instance DecCBOR Url where
  decCBOR :: Decoder s Url
  decCBOR =
    Url
      <$> ifDecoderVersionAtLeast
        (natVersion @9)
        (textDecCBOR 128)
        (textDecCBOR 64)

newtype DnsName = DnsName {dnsToText :: Text}
  deriving (Eq, Ord, Generic, Show)
  deriving newtype (EncCBOR, NoThunks, NFData, FromJSON, ToJSON)

instance DecCBOR DnsName where
  decCBOR =
    DnsName
      <$> ifDecoderVersionAtLeast
        (natVersion @9)
        (textDecCBOR 128)
        (textDecCBOR 64)

newtype Port = Port {portToWord16 :: Word16}
  deriving (Eq, Ord, Generic, Show)
  deriving newtype (Num, DecCBOR, EncCBOR, NFData, NoThunks, ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Active Slot Coefficent, named f in
-- "Ouroboros Praos: An adaptively-secure, semi-synchronous proof-of-stake protocol"
--------------------------------------------------------------------------------

data ActiveSlotCoeff = ActiveSlotCoeff
  { unActiveSlotVal :: !PositiveUnitInterval
  , unActiveSlotLog :: !Integer -- TODO mgudemann make this FixedPoint,
  -- currently a problem because of
  -- NoThunks instance for FixedPoint
  }
  deriving (Eq, Ord, Show, Generic)

instance NoThunks ActiveSlotCoeff

instance NFData ActiveSlotCoeff

instance FromCBOR ActiveSlotCoeff where
  fromCBOR = mkActiveSlotCoeff <$> fromCBOR

instance ToCBOR ActiveSlotCoeff where
  toCBOR x@(ActiveSlotCoeff _ _) =
    let ActiveSlotCoeff {..} = x
     in -- `unActiveSlotLog` is not encoded, since it can be derived from `unActiveSlotVal`
        toCBOR unActiveSlotVal

instance DecCBOR ActiveSlotCoeff

instance EncCBOR ActiveSlotCoeff

mkActiveSlotCoeff :: PositiveUnitInterval -> ActiveSlotCoeff
mkActiveSlotCoeff v =
  ActiveSlotCoeff
    { unActiveSlotVal = v
    , unActiveSlotLog =
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
  { epochInfo :: !(EpochInfo (Either Text))
  , slotsPerKESPeriod :: !Word64
  , stabilityWindow :: !Word64
  -- ^ The window size in which our chosen chain growth property
  --   guarantees at least k blocks. From the paper
  --   "Ouroboros praos: An adaptively-secure, semi-synchronous proof-of-stake protocol".
  --   The 'stabilityWindow' constant is used in a number of places; for example,
  --   protocol updates must be submitted at least twice this many slots before an epoch boundary.
  , randomnessStabilisationWindow :: !Word64
  -- ^ Number of slots before the end of the epoch at which we stop updating
  --   the candidate nonce for the next epoch.
  , securityParameter :: !(NonZero Word64)
  -- ^ Maximum number of blocks we are allowed to roll back
  , maxKESEvo :: !Word64
  -- ^ Maximum number of KES iterations
  , quorum :: !Word64
  -- ^ Quorum for update system votes and MIR certificates
  , maxLovelaceSupply :: !Word64
  -- ^ Maximum number of lovelace in the system
  , activeSlotCoeff :: !ActiveSlotCoeff
  -- ^ Active Slot Coefficient, named f in
  -- "Ouroboros Praos: An adaptively-secure, semi-synchronous proof-of-stake protocol"
  , networkId :: !Network
  -- ^ The network ID
  , systemStart :: !SystemStart
  -- ^ System start time
  }
  deriving (Show, Generic)

instance NoThunks Globals

instance NFData Globals where
  rnf (Globals {}) = ()

type ShelleyBase = ReaderT Globals Identity

-- | Pure epoch info via throw. Note that this should only be used when we can
-- guarantee the validity of the translation; in particular, the `EpochInfo`
-- used here should never be applied to user-supplied input.
epochInfoPure :: Globals -> EpochInfo Identity
epochInfoPure = hoistEpochInfo (either (throw . EpochErr) pure) . epochInfo

newtype EpochErr = EpochErr Text

deriving instance Show EpochErr

instance Exception EpochErr

-- | Relationship descriptor for the expectation in the 'Mismatch' type.
type data Relation
  = -- | Equal
    RelEQ
  | -- | Less then
    RelLT
  | -- | Greater then
    RelGT
  | -- | Less then or equal
    RelLTEQ
  | -- | Greater then or equal
    RelGTEQ
  | -- | Is subset of
    RelSubset

-- | This is intended to help clarify supplied and expected values reported by
-- predicate-failures in all eras.
data Mismatch (r :: Relation) a = Mismatch
  { mismatchSupplied :: !a
  , mismatchExpected :: !a
  }
  deriving (Eq, Ord, Generic, NFData, ToJSON, FromJSON, NoThunks)

instance (Typeable r, Show a) => Show (Mismatch (r :: Relation) a) where
  show (Mismatch {mismatchSupplied, mismatchExpected}) =
    let headerLine = "Mismatch (" <> show (typeRep @r) <> ")"
        suppliedLine = "supplied: " <> show mismatchSupplied
        expectedLine = "expected: " <> show mismatchExpected
     in headerLine <> " {" <> suppliedLine <> ", " <> expectedLine <> "}"

-- | Convert a `Mismatch` to a tuple that has "supplied" and "expected" swapped places
swapMismatch :: Mismatch r a -> (a, a)
swapMismatch Mismatch {mismatchSupplied, mismatchExpected} = (mismatchExpected, mismatchSupplied)

-- | Convert a tuple that has "supplied" and "expected" swapped places to a `Mismatch` type.
unswapMismatch :: (a, a) -> Mismatch r a
unswapMismatch (mismatchExpected, mismatchSupplied) = Mismatch {mismatchSupplied, mismatchExpected}

instance EncCBOR a => EncCBOR (Mismatch r a) where
  encCBOR (Mismatch supplied expected) =
    encode $
      Rec Mismatch
        !> To supplied
        !> To expected

instance (DecCBOR a, Typeable r) => DecCBOR (Mismatch r a) where
  decCBOR =
    decode $
      RecD Mismatch
        <! From
        <! From

instance EncCBOR a => EncCBORGroup (Mismatch r a) where
  encCBORGroup Mismatch {..} = encCBOR mismatchSupplied <> encCBOR mismatchExpected
  listLen _ = 2

instance (Typeable r, DecCBOR a) => DecCBORGroup (Mismatch r a) where
  decCBORGroup = do
    mismatchSupplied <- decCBOR
    mismatchExpected <- decCBOR
    pure Mismatch {..}

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

instance FromCBOR Network where
  fromCBOR = do
    w8 <- decodeWord8
    case word8ToNetwork w8 of
      Nothing -> cborError $ DecoderErrorCustom "Network" "Unknown network id"
      Just n -> pure n
  {-# INLINE fromCBOR #-}

instance ToCBOR Network where
  toCBOR = toCBOR . networkToWord8

instance EncCBOR Network

instance DecCBOR Network

-- | Number of blocks which have been created by stake pools in the current epoch.
newtype BlocksMade = BlocksMade
  { unBlocksMade :: Map (KeyHash StakePool) Natural
  }
  deriving (Eq, Generic)
  deriving (Show) via Quiet BlocksMade
  deriving newtype (NoThunks, NFData, ToJSON, FromJSON, EncCBOR, DecCBOR, Default)

-- | Transaction index.
newtype TxIx = TxIx {unTxIx :: Word16}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype
    (NFData, Enum, Bounded, NoThunks, FromCBOR, ToCBOR, EncCBOR, DecCBOR, ToJSON, MemPack)

instance Random TxIx

instance Uniform TxIx where
  uniformM g = TxIx <$> uniformM g

instance UniformRange TxIx where
  uniformRM r g = TxIx <$> uniformRM (coerce r) g

txIxToInt :: TxIx -> Int
txIxToInt (TxIx w16) = fromIntegral w16

txIxFromIntegral :: forall a m. (Integral a, MonadFail m) => a -> m TxIx
txIxFromIntegral = fmap (TxIx . fromIntegral) . integralToBounded @a @Word16 @m
{-# INLINE txIxFromIntegral #-}

-- | Construct a `TxIx` from an arbitrary precision `Integer`. Throws an error for
-- values out of range. Make sure to use it only for testing.
mkTxIxPartial :: HasCallStack => Integer -> TxIx
mkTxIxPartial i =
  fromMaybe (error $ "Value for TxIx is out of a valid range: " ++ show i) $
    txIxFromIntegral i

-- | Certificate index. There is `mkCertIxPartial` that can be used for testing when constructing
-- from other integral types that are larger than `Word16`
newtype CertIx = CertIx {unCertIx :: Word16}
  deriving stock (Eq, Ord, Show)
  deriving newtype (NFData, Enum, Bounded, NoThunks, EncCBOR, DecCBOR, ToCBOR, FromCBOR, ToJSON)

instance Random CertIx

instance Uniform CertIx where
  uniformM g = CertIx <$> uniformM g

instance UniformRange CertIx where
  uniformRM r g = CertIx <$> uniformRM (coerce r) g
#if MIN_VERSION_random(1,3,0)
  isInRange = isInRangeOrd
#endif

certIxToInt :: CertIx -> Int
certIxToInt (CertIx w16) = fromIntegral w16

certIxFromIntegral :: forall a m. (Integral a, MonadFail m) => a -> m CertIx
certIxFromIntegral = fmap (CertIx . fromIntegral) . integralToBounded @a @Word16 @m
{-# INLINE certIxFromIntegral #-}

-- | Construct a `CertIx` from an arbitrary precision `Integer`. Throws an error for
-- values out of range. Make sure to use it only for testing.
mkCertIxPartial :: HasCallStack => Integer -> CertIx
mkCertIxPartial i =
  fromMaybe (error $ "Value for CertIx is out of a valid range: " ++ show i) $
    certIxFromIntegral i

-- =================================

newtype AnchorData = AnchorData ByteString
  deriving (Eq)
  deriving newtype (SafeToHash)

instance HashAnnotated AnchorData AnchorData

data Anchor = Anchor
  { anchorUrl :: !Url
  , anchorDataHash :: !(SafeHash AnchorData)
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (ToJSON) via KeyValuePairs Anchor

instance NoThunks Anchor

instance NFData Anchor where
  rnf = rwhnf

instance DecCBOR Anchor where
  decCBOR =
    decode $
      RecD Anchor
        <! From
        <! From

instance EncCBOR Anchor where
  encCBOR Anchor {..} =
    encode $
      Rec Anchor
        !> To anchorUrl
        !> To anchorDataHash

instance FromJSON Anchor where
  parseJSON = withObject "Anchor" $ \o -> do
    anchorUrl <- o .: "url"
    anchorDataHash <- o .: "dataHash"
    pure $ Anchor {..}

instance ToKeyValuePairs Anchor where
  toKeyValuePairs vote@(Anchor _ _) =
    let Anchor {..} = vote
     in [ "url" .= anchorUrl
        , "dataHash" .= anchorDataHash
        ]

instance Default Anchor where
  def = Anchor (Url "") def

instance Default Network where
  def = Mainnet

class Inject t s where
  inject :: t -> s

instance Inject a a where
  inject = id

-- | Helper function for a common pattern of creating objects
kindObject :: Text -> [Pair] -> Value
kindObject name obj = object $ ("kind" .= name) : obj

positiveUnitIntervalNonZeroRational :: PositiveUnitInterval -> NonZero Rational
positiveUnitIntervalNonZeroRational = unsafeNonZero . unboundRational

class ToKeyValuePairs a where
  toKeyValuePairs :: KeyValue e kv => a -> [kv]

newtype KeyValuePairs a = KeyValuePairs {unKeyValuePairs :: a}

instance ToKeyValuePairs a => ToJSON (KeyValuePairs a) where
  toJSON = object . toKeyValuePairs . unKeyValuePairs
  toEncoding = pairs . mconcat . toKeyValuePairs . unKeyValuePairs
