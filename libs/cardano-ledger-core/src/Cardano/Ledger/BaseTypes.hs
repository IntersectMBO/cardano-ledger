{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Ledger.BaseTypes (
  module Slotting,
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
  BoundedRational (..),
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
  kindObject,

  -- * Indices
  TxIx (..),
  txIxToInt,
  txIxFromIntegral,
  mkTxIx,
  mkTxIxPartial,
  CertIx (..),
  certIxToInt,
  certIxFromIntegral,
  mkCertIx,
  mkCertIxPartial,
  Anchor (..),
  AnchorData (..),
  hashAnchorData,

  -- * STS Base
  Globals (..),
  epochInfoPure,
  ShelleyBase,

  -- * Injection
  Inject (..),
)
where

import Cardano.Crypto.Hash
import Cardano.Crypto.Util (SignableRepresentation (..))
import qualified Cardano.Crypto.VRF as VRF
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
  decodeRationalWithTag,
  encodeRatioWithTag,
  encodedSizeExpr,
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
  encodeListLen,
  invalidKey,
 )
import Cardano.Ledger.Binary.Version
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.NonIntegral (ln')
import Cardano.Ledger.SafeHash (HashWithCrypto (..), SafeHash, SafeToHash)
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
import Data.Default.Class (Default (def))
import qualified Data.Fixed as FP (Fixed, HasResolution, resolution)
import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Maybe.Strict
import Data.Proxy
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
import Data.Void (Void, absurd)
import Data.Word (Word16, Word64, Word8)
import GHC.Exception.Type (Exception)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)
import Quiet (Quiet (Quiet))

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
  encodedGroupSizeExpr l proxy =
    encodedSizeExpr l (pvMajor <$> proxy)
      + encodedSizeExpr l (toWord . pvMinor <$> proxy)
    where
      toWord :: Natural -> Word
      toWord = fromIntegral

  listLen _ = 2
  listLenBound _ = 2

instance DecCBORGroup ProtVer where
  decCBORGroup = ProtVer <$> decCBOR <*> decCBOR

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

instance (EncCBOR a, Integral a, Bounded a, Typeable b) => EncCBOR (BoundedRatio b a) where
  encCBOR (BoundedRatio u) = encodeRatioWithTag encCBOR u

instance
  (DecCBOR a, Bounded (BoundedRatio b a), Bounded a, Integral a, Typeable b, Show a) =>
  DecCBOR (BoundedRatio b a)
  where
  decCBOR = do
    r <- decodeRationalWithTag
    case boundRational r of
      Nothing ->
        cborError $ DecoderErrorCustom "BoundedRatio" (Text.pack $ show r)
      Just u -> pure u

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
    , ToJSON
    , FromJSON
    , NoThunks
    , NFData
    )

instance Bounded (BoundedRatio PositiveUnitInterval Word64) where
  minBound = positiveIntervalEpsilon
  maxBound = BoundedRatio (1 % 1)

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
  if BS.length (encodeUtf8 t) <= n
    then pure t
    else fail $ "Text exceeds " ++ show n ++ " bytes:" ++ show t

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

instance DecCBOR ActiveSlotCoeff where
  decCBOR = mkActiveSlotCoeff <$> decCBOR

instance EncCBOR ActiveSlotCoeff where
  encCBOR
    ActiveSlotCoeff
      { unActiveSlotVal = slotVal
      , unActiveSlotLog = _logVal
      } =
      encCBOR slotVal

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
  , securityParameter :: !Word64
  -- ^ Maximum number of blocks we are allowed to roll back
  , maxKESEvo :: !Word64
  -- ^ Maximum number of KES iterations
  , quorum :: !Word64
  -- ^ Quorum for update system votes and MIR certificates
  , maxMajorPV :: !Version
  -- ^ All blocks invalid after this protocol version
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

instance EncCBOR Network where
  encCBOR = encCBOR . networkToWord8

instance DecCBOR Network where
  decCBOR =
    word8ToNetwork <$> decCBOR >>= \case
      Nothing -> cborError $ DecoderErrorCustom "Network" "Unknown network id"
      Just n -> pure n
  {-# INLINE decCBOR #-}

-- | Blocks made
newtype BlocksMade c = BlocksMade
  { unBlocksMade :: Map (KeyHash 'StakePool c) Natural
  }
  deriving (Eq, Generic)
  deriving (Show) via Quiet (BlocksMade c)
  deriving newtype (NoThunks, NFData, ToJSON, FromJSON, EncCBOR, DecCBOR)

-- TODO: It is unfeasable to have 65535 outputs in a transaction,
-- but 255 is right on the border of a maximum TxIx on Mainnet at the moment,
-- that is why `Word16` was chosen as the smallest upper bound. Use
-- `txIxFromIntegral` in order to construct this index safely from anything
-- other than `Word16`. There is also `mkTxIxPartial` that can be used for
-- testing.

-- | Transaction index.
newtype TxIx = TxIx Word64
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (NFData, Enum, Bounded, NoThunks, EncCBOR, DecCBOR, ToCBOR, FromCBOR, ToJSON)

-- | Construct a `TxIx` from a 16 bit unsigned integer
mkTxIx :: Word16 -> TxIx
mkTxIx = TxIx . fromIntegral

txIxToInt :: TxIx -> Int
txIxToInt (TxIx w16) = fromIntegral w16

txIxFromIntegral :: Integral a => a -> Maybe TxIx
txIxFromIntegral = fmap (TxIx . fromIntegral) . word16FromInteger . toInteger
{-# INLINE txIxFromIntegral #-}

-- | Construct a `TxIx` from an arbitrary precision `Integer`. Throws an error for
-- values out of range. Make sure to use it only for testing.
mkTxIxPartial :: HasCallStack => Integer -> TxIx
mkTxIxPartial i =
  fromMaybe (error $ "Value for TxIx is out of a valid range: " ++ show i) $
    txIxFromIntegral i

-- | Certificate index. Use `certIxFromIntegral` in order to construct this
-- index safely from anything other than `Word16`. There is also
-- `mkCertIxPartial` that can be used for testing.
newtype CertIx = CertIx Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (NFData, Enum, Bounded, NoThunks, EncCBOR, DecCBOR, ToCBOR, FromCBOR, ToJSON)

-- | Construct a `CertIx` from a 16 bit unsigned integer
mkCertIx :: Word16 -> CertIx
mkCertIx = CertIx . fromIntegral

certIxToInt :: CertIx -> Int
certIxToInt (CertIx w16) = fromIntegral w16

certIxFromIntegral :: Integral a => a -> Maybe CertIx
certIxFromIntegral = fmap (CertIx . fromIntegral) . word16FromInteger . toInteger
{-# INLINE certIxFromIntegral #-}

-- | Construct a `CertIx` from an arbitrary precision `Integer`. Throws an error for
-- values out of range. Make sure to use it only for testing.
mkCertIxPartial :: HasCallStack => Integer -> CertIx
mkCertIxPartial i =
  fromMaybe (error $ "Value for CertIx is out of a valid range: " ++ show i) $
    certIxFromIntegral i

word16FromInteger :: Integer -> Maybe Word16
word16FromInteger i
  | i < fromIntegral (minBound :: Word16) || i > fromIntegral (maxBound :: Word16) = Nothing
  | otherwise = Just (fromInteger i)
{-# INLINE word16FromInteger #-}

-- =================================

newtype AnchorData = AnchorData ByteString
  deriving (Eq)
  deriving newtype (SafeToHash)

instance HashWithCrypto AnchorData AnchorData

-- | Hash `AnchorData`
hashAnchorData :: forall c. Crypto c => AnchorData -> SafeHash c AnchorData
hashAnchorData = hashWithCrypto (Proxy @c)

data Anchor c = Anchor
  { anchorUrl :: !Url
  , anchorDataHash :: !(SafeHash c AnchorData)
  }
  deriving (Eq, Ord, Show, Generic)

instance NoThunks (Anchor c)

instance Crypto c => NFData (Anchor c) where
  rnf = rwhnf

instance Crypto c => DecCBOR (Anchor c) where
  decCBOR =
    decode $
      RecD Anchor
        <! From
        <! From

instance Crypto c => EncCBOR (Anchor c) where
  encCBOR Anchor {..} =
    encode $
      Rec Anchor
        !> To anchorUrl
        !> To anchorDataHash

instance Crypto c => ToJSON (Anchor c) where
  toJSON = object . toAnchorPairs
  toEncoding = pairs . mconcat . toAnchorPairs

instance Crypto c => FromJSON (Anchor c) where
  parseJSON = withObject "Anchor" $ \o -> do
    anchorUrl <- o .: "url"
    anchorDataHash <- o .: "dataHash"
    pure $ Anchor {..}

instance Crypto c => Default (Anchor c) where
  def = Anchor (Url "") def

toAnchorPairs :: (KeyValue e a, Crypto c) => Anchor c -> [a]
toAnchorPairs vote@(Anchor _ _) =
  let Anchor {..} = vote
   in [ "url" .= anchorUrl
      , "dataHash" .= anchorDataHash
      ]

instance Default Network where
  def = Mainnet

class Inject t s where
  inject :: t -> s
  default inject :: t ~ s => t -> s
  inject = id

instance Inject t () where
  inject = const ()

instance Inject Void s where
  inject = absurd

-- | Helper function for a common pattern of creating objects
kindObject :: Text -> [Pair] -> Value
kindObject name obj = object $ ("kind" .= name) : obj
