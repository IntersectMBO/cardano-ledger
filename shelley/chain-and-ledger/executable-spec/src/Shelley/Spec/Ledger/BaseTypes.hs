{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Shelley.Spec.Ledger.BaseTypes
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
  , Text64
  , text64
  , text64Size
  , StrictMaybe (..)
  , strictMaybeToMaybe
  , maybeToStrictMaybe
  , fromSMaybe
  , Url
  , unUrl
  , mkUrl
  , DnsName
  , unDnsName
  , mkDnsName
  , dnsSize
  , IPv4
  , unIPv4
  , mkIPv4
  , IPv6
  , unIPv6
  , mkIPv6
  , Port
  , unPort
  , ActiveSlotCoeff
  , mkActiveSlotCoeff
  , activeSlotVal
  , activeSlotLog
    -- * STS Base
  , Globals (..)
  , ShelleyBase
  ) where


import           Cardano.Binary (Decoder, DecoderError (..), FromCBOR (fromCBOR), ToCBOR (toCBOR),
                     decodeListLen, decodeWord, encodeListLen, matchSize)
import           Cardano.Crypto.Hash
import           Cardano.Prelude (NoUnexpectedThunks (..), cborError)
import           Cardano.Slotting.EpochInfo
import           Control.Monad (join, unless)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as BS
import           Data.ByteString.Conversion (toByteString')
import           Data.Coerce (coerce)
import qualified Data.Fixed as FP (Fixed, HasResolution, resolution)
import           Data.Functor.Identity
import           Data.Ratio (denominator, numerator, (%))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8)
import           Data.Word (Word16, Word64, Word8)
import           GHC.Generics (Generic)
import           Network.Socket (HostAddress, HostAddress6, hostAddress6ToTuple)
import           Numeric.Natural (Natural)

import           Shelley.Spec.Ledger.Serialization (rationalFromCBOR, rationalToCBOR)
import           Shelley.Spec.NonIntegral (ln')

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
newtype UnitInterval = UnsafeUnitInterval Rational   -- TODO: Fixed precision
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

newtype Text64 = Text64 Text
  deriving (Eq, Ord, Generic, Show, ToCBOR, NoUnexpectedThunks)

text64 :: Text -> Text64
text64 t =
  let numBytes = BS.length . encodeUtf8 $ t
  in
    if numBytes <= 64
      then Text64 t
      else error $ "text64 received too many bytes: " <> show numBytes

text64Size :: Text64 -> Int
text64Size (Text64 t) = BS.length . encodeUtf8 $ t

instance FromCBOR Text64
 where
  fromCBOR = do
    t <- fromCBOR
    if (BS.length . encodeUtf8) t > 64
      then cborError $ DecoderErrorCustom "Text64 has too many bytes:" t
      else pure $ Text64 t

-- | Strict 'Maybe'.
--
-- TODO move to @cardano-prelude@
data StrictMaybe a
  = SNothing
  | SJust !a
  deriving (Eq, Ord, Show, Generic)

instance NoUnexpectedThunks a => NoUnexpectedThunks (StrictMaybe a)

instance Functor StrictMaybe where
  fmap _ SNothing  = SNothing
  fmap f (SJust a) = SJust (f a)

instance Applicative StrictMaybe where
  pure = SJust

  SJust f  <*> m   = fmap f m
  SNothing <*> _m  = SNothing

  SJust _m1 *> m2  = m2
  SNothing  *> _m2 = SNothing

instance Monad StrictMaybe where
    SJust x >>= k   = k x
    SNothing  >>= _ = SNothing

    (>>) = (*>)

    return = SJust
    fail _ = SNothing

instance ToCBOR a => ToCBOR (StrictMaybe a) where
  toCBOR SNothing  = encodeListLen 0
  toCBOR (SJust x) = encodeListLen 1 <> toCBOR x

instance FromCBOR a => FromCBOR (StrictMaybe a) where
  fromCBOR = do
    n <- decodeListLen
    case n of
      0 -> pure SNothing
      1 -> SJust <$> fromCBOR
      _ -> fail "unknown tag"

strictMaybeToMaybe :: StrictMaybe a -> Maybe a
strictMaybeToMaybe SNothing  = Nothing
strictMaybeToMaybe (SJust x) = Just x

maybeToStrictMaybe :: Maybe a -> StrictMaybe a
maybeToStrictMaybe Nothing  = SNothing
maybeToStrictMaybe (Just x) = SJust x

fromSMaybe :: a -> StrictMaybe a -> a
fromSMaybe d SNothing = d
fromSMaybe _ (SJust x) = x

newtype Url = Url { unUrl :: Text64 }
  deriving (Eq, Generic, Show, ToCBOR, FromCBOR, NoUnexpectedThunks)

mkUrl :: Text -> Url
mkUrl = Url . text64

newtype DnsName = DnsName { unDnsName :: Text64 }
  deriving (Eq, Generic, Show, ToCBOR, FromCBOR, NoUnexpectedThunks)

mkDnsName :: Text -> DnsName
mkDnsName = DnsName . text64

dnsSize :: DnsName -> Integer
dnsSize = toInteger . text64Size . unDnsName

newtype IPv4 = IPv4 { unIPv4 :: ByteString }
  deriving (Eq, Generic, Show, ToCBOR, NoUnexpectedThunks)

mkIPv4 :: HostAddress -> IPv4
mkIPv4 hostAddr = IPv4 $ toByteString' hostAddr

decodeMaxBytes :: String -> Int -> Decoder s BS.ByteString
decodeMaxBytes name m = do
  b <- fromCBOR
  unless (BS.length b <= m)
     (fail $ name <> " is too big: " <> show b)
  pure b

instance FromCBOR IPv4 where
  fromCBOR = IPv4 <$> decodeMaxBytes "IPv4" 4

newtype IPv6 = IPv6 { unIPv6 :: ByteString }
  deriving (Eq, Generic, Show, ToCBOR, NoUnexpectedThunks)

mkIPv6 :: HostAddress6 -> IPv6
mkIPv6 hostAddr = IPv6 . BS.pack . join . fmap encodeWord16
    $ [w1, w2, w3, w4, w5, w6, w7, w8]
  where
    (w1, w2, w3, w4, w5, w6, w7, w8) = hostAddress6ToTuple hostAddr
    encodeWord16 :: Word16 -> [Word8]
    encodeWord16 x = map fromIntegral [ x .&. 0xFF, (x .&. 0xFF00) `shiftR` 8 ]

instance FromCBOR IPv6 where
  fromCBOR = IPv6 <$> decodeMaxBytes "IPv6" 16

newtype Port = Port { unPort :: Word16 }
  deriving (Eq, Ord, Num, Generic, Show, ToCBOR, FromCBOR, NoUnexpectedThunks)

--------------------------------------------------------------------------------
-- Active Slot Coefficent, named f in
-- "Ouroboros Praos: An adaptively-secure, semi-synchronous proof-of-stake protocol"
--------------------------------------------------------------------------------

data ActiveSlotCoeff =
  ActiveSlotCoeff
  { unActiveSlotVal :: !UnitInterval
  , unActiveSlotLog :: !Integer  -- TODO mgudemann make this FixedPoint,
                                 -- currently a problem because of
                                 -- NoUnexpectedThunks instance for FixedPoint
  } deriving (Eq, Ord, Show, Generic)

instance NoUnexpectedThunks ActiveSlotCoeff

instance FromCBOR ActiveSlotCoeff
 where
   fromCBOR = do
     v <- fromCBOR
     pure $ mkActiveSlotCoeff v

instance ToCBOR ActiveSlotCoeff
 where
   toCBOR (ActiveSlotCoeff { unActiveSlotVal = slotVal
                           , unActiveSlotLog = _logVal}) =
     toCBOR slotVal

mkActiveSlotCoeff :: UnitInterval -> ActiveSlotCoeff
mkActiveSlotCoeff v =
  ActiveSlotCoeff { unActiveSlotVal = v
                  , unActiveSlotLog =
                    if (intervalValue v) == 1
                      -- If the active slot coefficient is equal to one,
                      -- then nearly every stake pool can produce a block every slot.
                      -- In this degenerate case, where ln (1-f) is not defined,
                      -- we set the unActiveSlotLog to zero.
                      then 0
                      else floor (fpPrecision * (
                        ln' $ (1 :: FixedPoint) - (fromRational $ intervalValue v))) }

activeSlotVal :: ActiveSlotCoeff -> UnitInterval
activeSlotVal = unActiveSlotVal

activeSlotLog :: ActiveSlotCoeff -> FixedPoint
activeSlotLog f = (fromIntegral $ unActiveSlotLog f) / fpPrecision

--------------------------------------------------------------------------------
-- Base monad for all STS systems
--------------------------------------------------------------------------------

data Globals = Globals
  { epochInfo :: !(EpochInfo Identity)
  , slotsPerKESPeriod :: !Word64
    -- | Number of slots before the end of the epoch at which we stop updating
    --   the candidate nonce for the next epoch.
    --
    --   This value is also used in a number of other places; for example,
    --   protocol updates must be submitted at least this many slots before an
    --   epoch boundary.
  , slotsPrior :: !Word64
    -- | Number of slots after the beginning of an epoch when we may begin to
    --   distribute rewards.
  , startRewards :: !Word64
    -- | Maximum number of blocks we are allowed to roll back
  , securityParameter :: !Word64
    -- | Maximum number of KES iterations
  , maxKESEvo :: !Word64
    -- | Quorum for update system votes and MIR certificates
  , quorum :: !Word64
    -- | All blocks invalid after this protocol version
  , maxMajorPV :: !Natural
    -- | Maximum number of lovelace in the system
  , maxLovelaceSupply :: !Word64
    -- | Active Slot Coefficient, named f in
    -- "Ouroboros Praos: An adaptively-secure, semi-synchronous proof-of-stake protocol"
  , activeSlotCoeff :: !ActiveSlotCoeff
  } deriving (Generic)

instance NoUnexpectedThunks Globals

type ShelleyBase = ReaderT Globals Identity
