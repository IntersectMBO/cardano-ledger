{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Protocol.TPraos.OCert (
  OCert (..),
  OCertEnv (..),
  OCertSignable (..),
  ocertToSignable,
  currentIssueNo,
  KESPeriod (..),
  slotsPerKESPeriod,
  kesPeriod,
)
where

import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.Util (SignableRepresentation (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (
  CBORGroup (..),
  DecCBOR (..),
  DecCBORGroup (..),
  EncCBOR (..),
  EncCBORGroup (..),
  FromCBOR (..),
  ToCBOR (..),
  encodedSigDSIGNSizeExpr,
  encodedVerKeyKESSizeExpr,
  fromPlainDecoder,
  fromPlainEncoding,
  runByteBuilder,
 )
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (
  KeyHash,
  KeyRole (..),
  SignedDSIGN,
  coerceKeyRole,
 )
import Cardano.Protocol.HeaderCrypto (HeaderCrypto, KES)
import Cardano.Protocol.HeaderKeys (VerKeyKES)
import Control.Monad.Trans.Reader (asks)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BS
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Quiet

data OCertEnv c = OCertEnv
  { ocertEnvStPools :: !(Set (KeyHash 'StakePool c))
  , ocertEnvGenDelegs :: !(Set (KeyHash 'GenesisDelegate c))
  }
  deriving (Show, Eq)

currentIssueNo ::
  OCertEnv c ->
  Map (KeyHash 'BlockIssuer c) Word64 ->
  -- | Pool hash
  KeyHash 'BlockIssuer c ->
  Maybe Word64
currentIssueNo (OCertEnv stPools genDelegs) cs hk
  | Map.member hk cs = Map.lookup hk cs
  | Set.member (coerceKeyRole hk) stPools = Just 0
  | Set.member (coerceKeyRole hk) genDelegs = Just 0
  | otherwise = Nothing

newtype KESPeriod = KESPeriod {unKESPeriod :: Word}
  deriving (Eq, Generic, Ord, NoThunks, DecCBOR, EncCBOR, ToCBOR, FromCBOR)
  deriving (Show) via Quiet KESPeriod

data OCert c hc = OCert
  { ocertVkHot :: !(VerKeyKES hc)
  -- ^ The operational hot key
  , ocertN :: !Word64
  -- ^ counter
  , ocertKESPeriod :: !KESPeriod
  -- ^ Start of key evolving signature period
  , ocertSigma :: !(SignedDSIGN c (OCertSignable hc))
  -- ^ Signature of block operational certificate content
  }
  deriving (Generic)
  deriving (EncCBOR) via (CBORGroup (OCert c hc))

deriving instance (Crypto c, HeaderCrypto hc) => Eq (OCert c hc)

deriving instance (Crypto c, HeaderCrypto hc) => Show (OCert c hc)

instance (Crypto c, HeaderCrypto hc) => NoThunks (OCert c hc)

-- Serialization of OCerts cannot be versioned, unless it gets parameterized by era.
-- Therefore we use plain encoding for defining the versioned one, instead of the oppoit
-- approach how it is done for types with versioned serialization

instance (Crypto c, HeaderCrypto hc) => EncCBORGroup (OCert c hc) where
  encCBORGroup = fromPlainEncoding . encodeOCertFields
  encodedGroupSizeExpr size proxy =
    encodedVerKeyKESSizeExpr (ocertVkHot <$> proxy)
      + encodedSizeExpr size (toWord . ocertN <$> proxy)
      + encodedSizeExpr size ((\(KESPeriod p) -> p) . ocertKESPeriod <$> proxy)
      + encodedSigDSIGNSizeExpr ((\(DSIGN.SignedDSIGN sig) -> sig) . ocertSigma <$> proxy)
    where
      toWord :: Word64 -> Word
      toWord = fromIntegral

  listLen _ = 4
  listLenBound _ = 4

instance (Crypto c, HeaderCrypto hc) => DecCBORGroup (OCert c hc) where
  decCBORGroup = fromPlainDecoder decodeOCertFields

instance (Crypto c, HeaderCrypto hc) => ToCBOR (OCert c hc) where
  toCBOR ocert = Plain.encodeListLen (listLen ocert) <> encodeOCertFields ocert

instance (Crypto c, HeaderCrypto hc) => FromCBOR (OCert c hc) where
  fromCBOR =
    Plain.decodeRecordNamed "OCert" (fromIntegral . listLen) decodeOCertFields

encodeOCertFields :: (Crypto c, HeaderCrypto hc) => OCert c hc -> Plain.Encoding
encodeOCertFields ocert =
  KES.encodeVerKeyKES (ocertVkHot ocert)
    <> Plain.toCBOR (ocertN ocert)
    <> Plain.toCBOR (ocertKESPeriod ocert)
    <> DSIGN.encodeSignedDSIGN (ocertSigma ocert)

decodeOCertFields :: (Crypto c, HeaderCrypto hc) => Plain.Decoder s (OCert c hc)
decodeOCertFields =
  OCert
    <$> KES.decodeVerKeyKES
    <*> Plain.fromCBOR
    <*> Plain.fromCBOR
    <*> DSIGN.decodeSignedDSIGN

kesPeriod :: SlotNo -> ShelleyBase KESPeriod
kesPeriod (SlotNo s) =
  asks slotsPerKESPeriod <&> \spkp ->
    if spkp == 0
      then error "kesPeriod: slots per KES period was set to zero"
      else KESPeriod . fromIntegral $ s `div` spkp

-- | Signable part of an operational certificate
data OCertSignable c
  = OCertSignable !(VerKeyKES c) !Word64 !KESPeriod

instance
  forall hc.
  HeaderCrypto hc =>
  SignableRepresentation (OCertSignable hc)
  where
  getSignableRepresentation (OCertSignable vk counter period) =
    runByteBuilder
      ( fromIntegral $
          KES.sizeVerKeyKES (Proxy @(KES hc))
            + 8
            + 8
      )
      $ BS.byteStringCopy (KES.rawSerialiseVerKeyKES vk)
        <> BS.word64BE counter
        <> BS.word64BE (fromIntegral $ unKESPeriod period)

-- | Extract the signable part of an operational certificate (for verification)
ocertToSignable :: OCert c hc -> OCertSignable hc
ocertToSignable OCert {ocertVkHot, ocertN, ocertKESPeriod} =
  OCertSignable ocertVkHot ocertN ocertKESPeriod
