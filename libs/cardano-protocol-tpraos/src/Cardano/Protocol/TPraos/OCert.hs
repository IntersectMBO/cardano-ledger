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
) where

import Cardano.Base.Proxy (asProxy)
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
  fromPlainDecoder,
  fromPlainEncoding,
  listLenInt,
  runByteBuilder,
 )
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Keys (
  DSIGN,
  KeyHash,
  KeyRole (..),
  coerceKeyRole,
 )
import Cardano.Protocol.Crypto (Crypto, KES)
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

data OCertEnv = OCertEnv
  { ocertEnvStPools :: Set (KeyHash StakePool)
  , ocertEnvGenDelegs :: Set (KeyHash GenesisDelegate)
  }
  deriving (Show, Eq)

currentIssueNo ::
  OCertEnv ->
  Map (KeyHash BlockIssuer) Word64 ->
  -- | Pool hash
  KeyHash BlockIssuer ->
  Maybe Word64
currentIssueNo (OCertEnv stPools genDelegs) cs hk
  | Map.member hk cs = Map.lookup hk cs
  | Set.member (coerceKeyRole hk) stPools = Just 0
  | Set.member (coerceKeyRole hk) genDelegs = Just 0
  | otherwise = Nothing

newtype KESPeriod = KESPeriod {unKESPeriod :: Word}
  deriving (Eq, Generic, Ord, NoThunks, DecCBOR, EncCBOR, ToCBOR, FromCBOR)
  deriving (Show) via Quiet KESPeriod

data OCert c = OCert
  { ocertVkHot :: !(KES.VerKeyKES (KES c))
  -- ^ The operational hot key
  , ocertN :: !Word64
  -- ^ counter
  , ocertKESPeriod :: !KESPeriod
  -- ^ Start of key evolving signature period
  , ocertSigma :: !(DSIGN.SignedDSIGN DSIGN (OCertSignable c))
  -- ^ Signature of block operational certificate content
  }
  deriving (Generic)
  deriving (EncCBOR) via (CBORGroup (OCert c))
  deriving (DecCBOR) via (CBORGroup (OCert c))

deriving instance Crypto c => Eq (OCert c)

deriving instance Crypto c => Show (OCert c)

instance Crypto c => NoThunks (OCert c)

-- Serialization of OCerts cannot be versioned, unless it gets parameterized by era.
-- Therefore we use plain encoding for defining the versioned one, instead of the oppoit
-- approach how it is done for types with versioned serialization

instance Crypto c => EncCBORGroup (OCert c) where
  encCBORGroup = fromPlainEncoding . encodeOCertFields
  listLen _ = 4

instance Crypto c => DecCBORGroup (OCert c) where
  decCBORGroup = fromPlainDecoder decodeOCertFields

instance Crypto c => ToCBOR (OCert c) where
  toCBOR ocert = Plain.encodeListLen (listLen (asProxy ocert)) <> encodeOCertFields ocert

instance Crypto c => FromCBOR (OCert c) where
  fromCBOR =
    Plain.decodeRecordNamed "OCert" (listLenInt . Just) decodeOCertFields

encodeOCertFields :: Crypto c => OCert c -> Plain.Encoding
encodeOCertFields ocert =
  KES.encodeVerKeyKES (ocertVkHot ocert)
    <> Plain.toCBOR (ocertN ocert)
    <> Plain.toCBOR (ocertKESPeriod ocert)
    <> DSIGN.encodeSignedDSIGN (ocertSigma ocert)

decodeOCertFields :: Crypto c => Plain.Decoder s (OCert c)
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
  = OCertSignable !(KES.VerKeyKES (KES c)) !Word64 !KESPeriod

instance Crypto c => SignableRepresentation (OCertSignable c) where
  getSignableRepresentation (OCertSignable vk counter period) =
    runByteBuilder
      ( fromIntegral $
          KES.sizeVerKeyKES (Proxy @(KES c))
            + 8
            + 8
      )
      $ BS.byteStringCopy (KES.rawSerialiseVerKeyKES vk)
        <> BS.word64BE counter
        <> BS.word64BE (fromIntegral $ unKESPeriod period)

-- | Extract the signable part of an operational certificate (for verification)
ocertToSignable :: OCert c -> OCertSignable c
ocertToSignable OCert {ocertVkHot, ocertN, ocertKESPeriod} =
  OCertSignable ocertVkHot ocertN ocertKESPeriod
