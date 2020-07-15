{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Shelley.Spec.Ledger.OCert
  ( OCert (..),
    OCertEnv (..),
    OCertSignable (..),
    ocertToSignable,
    currentIssueNo,
    KESPeriod (..),
    slotsPerKESPeriod,
    kesPeriod,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), toCBOR)
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.Util (SignableRepresentation (..))
import Cardano.Prelude (NoUnexpectedThunks (..))
import Control.Monad.Trans.Reader (asks)
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as BSL
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word, Word64)
import GHC.Generics (Generic)
import Quiet
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.Keys
  ( KeyHash,
    KeyRole (..),
    SignedDSIGN,
    VerKeyKES,
    coerceKeyRole,
    decodeSignedDSIGN,
    decodeVerKeyKES,
    encodeSignedDSIGN,
    encodeVerKeyKES,
  )
import Shelley.Spec.Ledger.Serialization
  ( CBORGroup (..),
    FromCBORGroup (..),
    ToCBORGroup (..),
  )
import Shelley.Spec.Ledger.Slot (SlotNo (..))

data OCertEnv crypto = OCertEnv
  { ocertEnvStPools :: Set (KeyHash 'StakePool crypto),
    ocertEnvGenDelegs :: Set (KeyHash 'GenesisDelegate crypto)
  }
  deriving (Show, Eq)

currentIssueNo ::
  OCertEnv crypto ->
  (Map (KeyHash 'BlockIssuer crypto) Word64) ->
  -- | Pool hash
  KeyHash 'BlockIssuer crypto ->
  Maybe Word64
currentIssueNo (OCertEnv stPools genDelegs) cs hk
  | Map.member hk cs = Map.lookup hk cs
  | Set.member (coerceKeyRole hk) stPools = Just 0
  | Set.member (coerceKeyRole hk) genDelegs = Just 0
  | otherwise = Nothing

newtype KESPeriod = KESPeriod {unKESPeriod :: Word}
  deriving (Eq, Generic, Ord, NoUnexpectedThunks, FromCBOR, ToCBOR)
  deriving (Show) via Quiet KESPeriod

data OCert crypto = OCert
  { -- | The operational hot key
    ocertVkHot :: !(VerKeyKES crypto),
    -- | counter
    ocertN :: !Word64,
    -- | Start of key evolving signature period
    ocertKESPeriod :: !KESPeriod,
    -- | Signature of block operational certificate content
    ocertSigma :: !(SignedDSIGN crypto (OCertSignable crypto))
  }
  deriving (Generic)
  deriving (ToCBOR) via (CBORGroup (OCert crypto))

deriving instance Crypto crypto => Eq (OCert crypto)

deriving instance Crypto crypto => Show (OCert crypto)

instance Crypto crypto => NoUnexpectedThunks (OCert crypto)

instance
  (Crypto crypto) =>
  ToCBORGroup (OCert crypto)
  where
  toCBORGroup ocert =
    encodeVerKeyKES (ocertVkHot ocert)
      <> toCBOR (ocertN ocert)
      <> toCBOR (ocertKESPeriod ocert)
      <> encodeSignedDSIGN (ocertSigma ocert)
  encodedGroupSizeExpr size proxy =
    KES.encodedVerKeyKESSizeExpr (ocertVkHot <$> proxy)
      + encodedSizeExpr size ((toWord . ocertN) <$> proxy)
      + encodedSizeExpr size ((\(KESPeriod p) -> p) . ocertKESPeriod <$> proxy)
      + DSIGN.encodedSigDSIGNSizeExpr (((\(DSIGN.SignedDSIGN sig) -> sig) . ocertSigma) <$> proxy)
    where
      toWord :: Word64 -> Word
      toWord = fromIntegral

  listLen _ = 4
  listLenBound _ = 4

instance
  (Crypto crypto) =>
  FromCBORGroup (OCert crypto)
  where
  fromCBORGroup =
    OCert
      <$> decodeVerKeyKES
      <*> fromCBOR
      <*> fromCBOR
      <*> decodeSignedDSIGN

kesPeriod :: SlotNo -> ShelleyBase KESPeriod
kesPeriod (SlotNo s) =
  asks slotsPerKESPeriod <&> \spkp ->
    if spkp == 0
      then error "kesPeriod: slots per KES period was set to zero"
      else KESPeriod . fromIntegral $ s `div` spkp

-- | Signable part of an operational certificate
data OCertSignable crypto
  = OCertSignable !(VerKeyKES crypto) !Word64 !KESPeriod

instance Crypto crypto => SignableRepresentation (OCertSignable crypto) where
  getSignableRepresentation (OCertSignable vk counter period) =
    BSL.toStrict . Binary.runPut $ do
      Binary.putByteString (KES.rawSerialiseVerKeyKES vk)
      Binary.putWord64be counter
      Binary.putWord64be (fromIntegral $ unKESPeriod period)

-- | Extract the signable part of an operational certificate (for verification)
ocertToSignable :: OCert crypto -> OCertSignable crypto
ocertToSignable OCert {ocertVkHot, ocertN, ocertKESPeriod} =
  OCertSignable ocertVkHot ocertN ocertKESPeriod
