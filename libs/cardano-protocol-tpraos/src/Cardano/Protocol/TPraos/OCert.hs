{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Protocol.TPraos.OCert
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

import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.Util (SignableRepresentation (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary
  ( CBORGroup (..),
    FromCBOR (..),
    FromCBORGroup (..),
    ToCBOR (..),
    ToCBORGroup (..),
    encodedSigDSIGNSizeExpr,
    encodedVerKeyKESSizeExpr,
    runByteBuilder,
  )
import Cardano.Ledger.Binary.Crypto
import Cardano.Ledger.Crypto (Crypto, KES)
import Cardano.Ledger.Keys
  ( KeyHash,
    KeyRole (..),
    SignedDSIGN,
    VerKeyKES,
    coerceKeyRole,
  )
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
  { ocertEnvStPools :: Set (KeyHash 'StakePool c),
    ocertEnvGenDelegs :: Set (KeyHash 'GenesisDelegate c)
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
  deriving (Eq, Generic, Ord, NoThunks, FromCBOR, ToCBOR)
  deriving (Show) via Quiet KESPeriod

data OCert c = OCert
  { -- | The operational hot key
    ocertVkHot :: !(VerKeyKES c),
    -- | counter
    ocertN :: !Word64,
    -- | Start of key evolving signature period
    ocertKESPeriod :: !KESPeriod,
    -- | Signature of block operational certificate content
    ocertSigma :: !(SignedDSIGN c (OCertSignable c))
  }
  deriving (Generic)
  deriving (ToCBOR) via (CBORGroup (OCert c))

deriving instance Crypto c => Eq (OCert c)

deriving instance Crypto c => Show (OCert c)

instance Crypto c => NoThunks (OCert c)

instance
  (Crypto c) =>
  ToCBORGroup (OCert c)
  where
  toCBORGroup ocert =
    encodeVerKeyKES (ocertVkHot ocert)
      <> toCBOR (ocertN ocert)
      <> toCBOR (ocertKESPeriod ocert)
      <> encodeSignedDSIGN (ocertSigma ocert)
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

instance
  (Crypto c) =>
  FromCBORGroup (OCert c)
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
data OCertSignable c
  = OCertSignable !(VerKeyKES c) !Word64 !KESPeriod

instance
  forall c.
  Crypto c =>
  SignableRepresentation (OCertSignable c)
  where
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
