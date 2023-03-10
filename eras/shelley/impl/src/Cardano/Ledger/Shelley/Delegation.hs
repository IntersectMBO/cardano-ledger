{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.Delegation.Certificates (
  Delegation (..),
  ShelleyDCert (..),
  DelegCert (..),
  PoolCert (..),
  ConstitutionalDelegCert (..),
  MIRCert (..),
  MIRPot (..),
  MIRTarget (..),
  delegCWitness,
  poolCWitness,
  genesisCWitness,
  isRegKey,
  isDeRegKey,
  isDelegation,
  isGenesisDelegation,
  isRegPool,
  isRetirePool,
  isInstantaneousRewards,
  isReservesMIRCert,
  isTreasuryMIRCert,
  requiresVKeyWitness,
)
where

import Cardano.Ledger.BaseTypes (invalidKey)
import Cardano.Ledger.Binary (
  DecCBOR (decCBOR),
  DecCBORGroup (..),
  EncCBOR (..),
  EncCBORGroup (..),
  FromCBOR (..),
  ToCBOR (..),
  TokenType (TypeMapLen, TypeMapLen64, TypeMapLenIndef),
  decodeRecordNamed,
  decodeRecordSum,
  decodeWord,
  encodeListLen,
  listLenInt,
  peekTokenType,
 )
import Cardano.Ledger.Coin (Coin (..), DeltaCoin)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), StakeCredential)
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (
  Hash,
  KeyHash (..),
  KeyRole (..),
  VerKeyVRF,
 )
import Cardano.Ledger.PoolParams
import Cardano.Ledger.Slot (EpochNo (..))
import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import Data.Word (Word8)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))


instance Crypto c => EraDCert (ShelleyEra c) where
  {-# SPECIALIZE instance EraDCert (ShelleyEra StandardCrypto) #-}

  type DCert (ShelleyEra c) = ShelleyDCert (ShelleyEra c)


class EraDCert era => ShelleyEraDCert era where

  getDCertMIR :: DCert era -> Maybe (MIRCert (EraCrypto era))

data MIRPot = ReservesMIR | TreasuryMIR
  deriving (Show, Generic, Eq, NFData, Ord, Enum, Bounded)

deriving instance NoThunks MIRPot

instance EncCBOR MIRPot where
  encCBOR ReservesMIR = encCBOR (0 :: Word8)
  encCBOR TreasuryMIR = encCBOR (1 :: Word8)

instance DecCBOR MIRPot where
  decCBOR =
    decodeWord >>= \case
      0 -> pure ReservesMIR
      1 -> pure TreasuryMIR
      k -> invalidKey k

-- | MIRTarget specifies if funds from either the reserves
-- or the treasury are to be handed out to a collection of
-- reward accounts or instead transfered to the opposite pot.
data MIRTarget c
  = StakeAddressesMIR (Map (Credential 'Staking c) DeltaCoin)
  | SendToOppositePotMIR Coin
  deriving (Show, Generic, Eq, NFData)

deriving instance NoThunks (MIRTarget c)

instance Crypto c => DecCBOR (MIRTarget c) where
  decCBOR = do
    peekTokenType >>= \case
      TypeMapLen -> StakeAddressesMIR <$> decCBOR
      TypeMapLen64 -> StakeAddressesMIR <$> decCBOR
      TypeMapLenIndef -> StakeAddressesMIR <$> decCBOR
      _ -> SendToOppositePotMIR <$> decCBOR

instance Crypto c => EncCBOR (MIRTarget c) where
  encCBOR (StakeAddressesMIR m) = encCBOR m
  encCBOR (SendToOppositePotMIR c) = encCBOR c

-- | Move instantaneous rewards certificate
data MIRCert c = MIRCert
  { mirPot :: MIRPot
  , mirRewards :: MIRTarget c
  }
  deriving (Show, Generic, Eq, NFData)

instance NoThunks (MIRCert c)

instance Crypto c => DecCBOR (MIRCert c) where
  decCBOR =
    decodeRecordNamed "MIRCert" (const 2) (MIRCert <$> decCBOR <*> decCBOR)

instance Crypto c => EncCBOR (MIRCert c) where
  encCBOR (MIRCert pot targets) =
    encodeListLen 2
      <> encCBOR pot
      <> encCBOR targets

-- | A heavyweight certificate.
data ShelleyDCert era
  = ShelleyDCertDeleg !(DelegCert (EraCrypto era))
  | ShelleyDCertPool !(PoolCert (EraCrypto era))
  | ShelleyDCertGenesis !(ConstitutionalDelegCert (EraCrypto era))
  | ShelleyDCertMir !(MIRCert (EraCrypto era))
  deriving (Show, Generic, Eq, NFData)

instance NoThunks (ShelleyDCert era)

-- CBOR

instance Era era => EncCBOR (ShelleyDCert era) where
  encCBOR = \case
    ShelleyDCertDeleg (RegKey cred) ->
      encodeListLen 2
        <> encCBOR (0 :: Word8)
        <> encCBOR cred
    ShelleyDCertDeleg (DeRegKey cred) ->
      encodeListLen 2
        <> encCBOR (1 :: Word8)
        <> encCBOR cred
    ShelleyDCertDeleg (Delegate (Delegation cred poolkh)) ->
      encodeListLen 3
        <> encCBOR (2 :: Word8)
        <> encCBOR cred
        <> encCBOR poolkh
    ShelleyDCertPool (RegPool poolParams) ->
      encodeListLen (1 + listLen poolParams)
        <> encCBOR (3 :: Word8)
        <> encCBORGroup poolParams
    ShelleyDCertPool (RetirePool vk epoch) ->
      encodeListLen 3
        <> encCBOR (4 :: Word8)
        <> encCBOR vk
        <> encCBOR epoch
    ShelleyDCertGenesis (ConstitutionalDelegCert gk kh vrf) ->
      encodeListLen 4
        <> encCBOR (5 :: Word8)
        <> encCBOR gk
        <> encCBOR kh
        <> encCBOR vrf
    ShelleyDCertMir mir ->
      encodeListLen 2
        <> encCBOR (6 :: Word8)
        <> encCBOR mir

instance Era era => ToCBOR (ShelleyDCert era) where
  toCBOR = toEraCBOR @era

instance Era era => FromCBOR (ShelleyDCert era) where
  fromCBOR = fromEraCBOR @era

instance Era era => DecCBOR (ShelleyDCert era) where
  decCBOR = decodeRecordSum "DCert crypto" $
    \case
      0 -> do
        x <- decCBOR
        pure (2, ShelleyDCertDeleg . RegKey $ x)
      1 -> do
        x <- decCBOR
        pure (2, ShelleyDCertDeleg . DeRegKey $ x)
      2 -> do
        a <- decCBOR
        b <- decCBOR
        pure (3, ShelleyDCertDeleg $ Delegate (Delegation a b))
      3 -> do
        group <- decCBORGroup
        pure (fromIntegral (1 + listLenInt group), ShelleyDCertPool (RegPool group))
      4 -> do
        a <- decCBOR
        b <- decCBOR
        pure (3, ShelleyDCertPool $ RetirePool a (EpochNo b))
      5 -> do
        a <- decCBOR
        b <- decCBOR
        c <- decCBOR
        pure (4, ShelleyDCertGenesis $ ConstitutionalDelegCert a b c)
      6 -> do
        x <- decCBOR
        pure (2, ShelleyDCertMir x)
      k -> invalidKey k

-- | Check for 'RegKey' constructor
isRegKey :: ShelleyDCert c -> Bool
isRegKey (ShelleyDCertDeleg (RegKey _)) = True
isRegKey _ = False

-- | Check for 'DeRegKey' constructor
isDeRegKey :: ShelleyDCert c -> Bool
isDeRegKey (ShelleyDCertDeleg (DeRegKey _)) = True
isDeRegKey _ = False

-- | Check for 'Delegation' constructor
isDelegation :: ShelleyDCert c -> Bool
isDelegation (ShelleyDCertDeleg (Delegate _)) = True
isDelegation _ = False

-- | Check for 'GenesisDelegate' constructor
isGenesisDelegation :: ShelleyDCert c -> Bool
isGenesisDelegation (ShelleyDCertGenesis ConstitutionalDelegCert {}) = True
isGenesisDelegation _ = False

-- | Check for 'RegPool' constructor
isRegPool :: ShelleyDCert c -> Bool
isRegPool (ShelleyDCertPool (RegPool _)) = True
isRegPool _ = False

-- | Check for 'RetirePool' constructor
isRetirePool :: ShelleyDCert c -> Bool
isRetirePool (ShelleyDCertPool (RetirePool _ _)) = True
isRetirePool _ = False

isInstantaneousRewards :: ShelleyDCert c -> Bool
isInstantaneousRewards (ShelleyDCertMir _) = True
isInstantaneousRewards _ = False

isReservesMIRCert :: ShelleyDCert c -> Bool
isReservesMIRCert (ShelleyDCertMir (MIRCert ReservesMIR _)) = True
isReservesMIRCert _ = False

isTreasuryMIRCert :: ShelleyDCert c -> Bool
isTreasuryMIRCert (ShelleyDCertMir (MIRCert TreasuryMIR _)) = True
isTreasuryMIRCert _ = False

-- | Returns True for delegation certificates that require at least
-- one witness, and False otherwise. It is mainly used to ensure
-- that calling a variant of 'cwitness' is safe.
requiresVKeyWitness :: ShelleyDCert c -> Bool
requiresVKeyWitness (ShelleyDCertMir (MIRCert _ _)) = False
requiresVKeyWitness (ShelleyDCertDeleg (RegKey _)) = False
requiresVKeyWitness _ = True
