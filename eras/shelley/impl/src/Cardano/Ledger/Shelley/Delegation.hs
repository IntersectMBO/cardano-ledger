{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Delegation (
  ShelleyEraDCert(..),
  pattern DCertMir,
  ShelleyDCert (..),
  MIRCert (..),
  MIRPot (..),
  MIRTarget (..),
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

  -- * Re-exports
  EraDCert (..),
  pattern DCertDeleg,
  pattern DCertPool,
  pattern DCertGenesis,
  Delegation (..),
  DelegCert (..),
  delegCWitness,
  PoolCert (..),
  poolCWitness,
  ConstitutionalDelegCert (..),
  genesisCWitness,
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
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Slot (EpochNo (..))
import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import Data.Word (Word8)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

instance Crypto c => EraDCert (ShelleyEra c) where
  {-# SPECIALIZE instance EraDCert (ShelleyEra StandardCrypto) #-}

  type DCert (ShelleyEra c) = ShelleyDCert (ShelleyEra c)

  mkDCertDeleg = ShelleyDCertDeleg

  getDCertDeleg (ShelleyDCertDeleg c) = Just c
  getDCertDeleg _ = Nothing

  mkDCertPool = ShelleyDCertPool

  getDCertPool (ShelleyDCertPool c) = Just c
  getDCertPool _ = Nothing

  mkDCertGenesis = ShelleyDCertGenesis

  getDCertGenesis (ShelleyDCertGenesis c) = Just c
  getDCertGenesis _ = Nothing

class EraDCert era => ShelleyEraDCert era where
  mkDCertMir :: MIRCert (EraCrypto era) -> DCert era
  getDCertMir :: DCert era -> Maybe (MIRCert (EraCrypto era))

instance Crypto c => ShelleyEraDCert (ShelleyEra c) where
  {-# SPECIALIZE instance ShelleyEraDCert (ShelleyEra StandardCrypto) #-}

  mkDCertMir = ShelleyDCertMir

  getDCertMir (ShelleyDCertMir c) = Just c
  getDCertMir _ = Nothing

pattern DCertMir :: ShelleyEraDCert era => MIRCert (EraCrypto era) -> DCert era
pattern DCertMir d <- (getDCertMir -> Just d)
  where
    DCertMir d = mkDCertMir d

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
isRegKey :: EraDCert era => DCert era -> Bool
isRegKey (DCertDeleg (RegKey _)) = True
isRegKey _ = False

-- | Check for 'DeRegKey' constructor
isDeRegKey :: EraDCert era => DCert era -> Bool
isDeRegKey (DCertDeleg (DeRegKey _)) = True
isDeRegKey _ = False

-- | Check for 'Delegation' constructor
isDelegation :: EraDCert era => DCert era -> Bool
isDelegation (DCertDeleg (Delegate _)) = True
isDelegation _ = False

-- | Check for 'GenesisDelegate' constructor
isGenesisDelegation :: EraDCert era => DCert era -> Bool
isGenesisDelegation (DCertGenesis ConstitutionalDelegCert {}) = True
isGenesisDelegation _ = False

-- | Check for 'RegPool' constructor
isRegPool :: EraDCert era => DCert era -> Bool
isRegPool (DCertPool (RegPool _)) = True
isRegPool _ = False

-- | Check for 'RetirePool' constructor
isRetirePool :: EraDCert era => DCert era -> Bool
isRetirePool (DCertPool (RetirePool _ _)) = True
isRetirePool _ = False

isInstantaneousRewards :: ShelleyEraDCert era => DCert era -> Bool
isInstantaneousRewards (DCertMir _) = True
isInstantaneousRewards _ = False

isReservesMIRCert :: ShelleyEraDCert era => DCert era -> Bool
isReservesMIRCert (DCertMir (MIRCert ReservesMIR _)) = True
isReservesMIRCert _ = False

isTreasuryMIRCert :: ShelleyEraDCert era => DCert era -> Bool
isTreasuryMIRCert (DCertMir (MIRCert TreasuryMIR _)) = True
isTreasuryMIRCert _ = False

-- | Returns True for delegation certificates that require at least
-- one witness, and False otherwise. It is mainly used to ensure
-- that calling a variant of 'cwitness' is safe.
requiresVKeyWitness :: ShelleyEraDCert era => DCert era -> Bool
requiresVKeyWitness (DCertMir (MIRCert _ _)) = False
requiresVKeyWitness (DCertDeleg (RegKey _)) = False
requiresVKeyWitness _ = True
