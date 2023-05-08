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
  ShelleyEraDCert (..),
  pattern DCertMir,
  pattern ShelleyDCertDeleg,
  ShelleyDelegCert (..),
  delegCWitness,
  ShelleyDCert (..),
  MIRCert (..),
  MIRPot (..),
  MIRTarget (..),
  isRegKey,
  isDeRegKey,
  isDelegation,
  isRegPool,
  isRetirePool,
  isGenesisDelegation,
  isInstantaneousRewards,
  isReservesMIRCert,
  isTreasuryMIRCert,
  requiresVKeyWitness,
  -- ** Serialization helpers
  shelleyDCertDelegDecoder,
  commonDCertDecoder,
  encodeShelleyDelegCert,
  encodePoolCert,
  encodeConstitutionalCert,

  -- * Re-exports
  EraDCert (..),
  pattern DCertPool,
  pattern DCertGenesis,
  Delegation (..),
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
  Decoder,
  EncCBOR (..),
  EncCBORGroup (..),
  Encoding,
  FromCBOR (..),
  ToCBOR (..),
  TokenType (TypeMapLen, TypeMapLen64, TypeMapLenIndef),
  decodeRecordNamed,
  decodeRecordSum,
  decodeWord,
  encodeListLen,
  encodeWord8,
  listLenInt,
  peekTokenType,
 )
import Cardano.Ledger.Coin (Coin (..), DeltaCoin)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), StakeCredential)
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Control.DeepSeq (NFData (..), rwhnf)
import Data.Map.Strict (Map)
import Data.Maybe (isJust, isNothing)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

instance Crypto c => EraDCert (ShelleyEra c) where
  {-# SPECIALIZE instance EraDCert (ShelleyEra StandardCrypto) #-}

  type DCert (ShelleyEra c) = ShelleyDCert (ShelleyEra c)

  mkDCertPool = ShelleyDCertPool

  getDCertPool (ShelleyDCertPool c) = Just c
  getDCertPool _ = Nothing

  mkDCertGenesis = ShelleyDCertGenesis

  getDCertGenesis (ShelleyDCertGenesis c) = Just c
  getDCertGenesis _ = Nothing

class EraDCert era => ShelleyEraDCert era where
  mkDCertMir :: ProtVerAtMost era 8 => MIRCert (EraCrypto era) -> DCert era
  getDCertMir :: DCert era -> Maybe (MIRCert (EraCrypto era))

  mkShelleyDCertDeleg :: ShelleyDelegCert (EraCrypto era) -> DCert era
  getShelleyDCertDeleg :: DCert era -> Maybe (ShelleyDelegCert (EraCrypto era))

instance Crypto c => ShelleyEraDCert (ShelleyEra c) where
  {-# SPECIALIZE instance ShelleyEraDCert (ShelleyEra StandardCrypto) #-}

  mkShelleyDCertDeleg = ShelleyDCertDelegCert

  getShelleyDCertDeleg (ShelleyDCertDelegCert c) = Just c
  getShelleyDCertDeleg _ = Nothing

  mkDCertMir = ShelleyDCertMir

  getDCertMir (ShelleyDCertMir c) = Just c
  getDCertMir _ = Nothing

pattern ShelleyDCertDeleg :: ShelleyEraDCert era => ShelleyDelegCert (EraCrypto era) -> DCert era
pattern ShelleyDCertDeleg d <- (getShelleyDCertDeleg -> Just d)
  where
    ShelleyDCertDeleg d = mkShelleyDCertDeleg d

pattern DCertMir :: (ShelleyEraDCert era, ProtVerAtMost era 8) => MIRCert (EraCrypto era) -> DCert era
pattern DCertMir d <- (getDCertMir -> Just d)
  where
    DCertMir d = mkDCertMir d

data MIRPot = ReservesMIR | TreasuryMIR
  deriving (Show, Generic, Eq, NFData, Ord, Enum, Bounded)

deriving instance NoThunks MIRPot

instance EncCBOR MIRPot where
  encCBOR ReservesMIR = encodeWord8 0
  encCBOR TreasuryMIR = encodeWord8 1

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
  = StakeAddressesMIR !(Map (Credential 'Staking c) DeltaCoin)
  | SendToOppositePotMIR !Coin
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
  { mirPot :: !MIRPot
  , mirRewards :: !(MIRTarget c)
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
  = ShelleyDCertDelegCert !(ShelleyDelegCert (EraCrypto era))
  | ShelleyDCertPool !(PoolCert (EraCrypto era))
  | ShelleyDCertGenesis !(ConstitutionalDelegCert (EraCrypto era))
  | ShelleyDCertMir !(MIRCert (EraCrypto era))
  deriving (Show, Generic, Eq, NFData)

instance NoThunks (ShelleyDCert era)

-- CBOR

instance Era era => EncCBOR (ShelleyDCert era) where
  encCBOR = \case
    ShelleyDCertDelegCert delegCert -> encodeShelleyDelegCert delegCert
    ShelleyDCertPool poolCert -> encodePoolCert poolCert
    ShelleyDCertGenesis constCert -> encodeConstitutionalCert constCert
    ShelleyDCertMir mir ->
      encodeListLen 2 <> encodeWord8 6 <> encCBOR mir

encodeShelleyDelegCert :: Crypto c => ShelleyDelegCert c -> Encoding
encodeShelleyDelegCert = \case
  RegKey cred ->
    encodeListLen 2 <> encodeWord8 0 <> encCBOR cred
  DeRegKey cred ->
    encodeListLen 2 <> encodeWord8 1 <> encCBOR cred
  Delegate (Delegation cred poolkh) ->
    encodeListLen 3 <> encodeWord8 2 <> encCBOR cred <> encCBOR poolkh

encodePoolCert :: Crypto c => PoolCert c -> Encoding
encodePoolCert = \case
  RegPool poolParams ->
    encodeListLen (1 + listLen poolParams)
      <> encodeWord8 3
      <> encCBORGroup poolParams
  RetirePool vk epoch ->
    encodeListLen 3
      <> encodeWord8 4
      <> encCBOR vk
      <> encCBOR epoch

encodeConstitutionalCert :: Crypto c => ConstitutionalDelegCert c -> Encoding
encodeConstitutionalCert (ConstitutionalDelegCert gk kh vrf) =
  encodeListLen 4
    <> encodeWord8 5
    <> encCBOR gk
    <> encCBOR kh
    <> encCBOR vrf

instance Era era => ToCBOR (ShelleyDCert era) where
  toCBOR = toEraCBOR @era

instance
  ( ShelleyEraDCert era
  , DCert era ~ ShelleyDCert era
  ) =>
  FromCBOR (ShelleyDCert era)
  where
  fromCBOR = fromEraCBOR @era

instance
  ( ShelleyEraDCert era
  , DCert era ~ ShelleyDCert era
  ) =>
  DecCBOR (ShelleyDCert era)
  where
  decCBOR = decodeRecordSum "ShelleyDCert" $ \case
    t
      | 0 <= t && t < 3 -> shelleyDCertDelegDecoder t
      | 3 <= t && t < 6 -> commonDCertDecoder t
    6 -> do
      x <- decCBOR
      pure (2, ShelleyDCertMir x)
    x -> invalidKey x
  {-# INLINE decCBOR #-}

shelleyDCertDelegDecoder ::
  ShelleyEraDCert era =>
  Word ->
  Decoder s (Int, DCert era)
shelleyDCertDelegDecoder = \case
  0 -> do
    cred <- decCBOR
    pure (2, ShelleyDCertDeleg $ RegKey cred)
  1 -> do
    cred <- decCBOR
    pure (2, ShelleyDCertDeleg $ DeRegKey cred)
  2 -> do
    cred <- decCBOR
    stakePool <- decCBOR
    pure (3, ShelleyDCertDeleg $ Delegate (Delegation cred stakePool))
  k -> invalidKey k
{-# INLINE shelleyDCertDelegDecoder #-}

commonDCertDecoder :: EraDCert era => Word -> Decoder s (Int, DCert era)
commonDCertDecoder = \case
  3 -> do
    group <- decCBORGroup
    pure (1 + listLenInt group, DCertPool (RegPool group))
  4 -> do
    a <- decCBOR
    b <- decCBOR
    pure (3, DCertPool $ RetirePool a b)
  5 -> do
    a <- decCBOR
    b <- decCBOR
    c <- decCBOR
    pure (4, DCertGenesis $ ConstitutionalDelegCert a b c)
  k -> invalidKey k
{-# INLINE commonDCertDecoder #-}

data ShelleyDelegCert c
  = -- | A stake key registration certificate.
    RegKey !(StakeCredential c)
  | -- | A stake key deregistration certificate.
    DeRegKey !(StakeCredential c)
  | -- | A stake delegation certificate.
    Delegate !(Delegation c)
  deriving (Show, Generic, Eq)

instance NoThunks (ShelleyDelegCert c)

instance NFData (ShelleyDelegCert c) where
  rnf = rwhnf

-- | Determine the certificate author
delegCWitness :: ShelleyDelegCert c -> Credential 'Staking c
delegCWitness (RegKey _) = error "no witness in key registration certificate"
delegCWitness (DeRegKey hk) = hk
delegCWitness (Delegate delegation) = dDelegator delegation

-- | Check for 'RegKey' constructor
isRegKey :: (ShelleyEraDCert era) => DCert era -> Bool
isRegKey (ShelleyDCertDeleg (RegKey _)) = True
isRegKey _ = False

-- | Check for 'DeRegKey' constructor
isDeRegKey :: (ShelleyEraDCert era) => DCert era -> Bool
isDeRegKey (ShelleyDCertDeleg (DeRegKey _)) = True
isDeRegKey _ = False

-- | Check for 'Delegation' constructor
isDelegation :: (ShelleyEraDCert era) => DCert era -> Bool
isDelegation (ShelleyDCertDeleg (Delegate _)) = True
isDelegation _ = False

-- | Check for 'GenesisDelegate' constructor
isGenesisDelegation :: EraDCert era => DCert era -> Bool
isGenesisDelegation = isJust . getDCertGenesis

-- | Check for 'RegPool' constructor
isRegPool :: EraDCert era => DCert era -> Bool
isRegPool (DCertPool (RegPool _)) = True
isRegPool _ = False

-- | Check for 'RetirePool' constructor
isRetirePool :: EraDCert era => DCert era -> Bool
isRetirePool (DCertPool (RetirePool _ _)) = True
isRetirePool _ = False

isInstantaneousRewards :: ShelleyEraDCert era => DCert era -> Bool
isInstantaneousRewards = isJust . getDCertMir

isReservesMIRCert :: ShelleyEraDCert era => DCert era -> Bool
isReservesMIRCert x = case getDCertMir x of
  Just (MIRCert ReservesMIR _) -> True
  _ -> False

isTreasuryMIRCert :: ShelleyEraDCert era => DCert era -> Bool
isTreasuryMIRCert x = case getDCertMir x of
  Just (MIRCert TreasuryMIR _) -> True
  _ -> False

-- | Returns True for delegation certificates that require at least
-- one witness, and False otherwise. It is mainly used to ensure
-- that calling a variant of 'cwitness' is safe.
requiresVKeyWitness :: ShelleyEraDCert era => DCert era -> Bool
requiresVKeyWitness (ShelleyDCertDeleg (RegKey _)) = False
requiresVKeyWitness x = isNothing $ getDCertMir x
