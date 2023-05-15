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
-- Due to Delegation usage.
-- TODO: remove when Delegation is gone
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- Due to deprecated requiresVKeyWitness.
-- TODO: remove when requiresVKeyWitness is gone:
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Shelley.TxCert (
  ShelleyEraTxCert (..),
  pattern TxCertMir,
  pattern ShelleyTxCertDeleg,
  ShelleyDelegCert (.., RegKey, DeRegKey, Delegate),
  getVKeyWitnessShelleyTxCert,
  getScriptWitnessShelleyTxCert,
  delegCWitness,
  ShelleyTxCert (..),
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
  shelleyTxCertDelegDecoder,
  commonTxCertDecoder,
  encodeShelleyDelegCert,
  encodePoolCert,
  encodeConstitutionalCert,

  -- * Re-exports
  EraTxCert (..),
  pattern TxCertPool,
  pattern TxCertGenesis,
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
import Cardano.Ledger.Credential (Credential (..), StakeCredential, credKeyHashWitness, credScriptHash)
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Control.DeepSeq (NFData (..), rwhnf)
import Data.Map.Strict (Map)
import Data.Maybe (isJust, isNothing)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

instance Crypto c => EraTxCert (ShelleyEra c) where
  {-# SPECIALIZE instance EraTxCert (ShelleyEra StandardCrypto) #-}

  type TxCert (ShelleyEra c) = ShelleyTxCert (ShelleyEra c)

  getVKeyWitnessTxCert = getVKeyWitnessShelleyTxCert

  getScriptWitnessTxCert = getScriptWitnessShelleyTxCert

  mkTxCertPool = ShelleyTxCertPool

  getTxCertPool (ShelleyTxCertPool c) = Just c
  getTxCertPool _ = Nothing

  mkTxCertGenesis = ShelleyTxCertGenesis

  getTxCertGenesis (ShelleyTxCertGenesis c) = Just c
  getTxCertGenesis _ = Nothing

class EraTxCert era => ShelleyEraTxCert era where
  mkTxCertMir :: ProtVerAtMost era 8 => MIRCert (EraCrypto era) -> TxCert era
  getTxCertMir :: TxCert era -> Maybe (MIRCert (EraCrypto era))

  mkShelleyTxCertDeleg :: ShelleyDelegCert (EraCrypto era) -> TxCert era
  getShelleyTxCertDeleg :: TxCert era -> Maybe (ShelleyDelegCert (EraCrypto era))

instance Crypto c => ShelleyEraTxCert (ShelleyEra c) where
  {-# SPECIALIZE instance ShelleyEraTxCert (ShelleyEra StandardCrypto) #-}

  mkShelleyTxCertDeleg = ShelleyTxCertDelegCert

  getShelleyTxCertDeleg (ShelleyTxCertDelegCert c) = Just c
  getShelleyTxCertDeleg _ = Nothing

  mkTxCertMir = ShelleyTxCertMir

  getTxCertMir (ShelleyTxCertMir c) = Just c
  getTxCertMir _ = Nothing

pattern ShelleyTxCertDeleg :: ShelleyEraTxCert era => ShelleyDelegCert (EraCrypto era) -> TxCert era
pattern ShelleyTxCertDeleg d <- (getShelleyTxCertDeleg -> Just d)
  where
    ShelleyTxCertDeleg d = mkShelleyTxCertDeleg d

pattern TxCertMir :: (ShelleyEraTxCert era, ProtVerAtMost era 8) => MIRCert (EraCrypto era) -> TxCert era
pattern TxCertMir d <- (getTxCertMir -> Just d)
  where
    TxCertMir d = mkTxCertMir d

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
data ShelleyTxCert era
  = ShelleyTxCertDelegCert !(ShelleyDelegCert (EraCrypto era))
  | ShelleyTxCertPool !(PoolCert (EraCrypto era))
  | ShelleyTxCertGenesis !(ConstitutionalDelegCert (EraCrypto era))
  | ShelleyTxCertMir !(MIRCert (EraCrypto era))
  deriving (Show, Generic, Eq, NFData)

instance NoThunks (ShelleyTxCert era)

-- CBOR

instance Era era => EncCBOR (ShelleyTxCert era) where
  encCBOR = \case
    ShelleyTxCertDelegCert delegCert -> encodeShelleyDelegCert delegCert
    ShelleyTxCertPool poolCert -> encodePoolCert poolCert
    ShelleyTxCertGenesis constCert -> encodeConstitutionalCert constCert
    ShelleyTxCertMir mir ->
      encodeListLen 2 <> encodeWord8 6 <> encCBOR mir

encodeShelleyDelegCert :: Crypto c => ShelleyDelegCert c -> Encoding
encodeShelleyDelegCert = \case
  ShelleyRegCert cred ->
    encodeListLen 2 <> encodeWord8 0 <> encCBOR cred
  ShelleyUnRegCert cred ->
    encodeListLen 2 <> encodeWord8 1 <> encCBOR cred
  ShelleyDelegCert cred poolId ->
    encodeListLen 3 <> encodeWord8 2 <> encCBOR cred <> encCBOR poolId

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

instance Era era => ToCBOR (ShelleyTxCert era) where
  toCBOR = toEraCBOR @era

instance
  ( ShelleyEraTxCert era
  , TxCert era ~ ShelleyTxCert era
  ) =>
  FromCBOR (ShelleyTxCert era)
  where
  fromCBOR = fromEraCBOR @era

instance
  ( ShelleyEraTxCert era
  , TxCert era ~ ShelleyTxCert era
  ) =>
  DecCBOR (ShelleyTxCert era)
  where
  decCBOR = decodeRecordSum "ShelleyTxCert" $ \case
    t
      | 0 <= t && t < 3 -> shelleyTxCertDelegDecoder t
      | 3 <= t && t < 6 -> commonTxCertDecoder t
    6 -> do
      x <- decCBOR
      pure (2, ShelleyTxCertMir x)
    x -> invalidKey x
  {-# INLINE decCBOR #-}

shelleyTxCertDelegDecoder ::
  ShelleyEraTxCert era =>
  Word ->
  Decoder s (Int, TxCert era)
shelleyTxCertDelegDecoder = \case
  0 -> do
    cred <- decCBOR
    pure (2, ShelleyTxCertDeleg $ RegKey cred)
  1 -> do
    cred <- decCBOR
    pure (2, ShelleyTxCertDeleg $ DeRegKey cred)
  2 -> do
    cred <- decCBOR
    stakePool <- decCBOR
    pure (3, ShelleyTxCertDeleg $ ShelleyDelegCert cred stakePool)
  k -> invalidKey k
{-# INLINE shelleyTxCertDelegDecoder #-}

commonTxCertDecoder :: EraTxCert era => Word -> Decoder s (Int, TxCert era)
commonTxCertDecoder = \case
  3 -> do
    group <- decCBORGroup
    pure (1 + listLenInt group, TxCertPool (RegPool group))
  4 -> do
    a <- decCBOR
    b <- decCBOR
    pure (3, TxCertPool $ RetirePool a b)
  5 -> do
    a <- decCBOR
    b <- decCBOR
    c <- decCBOR
    pure (4, TxCertGenesis $ ConstitutionalDelegCert a b c)
  k -> invalidKey k
{-# INLINE commonTxCertDecoder #-}

data ShelleyDelegCert c
  = -- | A stake credential registration certificate.
    ShelleyRegCert !(StakeCredential c)
  | -- | A stake credential deregistration certificate.
    ShelleyUnRegCert !(StakeCredential c)
  | -- | A stake delegation certificate.
    ShelleyDelegCert !(StakeCredential c) !(KeyHash 'StakePool c)
  deriving (Show, Generic, Eq)

pattern RegKey :: StakeCredential c -> ShelleyDelegCert c
pattern RegKey cred = ShelleyRegCert cred
{-# DEPRECATED RegKey "In favor of `ShelleyRegCert`" #-}

pattern DeRegKey :: StakeCredential c -> ShelleyDelegCert c
pattern DeRegKey cred = ShelleyUnRegCert cred
{-# DEPRECATED DeRegKey "In favor of `ShelleyUnRegCert`" #-}

pattern Delegate :: Delegation c -> ShelleyDelegCert c
pattern Delegate delegation <- (mkDelegation -> Just delegation)
  where
    Delegate (Delegation cred poolId) = ShelleyDelegCert cred poolId
{-# DEPRECATED Delegate "In favor of `ShelleyDelegCert`" #-}

{-# COMPLETE RegKey, DeRegKey, Delegate #-}

mkDelegation :: ShelleyDelegCert c -> Maybe (Delegation c)
mkDelegation (ShelleyDelegCert cred poolId) = Just (Delegation cred poolId)
mkDelegation _ = Nothing

instance NoThunks (ShelleyDelegCert c)

instance NFData (ShelleyDelegCert c) where
  rnf = rwhnf

-- | Determine the certificate author
delegCWitness :: ShelleyDelegCert c -> Credential 'Staking c
delegCWitness (ShelleyRegCert _) = error "no witness in key registration certificate"
delegCWitness (ShelleyUnRegCert cred) = cred
delegCWitness (ShelleyDelegCert cred _) = cred
{-# DEPRECATED delegCWitness "This was a partial function, logic rewritten in a safer way" #-}

-- | Check for 'ShelleyRegCert' constructor
isRegKey :: (ShelleyEraTxCert era) => TxCert era -> Bool
isRegKey (ShelleyTxCertDeleg (ShelleyRegCert _)) = True
isRegKey _ = False

-- | Check for 'ShelleyUnRegCert' constructor
isDeRegKey :: (ShelleyEraTxCert era) => TxCert era -> Bool
isDeRegKey (ShelleyTxCertDeleg (ShelleyUnRegCert _)) = True
isDeRegKey _ = False

-- | Check for 'ShelleyDelegCert' constructor
isDelegation :: (ShelleyEraTxCert era) => TxCert era -> Bool
isDelegation (ShelleyTxCertDeleg (ShelleyDelegCert _ _)) = True
isDelegation _ = False

-- | Check for 'GenesisDelegate' constructor
isGenesisDelegation :: EraTxCert era => TxCert era -> Bool
isGenesisDelegation = isJust . getTxCertGenesis

-- | Check for 'RegPool' constructor
isRegPool :: EraTxCert era => TxCert era -> Bool
isRegPool (TxCertPool (RegPool _)) = True
isRegPool _ = False

-- | Check for 'RetirePool' constructor
isRetirePool :: EraTxCert era => TxCert era -> Bool
isRetirePool (TxCertPool (RetirePool _ _)) = True
isRetirePool _ = False

isInstantaneousRewards :: ShelleyEraTxCert era => TxCert era -> Bool
isInstantaneousRewards = isJust . getTxCertMir

isReservesMIRCert :: ShelleyEraTxCert era => TxCert era -> Bool
isReservesMIRCert x = case getTxCertMir x of
  Just (MIRCert ReservesMIR _) -> True
  _ -> False

isTreasuryMIRCert :: ShelleyEraTxCert era => TxCert era -> Bool
isTreasuryMIRCert x = case getTxCertMir x of
  Just (MIRCert TreasuryMIR _) -> True
  _ -> False

-- | Returns True for delegation certificates that require at least
-- one witness, and False otherwise. It is mainly used to ensure
-- that calling a variant of 'cwitness' is safe.
--
-- Note: This will not compile for Conway, because it is incorrect for Conway, use
-- `getVKeyWitnessTxCert` instead.
requiresVKeyWitness :: (ShelleyEraTxCert era, ProtVerAtMost era 8) => TxCert era -> Bool
requiresVKeyWitness (ShelleyTxCertDeleg (ShelleyRegCert _)) = False
requiresVKeyWitness x = isNothing $ getTxCertMir x
{-# DEPRECATED requiresVKeyWitness "In favor of `getVKeyWitnessTxCert`" #-}

getScriptWitnessShelleyTxCert ::
  ShelleyTxCert era ->
  Maybe (ScriptHash (EraCrypto era))
getScriptWitnessShelleyTxCert = \case
  ShelleyTxCertDelegCert delegCert ->
    case delegCert of
      ShelleyRegCert _ -> Nothing
      ShelleyUnRegCert cred -> credScriptHash cred
      ShelleyDelegCert cred _ -> credScriptHash cred
  _ -> Nothing

getVKeyWitnessShelleyTxCert :: ShelleyTxCert era -> Maybe (KeyHash 'Witness (EraCrypto era))
getVKeyWitnessShelleyTxCert = \case
  ShelleyTxCertDelegCert delegCert ->
    case delegCert of
      -- Registration certificates do not require a witness
      ShelleyRegCert _ -> Nothing
      ShelleyUnRegCert cred -> credKeyHashWitness cred
      ShelleyDelegCert cred _ -> credKeyHashWitness cred
  ShelleyTxCertPool poolCert -> Just $ poolCertKeyHashWitness poolCert
  ShelleyTxCertGenesis genesisCert -> Just $ genesisKeyHashWitness genesisCert
  ShelleyTxCertMir {} -> Nothing
