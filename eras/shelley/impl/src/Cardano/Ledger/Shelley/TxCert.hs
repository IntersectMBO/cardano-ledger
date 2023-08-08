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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
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
  pattern MirTxCert,
  pattern GenesisDelegTxCert,
  pattern RegTxCert,
  pattern UnRegTxCert,
  pattern DelegStakeTxCert,
  ShelleyDelegCert (.., RegKey, DeRegKey, Delegate),
  getVKeyWitnessShelleyTxCert,
  getScriptWitnessShelleyTxCert,
  delegCWitness,
  ShelleyTxCert (..),
  upgradeShelleyTxCert,

  -- ** GenesisDelegCert
  GenesisDelegCert (..),
  genesisCWitness,
  genesisKeyHashWitness,

  -- ** MIRCert
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
  poolTxCertDecoder,
  encodeShelleyDelegCert,
  encodePoolCert,
  encodeGenesisDelegCert,

  -- * Re-exports
  EraTxCert (..),
  pattern RegPoolTxCert,
  pattern RetirePoolTxCert,
  Delegation (..),
  PoolCert (..),
  poolCWitness,
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
import Cardano.Ledger.Keys (Hash, KeyHash (..), KeyRole (..), VerKeyVRF, asWitness)
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Control.DeepSeq (NFData (..), rwhnf)
import Data.Map.Strict (Map)
import Data.Maybe (isJust, isNothing)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

instance Crypto c => EraTxCert (ShelleyEra c) where
  {-# SPECIALIZE instance EraTxCert (ShelleyEra StandardCrypto) #-}

  type TxCert (ShelleyEra c) = ShelleyTxCert (ShelleyEra c)

  -- Calling this partial function will result in compilation error, since ByronEra has
  -- no instance for EraTxOut type class.
  upgradeTxCert = error "Byron does not have any TxCerts to upgrade with 'upgradeTxCert'"

  getVKeyWitnessTxCert = getVKeyWitnessShelleyTxCert

  getScriptWitnessTxCert = getScriptWitnessShelleyTxCert

  mkRegPoolTxCert = ShelleyTxCertPool . RegPool

  getRegPoolTxCert (ShelleyTxCertPool (RegPool poolParams)) = Just poolParams
  getRegPoolTxCert _ = Nothing

  mkRetirePoolTxCert poolId epochNo = ShelleyTxCertPool $ RetirePool poolId epochNo

  getRetirePoolTxCert (ShelleyTxCertPool (RetirePool poolId epochNo)) = Just (poolId, epochNo)
  getRetirePoolTxCert _ = Nothing

class EraTxCert era => ShelleyEraTxCert era where
  mkRegTxCert :: StakeCredential (EraCrypto era) -> TxCert era
  getRegTxCert :: TxCert era -> Maybe (StakeCredential (EraCrypto era))

  mkUnRegTxCert :: StakeCredential (EraCrypto era) -> TxCert era
  getUnRegTxCert :: TxCert era -> Maybe (StakeCredential (EraCrypto era))

  mkDelegStakeTxCert ::
    StakeCredential (EraCrypto era) -> KeyHash 'StakePool (EraCrypto era) -> TxCert era
  getDelegStakeTxCert ::
    TxCert era -> Maybe (StakeCredential (EraCrypto era), KeyHash 'StakePool (EraCrypto era))

  mkGenesisDelegTxCert :: ProtVerAtMost era 8 => GenesisDelegCert (EraCrypto era) -> TxCert era
  getGenesisDelegTxCert ::
    ProtVerAtMost era 8 => TxCert era -> Maybe (GenesisDelegCert (EraCrypto era))

  mkMirTxCert :: ProtVerAtMost era 8 => MIRCert (EraCrypto era) -> TxCert era
  getMirTxCert ::
    ProtVerAtMost era 8 => TxCert era -> Maybe (MIRCert (EraCrypto era))

instance Crypto c => ShelleyEraTxCert (ShelleyEra c) where
  {-# SPECIALIZE instance ShelleyEraTxCert (ShelleyEra StandardCrypto) #-}

  mkRegTxCert = ShelleyTxCertDelegCert . ShelleyRegCert

  getRegTxCert (ShelleyTxCertDelegCert (ShelleyRegCert c)) = Just c
  getRegTxCert _ = Nothing

  mkUnRegTxCert = ShelleyTxCertDelegCert . ShelleyUnRegCert

  getUnRegTxCert (ShelleyTxCertDelegCert (ShelleyUnRegCert c)) = Just c
  getUnRegTxCert _ = Nothing

  mkDelegStakeTxCert c kh = ShelleyTxCertDelegCert $ ShelleyDelegCert c kh

  getDelegStakeTxCert (ShelleyTxCertDelegCert (ShelleyDelegCert c kh)) = Just (c, kh)
  getDelegStakeTxCert _ = Nothing

  mkGenesisDelegTxCert = ShelleyTxCertGenesisDeleg

  getGenesisDelegTxCert (ShelleyTxCertGenesisDeleg c) = Just c
  getGenesisDelegTxCert _ = Nothing

  mkMirTxCert = ShelleyTxCertMir

  getMirTxCert (ShelleyTxCertMir c) = Just c
  getMirTxCert _ = Nothing

pattern RegTxCert :: ShelleyEraTxCert era => StakeCredential (EraCrypto era) -> TxCert era
pattern RegTxCert c <- (getRegTxCert -> Just c)
  where
    RegTxCert c = mkRegTxCert c

pattern UnRegTxCert :: ShelleyEraTxCert era => StakeCredential (EraCrypto era) -> TxCert era
pattern UnRegTxCert c <- (getUnRegTxCert -> Just c)
  where
    UnRegTxCert c = mkUnRegTxCert c

pattern DelegStakeTxCert ::
  ShelleyEraTxCert era =>
  StakeCredential (EraCrypto era) ->
  KeyHash 'StakePool (EraCrypto era) ->
  TxCert era
pattern DelegStakeTxCert c kh <- (getDelegStakeTxCert -> Just (c, kh))
  where
    DelegStakeTxCert c kh = mkDelegStakeTxCert c kh

pattern MirTxCert :: (ShelleyEraTxCert era, ProtVerAtMost era 8) => MIRCert (EraCrypto era) -> TxCert era
pattern MirTxCert d <- (getMirTxCert -> Just d)
  where
    MirTxCert d = mkMirTxCert d

pattern GenesisDelegTxCert ::
  (ShelleyEraTxCert era, ProtVerAtMost era 8) =>
  KeyHash 'Genesis (EraCrypto era) ->
  KeyHash 'GenesisDelegate (EraCrypto era) ->
  Hash (EraCrypto era) (VerKeyVRF (EraCrypto era)) ->
  TxCert era
pattern GenesisDelegTxCert genKey genDelegKey vrf <-
  (getGenesisDelegTxCert -> Just (GenesisDelegCert genKey genDelegKey vrf))
  where
    GenesisDelegTxCert genKey genDelegKey vrf =
      mkGenesisDelegTxCert $ GenesisDelegCert genKey genDelegKey vrf

{-# COMPLETE
  RegPoolTxCert
  , RetirePoolTxCert
  , RegTxCert
  , UnRegTxCert
  , DelegStakeTxCert
  , MirTxCert
  , GenesisDelegTxCert
  #-}

-- | Genesis key delegation certificate
data GenesisDelegCert c
  = GenesisDelegCert
      !(KeyHash 'Genesis c)
      !(KeyHash 'GenesisDelegate c)
      !(Hash c (VerKeyVRF c))
  deriving (Show, Generic, Eq)

instance NoThunks (GenesisDelegCert c)

instance NFData (GenesisDelegCert c) where
  rnf = rwhnf

genesisKeyHashWitness :: GenesisDelegCert c -> KeyHash 'Witness c
genesisKeyHashWitness (GenesisDelegCert gk _ _) = asWitness gk

genesisCWitness :: GenesisDelegCert c -> KeyHash 'Genesis c
genesisCWitness (GenesisDelegCert gk _ _) = gk

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
  | ShelleyTxCertGenesisDeleg !(GenesisDelegCert (EraCrypto era))
  | ShelleyTxCertMir !(MIRCert (EraCrypto era))
  deriving (Show, Generic, Eq, NFData)

instance NoThunks (ShelleyTxCert era)

upgradeShelleyTxCert ::
  EraCrypto era1 ~ EraCrypto era2 =>
  ShelleyTxCert era1 ->
  ShelleyTxCert era2
upgradeShelleyTxCert = \case
  ShelleyTxCertDelegCert cert -> ShelleyTxCertDelegCert cert
  ShelleyTxCertPool cert -> ShelleyTxCertPool cert
  ShelleyTxCertGenesisDeleg cert -> ShelleyTxCertGenesisDeleg cert
  ShelleyTxCertMir cert -> ShelleyTxCertMir cert

-- CBOR

instance Era era => EncCBOR (ShelleyTxCert era) where
  encCBOR = \case
    ShelleyTxCertDelegCert delegCert -> encodeShelleyDelegCert delegCert
    ShelleyTxCertPool poolCert -> encodePoolCert poolCert
    ShelleyTxCertGenesisDeleg constCert -> encodeGenesisDelegCert constCert
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

encodeGenesisDelegCert :: Crypto c => GenesisDelegCert c -> Encoding
encodeGenesisDelegCert (GenesisDelegCert gk kh vrf) =
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
      | 3 <= t && t < 5 -> poolTxCertDecoder t
    5 -> do
      gen <- decCBOR
      genDeleg <- decCBOR
      vrf <- decCBOR
      pure (4, ShelleyTxCertGenesisDeleg $ GenesisDelegCert gen genDeleg vrf)
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
    pure (2, RegTxCert cred)
  1 -> do
    cred <- decCBOR
    pure (2, UnRegTxCert cred)
  2 -> do
    cred <- decCBOR
    stakePool <- decCBOR
    pure (3, DelegStakeTxCert cred stakePool)
  k -> invalidKey k
{-# INLINE shelleyTxCertDelegDecoder #-}

poolTxCertDecoder :: EraTxCert era => Word -> Decoder s (Int, TxCert era)
poolTxCertDecoder = \case
  3 -> do
    group <- decCBORGroup
    pure (1 + listLenInt group, RegPoolTxCert group)
  4 -> do
    a <- decCBOR
    b <- decCBOR
    pure (3, RetirePoolTxCert a b)
  k -> invalidKey k
{-# INLINE poolTxCertDecoder #-}

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
isRegKey :: ShelleyEraTxCert era => TxCert era -> Bool
isRegKey (RegTxCert _) = True
isRegKey _ = False

-- | Check for 'ShelleyUnRegCert' constructor
isDeRegKey :: ShelleyEraTxCert era => TxCert era -> Bool
isDeRegKey (UnRegTxCert _) = True
isDeRegKey _ = False

-- | Check for 'ShelleyDelegCert' constructor
isDelegation :: ShelleyEraTxCert era => TxCert era -> Bool
isDelegation (DelegStakeTxCert _ _) = True
isDelegation _ = False

-- | Check for 'GenesisDelegate' constructor
isGenesisDelegation :: (ShelleyEraTxCert era, ProtVerAtMost era 8) => TxCert era -> Bool
isGenesisDelegation = isJust . getGenesisDelegTxCert

-- | Check for 'RegPool' constructor
isRegPool :: EraTxCert era => TxCert era -> Bool
isRegPool (RegPoolTxCert _) = True
isRegPool _ = False

-- | Check for 'RetirePool' constructor
isRetirePool :: EraTxCert era => TxCert era -> Bool
isRetirePool (RetirePoolTxCert _ _) = True
isRetirePool _ = False

isInstantaneousRewards :: (ShelleyEraTxCert era, ProtVerAtMost era 8) => TxCert era -> Bool
isInstantaneousRewards = isJust . getMirTxCert

isReservesMIRCert :: (ShelleyEraTxCert era, ProtVerAtMost era 8) => TxCert era -> Bool
isReservesMIRCert x = case getMirTxCert x of
  Just (MIRCert ReservesMIR _) -> True
  _ -> False

isTreasuryMIRCert :: (ShelleyEraTxCert era, ProtVerAtMost era 8) => TxCert era -> Bool
isTreasuryMIRCert x = case getMirTxCert x of
  Just (MIRCert TreasuryMIR _) -> True
  _ -> False

-- | Returns True for delegation certificates that require at least
-- one witness, and False otherwise. It is mainly used to ensure
-- that calling a variant of 'cwitness' is safe.
--
-- Note: This will not compile for Conway, because it is incorrect for Conway, use
-- `getVKeyWitnessTxCert` instead.
requiresVKeyWitness :: (ShelleyEraTxCert era, ProtVerAtMost era 8) => TxCert era -> Bool
requiresVKeyWitness (RegTxCert _) = False
requiresVKeyWitness x = isNothing $ getMirTxCert x
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
  ShelleyTxCertGenesisDeleg genesisCert -> Just $ genesisKeyHashWitness genesisCert
  ShelleyTxCertMir {} -> Nothing
