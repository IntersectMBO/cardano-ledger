{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.TxCert (
  ConwayTxCert (..),
  ConwayDelegCert (..),
  ConwayCommitteeCert (..),
  Delegatee (..),
  ConwayEraTxCert (..),
  fromShelleyDelegCert,
  toShelleyDelegCert,
  getScriptWitnessConwayTxCert,
  getVKeyWitnessConwayTxCert,
  pattern RegDepositTxCert,
  pattern UnRegDepositTxCert,
  pattern DelegTxCert,
  pattern RegDepositDelegTxCert,
  pattern RegCommitteeHotTxCert,
  pattern UnRegCommitteeHotTxCert,
)
where

import Cardano.Ledger.BaseTypes (StrictMaybe (..), invalidKey)
import Cardano.Ledger.Binary (
  DecCBOR (..),
  Decoder,
  EncCBOR (..),
  Encoding,
  FromCBOR (..),
  ToCBOR (..),
  decodeRecordSum,
  encodeListLen,
  encodeWord8,
  toPlainDecoder,
  toPlainEncoding,
 )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Core (
  Era (EraCrypto),
  EraTxCert (..),
  PoolCert,
  ScriptHash,
  Value,
  eraProtVerLow,
  notSupportedInThisEra,
  poolCertKeyHashWitness,
 )
import Cardano.Ledger.Credential (Credential, StakeCredential, credKeyHashWitness, credScriptHash)
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (KeyHash, KeyRole (..), asWitness)
import Cardano.Ledger.Shelley.TxCert (
  PoolCert (..),
  ShelleyDelegCert (..),
  ShelleyEraTxCert (..),
  encodePoolCert,
  encodeShelleyDelegCert,
  poolTxCertDecoder,
  shelleyTxCertDelegDecoder,
 )
import Cardano.Ledger.Val (Val (..))
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

instance Crypto c => EraTxCert (ConwayEra c) where
  type TxCert (ConwayEra c) = ConwayTxCert (ConwayEra c)

  getVKeyWitnessTxCert = getVKeyWitnessConwayTxCert

  getScriptWitnessTxCert = getScriptWitnessConwayTxCert

  mkRegPoolTxCert = ConwayTxCertPool . RegPool

  getRegPoolTxCert (ConwayTxCertPool (RegPool poolParams)) = Just poolParams
  getRegPoolTxCert _ = Nothing

  mkRetirePoolTxCert poolId epochNo = ConwayTxCertPool $ RetirePool poolId epochNo

  getRetirePoolTxCert (ConwayTxCertPool (RetirePool poolId epochNo)) = Just (poolId, epochNo)
  getRetirePoolTxCert _ = Nothing

instance Crypto c => ShelleyEraTxCert (ConwayEra c) where
  mkRegTxCert c = ConwayTxCertDeleg $ ConwayRegCert c SNothing

  getRegTxCert (ConwayTxCertDeleg (ConwayRegCert c _)) = Just c
  getRegTxCert _ = Nothing

  mkUnRegTxCert c = ConwayTxCertDeleg $ ConwayUnRegCert c SNothing

  getUnRegTxCert (ConwayTxCertDeleg (ConwayUnRegCert c _)) = Just c
  getUnRegTxCert _ = Nothing

  mkDelegStakeTxCert c kh = ConwayTxCertDeleg $ ConwayDelegCert c (DelegStake kh)

  getDelegStakeTxCert (ConwayTxCertDeleg (ConwayDelegCert c (DelegStake kh))) = Just (c, kh)
  getDelegStakeTxCert _ = Nothing

  mkGenesisDelegTxCert = notSupportedInThisEra
  getGenesisDelegTxCert _ = Nothing

  mkMirTxCert = notSupportedInThisEra
  getMirTxCert = const Nothing

class ShelleyEraTxCert era => ConwayEraTxCert era where
  mkRegDepositTxCert :: StakeCredential (EraCrypto era) -> Coin -> TxCert era
  getRegDepositTxCert :: TxCert era -> Maybe (StakeCredential (EraCrypto era), Coin)

  mkUnRegDepositTxCert :: StakeCredential (EraCrypto era) -> Coin -> TxCert era
  getUnRegDepositTxCert :: TxCert era -> Maybe (StakeCredential (EraCrypto era), Coin)

  mkDelegTxCert ::
    StakeCredential (EraCrypto era) -> Delegatee (EraCrypto era) -> TxCert era
  getDelegTxCert ::
    TxCert era -> Maybe (StakeCredential (EraCrypto era), Delegatee (EraCrypto era))

  mkRegDepositDelegTxCert ::
    StakeCredential (EraCrypto era) -> Delegatee (EraCrypto era) -> Coin -> TxCert era
  getRegDepositDelegTxCert ::
    TxCert era -> Maybe (StakeCredential (EraCrypto era), Delegatee (EraCrypto era), Coin)

  mkRegCommitteeHotTxCert ::
    KeyHash 'CommitteeColdKey (EraCrypto era) -> KeyHash 'CommitteeHotKey (EraCrypto era) -> TxCert era
  getRegCommitteeHotTxCert ::
    TxCert era -> Maybe (KeyHash 'CommitteeColdKey (EraCrypto era), KeyHash 'CommitteeHotKey (EraCrypto era))

  mkUnRegCommitteeHotTxCert :: KeyHash 'CommitteeColdKey (EraCrypto era) -> TxCert era
  getUnRegCommitteeHotTxCert :: TxCert era -> Maybe (KeyHash 'CommitteeColdKey (EraCrypto era))

instance Crypto c => ConwayEraTxCert (ConwayEra c) where
  mkRegDepositTxCert cred c = ConwayTxCertDeleg $ ConwayRegCert cred $ SJust c

  getRegDepositTxCert (ConwayTxCertDeleg (ConwayRegCert cred (SJust c))) = Just (cred, c)
  getRegDepositTxCert _ = Nothing

  mkUnRegDepositTxCert cred c = ConwayTxCertDeleg $ ConwayUnRegCert cred (SJust c)
  getUnRegDepositTxCert (ConwayTxCertDeleg (ConwayUnRegCert cred (SJust c))) = Just (cred, c)
  getUnRegDepositTxCert _ = Nothing

  mkDelegTxCert cred d = ConwayTxCertDeleg $ ConwayDelegCert cred d
  getDelegTxCert (ConwayTxCertDeleg (ConwayDelegCert cred d)) = Just (cred, d)
  getDelegTxCert _ = Nothing

  mkRegDepositDelegTxCert cred d c = ConwayTxCertDeleg $ ConwayRegDelegCert cred d c
  getRegDepositDelegTxCert (ConwayTxCertDeleg (ConwayRegDelegCert cred d c)) = Just (cred, d, c)
  getRegDepositDelegTxCert _ = Nothing

  mkRegCommitteeHotTxCert ck hk = ConwayTxCertCommittee $ ConwayRegCommitteeHotKey ck hk
  getRegCommitteeHotTxCert (ConwayTxCertCommittee (ConwayRegCommitteeHotKey ck hk)) = Just (ck, hk)
  getRegCommitteeHotTxCert _ = Nothing

  mkUnRegCommitteeHotTxCert = ConwayTxCertCommittee . ConwayUnRegCommitteeHotKey
  getUnRegCommitteeHotTxCert (ConwayTxCertCommittee (ConwayUnRegCommitteeHotKey ck)) = Just ck
  getUnRegCommitteeHotTxCert _ = Nothing

pattern RegDepositTxCert ::
  ConwayEraTxCert era =>
  StakeCredential (EraCrypto era) ->
  Coin ->
  TxCert era
pattern RegDepositTxCert cred c <- (getRegDepositTxCert -> Just (cred, c))
  where
    RegDepositTxCert cred c = mkRegDepositTxCert cred c

pattern UnRegDepositTxCert ::
  ConwayEraTxCert era =>
  StakeCredential (EraCrypto era) ->
  Coin ->
  TxCert era
pattern UnRegDepositTxCert cred c <- (getUnRegDepositTxCert -> Just (cred, c))
  where
    UnRegDepositTxCert cred c = mkUnRegDepositTxCert cred c

pattern DelegTxCert ::
  ConwayEraTxCert era =>
  StakeCredential (EraCrypto era) ->
  Delegatee (EraCrypto era) ->
  TxCert era
pattern DelegTxCert cred d <- (getDelegTxCert -> Just (cred, d))
  where
    DelegTxCert cred d = mkDelegTxCert cred d

pattern RegDepositDelegTxCert ::
  ConwayEraTxCert era =>
  StakeCredential (EraCrypto era) ->
  Delegatee (EraCrypto era) ->
  Coin ->
  TxCert era
pattern RegDepositDelegTxCert cred d c <- (getRegDepositDelegTxCert -> Just (cred, d, c))
  where
    RegDepositDelegTxCert cred d c = mkRegDepositDelegTxCert cred d c

pattern RegCommitteeHotTxCert ::
  ConwayEraTxCert era =>
  KeyHash 'CommitteeColdKey (EraCrypto era) ->
  KeyHash 'CommitteeHotKey (EraCrypto era) ->
  TxCert era
pattern RegCommitteeHotTxCert ck hk <- (getRegCommitteeHotTxCert -> Just (ck, hk))
  where
    RegCommitteeHotTxCert ck hk = mkRegCommitteeHotTxCert ck hk

pattern UnRegCommitteeHotTxCert ::
  ConwayEraTxCert era =>
  KeyHash 'CommitteeColdKey (EraCrypto era) ->
  TxCert era
pattern UnRegCommitteeHotTxCert ck <- (getUnRegCommitteeHotTxCert -> Just ck)
  where
    UnRegCommitteeHotTxCert ck = mkUnRegCommitteeHotTxCert ck

-- | First type argument is the deposit
data Delegatee c
  = DelegStake !(KeyHash 'StakePool c)
  | DelegVote !(Credential 'Voting c)
  | DelegStakeVote !(KeyHash 'StakePool c) !(Credential 'Voting c)
  deriving (Show, Generic, Eq)

instance NFData (Delegatee c)

instance NoThunks (Delegatee c)

-- | Certificates for registration and delegation of stake to Pools and DReps. Comparing
-- to previous eras, there is now ability to:
--
-- * Register and delegate with a single certificate: `ConwayRegDelegCert`
--
-- * Ability to delegate to DReps with `DelegVote` and `DelegStakeVote`
--
-- * Ability to specify the deposit amount. Deposits during registration and
--   unregistration in Conway are optional, which will change in the future era. They are
--   optional only for the smooth transition from Babbage to Conway. Validity of deposits
--   is checked by the @CERT@ rule.
data ConwayDelegCert c
  = -- | Register staking credential. Deposit, when present, must match the expected deposit
    -- amount specified by `ppKeyDepositL` in the protocol parameters.
    ConwayRegCert !(StakeCredential c) !(StrictMaybe Coin)
  | -- | De-Register the staking credential. Deposit, if present, must match the amount
    -- that was left as a deposit upon stake credential registration.
    ConwayUnRegCert !(StakeCredential c) !(StrictMaybe Coin)
  | -- | Redelegate to another delegatee. Staking credential must already be registered.
    ConwayDelegCert !(StakeCredential c) !(Delegatee c)
  | -- | This is a new type of certificate, which allows to register staking credential
    -- and delegate within a single certificate. Deposit is required and must match the
    -- expected deposit amount specified by `ppKeyDepositL` in the protocol parameters.
    ConwayRegDelegCert !(StakeCredential c) !(Delegatee c) !Coin
  deriving (Show, Generic, Eq)

instance NFData (ConwayDelegCert c)

instance NoThunks (ConwayDelegCert c)

data ConwayCommitteeCert c
  = ConwayRegCommitteeHotKey !(KeyHash 'CommitteeColdKey c) !(KeyHash 'CommitteeHotKey c)
  | ConwayUnRegCommitteeHotKey !(KeyHash 'CommitteeColdKey c)
  deriving (Show, Generic, Eq)

instance NFData (ConwayCommitteeCert c)

instance NoThunks (ConwayCommitteeCert c)

committeeKeyHashWitness :: ConwayCommitteeCert c -> KeyHash 'Witness c
committeeKeyHashWitness = \case
  ConwayRegCommitteeHotKey coldKeyHash _ -> asWitness coldKeyHash
  ConwayUnRegCommitteeHotKey coldKeyHash -> asWitness coldKeyHash

data ConwayTxCert era
  = ConwayTxCertDeleg !(ConwayDelegCert (EraCrypto era))
  | ConwayTxCertPool !(PoolCert (EraCrypto era))
  | ConwayTxCertCommittee !(ConwayCommitteeCert (EraCrypto era))
  deriving (Show, Generic, Eq)

instance NFData (ConwayTxCert c)

instance NoThunks (ConwayTxCert c)

instance
  ( ShelleyEraTxCert era
  , TxCert era ~ ConwayTxCert era
  ) =>
  FromCBOR (ConwayTxCert era)
  where
  fromCBOR = toPlainDecoder (eraProtVerLow @era) decCBOR

instance
  ( ConwayEraTxCert era
  , TxCert era ~ ConwayTxCert era
  ) =>
  DecCBOR (ConwayTxCert era)
  where
  decCBOR = decodeRecordSum "ConwayTxCert" $ \case
    t
      | 0 <= t && t < 3 -> shelleyTxCertDelegDecoder t
      | 3 <= t && t < 5 -> poolTxCertDecoder t
      | t == 5 -> fail "Genesis delegation certificates are no longer supported"
      | t == 6 -> fail "MIR certificates are no longer supported"
      | 7 <= t -> conwayTxCertDelegDecoder t
    t -> invalidKey t

conwayTxCertDelegDecoder :: ConwayEraTxCert era => Word -> Decoder s (Int, TxCert era)
conwayTxCertDelegDecoder = \case
  7 -> do
    cred <- decCBOR
    deposit <- decCBOR
    pure (3, RegDepositTxCert cred deposit)
  8 -> do
    cred <- decCBOR
    deposit <- decCBOR
    pure (3, UnRegDepositTxCert cred deposit)
  9 -> delegCertDecoder 3 (DelegVote <$> decCBOR)
  10 -> delegCertDecoder 4 (DelegStakeVote <$> decCBOR <*> decCBOR)
  11 -> regDelegCertDecoder 4 (DelegStake <$> decCBOR)
  12 -> regDelegCertDecoder 4 (DelegVote <$> decCBOR)
  13 -> regDelegCertDecoder 5 (DelegStakeVote <$> decCBOR <*> decCBOR)
  14 -> do
    cred <- decCBOR
    key <- decCBOR
    pure (3, RegCommitteeHotTxCert cred key)
  15 -> do
    cred <- decCBOR
    pure (2, UnRegCommitteeHotTxCert cred)
  k -> invalidKey k
  where
    delegCertDecoder n decodeDelegatee = do
      cred <- decCBOR
      delegatee <- decodeDelegatee
      pure (n, DelegTxCert cred delegatee)
    {-# INLINE delegCertDecoder #-}
    regDelegCertDecoder n decodeDelegatee = do
      cred <- decCBOR
      delegatee <- decodeDelegatee
      deposit <- decCBOR
      pure (n, RegDepositDelegTxCert cred delegatee deposit)
    {-# INLINE regDelegCertDecoder #-}
{-# INLINE conwayTxCertDelegDecoder #-}

instance (Era era, Val (Value era)) => ToCBOR (ConwayTxCert era) where
  toCBOR = toPlainEncoding (eraProtVerLow @era) . encCBOR

instance (Era era, Val (Value era)) => EncCBOR (ConwayTxCert era) where
  encCBOR = \case
    ConwayTxCertDeleg delegCert -> encodeConwayDelegCert delegCert
    ConwayTxCertPool poolCert -> encodePoolCert poolCert
    ConwayTxCertCommittee committeeCert -> encodeCommitteeHotKey committeeCert

encodeConwayDelegCert :: Crypto c => ConwayDelegCert c -> Encoding
encodeConwayDelegCert = \case
  -- Shelley backwards compatibility
  ConwayRegCert cred SNothing -> encodeShelleyDelegCert $ ShelleyRegCert cred
  ConwayUnRegCert cred SNothing -> encodeShelleyDelegCert $ ShelleyUnRegCert cred
  ConwayDelegCert cred (DelegStake poolId) -> encodeShelleyDelegCert $ ShelleyDelegCert cred poolId
  -- New in Conway
  ConwayRegCert cred (SJust deposit) ->
    encodeListLen 3
      <> encodeWord8 7
      <> encCBOR cred
      <> encCBOR deposit
  ConwayUnRegCert cred (SJust deposit) ->
    encodeListLen 3
      <> encodeWord8 8
      <> encCBOR cred
      <> encCBOR deposit
  ConwayDelegCert cred (DelegVote drep) ->
    encodeListLen 3
      <> encodeWord8 9
      <> encCBOR cred
      <> encCBOR drep
  ConwayDelegCert cred (DelegStakeVote poolId dRep) ->
    encodeListLen 4
      <> encodeWord8 10
      <> encCBOR cred
      <> encCBOR poolId
      <> encCBOR dRep
  ConwayRegDelegCert cred (DelegStake poolId) deposit ->
    encodeListLen 4
      <> encodeWord8 11
      <> encCBOR cred
      <> encCBOR poolId
      <> encCBOR deposit
  ConwayRegDelegCert cred (DelegVote drep) deposit ->
    encodeListLen 4
      <> encodeWord8 12
      <> encCBOR cred
      <> encCBOR drep
      <> encCBOR deposit
  ConwayRegDelegCert cred (DelegStakeVote poolId dRep) deposit ->
    encodeListLen 5
      <> encodeWord8 13
      <> encCBOR cred
      <> encCBOR poolId
      <> encCBOR dRep
      <> encCBOR deposit

encodeCommitteeHotKey :: Crypto c => ConwayCommitteeCert c -> Encoding
encodeCommitteeHotKey = \case
  ConwayRegCommitteeHotKey cred key ->
    encodeListLen 3
      <> encodeWord8 14
      <> encCBOR cred
      <> encCBOR key
  ConwayUnRegCommitteeHotKey cred ->
    encodeListLen 2
      <> encodeWord8 15
      <> encCBOR cred

fromShelleyDelegCert :: ShelleyDelegCert c -> ConwayDelegCert c
fromShelleyDelegCert = \case
  ShelleyRegCert cred -> ConwayRegCert cred SNothing
  ShelleyUnRegCert cred -> ConwayUnRegCert cred SNothing
  ShelleyDelegCert cred poolId -> ConwayDelegCert cred (DelegStake poolId)

toShelleyDelegCert :: ConwayDelegCert c -> Maybe (ShelleyDelegCert c)
toShelleyDelegCert = \case
  ConwayRegCert cred SNothing -> Just $ ShelleyRegCert cred
  ConwayUnRegCert cred SNothing -> Just $ ShelleyUnRegCert cred
  ConwayDelegCert cred (DelegStake poolId) -> Just $ ShelleyDelegCert cred poolId
  _ -> Nothing

-- For both of the functions `getScriptWitnessConwayTxCert` and
-- `getVKeyWitnessConwayTxCert` we preserve the old behavior of not requiring a witness,
-- but only during the transitional period of Conway era and only for registration
-- cdertificates without a deposit. Future eras will require a witness for registration
-- certificates, because the one without a deposit will be removed.

getScriptWitnessConwayTxCert ::
  ConwayTxCert era ->
  Maybe (ScriptHash (EraCrypto era))
getScriptWitnessConwayTxCert = \case
  ConwayTxCertDeleg delegCert ->
    case delegCert of
      ConwayRegCert _ SNothing -> Nothing
      ConwayRegCert cred (SJust _) -> credScriptHash cred
      ConwayUnRegCert cred _ -> credScriptHash cred
      ConwayDelegCert cred _ -> credScriptHash cred
      ConwayRegDelegCert cred _ _ -> credScriptHash cred
  _ -> Nothing

getVKeyWitnessConwayTxCert :: ConwayTxCert era -> Maybe (KeyHash 'Witness (EraCrypto era))
getVKeyWitnessConwayTxCert = \case
  ConwayTxCertDeleg delegCert ->
    case delegCert of
      ConwayRegCert _ SNothing -> Nothing
      ConwayRegCert cred (SJust _) -> credKeyHashWitness cred
      ConwayUnRegCert cred _ -> credKeyHashWitness cred
      ConwayDelegCert cred _ -> credKeyHashWitness cred
      ConwayRegDelegCert cred _ _ -> credKeyHashWitness cred
  ConwayTxCertPool poolCert -> Just $ poolCertKeyHashWitness poolCert
  ConwayTxCertCommittee committeeCert -> Just $ committeeKeyHashWitness committeeCert
