{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
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
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Shelley.TxCert (
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

  mkTxCertPool = ConwayTxCertPool
  getRegPoolTxCert (ConwayTxCertPool x) = Just x
  getRegPoolTxCert _ = Nothing

instance Crypto c => ShelleyEraTxCert (ConwayEra c) where
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

  mkTxCertGenesisDeleg = notSupportedInThisEra
  getTxCertGenesisDeleg _ = Nothing

  mkTxCertMir = notSupportedInThisEra
  getTxCertMir = const Nothing

class EraTxCert era => ConwayEraTxCert era where
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
    KeyHash 'Committee (EraCrypto era) -> KeyHash 'CommitteeCold (EraCrypto era) -> TxCert era
  getRegCommitteeHotTxCert ::
    TxCert era -> Maybe (KeyHash 'Committee (EraCrypto era), KeyHash 'CommitteeCold (EraCrypto era))

  mkUnRegCommitteeHotTxCert :: KeyHash 'Committee (EraCrypto era) -> TxCert era
  getUnRegCommitteeHotTxCert :: TxCert era -> Maybe (KeyHash 'Committee (EraCrypto era))

instance Crypto c => ConwayEraTxCert (ConwayEra c) where
  mkConwayTxCertDeleg = ConwayTxCertDeleg
  getConwayTxCertDeleg (ConwayTxCertDeleg x) = Just x
  getConwayTxCertDeleg _ = Nothing

  mkConwayTxCertCommittee = ConwayTxCertCommittee
  getConwayTxCertCommittee (ConwayTxCertCommittee x) = Just x
  getConwayTxCertCommittee _ = Nothing

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

data ConwayDelegCert c
  = -- | Register staking credential. Deposit, when present, must match the expected deposit
    -- amount specified by `ppKeyDepositL` in the protocol parameters.
    ConwayRegCert !(StakeCredential c) !Coin
  | -- | De-Register the staking credential. Deposit, if present, must match the amount
    -- that was left as a deposit upon stake credential registration.
    ConwayUnRegCert !(StakeCredential c) !Coin
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
  = ConwayRegCommitteeHot !(Credential 'Committee c) !(KeyHash 'CommitteeCold c)
  | ConwayUnRegCommitteeHot !(Credential 'Committee c)
  deriving (Show, Generic, Eq)

instance NFData (ConwayCommitteeCert c)

instance NoThunks (ConwayCommitteeCert c)

-- TODO: Implement
committeeScriptHash :: ConwayCommitteeCert c -> Maybe (ScriptHash c)
committeeScriptHash _ = error "Unimplemented"

-- TODO: Implement
committeeKeyHashWitness :: ConwayCommitteeCert c -> Maybe (KeyHash 'Witness c)
committeeKeyHashWitness _ = error "Unimplemented"

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
    deposit <- SJust <$> decCBOR
    pure (3, mkConwayTxCertDeleg $ ConwayRegCert cred deposit)
  8 -> do
    cred <- decCBOR
    deposit <- SJust <$> decCBOR
    pure (3, mkConwayTxCertDeleg $ ConwayUnRegCert cred deposit)
  9 -> delegCertDecoder 3 (DelegVote <$> decCBOR)
  10 -> delegCertDecoder 4 (DelegStakeVote <$> decCBOR <*> decCBOR)
  11 -> regDelegCertDecoder 4 (DelegStake <$> decCBOR)
  12 -> regDelegCertDecoder 4 (DelegVote <$> decCBOR)
  13 -> regDelegCertDecoder 5 (DelegStakeVote <$> decCBOR <*> decCBOR)
  14 -> do
    cred <- decCBOR
    key <- decCBOR
    pure (3, mkConwayTxCertCommittee $ ConwayRegCommitteeHot cred key)
  15 -> do
    cred <- decCBOR
    pure (2, mkConwayTxCertCommittee $ ConwayUnRegCommitteeHot cred)
  k -> invalidKey k
  where
    delegCertDecoder n decodeDelegatee = do
      cred <- decCBOR
      delegatee <- decodeDelegatee
      pure (n, mkConwayTxCertDeleg $ ConwayDelegCert cred delegatee)
    {-# INLINE delegCertDecoder #-}
    regDelegCertDecoder n decodeDelegatee = do
      cred <- decCBOR
      delegatee <- decodeDelegatee
      deposit <- decCBOR
      pure (n, mkConwayTxCertDeleg $ ConwayRegDelegCert cred delegatee deposit)
    {-# INLINE regDelegCertDecoder #-}
{-# INLINE conwayTxCertDelegDecoder #-}

instance (Era era, Val (Value era)) => ToCBOR (ConwayTxCert era) where
  toCBOR = toPlainEncoding (eraProtVerLow @era) . encCBOR

instance (Era era, Val (Value era)) => EncCBOR (ConwayTxCert era) where
  encCBOR = \case
    ConwayTxCertDeleg delegCert -> encodeConwayDelegCert delegCert
    ConwayTxCertPool poolCert -> encodePoolCert poolCert
    ConwayTxCertCommittee committeeCert -> encodeCommitteeCert committeeCert

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

encodeCommitteeCert :: Crypto c => ConwayCommitteeCert c -> Encoding
encodeCommitteeCert = \case
  ConwayRegCommitteeHot cred key ->
    encodeListLen 3
      <> encodeWord8 14
      <> encCBOR cred
      <> encCBOR key
  ConwayUnRegCommitteeHot cred ->
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

-- For both of the fucntions `getScriptWitnessConwayTxCert` and
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
  ConwayTxCertCommittee committeeCert -> committeeScriptHash committeeCert
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
  ConwayTxCertCommittee committeeCert -> committeeKeyHashWitness committeeCert
