{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Delegation (
  ConwayDCert (..),
  ConwayDelegCert (..),
  Delegatee (..),
  ConwayEraDCert (..),
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
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential, StakeCredential)
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Shelley.Delegation (
  ShelleyDelegCert (..),
  ShelleyEraDCert (..),
  commonDCertDecoder,
  encodeConstitutionalCert,
  encodePoolCert,
  encodeShelleyDelegCert,
  shelleyDCertDelegDecoder,
 )
import Cardano.Ledger.Val (Val (..))
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

instance Crypto c => EraDCert (ConwayEra c) where
  type DCert (ConwayEra c) = ConwayDCert (ConwayEra c)

  mkDCertPool = ConwayDCertPool
  getDCertPool (ConwayDCertPool x) = Just x
  getDCertPool _ = Nothing

  mkDCertGenesis = ConwayDCertConstitutional
  getDCertGenesis (ConwayDCertConstitutional x) = Just x
  getDCertGenesis _ = Nothing

instance Crypto c => ShelleyEraDCert (ConwayEra c) where
  mkShelleyDCertDeleg = ConwayDCertDeleg . fromShelleyDelegCert

  getShelleyDCertDeleg (ConwayDCertDeleg conwayDelegCert) = toShelleyDelegCert conwayDelegCert
  getShelleyDCertDeleg _ = Nothing

  mkDCertMir = notSupportedInThisEra
  getDCertMir = const Nothing

class ShelleyEraDCert era => ConwayEraDCert era where
  mkConwayDCertDeleg :: ConwayDelegCert (EraCrypto era) -> DCert era
  getConwayDCertDeleg :: DCert era -> Maybe (ConwayDelegCert (EraCrypto era))

instance Crypto c => ConwayEraDCert (ConwayEra c) where
  mkConwayDCertDeleg = ConwayDCertDeleg
  getConwayDCertDeleg (ConwayDCertDeleg x) = Just x
  getConwayDCertDeleg _ = Nothing

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

data ConwayDCert era
  = ConwayDCertDeleg !(ConwayDelegCert (EraCrypto era))
  | ConwayDCertPool !(PoolCert (EraCrypto era))
  | ConwayDCertConstitutional !(ConstitutionalDelegCert (EraCrypto era))
  deriving (Show, Generic, Eq)

instance NFData (ConwayDCert c)

instance NoThunks (ConwayDCert c)

instance
  ( ShelleyEraDCert era
  , DCert era ~ ConwayDCert era
  ) =>
  FromCBOR (ConwayDCert era)
  where
  fromCBOR = toPlainDecoder (eraProtVerLow @era) decCBOR

instance
  ( ConwayEraDCert era
  , DCert era ~ ConwayDCert era
  ) =>
  DecCBOR (ConwayDCert era)
  where
  decCBOR = decodeRecordSum "ConwayDCert" $ \case
    t
      | 0 <= t && t < 3 -> shelleyDCertDelegDecoder t
      | 3 <= t && t < 6 -> commonDCertDecoder t
      | t == 6 -> fail "MIR certificates are no longer supported"
      | 7 <= t -> conwayDCertDelegDecoder t
    t -> invalidKey t

conwayDCertDelegDecoder :: ConwayEraDCert era => Word -> Decoder s (Int, DCert era)
conwayDCertDelegDecoder = \case
  7 -> do
    cred <- decCBOR
    deposit <- SJust <$> decCBOR
    pure (3, mkConwayDCertDeleg $ ConwayRegCert cred deposit)
  8 -> do
    cred <- decCBOR
    deposit <- SJust <$> decCBOR
    pure (3, mkConwayDCertDeleg $ ConwayUnRegCert cred deposit)
  9 -> delegCertDecoder 3 (DelegVote <$> decCBOR)
  10 -> delegCertDecoder 4 (DelegStakeVote <$> decCBOR <*> decCBOR)
  11 -> regDelegCertDecoder 4 (DelegStake <$> decCBOR)
  12 -> regDelegCertDecoder 4 (DelegVote <$> decCBOR)
  13 -> regDelegCertDecoder 5 (DelegStakeVote <$> decCBOR <*> decCBOR)
  k -> invalidKey k
  where
    delegCertDecoder n decodeDelegatee = do
      cred <- decCBOR
      delegatee <- decodeDelegatee
      pure (n, mkConwayDCertDeleg $ ConwayDelegCert cred delegatee)
    {-# INLINE delegCertDecoder #-}
    regDelegCertDecoder n decodeDelegatee = do
      cred <- decCBOR
      delegatee <- decodeDelegatee
      deposit <- decCBOR
      pure (n, mkConwayDCertDeleg $ ConwayRegDelegCert cred delegatee deposit)
    {-# INLINE regDelegCertDecoder #-}
{-# INLINE conwayDCertDelegDecoder #-}

instance (Era era, Val (Value era)) => ToCBOR (ConwayDCert era) where
  toCBOR = toPlainEncoding (eraProtVerLow @era) . encCBOR

instance (Era era, Val (Value era)) => EncCBOR (ConwayDCert era) where
  encCBOR = \case
    ConwayDCertDeleg delegCert -> encodeConwayDelegCert delegCert
    ConwayDCertPool poolCert -> encodePoolCert poolCert
    ConwayDCertConstitutional constCert -> encodeConstitutionalCert constCert

encodeConwayDelegCert :: Crypto c => ConwayDelegCert c -> Encoding
encodeConwayDelegCert = \case
  -- Shelley backwards compatibility
  ConwayRegCert cred SNothing -> encodeShelleyDelegCert $ RegKey cred
  ConwayUnRegCert cred SNothing -> encodeShelleyDelegCert $ DeRegKey cred
  ConwayDelegCert cred (DelegStake poolId) ->
    encodeShelleyDelegCert $ Delegate (Delegation cred poolId)
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

fromShelleyDelegCert :: ShelleyDelegCert c -> ConwayDelegCert c
fromShelleyDelegCert = \case
  RegKey c -> ConwayRegCert c SNothing
  DeRegKey c -> ConwayUnRegCert c SNothing
  Delegate (Delegation c kh) -> ConwayDelegCert c (DelegStake kh)

toShelleyDelegCert :: ConwayDelegCert c -> Maybe (ShelleyDelegCert c)
toShelleyDelegCert = \case
  ConwayRegCert c SNothing -> Just $ RegKey c
  ConwayUnRegCert c SNothing -> Just $ DeRegKey c
  ConwayDelegCert c (DelegStake kh) -> Just $ Delegate (Delegation c kh)
  _ -> Nothing
