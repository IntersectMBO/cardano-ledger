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
  EncCBOR (..),
  EncCBORGroup (..),
  FromCBOR (..),
  ToCBOR (..),
  decodeRecordSum,
  encodeListLen,
  toPlainDecoder,
  toPlainEncoding,
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential, StakeCredential)
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Shelley.Delegation (
  ShelleyDCert (..),
  ShelleyDelegCert (..),
  ShelleyEraDCert (..),
  shelleyDCertDecoder,
 )
import Cardano.Ledger.Val (Val (..))
import Control.DeepSeq (NFData)
import Data.Word (Word8)
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
  mkShelleyDCertDeleg = transShelleyDeleg

  getShelleyDCertDeleg (ConwayDCertDeleg (ConwayDeleg c (DelegStake kh) SNothing)) =
    Just $ Delegate (Delegation c kh)
  getShelleyDCertDeleg (ConwayDCertDeleg (ConwayReDeleg c (DelegStake kh))) =
    Just $ Delegate (Delegation c kh)
  getShelleyDCertDeleg (ConwayDCertDeleg (ConwayUnDeleg c SNothing)) =
    Just $ DeRegKey c
  getShelleyDCertDeleg (ConwayDCertDeleg (ConwayRegKey c)) =
    Just $ RegKey c
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

instance Crypto c => DecCBOR (Delegatee c) where
  decCBOR = decode . Summands "Delegatee" $ \case
    0 ->
      SumD DelegStake
        <! From
    1 ->
      SumD DelegVote
        <! From
    2 ->
      SumD DelegStakeVote
        <! From
        <! From
    k -> Invalid k

instance Crypto c => EncCBOR (Delegatee c) where
  encCBOR x = encode $ case x of
    DelegStake kh ->
      Sum DelegStake 0
        !> To kh
    DelegVote c ->
      Sum DelegVote 1
        !> To c
    DelegStakeVote kh c ->
      Sum DelegStakeVote 2
        !> To kh
        !> To c

instance NFData (Delegatee c)

instance NoThunks (Delegatee c)

data ConwayDelegCert c
  = ConwayDeleg !(StakeCredential c) !(Delegatee c) !(StrictMaybe Coin)
  | ConwayReDeleg !(StakeCredential c) !(Delegatee c)
  | ConwayUnDeleg !(StakeCredential c) !(StrictMaybe Coin)
  | ConwayRegKey !(StakeCredential c)
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
  ( ShelleyEraDCert era
  , DCert era ~ ConwayDCert era
  ) =>
  DecCBOR (ConwayDCert era)
  where
  decCBOR = decodeRecordSum "ConwayDCert" $
    \case
      t | t <= 5 -> do
        shelleyDCertDecoder @era t
      7 -> do
        cred <- decCBOR
        delegatee <- decCBOR
        deposit <- decCBOR
        pure (4, ConwayDCertDeleg $ ConwayDeleg cred delegatee (SJust deposit))
      8 -> do
        cred <- decCBOR
        delegatee <- decCBOR
        pure (3, ConwayDCertDeleg $ ConwayReDeleg cred delegatee)
      9 -> do
        cred <- decCBOR
        deposit <- decCBOR
        pure (3, ConwayDCertDeleg $ ConwayUnDeleg cred (SJust deposit))
      k -> invalidKey k

instance (Era era, Val (Value era)) => ToCBOR (ConwayDCert era) where
  toCBOR = toPlainEncoding (eraProtVerLow @era) . encCBOR

instance (Era era, Val (Value era)) => EncCBOR (ConwayDCert era) where
  encCBOR = \case
    ConwayDCertDeleg (ConwayRegKey c) ->
      -- Shelley backwards compatibility
      encCBOR . ShelleyDCertDelegCert @era $ RegKey c
    ConwayDCertPool (RegPool poolParams) ->
      encodeListLen (1 + listLen poolParams)
        <> encCBOR (3 :: Word8)
        <> encCBORGroup poolParams
    ConwayDCertPool (RetirePool vk epoch) ->
      encodeListLen 3
        <> encCBOR (4 :: Word8)
        <> encCBOR vk
        <> encCBOR epoch
    ConwayDCertConstitutional (ConstitutionalDelegCert gk kh vrf) ->
      encodeListLen 4
        <> encCBOR (5 :: Word8)
        <> encCBOR gk
        <> encCBOR kh
        <> encCBOR vrf
    ConwayDCertDeleg (ConwayDeleg cred delegatee (SJust deposit)) ->
      encodeListLen 4
        <> encCBOR (7 :: Word8)
        <> encCBOR cred
        <> encCBOR delegatee
        <> encCBOR deposit
    ConwayDCertDeleg (ConwayDeleg cred (DelegStake kh) SNothing) ->
      -- Shelley backwards compatibility
      encCBOR . ShelleyDCertDelegCert @era . Delegate $ Delegation cred kh
    ConwayDCertDeleg (ConwayDeleg cred delegatee SNothing) ->
      -- This should never happen, since we don't allow delegating to DReps
      -- using Shelley delegation certs.
      encodeListLen 4
        <> encCBOR (7 :: Word8)
        <> encCBOR cred
        <> encCBOR delegatee
        <> encCBOR (zero :: Value era)
    ConwayDCertDeleg (ConwayReDeleg cred delegatee) ->
      encodeListLen 3
        <> encCBOR (8 :: Word8)
        <> encCBOR cred
        <> encCBOR delegatee
    ConwayDCertDeleg (ConwayUnDeleg cred (SJust deposit)) ->
      encodeListLen 3
        <> encCBOR (9 :: Word8)
        <> encCBOR cred
        <> encCBOR deposit
    ConwayDCertDeleg (ConwayUnDeleg cred SNothing) ->
      -- Shelley backwards compatibility
      encCBOR . ShelleyDCertDelegCert @era $ DeRegKey cred

transShelleyDeleg :: ShelleyDelegCert (EraCrypto era) -> ConwayDCert era
transShelleyDeleg (Delegate (Delegation c kh)) =
  ConwayDCertDeleg (ConwayDeleg c (DelegStake kh) SNothing)
transShelleyDeleg (RegKey c) = ConwayDCertDeleg $ ConwayRegKey c
transShelleyDeleg (DeRegKey c) =
  ConwayDCertDeleg (ConwayUnDeleg c SNothing)
