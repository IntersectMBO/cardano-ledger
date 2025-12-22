{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.TxCert (
  DijkstraTxCertUpgradeError,
  DijkstraTxCert (..),
  DijkstraDelegCert (..),
) where

import Cardano.Ledger.BaseTypes (kindObject)
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
  decodeRecordSum,
  encodeListLen,
  encodeWord8,
  invalidKey,
  toPlainEncoding,
 )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Core (
  ConwayEraTxCert,
  poolCertKeyHashWitness,
  pattern AuthCommitteeHotKeyTxCert,
  pattern DelegTxCert,
  pattern RegDRepTxCert,
  pattern RegDepositDelegTxCert,
  pattern RegDepositTxCert,
  pattern RegPoolTxCert,
  pattern RegTxCert,
  pattern ResignCommitteeColdTxCert,
  pattern RetirePoolTxCert,
  pattern UnRegDRepTxCert,
  pattern UnRegDepositTxCert,
  pattern UnRegTxCert,
  pattern UpdateDRepTxCert,
 )
import Cardano.Ledger.Conway.TxCert (
  ConwayEraTxCert (..),
  ConwayGovCert (..),
  Delegatee (..),
  conwayGovCertVKeyWitness,
  conwayTotalDepositsTxCerts,
  conwayTxCertDelegDecoder,
 )
import Cardano.Ledger.Core (
  Era,
  EraTxCert (..),
  KeyHash,
  KeyRole (..),
  PoolCert (..),
  ScriptHash,
  Value,
  eraProtVerLow,
  fromEraCBOR,
 )
import Cardano.Ledger.Credential (Credential, credKeyHashWitness, credScriptHash)
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.PParams ()
import Cardano.Ledger.Shelley.TxCert (
  ShelleyDelegCert (..),
  encodePoolCert,
  encodeShelleyDelegCert,
  poolTxCertDecoder,
 )
import Cardano.Ledger.Val (Val (..))
import Control.DeepSeq (NFData)
import Data.Aeson (KeyValue ((.=)), ToJSON (..))
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

data DijkstraDelegCert
  = DijkstraRegCert !(Credential Staking) !Coin
  | DijkstraUnRegCert !(Credential Staking) !Coin
  | DijkstraDelegCert !(Credential Staking) !Delegatee
  | DijkstraRegDelegCert !(Credential Staking) !Delegatee !Coin
  deriving (Show, Generic, Eq, Ord)

instance EncCBOR DijkstraDelegCert where
  encCBOR = \case
    DijkstraRegCert cred deposit ->
      encodeListLen 3
        <> encodeWord8 7
        <> encCBOR cred
        <> encCBOR deposit
    DijkstraUnRegCert cred deposit ->
      encodeListLen 3
        <> encodeWord8 8
        <> encCBOR cred
        <> encCBOR deposit
    DijkstraDelegCert cred (DelegStake poolId) -> encodeShelleyDelegCert $ ShelleyDelegCert cred poolId
    DijkstraDelegCert cred (DelegVote drep) ->
      encodeListLen 3
        <> encodeWord8 9
        <> encCBOR cred
        <> encCBOR drep
    DijkstraDelegCert cred (DelegStakeVote poolId dRep) ->
      encodeListLen 4
        <> encodeWord8 10
        <> encCBOR cred
        <> encCBOR poolId
        <> encCBOR dRep
    DijkstraRegDelegCert cred (DelegStake poolId) deposit ->
      encodeListLen 4
        <> encodeWord8 11
        <> encCBOR cred
        <> encCBOR poolId
        <> encCBOR deposit
    DijkstraRegDelegCert cred (DelegVote drep) deposit ->
      encodeListLen 4
        <> encodeWord8 12
        <> encCBOR cred
        <> encCBOR drep
        <> encCBOR deposit
    DijkstraRegDelegCert cred (DelegStakeVote poolId dRep) deposit ->
      encodeListLen 5
        <> encodeWord8 13
        <> encCBOR cred
        <> encCBOR poolId
        <> encCBOR dRep
        <> encCBOR deposit

instance NFData DijkstraDelegCert

instance NoThunks DijkstraDelegCert

instance ToJSON DijkstraDelegCert where
  toJSON = \case
    DijkstraRegCert cred deposit ->
      kindObject
        "RegCert"
        [ "credential" .= toJSON cred
        , "deposit" .= toJSON deposit
        ]
    DijkstraUnRegCert cred refund ->
      kindObject
        "UnRegCert"
        [ "credential" .= toJSON cred
        , "refund" .= toJSON refund
        ]
    DijkstraDelegCert cred delegatee ->
      kindObject
        "DelegCert"
        [ "credential" .= toJSON cred
        , "delegatee" .= toJSON delegatee
        ]
    DijkstraRegDelegCert cred delegatee deposit ->
      kindObject
        "RegDelegCert"
        [ "credential" .= toJSON cred
        , "delegatee" .= toJSON delegatee
        , "deposit" .= toJSON deposit
        ]

data DijkstraTxCert era
  = DijkstraTxCertDeleg !DijkstraDelegCert
  | DijkstraTxCertPool !PoolCert
  | DijkstraTxCertGov !ConwayGovCert
  deriving (Show, Generic, Eq, Ord)

data DijkstraTxCertUpgradeError
  = RegTxCertExpunged
  | UnRegTxCertExpunged
  deriving (Eq, Show)

instance NFData (DijkstraTxCert era)

instance NoThunks (DijkstraTxCert era)

instance Era era => ToJSON (DijkstraTxCert era) where
  toJSON = \case
    DijkstraTxCertDeleg delegCert -> toJSON delegCert
    DijkstraTxCertPool poolCert -> toJSON poolCert
    DijkstraTxCertGov govCert -> toJSON govCert

instance
  ( EraTxCert era
  , TxCert era ~ DijkstraTxCert era
  ) =>
  FromCBOR (DijkstraTxCert era)
  where
  fromCBOR = fromEraCBOR @era

instance
  ( ConwayEraTxCert era
  , TxCert era ~ DijkstraTxCert era
  ) =>
  DecCBOR (DijkstraTxCert era)
  where
  decCBOR = decodeRecordSum "DijkstraTxCert" $ \case
    t
      | 0 <= t && t < 2 -> fail "Certificates without deposits are no longer supported"
      | t == 2 -> do
          cred <- decCBOR
          stakePool <- decCBOR
          pure (3, DelegTxCert cred (DelegStake stakePool))
      | 3 <= t && t < 5 -> poolTxCertDecoder t
      | t == 5 -> fail "Genesis delegation certificates are no longer supported"
      | t == 6 -> fail "MIR certificates are no longer supported"
      | 7 <= t -> conwayTxCertDelegDecoder t
    t -> invalidKey t

instance (Era era, Val (Value era)) => ToCBOR (DijkstraTxCert era) where
  toCBOR = toPlainEncoding (eraProtVerLow @era) . encCBOR

instance Era era => EncCBOR (DijkstraTxCert era) where
  encCBOR = \case
    DijkstraTxCertDeleg delegCert -> encCBOR delegCert
    DijkstraTxCertPool poolCert -> encodePoolCert poolCert
    DijkstraTxCertGov govCert -> encCBOR govCert

-- | Unlike previous eras, we no longer need to lookup refunds from the ledger state, since all of the certificates specify the actual refund and ledger rules will validate that they are accurate.
dijkstraTotalRefundsTxCerts ::
  ( Foldable f
  , ConwayEraTxCert era
  ) =>
  f (TxCert era) ->
  Coin
dijkstraTotalRefundsTxCerts = foldMap' $ \case
  UnRegDepositTxCert _ deposit -> deposit
  UnRegDRepTxCert _ deposit -> deposit
  _ -> zero

instance EraTxCert DijkstraEra where
  type TxCert DijkstraEra = DijkstraTxCert DijkstraEra

  type TxCertUpgradeError DijkstraEra = DijkstraTxCertUpgradeError

  upgradeTxCert = \case
    RegPoolTxCert poolParams -> Right $ RegPoolTxCert poolParams
    RetirePoolTxCert poolId epochNo -> Right $ RetirePoolTxCert poolId epochNo
    RegDepositTxCert cred c -> Right $ RegDepositTxCert cred c
    UnRegDepositTxCert cred c -> Right $ UnRegDepositTxCert cred c
    DelegTxCert cred d -> Right $ DelegTxCert cred d
    RegDepositDelegTxCert cred d c -> Right $ RegDepositDelegTxCert cred d c
    AuthCommitteeHotKeyTxCert ck hk -> Right $ AuthCommitteeHotKeyTxCert ck hk
    ResignCommitteeColdTxCert ck a -> Right $ ResignCommitteeColdTxCert ck a
    RegDRepTxCert cred deposit mAnchor -> Right $ RegDRepTxCert cred deposit mAnchor
    UnRegDRepTxCert cred deposit -> Right $ UnRegDRepTxCert cred deposit
    UpdateDRepTxCert cred mAnchor -> Right $ UpdateDRepTxCert cred mAnchor
    RegTxCert {} -> Left RegTxCertExpunged
    UnRegTxCert {} -> Left UnRegTxCertExpunged
    -- Using wildcard here in order to workaround ghc disrespecting
    -- the completeness pragma in presence of `PreviousEra` type family.
    _ -> error "Impossible: all patterns have been accounted for"

  getVKeyWitnessTxCert = getVKeyWitnessDijkstraTxCert

  getScriptWitnessTxCert = getScriptWitnessDijkstraTxCert

  mkRegPoolTxCert = DijkstraTxCertPool . RegPool

  getRegPoolTxCert (DijkstraTxCertPool (RegPool poolParams)) = Just poolParams
  getRegPoolTxCert _ = Nothing

  mkRetirePoolTxCert poolId epochNo = DijkstraTxCertPool $ RetirePool poolId epochNo

  getRetirePoolTxCert (DijkstraTxCertPool (RetirePool poolId epochNo)) = Just (poolId, epochNo)
  getRetirePoolTxCert _ = Nothing

  lookupRegStakeTxCert = \case
    RegDepositTxCert c _ -> Just c
    RegDepositDelegTxCert c _ _ -> Just c
    _ -> Nothing
  lookupUnRegStakeTxCert = \case
    UnRegDepositTxCert c _ -> Just c
    _ -> Nothing

  getTotalRefundsTxCerts _ _ _ = dijkstraTotalRefundsTxCerts

  getTotalDepositsTxCerts = conwayTotalDepositsTxCerts

getScriptWitnessDijkstraTxCert :: DijkstraTxCert era -> Maybe ScriptHash
getScriptWitnessDijkstraTxCert = \case
  DijkstraTxCertDeleg delegCert ->
    case delegCert of
      DijkstraRegCert cred _ -> credScriptHash cred
      DijkstraUnRegCert cred _ -> credScriptHash cred
      DijkstraDelegCert cred _ -> credScriptHash cred
      DijkstraRegDelegCert cred _ _ -> credScriptHash cred
  DijkstraTxCertPool {} -> Nothing
  DijkstraTxCertGov govCert -> govWitness govCert
  where
    govWitness :: ConwayGovCert -> Maybe ScriptHash
    govWitness = \case
      ConwayAuthCommitteeHotKey coldCred _hotCred -> credScriptHash coldCred
      ConwayResignCommitteeColdKey coldCred _ -> credScriptHash coldCred
      ConwayRegDRep cred _ _ -> credScriptHash cred
      ConwayUnRegDRep cred _ -> credScriptHash cred
      ConwayUpdateDRep cred _ -> credScriptHash cred

getVKeyWitnessDijkstraTxCert :: DijkstraTxCert era -> Maybe (KeyHash Witness)
getVKeyWitnessDijkstraTxCert = \case
  DijkstraTxCertDeleg delegCert ->
    case delegCert of
      DijkstraRegCert cred _ -> credKeyHashWitness cred
      DijkstraUnRegCert cred _ -> credKeyHashWitness cred
      DijkstraDelegCert cred _ -> credKeyHashWitness cred
      DijkstraRegDelegCert cred _ _ -> credKeyHashWitness cred
  DijkstraTxCertPool poolCert -> Just $ poolCertKeyHashWitness poolCert
  DijkstraTxCertGov govCert -> conwayGovCertVKeyWitness govCert

instance ConwayEraTxCert DijkstraEra where
  mkRegDepositTxCert cred c = DijkstraTxCertDeleg $ DijkstraRegCert cred c

  getRegDepositTxCert (DijkstraTxCertDeleg (DijkstraRegCert cred c)) = Just (cred, c)
  getRegDepositTxCert _ = Nothing

  mkUnRegDepositTxCert cred c = DijkstraTxCertDeleg $ DijkstraUnRegCert cred c

  getUnRegDepositTxCert (DijkstraTxCertDeleg (DijkstraUnRegCert cred c)) = Just (cred, c)
  getUnRegDepositTxCert _ = Nothing

  mkDelegTxCert cred d = DijkstraTxCertDeleg $ DijkstraDelegCert cred d

  getDelegTxCert (DijkstraTxCertDeleg (DijkstraDelegCert cred d)) = Just (cred, d)
  getDelegTxCert _ = Nothing

  mkRegDepositDelegTxCert cred d c = DijkstraTxCertDeleg $ DijkstraRegDelegCert cred d c
  getRegDepositDelegTxCert (DijkstraTxCertDeleg (DijkstraRegDelegCert cred d c)) = Just (cred, d, c)
  getRegDepositDelegTxCert _ = Nothing

  mkAuthCommitteeHotKeyTxCert ck hk = DijkstraTxCertGov $ ConwayAuthCommitteeHotKey ck hk
  getAuthCommitteeHotKeyTxCert (DijkstraTxCertGov (ConwayAuthCommitteeHotKey ck hk)) = Just (ck, hk)
  getAuthCommitteeHotKeyTxCert _ = Nothing

  mkResignCommitteeColdTxCert ck a = DijkstraTxCertGov $ ConwayResignCommitteeColdKey ck a
  getResignCommitteeColdTxCert (DijkstraTxCertGov (ConwayResignCommitteeColdKey ck a)) = Just (ck, a)
  getResignCommitteeColdTxCert _ = Nothing

  mkRegDRepTxCert cred deposit mAnchor = DijkstraTxCertGov $ ConwayRegDRep cred deposit mAnchor
  getRegDRepTxCert = \case
    DijkstraTxCertGov (ConwayRegDRep cred deposit mAnchor) -> Just (cred, deposit, mAnchor)
    _ -> Nothing

  mkUnRegDRepTxCert cred deposit = DijkstraTxCertGov $ ConwayUnRegDRep cred deposit
  getUnRegDRepTxCert = \case
    DijkstraTxCertGov (ConwayUnRegDRep cred deposit) -> Just (cred, deposit)
    _ -> Nothing

  mkUpdateDRepTxCert cred mAnchor = DijkstraTxCertGov $ ConwayUpdateDRep cred mAnchor
  getUpdateDRepTxCert = \case
    DijkstraTxCertGov (ConwayUpdateDRep cred mAnchor) -> Just (cred, mAnchor)
    _ -> Nothing
