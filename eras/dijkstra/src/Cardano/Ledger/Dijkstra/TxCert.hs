{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.TxCert () where

import Cardano.Ledger.Conway.Core (
  ConwayEraTxCert,
  ShelleyEraTxCert (..),
  notSupportedInThisEra,
  pattern RegDepositDelegTxCert,
  pattern RegDepositTxCert,
  pattern RegTxCert,
  pattern UnRegDepositTxCert,
  pattern UnRegTxCert,
 )
import Cardano.Ledger.Conway.TxCert (
  ConwayDelegCert (..),
  ConwayEraTxCert (..),
  ConwayGovCert (..),
  ConwayTxCert (..),
  conwayTotalDepositsTxCerts,
  conwayTotalRefundsTxCerts,
  getScriptWitnessConwayTxCert,
  getVKeyWitnessConwayTxCert,
  pattern ConwayRegCert,
  pattern DelegStake,
 )
import Cardano.Ledger.Core (EraTxCert (..), PoolCert (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.PParams ()
import Data.Coerce (coerce)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Void (Void)

instance EraTxCert DijkstraEra where
  type TxCert DijkstraEra = ConwayTxCert DijkstraEra

  type TxCertUpgradeError DijkstraEra = Void

  upgradeTxCert = Right . coerce

  getVKeyWitnessTxCert = getVKeyWitnessConwayTxCert

  getScriptWitnessTxCert = getScriptWitnessConwayTxCert

  mkRegPoolTxCert = ConwayTxCertPool . RegPool

  getRegPoolTxCert (ConwayTxCertPool (RegPool poolParams)) = Just poolParams
  getRegPoolTxCert _ = Nothing

  mkRetirePoolTxCert poolId epochNo = ConwayTxCertPool $ RetirePool poolId epochNo

  getRetirePoolTxCert (ConwayTxCertPool (RetirePool poolId epochNo)) = Just (poolId, epochNo)
  getRetirePoolTxCert _ = Nothing

  lookupRegStakeTxCert = \case
    RegTxCert c -> Just c
    RegDepositTxCert c _ -> Just c
    RegDepositDelegTxCert c _ _ -> Just c
    _ -> Nothing
  lookupUnRegStakeTxCert = \case
    UnRegTxCert c -> Just c
    UnRegDepositTxCert c _ -> Just c
    _ -> Nothing

  getTotalRefundsTxCerts = conwayTotalRefundsTxCerts

  getTotalDepositsTxCerts = conwayTotalDepositsTxCerts

instance ShelleyEraTxCert DijkstraEra where
  mkRegTxCert c = ConwayTxCertDeleg $ ConwayRegCert c SNothing

  getRegTxCert (ConwayTxCertDeleg (ConwayRegCert c SNothing)) = Just c
  getRegTxCert _ = Nothing

  mkUnRegTxCert c = ConwayTxCertDeleg $ ConwayUnRegCert c SNothing

  getUnRegTxCert (ConwayTxCertDeleg (ConwayUnRegCert c SNothing)) = Just c
  getUnRegTxCert _ = Nothing

  mkDelegStakeTxCert c kh = ConwayTxCertDeleg $ ConwayDelegCert c (DelegStake kh)

  getDelegStakeTxCert (ConwayTxCertDeleg (ConwayDelegCert c (DelegStake kh))) = Just (c, kh)
  getDelegStakeTxCert _ = Nothing

  mkGenesisDelegTxCert = notSupportedInThisEra
  getGenesisDelegTxCert _ = Nothing

  mkMirTxCert = notSupportedInThisEra
  getMirTxCert = const Nothing

instance ConwayEraTxCert DijkstraEra where
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

  mkAuthCommitteeHotKeyTxCert ck hk = ConwayTxCertGov $ ConwayAuthCommitteeHotKey ck hk
  getAuthCommitteeHotKeyTxCert (ConwayTxCertGov (ConwayAuthCommitteeHotKey ck hk)) = Just (ck, hk)
  getAuthCommitteeHotKeyTxCert _ = Nothing

  mkResignCommitteeColdTxCert ck a = ConwayTxCertGov $ ConwayResignCommitteeColdKey ck a
  getResignCommitteeColdTxCert (ConwayTxCertGov (ConwayResignCommitteeColdKey ck a)) = Just (ck, a)
  getResignCommitteeColdTxCert _ = Nothing

  mkRegDRepTxCert cred deposit mAnchor = ConwayTxCertGov $ ConwayRegDRep cred deposit mAnchor
  getRegDRepTxCert = \case
    ConwayTxCertGov (ConwayRegDRep cred deposit mAnchor) -> Just (cred, deposit, mAnchor)
    _ -> Nothing

  mkUnRegDRepTxCert cred deposit = ConwayTxCertGov $ ConwayUnRegDRep cred deposit
  getUnRegDRepTxCert = \case
    ConwayTxCertGov (ConwayUnRegDRep cred deposit) -> Just (cred, deposit)
    _ -> Nothing

  mkUpdateDRepTxCert cred mAnchor = ConwayTxCertGov $ ConwayUpdateDRep cred mAnchor
  getUpdateDRepTxCert = \case
    ConwayTxCertGov (ConwayUpdateDRep cred mAnchor) -> Just (cred, mAnchor)
    _ -> Nothing
