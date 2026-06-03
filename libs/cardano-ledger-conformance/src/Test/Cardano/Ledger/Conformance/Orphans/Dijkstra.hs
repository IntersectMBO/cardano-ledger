{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.Orphans.Dijkstra where

import MAlonzo.Code.Ledger.Dijkstra.Foreign.API
import Test.Cardano.Ledger.Common (NFData, ToExpr)
import Test.Cardano.Ledger.Conformance.Orphans.Core ()

deriving instance Ord Tag

deriving instance Ord HSLanguage

deriving instance Ord LanguageCostModels

deriving instance Ord GovRole

deriving instance Ord Vote

deriving instance Ord GovVotes

deriving instance Ord VDeleg

deriving instance Ord PoolThresholds

deriving instance Ord DrepThresholds

deriving instance Ord PParamsUpdate

deriving instance Ord GovAction

deriving instance Ord GovActionState

instance NFData PParamsUpdate

instance NFData GovAction

instance NFData Anchor

instance NFData NativeScript

instance NFData HSNativeScript

instance NFData HSLanguage

instance NFData LanguageCostModels

instance NFData HSPlutusScript

instance NFData UTxOState

instance NFData Vote

instance NFData GovRole

instance NFData GovVotes

instance NFData GovActionState

instance NFData GovVote

instance NFData GovProposal

instance NFData DrepThresholds

instance NFData PoolThresholds

instance NFData PParams

instance NFData EnactState

instance NFData GovEnv

instance NFData VDeleg

instance NFData StakePoolParams

instance NFData DCert

instance NFData TxBodyTop

instance NFData TxBodySub

instance NFData Tag

instance NFData TxWitnesses

instance NFData TxTop

instance NFData TxSub

instance NFData CertEnv

instance NFData PState

instance NFData DState

instance NFData GState

instance NFData CertState

instance NFData StakeDistrs

instance NFData RatifyEnv

instance NFData RatifyState

instance NFData EnactEnv

instance NFData DelegEnv

instance NFData EpochState

instance NFData Snapshots

instance NFData Snapshot

instance NFData Acnt

instance NFData LedgerState

instance NFData LedgerEnv

instance NFData RewardUpdate

instance NFData NewEpochState

instance NFData BalanceInterval

instance ToExpr PParamsUpdate

instance ToExpr GovAction

instance ToExpr GovRole

instance ToExpr GovVotes

instance ToExpr Vote

instance ToExpr GovActionState

instance ToExpr GovProposal

instance ToExpr GovVote

instance ToExpr PoolThresholds

instance ToExpr DrepThresholds

instance ToExpr PParams

instance ToExpr GovEnv

instance ToExpr EnactState

instance ToExpr VDeleg

instance ToExpr StakePoolParams

instance ToExpr DCert

instance ToExpr Anchor

instance ToExpr NativeScript

instance ToExpr HSNativeScript

instance ToExpr HSLanguage

instance ToExpr LanguageCostModels

instance ToExpr HSPlutusScript

instance ToExpr TxBodyTop

instance ToExpr TxBodySub

instance ToExpr Tag

instance ToExpr TxWitnesses

instance ToExpr TxTop

instance ToExpr TxSub

instance ToExpr UTxOState

instance ToExpr CertEnv

instance ToExpr DState

instance ToExpr PState

instance ToExpr GState

instance ToExpr CertState

instance ToExpr StakeDistrs

instance ToExpr RatifyEnv

instance ToExpr RatifyState

instance ToExpr EnactEnv

instance ToExpr DelegEnv

instance ToExpr EpochState

instance ToExpr Snapshots

instance ToExpr Snapshot

instance ToExpr LedgerState

instance ToExpr LedgerEnv

instance ToExpr Acnt

instance ToExpr RewardUpdate

instance ToExpr NewEpochState

instance ToExpr BalanceInterval
