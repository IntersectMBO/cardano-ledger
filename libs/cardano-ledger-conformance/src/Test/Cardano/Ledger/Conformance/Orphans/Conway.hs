{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.Orphans.Conway where

import GHC.Generics (Generic)
import MAlonzo.Code.Ledger.Conway.Foreign.API as Agda
import Test.Cardano.Ledger.Common (NFData, ToExpr)
import Test.Cardano.Ledger.Conformance.Orphans.Core ()

deriving instance Generic HsRewardUpdate

deriving instance Ord DepositPurpose

deriving instance Ord Tag

deriving instance Ord HSLanguage

deriving instance Ord LanguageCostModels

deriving instance Ord GovRole

deriving instance Ord GovVotes

deriving instance Ord VDeleg

deriving instance Ord Vote

deriving instance Ord PoolThresholds

deriving instance Ord DrepThresholds

deriving instance Ord PParamsUpdate

deriving instance Ord GovAction

deriving instance Ord GovActionState

instance NFData PParamsUpdate

instance NFData GovAction

instance NFData Timelock

instance NFData HSTimelock

instance NFData HSLanguage

instance NFData LanguageCostModels

instance NFData HSPlutusScript

instance NFData UTxOState

instance NFData Vote

instance NFData GovRole

instance NFData GovVotes

instance NFData GovActionState

instance NFData Anchor

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

instance NFData TxBody

instance NFData Tag

instance NFData TxWitnesses

instance NFData Tx

instance NFData UTxOEnv

instance NFData DepositPurpose

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

instance NFData LState

instance NFData HsRewardUpdate

instance NFData NewEpochState

instance NFData LEnv

instance ToExpr PParamsUpdate

instance ToExpr GovAction

instance ToExpr GovRole

instance ToExpr GovVotes

instance ToExpr Vote

instance ToExpr GovActionState

instance ToExpr Anchor

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

instance ToExpr Timelock

instance ToExpr HSTimelock

instance ToExpr HSLanguage

instance ToExpr LanguageCostModels

instance ToExpr HSPlutusScript

instance ToExpr TxBody

instance ToExpr Tag

instance ToExpr TxWitnesses

instance ToExpr Tx

instance ToExpr UTxOState

instance ToExpr UTxOEnv

instance ToExpr DepositPurpose

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

instance ToExpr LState

instance ToExpr Acnt

instance ToExpr HsRewardUpdate

instance ToExpr NewEpochState

instance ToExpr LEnv
