{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Shelley.TreeDiff (
  module Test.Cardano.Ledger.TreeDiff,
) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.AdaPots (AdaPots)
import Cardano.Ledger.Shelley.Governance
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.PParams
import Cardano.Ledger.Shelley.PoolRank
import Cardano.Ledger.Shelley.Rules
import Cardano.Ledger.Shelley.Scripts
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.Shelley.Tx
import Cardano.Ledger.Shelley.TxAuxData
import Cardano.Ledger.Shelley.TxBody
import Cardano.Ledger.Shelley.TxCert
import Cardano.Ledger.Shelley.TxOut
import Cardano.Ledger.Shelley.TxWits
import Cardano.Ledger.Shelley.UTxO
import Test.Cardano.Ledger.TreeDiff

-- PParams
instance ToExpr (PParamsUpdate era) => ToExpr (ProposedPPUpdates era)

instance ToExpr (ShelleyPParams StrictMaybe era)

instance ToExpr (ShelleyPParams Identity era)

instance ToExpr (PParamsUpdate era) => ToExpr (Update era)

-- Scripts
instance ToExpr (MultiSigRaw era)

instance ToExpr (MultiSig era)

-- TxAuxData
instance ToExpr Metadatum

instance ToExpr (ShelleyTxAuxDataRaw era)

instance ToExpr (ShelleyTxAuxData era)

-- Governance

instance ToExpr (PParams era) => ToExpr (FuturePParams era)

instance
  ( ToExpr (PParamsUpdate era)
  , ToExpr (PParams era)
  ) =>
  ToExpr (ShelleyGovState era)

-- TxCert
instance ToExpr GenesisDelegCert

instance ToExpr MIRPot

instance ToExpr MIRTarget

instance ToExpr MIRCert

instance ToExpr (ShelleyTxCert era)

instance ToExpr ShelleyDelegCert

-- TxWits
instance (Era era, ToExpr (Script era)) => ToExpr (ShelleyTxWits era)

instance (Era era, ToExpr (Script era)) => ToExpr (ShelleyTxWitsRaw era)

-- Rules/Ppup
instance ToExpr VotingPeriod

instance ToExpr (ShelleyPpupPredFailure era)

-- TxOut
instance (EraTxOut era, ToExpr (Value era)) => ToExpr (ShelleyTxOut era) where
  toExpr (ShelleyTxOut x y) = App "ShelleyTxOut" [toExpr x, toExpr y]

-- TxBody
instance ToExpr ShelleyTxBodyRaw

instance ToExpr (TxBody ShelleyEra)

-- PoolRank
instance ToExpr Likelihood

instance ToExpr LogWeight

instance ToExpr NonMyopic

instance
  ( ToExpr (TxAuxData era)
  , ToExpr (TxBody era)
  , ToExpr (TxWits era)
  ) =>
  ToExpr (ShelleyTx era)

-- RewardUpdate

-- | You really don't want to see what is inside this.
instance ToExpr PulsingRewUpdate where
  toExpr _ = App "PulsingRewUpdate..." []

instance ToExpr RewardUpdate

-- LedgerState/Types

instance
  ( ToExpr (TxOut era)
  , ToExpr (PParams era)
  , ToExpr (StashedAVVMAddresses era)
  , ToExpr (GovState era)
  , ToExpr (CertState era)
  , ToExpr (InstantStake era)
  ) =>
  ToExpr (NewEpochState era)

instance
  ( ToExpr (TxOut era)
  , ToExpr (PParams era)
  , ToExpr (GovState era)
  , ToExpr (CertState era)
  , ToExpr (InstantStake era)
  ) =>
  ToExpr (EpochState era)

instance
  ( ToExpr (TxOut era)
  , ToExpr (GovState era)
  , ToExpr (CertState era)
  , ToExpr (InstantStake era)
  ) =>
  ToExpr (LedgerState era)

instance
  ( ToExpr (TxOut era)
  , ToExpr (GovState era)
  , ToExpr (InstantStake era)
  ) =>
  ToExpr (UTxOState era)

instance ToExpr (ShelleyInstantStake era)

instance ToExpr (ShelleyScriptsNeeded era)

-- Rules/Utxo
instance
  ( ToExpr (EraRuleFailure "PPUP" era)
  , ToExpr (Value era)
  , ToExpr (TxOut era)
  ) =>
  ToExpr (ShelleyUtxoPredFailure era)

-- Rules/Pool
instance ToExpr (ShelleyPoolPredFailure era)

-- Rules/Deleg
instance ToExpr (ShelleyDelegPredFailure era)

-- Rules/Utxow
instance
  ( Era era
  , ToExpr (PredicateFailure (EraRule "UTXO" era))
  ) =>
  ToExpr (ShelleyUtxowPredFailure era)

-- Rules/Delpl
instance
  ( ToExpr (PredicateFailure (EraRule "DELEG" era))
  , ToExpr (PredicateFailure (EraRule "POOL" era))
  ) =>
  ToExpr (ShelleyDelplPredFailure era)

-- Rules/Delegs
instance
  ToExpr (PredicateFailure (EraRule "DELPL" era)) =>
  ToExpr (ShelleyDelegsPredFailure era)

-- Rules/Ledger
instance
  ( ToExpr (PredicateFailure (EraRule "UTXOW" era))
  , ToExpr (PredicateFailure (EraRule "DELEGS" era))
  ) =>
  ToExpr (ShelleyLedgerPredFailure era)

instance ToExpr Obligations

instance ToExpr AdaPots

-- Events
instance
  ( ToExpr (Event (EraRule "UTXOW" era))
  , ToExpr (Event (EraRule "DELEGS" era))
  ) =>
  ToExpr (ShelleyLedgerEvent era)

instance
  ToExpr (Event (EraRule "UTXO" era)) =>
  ToExpr (ShelleyUtxowEvent era)

instance
  ( ToExpr (Event (EraRule "PPUP" era))
  , ToExpr (TxOut era)
  ) =>
  ToExpr (UtxoEvent era)

instance ToExpr (PpupEvent era)

instance ToExpr (Event (EraRule "DELPL" era)) => ToExpr (ShelleyDelegsEvent era)

instance
  ( ToExpr (Event (EraRule "DELEG" era))
  , ToExpr (Event (EraRule "POOL" era))
  ) =>
  ToExpr (ShelleyDelplEvent era)

instance ToExpr (ShelleyDelegEvent era)

instance ToExpr (PoolEvent era)

instance ToExpr (PParamsHKD Identity era) => ToExpr (PoolEnv era)

instance
  ( ToExpr (Event (EraRule "NEWEPOCH" era))
  , ToExpr (Event (EraRule "RUPD" era))
  ) =>
  ToExpr (ShelleyTickEvent era)

instance ToExpr RewardType

instance ToExpr Reward

instance
  ( ToExpr (Event (EraRule "EPOCH" era))
  , ToExpr (Event (EraRule "MIR" era))
  , ToExpr (Event (EraRule "RUPD" era))
  ) =>
  ToExpr (ShelleyNewEpochEvent era)

instance
  ( ToExpr (Event (EraRule "POOLREAP" era))
  , ToExpr (Event (EraRule "SNAP" era))
  , ToExpr (Event (EraRule "UPEC" era))
  ) =>
  ToExpr (ShelleyEpochEvent era)

instance ToExpr (ShelleyPoolreapEvent era)

instance ToExpr (SnapEvent era)

instance ToExpr (ShelleyMirEvent era)

instance ToExpr RupdEvent

instance (ToExpr (PParamsHKD Identity era), ToExpr (CertState era)) => ToExpr (UtxoEnv era)

instance ToExpr (PParamsHKD Identity era) => ToExpr (LedgerEnv era)

instance ToExpr (PParamsHKD Identity era) => ToExpr (ShelleyLedgersEnv era)

instance
  ToExpr (PredicateFailure (EraRule "LEDGER" era)) =>
  ToExpr (ShelleyLedgersPredFailure era)

instance (EraCertState era, ToExpr (Accounts era)) => ToExpr (ShelleyCertState era)

instance ToExpr (ShelleyAccountState era)

instance ToExpr (ShelleyAccounts era)

instance
  ToExpr (PredicateFailure (EraRule "LEDGERS" era)) =>
  ToExpr (ShelleyBbodyPredFailure era)

instance
  ToExpr (State (EraRule "LEDGERS" era)) =>
  ToExpr (ShelleyBbodyState era)

instance
  ( ToExpr (PredicateFailure (EraRule "NEWEPOCH" era))
  , ToExpr (PredicateFailure (EraRule "RUPD" era))
  ) =>
  ToExpr (ShelleyTickPredFailure era)

instance
  ( ToExpr (PredicateFailure (EraRule "EPOCH" era))
  , ToExpr (PredicateFailure (EraRule "MIR" era))
  ) =>
  ToExpr (ShelleyNewEpochPredFailure era)

instance
  ( ToExpr (UpecPredFailure era)
  , ToExpr (PredicateFailure (EraRule "POOLREAP" era))
  , ToExpr (PredicateFailure (EraRule "SNAP" era))
  ) =>
  ToExpr (ShelleyEpochPredFailure era)

instance ToExpr (ShelleyUpecPredFailure era)

instance ToExpr (ShelleyPoolreapPredFailure era)

instance ToExpr (ShelleySnapPredFailure era)

instance ToExpr (ShelleyMirPredFailure era)

instance ToExpr (ShelleyRupdPredFailure era)
