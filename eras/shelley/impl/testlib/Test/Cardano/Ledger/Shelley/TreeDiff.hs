{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Shelley.TreeDiff (
  module Test.Cardano.Ledger.TreeDiff,
) where

import Cardano.Ledger.AuxiliaryData
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Governance
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.PParams
import Cardano.Ledger.Shelley.PoolRank
import Cardano.Ledger.Shelley.Rules
import Cardano.Ledger.Shelley.Scripts
import Cardano.Ledger.Shelley.Tx
import Cardano.Ledger.Shelley.TxAuxData
import Cardano.Ledger.Shelley.TxBody
import Cardano.Ledger.Shelley.TxCert
import Cardano.Ledger.Shelley.TxOut
import Cardano.Ledger.Shelley.TxWits
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

instance ToExpr (ShelleyTxAuxData era)

-- Governance
instance
  ( ToExpr (PParamsUpdate era)
  , ToExpr (PParams era)
  ) =>
  ToExpr (ShelleyGovState era)

instance ToExpr (Constitution era)

-- TxCert
instance ToExpr (GenesisDelegCert c)
instance ToExpr MIRPot
instance ToExpr (MIRTarget c)
instance ToExpr (MIRCert c)
instance ToExpr (ShelleyTxCert era)
instance ToExpr (ShelleyDelegCert c)

-- TxWits
instance (Era era, ToExpr (Script era)) => ToExpr (ShelleyTxWits era)

-- Rules/Ppup
instance ToExpr VotingPeriod

instance ToExpr (ShelleyPpupPredFailure era)

-- TxOut
instance (EraTxOut era, ToExpr (Value era)) => ToExpr (ShelleyTxOut era) where
  toExpr (ShelleyTxOut x y) = App "ShelleyTxOut" [toExpr x, toExpr y]

-- TxBody
instance
  ( ToExpr (TxOut era)
  , ToExpr (TxCert era)
  , ToExpr (Update era)
  ) =>
  ToExpr (ShelleyTxBodyRaw era)

instance
  ( ToExpr (TxOut era)
  , ToExpr (TxCert era)
  , ToExpr (Update era)
  ) =>
  ToExpr (ShelleyTxBody era)

-- PoolRank
instance ToExpr Likelihood

instance ToExpr LogWeight

instance ToExpr (NonMyopic c)

-- Tx
instance
  ( ToExpr (TxAuxData era)
  , ToExpr (TxBody era)
  , ToExpr (TxWits era)
  ) =>
  ToExpr (ShelleyTxRaw era)

instance
  ( ToExpr (TxAuxData era)
  , ToExpr (TxBody era)
  , ToExpr (TxWits era)
  ) =>
  ToExpr (ShelleyTx era)

-- RewardUpdate

-- | You really don't want to see what is inside this.
instance ToExpr (PulsingRewUpdate c) where
  toExpr _ = App "PulsingRewUpdate..." []

-- LedgerState/Types
instance ToExpr AccountState

instance
  ( ToExpr (TxOut era)
  , ToExpr (PParams era)
  , ToExpr (StashedAVVMAddresses era)
  , ToExpr (GovState era)
  ) =>
  ToExpr (NewEpochState era)

instance
  ( ToExpr (TxOut era)
  , ToExpr (PParams era)
  , ToExpr (GovState era)
  ) =>
  ToExpr (EpochState era)

instance
  ( ToExpr (TxOut era)
  , ToExpr (GovState era)
  ) =>
  ToExpr (LedgerState era)

instance
  ( ToExpr (TxOut era)
  , ToExpr (GovState era)
  ) =>
  ToExpr (UTxOState era)

instance ToExpr (IncrementalStake c)

-- Rules/Utxo
instance
  ( ToExpr (PPUPPredFailure era)
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
  , ToExpr (AuxiliaryDataHash (EraCrypto era))
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
