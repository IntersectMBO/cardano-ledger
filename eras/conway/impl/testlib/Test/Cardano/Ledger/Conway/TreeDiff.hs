{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.TreeDiff (
  module Test.Cardano.Ledger.Babbage.TreeDiff,
) where

import Cardano.Ledger.Alonzo.Plutus.Context (ContextError)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.Scripts
import Cardano.Ledger.Conway.TxBody
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Conway.TxInfo (ConwayContextError)
import Cardano.Ledger.Crypto
import Cardano.Ledger.HKD
import Control.State.Transition.Extended (STS (..))
import Data.Functor.Identity
import Test.Cardano.Data.TreeDiff ()
import Test.Cardano.Ledger.Babbage.TreeDiff

-- Core
instance ToExpr PoolVotingThresholds

instance ToExpr DRepVotingThresholds

-- Scripts
instance ToExpr (PlutusScript (ConwayEra c))

instance ToExpr (ConwayPlutusPurpose AsIx era)

instance
  ( Era era
  , ToExpr (TxCert era)
  , ToExpr (PParamsHKD StrictMaybe era)
  ) =>
  ToExpr (ConwayPlutusPurpose AsItem era)

-- PlutusContext
instance
  ( Era era
  , ToExpr (PParamsHKD StrictMaybe era)
  , ToExpr (TxCert era)
  , ToExpr (PlutusPurpose AsIx era)
  , ToExpr (PlutusPurpose AsItem era)
  ) =>
  ToExpr (ConwayContextError era)

-- Governance/Procedure
instance ToExpr GovActionIx

instance ToExpr (GovActionId c)

instance ToExpr (PParamsHKD StrictMaybe era) => ToExpr (GovActionState era)

instance Crypto c => ToExpr (Voter c)

instance ToExpr Vote

instance Era era => ToExpr (VotingProcedures era)

instance ToExpr (VotingProcedure era)

instance ToExpr (PParamsHKD StrictMaybe era) => ToExpr (ProposalProcedure era)

instance ToExpr (Committee era)

instance ToExpr (GovPurposeId purpose era)

instance ToExpr (PParamsHKD StrictMaybe era) => ToExpr (GovAction era)

-- Governance/Proposals
instance ToExpr a => ToExpr (PRoot a)

instance ToExpr a => ToExpr (PEdges a)

instance ToExpr a => ToExpr (PGraph a)

instance
  (forall p. ToExpr (f (GovPurposeId (p :: GovActionPurpose) era))) =>
  ToExpr (GovRelation f era)

instance (Era era, ToExpr (PParamsHKD StrictMaybe era)) => ToExpr (Proposals era)

-- PParams
instance ToExpr (HKD f a) => ToExpr (THKD t f a) where
  toExpr = toExpr . unTHKD

instance ToExpr (ConwayPParams Identity era)

instance ToExpr (ConwayPParams StrictMaybe era)

-- Governance
instance (EraPParams era, ToExpr (PParamsHKD StrictMaybe era)) => ToExpr (PulsingSnapshot era)

instance (Era era, ToExpr (PParams era)) => ToExpr (EnactState era)

instance
  ( EraPParams era
  , ToExpr (PParams era)
  , ToExpr (PParamsHKD StrictMaybe era)
  ) =>
  ToExpr (RatifyState era)

instance
  (EraPParams era, ToExpr (PParams era), ToExpr (PParamsHKD StrictMaybe era)) =>
  ToExpr (ConwayGovState era)

instance
  (EraPParams era, ToExpr (PParamsHKD StrictMaybe era), ToExpr (PParams era)) =>
  ToExpr (DRepPulsingState era)
  where
  toExpr (DRComplete x y) = App "DRComplete" [toExpr x, toExpr y]
  toExpr x@(DRPulsing (DRepPulser {})) = App "DRComplete" [toExpr a, toExpr b]
    where
      (a, b) = finishDRepPulser x

instance ToExpr (RatifyEnv era) where
  toExpr (RatifyEnv stake pool drep dstate ep cs) =
    App "RatifyEnv" [toExpr stake, toExpr pool, toExpr drep, toExpr dstate, toExpr ep, toExpr cs]

-- Rules/Gov
instance (EraPParams era, ToExpr (PParamsHKD StrictMaybe era)) => ToExpr (ConwayGovPredFailure era)

-- TxCert
instance ToExpr (Delegatee c)

instance ToExpr (ConwayDelegCert c)

instance ToExpr (ConwayGovCert c)

instance ToExpr (ConwayTxCert c)

-- Rules
instance ToExpr (ConwayGovCertPredFailure era)

instance ToExpr (ConwayDelegPredFailure era)

instance
  ( ToExpr (PlutusPurpose AsItem era)
  , ToExpr (ContextError era)
  , ToExpr (TxCert era)
  ) =>
  ToExpr (ConwayUtxosPredFailure era)

-- TxBody
instance
  (EraPParams era, ToExpr (PParamsHKD StrictMaybe era), ToExpr (TxOut era)) =>
  ToExpr (ConwayTxBodyRaw era)

instance
  (EraPParams era, ToExpr (PParamsHKD StrictMaybe era), ToExpr (TxOut era)) =>
  ToExpr (ConwayTxBody era)

-- Rules/Cert
instance
  ( ToExpr (PredicateFailure (EraRule "DELEG" era))
  , ToExpr (PredicateFailure (EraRule "GOVCERT" era))
  , ToExpr (PredicateFailure (EraRule "POOL" era))
  ) =>
  ToExpr (ConwayCertPredFailure era)

-- Rules/Certs
instance
  ToExpr (PredicateFailure (EraRule "CERT" era)) =>
  ToExpr (ConwayCertsPredFailure era)

-- Rules/Ledger
instance
  ( ToExpr (PredicateFailure (EraRule "UTXOW" era))
  , ToExpr (PredicateFailure (EraRule "GOV" era))
  , ToExpr (PredicateFailure (EraRule "CERTS" era))
  ) =>
  ToExpr (ConwayLedgerPredFailure era)

instance ToExpr (Constitution era)

instance
  ( ToExpr (Event (EraRule "EPOCH" era))
  , ToExpr (Event (EraRule "RUPD" era))
  ) =>
  ToExpr (ConwayNewEpochEvent era)

instance
  ( EraPParams era
  , ToExpr (PParams era)
  , ToExpr (PParamsHKD StrictMaybe era)
  , ToExpr (Event (EraRule "POOLREAP" era))
  , ToExpr (Event (EraRule "SNAP" era))
  ) =>
  ToExpr (ConwayEpochEvent era)

instance
  ( ToExpr (Event (EraRule "CERTS" era))
  , ToExpr (Event (EraRule "UTXOW" era))
  , ToExpr (Event (EraRule "GOV" era))
  , ToExpr (Event (EraRule "MEMPOOL" era))
  ) =>
  ToExpr (ConwayLedgerEvent era)

instance
  ToExpr (Event (EraRule "CERT" era)) =>
  ToExpr (ConwayCertsEvent era)

instance
  ( ToExpr (Event (EraRule "DELEG" era))
  , ToExpr (Event (EraRule "GOVCERT" era))
  , ToExpr (Event (EraRule "POOL" era))
  ) =>
  ToExpr (ConwayCertEvent era)

instance ToExpr (TxOut era) => ToExpr (ConwayUtxosEvent era)

instance
  ( Era era
  , ToExpr (PParamsHKD StrictMaybe era)
  ) =>
  ToExpr (ConwayGovEvent era)

instance
  ( EraPParams era
  , ToExpr (PParamsHKD StrictMaybe era)
  , ToExpr (TxCert era)
  ) =>
  ToExpr (GovSignal era)

instance ToExpr (PParams era) => ToExpr (GovEnv era)

instance
  ( ToExpr (PParams era)
  , ToExpr (PParamsHKD StrictMaybe era)
  ) =>
  ToExpr (ConwayGovCertEnv era)

instance
  ( ToExpr (Value era)
  , ToExpr (TxOut era)
  , ToExpr (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  ToExpr (ConwayUtxoPredFailure era)

instance
  ( Era era
  , ToExpr (PredicateFailure (EraRule "UTXO" era))
  , ToExpr (PlutusPurpose AsIx era)
  , ToExpr (PlutusPurpose AsItem era)
  , ToExpr (TxCert era)
  ) =>
  ToExpr (ConwayUtxowPredFailure era)

instance
  ( ToExpr (PParams era)
  , ToExpr (PParamsHKD StrictMaybe era)
  ) =>
  ToExpr (CertEnv era)

instance ToExpr (PParams era) => ToExpr (ConwayDelegEnv era)

instance ToExpr (PParamsHKD StrictMaybe era) => ToExpr (RatifySignal era)

instance ToExpr (PParamsHKD StrictMaybe era) => ToExpr (EnactSignal era)

instance ToExpr (ConwayNewEpochPredFailure era)

instance
  ( ToExpr (PParamsHKD Identity era)
  , ToExpr (PParamsHKD StrictMaybe era)
  , ToExpr (Tx era)
  ) =>
  ToExpr (CertsEnv era)

instance ToExpr (ConwayMempoolEvent era)
