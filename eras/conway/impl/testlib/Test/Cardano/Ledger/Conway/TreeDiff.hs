{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.TreeDiff (
  module Test.Cardano.Ledger.Babbage.TreeDiff,
) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.TxBody
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Conway.TxInfo (ContextError)
import Cardano.Ledger.Crypto
import Cardano.Ledger.HKD
import Data.Functor.Identity
import Test.Cardano.Data.TreeDiff ()
import Test.Cardano.Ledger.Babbage.TreeDiff

-- Core
instance ToExpr PoolVotingThresholds

instance ToExpr DRepVotingThresholds

-- Scripts
instance ToExpr (PlutusScript (ConwayEra c))

-- PlutusContext
instance ToExpr (ContextError (ConwayEra c))

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

instance ToExpr (PrevGovActionId r ci)

instance ToExpr (PParamsHKD StrictMaybe era) => ToExpr (GovAction era)

-- Governance/Proposals
instance ToExpr (PParamsHKD StrictMaybe era) => ToExpr (Proposals era)

-- PParams
instance ToExpr (HKD f a) => ToExpr (THKD t f a) where
  toExpr = toExpr . unTHKD

instance ToExpr (ConwayPParams Identity era)

instance ToExpr (ConwayPParams StrictMaybe era)

-- Governance
instance (EraPParams era, ToExpr (PParamsHKD StrictMaybe era)) => ToExpr (PulsingSnapshot era)

instance ToExpr (PrevGovActionIds era)

instance ToExpr (PrevGovActionIdsChildren era)

instance ToExpr (PParamsHKD Identity era) => ToExpr (EnactState era)

instance (EraPParams era, ToExpr (PParamsHKD Identity era)) => ToExpr (RatifyState era)

instance (EraPParams era, ToExpr (PParamsHKD Identity era), ToExpr (PParamsHKD StrictMaybe era)) => ToExpr (ConwayGovState era)

instance (EraPParams era, ToExpr (PParamsHKD StrictMaybe era), ToExpr (PParamsHKD Identity era)) => ToExpr (DRepPulsingState era) where
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

-- Rules/GovCert
instance ToExpr (ConwayGovCertPredFailure era)

-- Rules/Delegs
instance ToExpr (ConwayDelegPredFailure era)

-- TxBody
instance (EraPParams era, ToExpr (PParamsHKD StrictMaybe era), ToExpr (TxOut era)) => ToExpr (ConwayTxBodyRaw era)

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
