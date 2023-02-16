{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Pretty.Conway () where

import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Delegation.Certificates (ConwayDCert (..), transDCert)
import Cardano.Ledger.Conway.Governance (
  ConwayTallyState (..),
  GovernanceAction (..),
  GovernanceActionId (..),
  GovernanceActionInfo (..),
  GovernanceActionIx (..),
  GovernanceActionState (..),
  Vote (..),
  VoterRole (..), VoteDecision,
 )
import Cardano.Ledger.Conway.Rules (ConwayLedgerPredFailure (..), ConwayTallyPredFailure (..), PredicateFailure)
import Cardano.Ledger.Conway.TxBody (ConwayTxBody (..))
import Cardano.Ledger.Pretty
import Cardano.Ledger.Pretty.Babbage ()
import Prettyprinter (viaShow)
import Lens.Micro ((^.))

instance
  ( ConwayEraTxBody era
  , PrettyA (TxOut era)
  , TxBody era ~ ConwayTxBody era
  , PrettyA (PParamsUpdate era)
  ) =>
  PrettyA (ConwayTxBody era) where
    prettyA txb = ppRecord
      "TxBody (Conway)"
      [ ("spending inputs", prettyA $ txb ^. inputsTxBodyL)
      , ("collateral inputs", prettyA $ txb ^. collateralInputsTxBodyL)
      , ("reference inputs", prettyA $ txb ^. referenceInputsTxBodyL)
      , ("outputs", prettyA $ txb ^. outputsTxBodyL)
      , ("collateral return", prettyA $ txb ^. collateralReturnTxBodyL)
      , ("total collateral", prettyA $ txb ^. totalCollateralTxBodyL)
      , ("certificates", prettyA $ txb ^. conwayCertsTxBodyL)
      , ("withdrawals", prettyA $ txb ^. withdrawalsTxBodyL)
      , ("transaction fee", prettyA $ txb ^. feeTxBodyL)
      , ("validity interval", prettyA $ txb ^. vldtTxBodyL)
      , ("required signer hashes", prettyA $ txb ^. reqSignerHashesTxBodyL)
      , ("mint", prettyA $ txb ^. mintTxBodyL)
      , ("script integrity hash", prettyA $ txb ^. scriptIntegrityHashTxBodyL)
      , ("auxiliary data hash", prettyA $ txb ^. auxDataHashTxBodyL)
      , ("network id", prettyA $ txb ^. networkIdTxBodyL)
      , ("governance actions", prettyA $ txb ^. govActionsTxBodyL)
      , ("votes", prettyA $ txb ^. votesTxBodyL)
      ]

instance PrettyA VoteDecision

instance PrettyA (Vote era)

instance PrettyA (PParamsUpdate era) => PrettyA (GovernanceAction era)

instance PrettyA (PParamsUpdate era) => PrettyA (GovernanceActionInfo era)

instance forall c. PrettyA (ConwayDCert c) where
  prettyA = prettyA . transDCert

instance
  ( PrettyA (PredicateFailure (EraRule "UTXOW" era))
  , PrettyA (PredicateFailure (EraRule "DELEGS" era))
  , PrettyA (PredicateFailure (EraRule "TALLY" era))
  ) =>
  PrettyA (ConwayLedgerPredFailure era)
  where
  prettyA (ConwayUtxowFailure x) = prettyA x
  prettyA (ConwayDelegsFailure x) = prettyA x
  prettyA (ConwayTallyFailure x) = prettyA x

instance PrettyA (ConwayTallyPredFailure era)

instance PrettyA (PParamsUpdate era) => PrettyA (ConwayTallyState era) where

instance PrettyA (GovernanceActionId era) where

instance PrettyA GovernanceActionIx where

instance PrettyA VoterRole where
  prettyA = viaShow

instance PrettyA (PParamsUpdate era) => PrettyA (GovernanceActionState era) where
