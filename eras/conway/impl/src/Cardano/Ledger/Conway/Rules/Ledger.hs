{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Ledger (
  ConwayLEDGER,
  ConwayLedgerPredFailure (..),
) where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..))
import Cardano.Crypto.Hash.Class (Hash)
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxowEvent)
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript)
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded (..))
import Cardano.Ledger.Babbage.Core (BabbageEraTxBody)
import Cardano.Ledger.Babbage.Rules (BabbageUTXOW, BabbageUtxowPredFailure)
import Cardano.Ledger.Babbage.Tx (IsValid (..))
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Block (txid)
import Cardano.Ledger.Conway.Core (ConwayEraTxBody (..))
import Cardano.Ledger.Conway.Era (ConwayLEDGER, ConwayTALLY)
import Cardano.Ledger.Conway.Governance (ConwayTallyState)
import Cardano.Ledger.Conway.Rules.Tally (
  ConwayTallyPredFailure,
  GovernanceProcedure (..),
  TallyEnv (..),
 )
import Cardano.Ledger.Conway.Tx (AlonzoEraTx (..))
import Cardano.Ledger.Core (
  Era (..),
  EraIndependentTxBody,
  EraRule,
  EraScript (..),
  EraTx (..),
  EraTxOut (..),
 )
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.Shelley.Core (EraTallyState (..), ShelleyEraTxBody (..))
import Cardano.Ledger.Shelley.Delegation.Certificates (DCert)
import Cardano.Ledger.Shelley.LedgerState (
  DPState (..),
  DState (..),
  LedgerState (..),
  PPUPState,
  UTxOState (..),
  obligationDPState,
 )
import Cardano.Ledger.Shelley.Rules (
  DelegsEnv (..),
  DelplEnv (..),
  LedgerEnv (..),
  ShelleyDELEGS,
  ShelleyDelegsEvent,
  ShelleyDelegsPredFailure,
  ShelleyLEDGERS,
  ShelleyLedgersEvent (..),
  ShelleyLedgersPredFailure (..),
  UtxoEnv (..),
 )
import Cardano.Ledger.UTxO (EraUTxO (..))
import Control.State.Transition.Extended (
  Assertion (..),
  AssertionViolation (..),
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  trans,
 )
import Data.Kind (Type)
import Data.Sequence (Seq)
import qualified Data.Sequence.Strict as StrictSeq
import GHC.Generics (Generic (..))
import Lens.Micro ((^.))

data ConwayLedgerPredFailure era
  = ConwayUtxowFailure (PredicateFailure (EraRule "UTXOW" era))
  | ConwayDelegsFailure (PredicateFailure (EraRule "DELEGS" era))
  | ConwayTallyFailure (PredicateFailure (EraRule "TALLY" era)) -- Subtransition Failures
  deriving (Generic)

deriving instance
  ( Era era
  , Eq (PredicateFailure (EraRule "UTXOW" era))
  , Eq (PredicateFailure (EraRule "DELEGS" era))
  , Eq (PredicateFailure (EraRule "TALLY" era))
  ) =>
  Eq (ConwayLedgerPredFailure era)

deriving instance
  ( Era era
  , Show (PredicateFailure (EraRule "UTXOW" era))
  , Show (PredicateFailure (EraRule "DELEGS" era))
  , Show (PredicateFailure (EraRule "TALLY" era))
  ) =>
  Show (ConwayLedgerPredFailure era)

data ConwayLedgerEvent era
  = UtxowEvent (Event (EraRule "UTXOW" era))
  | DelegsEvent (Event (EraRule "DELEGS" era))
  | TallyEvent (Event (EraRule "TALLY" era))

instance
  ( Era era
  , Embed (EraRule "UTXOW" era) (ConwayLEDGER era)
  , Embed (EraRule "TALLY" era) (ConwayLEDGER era)
  , Embed (EraRule "DELEGS" era) (ConwayLEDGER era)
  , AlonzoEraTx era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , Environment (EraRule "DELEGS" era) ~ DelegsEnv era
  , State (EraRule "DELEGS" era) ~ DPState (EraCrypto era)
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , Signal (EraRule "DELEGS" era) ~ Seq (DCert (EraCrypto era))
  , Signal (EraRule "TALLY" era) ~ Seq (GovernanceProcedure era)
  , Environment (EraRule "TALLY" era) ~ TallyEnv era
  , State (EraRule "TALLY" era) ~ ConwayTallyState era
  , ConwayEraTxBody era
  , TallyState era ~ ConwayTallyState era
  , Show (PPUPState era)
  ) =>
  STS (ConwayLEDGER era)
  where
  type State (ConwayLEDGER era) = LedgerState era
  type Signal (ConwayLEDGER era) = Tx era
  type Environment (ConwayLEDGER era) = LedgerEnv era
  type BaseM (ConwayLEDGER era) = ShelleyBase
  type PredicateFailure (ConwayLEDGER era) = ConwayLedgerPredFailure era
  type Event (ConwayLEDGER era) = ConwayLedgerEvent era

  initialRules = []
  transitionRules = [ledgerTransition @ConwayLEDGER]

  renderAssertionViolation AssertionViolation {avSTS, avMsg, avCtx, avState} =
    "AssertionViolation ("
      <> avSTS
      <> "): "
      <> avMsg
      <> "\n"
      <> show avCtx
      <> "\n"
      <> show avState

  assertions =
    [ PostCondition
        "Deposit pot must equal obligation"
        ( \(TRC (_, _, _))
           (LedgerState utxoSt dpstate _tallySt) ->
              obligationDPState dpstate
                == utxosDeposited utxoSt
        )
    ]

-- =======================================

ledgerTransition ::
  forall (someLEDGER :: Type -> Type) era.
  ( Signal (someLEDGER era) ~ Tx era
  , State (someLEDGER era) ~ LedgerState era
  , Environment (someLEDGER era) ~ LedgerEnv era
  , Embed (EraRule "UTXOW" era) (someLEDGER era)
  , Embed (EraRule "TALLY" era) (someLEDGER era)
  , Embed (EraRule "DELEGS" era) (someLEDGER era)
  , Environment (EraRule "DELEGS" era) ~ DelegsEnv era
  , State (EraRule "DELEGS" era) ~ DPState (EraCrypto era)
  , Signal (EraRule "DELEGS" era) ~ Seq (DCert (EraCrypto era))
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , Signal (EraRule "TALLY" era) ~ Seq (GovernanceProcedure era)
  , AlonzoEraTx era
  , Environment (EraRule "TALLY" era) ~ TallyEnv era
  , State (EraRule "TALLY" era) ~ ConwayTallyState era
  , ConwayEraTxBody era
  , TallyState era ~ ConwayTallyState era
  ) =>
  TransitionRule (someLEDGER era)
ledgerTransition = do
  TRC (LedgerEnv slot txIx pp account, LedgerState utxoSt dpstate tallySt, tx) <- judgmentContext
  let txBody = tx ^. bodyTxL

  dpstate' <-
    if tx ^. isValidTxL == IsValid True
      then
        trans @(EraRule "DELEGS" era) $
          TRC
            ( DelegsEnv slot txIx pp tx account
            , dpstate
            , StrictSeq.fromStrict $ txBody ^. certsTxBodyG
            )
      else pure dpstate

  let DPState dstate _pstate = dpstate
      genDelegs = dsGenDelegs dstate

  let govProcedures =
        mconcat
          [ fmap ProposalProcedure $ txBody ^. govActionsTxBodyL
          , fmap VotingProcedure $ txBody ^. votesTxBodyL
          ]
  tallySt' <-
    trans @(EraRule "TALLY" era) $
      TRC
        ( TallyEnv $ txid txBody
        , tallySt
        , StrictSeq.fromStrict govProcedures
        )

  utxoSt' <-
    trans @(EraRule "UTXOW" era) $
      TRC
        ( UtxoEnv @era slot pp dpstate genDelegs
        , utxoSt
        , tx
        )
  pure $ LedgerState utxoSt' dpstate' tallySt'

instance
  ( Signable (DSIGN (EraCrypto era)) (Hash (HASH (EraCrypto era)) EraIndependentTxBody)
  , BaseM (BabbageUTXOW era) ~ ShelleyBase
  , AlonzoEraTx era
  , EraUTxO era
  , BabbageEraTxBody era
  , Embed (EraRule "UTXO" era) (BabbageUTXOW era)
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Environment (EraRule "UTXO" era) ~ UtxoEnv era
  , Script era ~ AlonzoScript era
  , TxOut era ~ BabbageTxOut era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (EraRule "UTXO" era) ~ Tx era
  , PredicateFailure (EraRule "UTXOW" era) ~ BabbageUtxowPredFailure era
  , Event (EraRule "UTXOW" era) ~ AlonzoUtxowEvent era
  , STS (BabbageUTXOW era)
  , PredicateFailure (BabbageUTXOW era) ~ BabbageUtxowPredFailure era
  , Event (BabbageUTXOW era) ~ AlonzoUtxowEvent era
  ) =>
  Embed (BabbageUTXOW era) (ConwayLEDGER era)
  where
  wrapFailed = ConwayUtxowFailure
  wrapEvent = UtxowEvent

instance
  ( EraTx era
  , ShelleyEraTxBody era
  , Embed (EraRule "DELPL" era) (ShelleyDELEGS era)
  , State (EraRule "DELPL" era) ~ DPState (EraCrypto era)
  , Environment (EraRule "DELPL" era) ~ DelplEnv era
  , Signal (EraRule "DELPL" era) ~ DCert (EraCrypto era)
  , PredicateFailure (EraRule "DELEGS" era) ~ ShelleyDelegsPredFailure era
  , Event (EraRule "DELEGS" era) ~ ShelleyDelegsEvent era
  ) =>
  Embed (ShelleyDELEGS era) (ConwayLEDGER era)
  where
  wrapFailed = ConwayDelegsFailure
  wrapEvent = DelegsEvent

instance
  ( Embed (EraRule "UTXOW" era) (ConwayLEDGER era)
  , Embed (EraRule "DELEGS" era) (ConwayLEDGER era)
  , Embed (EraRule "TALLY" era) (ConwayLEDGER era)
  , AlonzoEraTx era
  , Show (PPUPState era)
  , ConwayEraTxBody era
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , Environment (EraRule "DELEGS" era) ~ DelegsEnv era
  , Environment (EraRule "TALLY" era) ~ TallyEnv era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , Signal (EraRule "DELEGS" era) ~ Seq (DCert (EraCrypto era))
  , Signal (EraRule "TALLY" era) ~ Seq (GovernanceProcedure era)
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , State (EraRule "DELEGS" era) ~ DPState (EraCrypto era)
  , State (EraRule "TALLY" era) ~ ConwayTallyState era
  , TallyState era ~ ConwayTallyState era
  , PredicateFailure (EraRule "LEDGER" era) ~ ConwayLedgerPredFailure era
  , Event (EraRule "LEDGER" era) ~ ConwayLedgerEvent era
  ) =>
  Embed (ConwayLEDGER era) (ShelleyLEDGERS era)
  where
  wrapFailed = LedgerFailure
  wrapEvent = LedgerEvent

instance
  ( Era era
  , BaseM (ConwayLEDGER era) ~ ShelleyBase
  , PredicateFailure (EraRule "TALLY" era) ~ ConwayTallyPredFailure era
  , Event (EraRule "TALLY" era) ~ ()
  ) =>
  Embed (ConwayTALLY era) (ConwayLEDGER era)
  where
  wrapFailed = ConwayTallyFailure
  wrapEvent = TallyEvent
