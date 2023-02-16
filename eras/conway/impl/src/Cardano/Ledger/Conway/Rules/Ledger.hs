{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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
import Cardano.Ledger.Babbage.Rules (BabbageUTXOW, BabbageUtxowPredFailure)
import Cardano.Ledger.Babbage.Tx (IsValid (..))
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Block (txid)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayLEDGER, ConwayTALLY)
import Cardano.Ledger.Conway.Governance (ConwayTallyState)
import Cardano.Ledger.Conway.Rules.Tally (
  ConwayTallyPredFailure,
  GovernanceProcedure (..),
  TallyEnv (..),
 )
import Cardano.Ledger.Conway.Tx (AlonzoEraTx (..))
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.Shelley.Delegation.Certificates (DCert)
import Cardano.Ledger.Shelley.LedgerState (
  DPState (..),
  DState (..),
  LedgerState (..),
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
import Control.DeepSeq (NFData)
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
import NoThunks.Class (NoThunks (..))

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

instance
  ( Era era
  , NoThunks (PredicateFailure (EraRule "UTXOW" era))
  , NoThunks (PredicateFailure (EraRule "DELEGS" era))
  , NoThunks (PredicateFailure (EraRule "TALLY" era))
  ) =>
  NoThunks (ConwayLedgerPredFailure era)

instance
  ( Era era
  , NFData (PredicateFailure (EraRule "UTXOW" era))
  , NFData (PredicateFailure (EraRule "DELEGS" era))
  , NFData (PredicateFailure (EraRule "TALLY" era))
  ) =>
  NFData (ConwayLedgerPredFailure era)

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "UTXOW" era))
  , EncCBOR (PredicateFailure (EraRule "DELEGS" era))
  , EncCBOR (PredicateFailure (EraRule "TALLY" era))
  ) =>
  EncCBOR (ConwayLedgerPredFailure era)
  where
  encCBOR =
    encode . \case
      ConwayUtxowFailure x -> Sum (ConwayUtxowFailure @era) 1 !> To x
      ConwayDelegsFailure x -> Sum (ConwayDelegsFailure @era) 2 !> To x
      ConwayTallyFailure x -> Sum (ConwayTallyFailure @era) 3 !> To x

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "UTXOW" era))
  , DecCBOR (PredicateFailure (EraRule "DELEGS" era))
  , DecCBOR (PredicateFailure (EraRule "TALLY" era))
  ) =>
  DecCBOR (ConwayLedgerPredFailure era)
  where
  decCBOR =
    decode $ Summands "ConwayLedgerPredFailure" $ \case
      1 -> SumD ConwayUtxowFailure <! From
      2 -> SumD ConwayDelegsFailure <! From
      3 -> SumD ConwayTallyFailure <! From
      n -> Invalid n

data ConwayLedgerEvent era
  = UtxowEvent (Event (EraRule "UTXOW" era))
  | DelegsEvent (Event (EraRule "DELEGS" era))
  | TallyEvent (Event (EraRule "TALLY" era))

instance
  ( AlonzoEraTx era
  , ConwayEraTxBody era
  , GovernanceState era ~ ConwayTallyState era
  , Embed (EraRule "UTXOW" era) (ConwayLEDGER era)
  , Embed (EraRule "TALLY" era) (ConwayLEDGER era)
  , Embed (EraRule "DELEGS" era) (ConwayLEDGER era)
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , Environment (EraRule "DELEGS" era) ~ DelegsEnv era
  , State (EraRule "DELEGS" era) ~ DPState (EraCrypto era)
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , Signal (EraRule "DELEGS" era) ~ Seq (DCert (EraCrypto era))
  , Signal (EraRule "TALLY" era) ~ Seq (GovernanceProcedure era)
  , Environment (EraRule "TALLY" era) ~ TallyEnv era
  , State (EraRule "TALLY" era) ~ ConwayTallyState era
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
           (LedgerState utxoSt dpstate) ->
              obligationDPState dpstate
                == utxosDeposited utxoSt
        )
    ]

-- =======================================

ledgerTransition ::
  forall (someLEDGER :: Type -> Type) era.
  ( AlonzoEraTx era
  , ConwayEraTxBody era
  , GovernanceState era ~ ConwayTallyState era
  , Signal (someLEDGER era) ~ Tx era
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
  , Environment (EraRule "TALLY" era) ~ TallyEnv era
  , State (EraRule "TALLY" era) ~ GovernanceState era
  ) =>
  TransitionRule (someLEDGER era)
ledgerTransition = do
  TRC (LedgerEnv slot txIx pp account, LedgerState utxoSt dpstate, tx) <- judgmentContext
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
        , utxosGovernance utxoSt
        , StrictSeq.fromStrict govProcedures
        )

  utxoSt' <-
    trans @(EraRule "UTXOW" era) $
      TRC
        ( UtxoEnv @era slot pp dpstate genDelegs
        , utxoSt {utxosGovernance = tallySt'}
        , tx
        )
  pure $ LedgerState utxoSt' dpstate'

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
  , ConwayEraTxBody era
  , GovernanceState era ~ ConwayTallyState era
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , Environment (EraRule "DELEGS" era) ~ DelegsEnv era
  , Environment (EraRule "TALLY" era) ~ TallyEnv era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , Signal (EraRule "DELEGS" era) ~ Seq (DCert (EraCrypto era))
  , Signal (EraRule "TALLY" era) ~ Seq (GovernanceProcedure era)
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , State (EraRule "DELEGS" era) ~ DPState (EraCrypto era)
  , State (EraRule "TALLY" era) ~ ConwayTallyState era
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
