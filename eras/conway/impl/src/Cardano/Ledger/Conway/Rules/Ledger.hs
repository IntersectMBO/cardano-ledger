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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Ledger (
  ConwayLEDGER,
  ConwayLedgerPredFailure (..),
  ConwayLedgerEvent (..),
) where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..))
import Cardano.Crypto.Hash.Class (Hash)
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxowEvent)
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript)
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded (..))
import Cardano.Ledger.Babbage.Rules (BabbageUtxowPredFailure)
import Cardano.Ledger.Babbage.Tx (IsValid (..))
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes (ShelleyBase, epochInfoPure)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Block (txid)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayCERTS, ConwayLEDGER, ConwayTALLY)
import Cardano.Ledger.Conway.Governance (
  ConwayGovernance (..),
  ConwayTallyState,
  GovernanceProcedure (..),
 )
import Cardano.Ledger.Conway.Rules.Certs (ConwayCertsEvent, ConwayCertsPredFailure)
import Cardano.Ledger.Conway.Rules.Tally (ConwayTallyPredFailure, TallyEnv (..))
import Cardano.Ledger.Conway.Rules.Utxow (ConwayUTXOW)
import Cardano.Ledger.Conway.Tx (AlonzoEraTx (..))
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.Shelley.LedgerState (
  CertState (..),
  DState (..),
  LedgerState (..),
  UTxOState (..),
  obligationCertState,
 )
import Cardano.Ledger.Shelley.Rules (
  DelegsEnv (DelegsEnv),
  DelplEnv,
  LedgerEnv (..),
  ShelleyLEDGERS,
  ShelleyLedgersEvent (..),
  ShelleyLedgersPredFailure (..),
  UtxoEnv (..),
 )
import Cardano.Ledger.Slot (epochInfoEpoch)
import Cardano.Ledger.UTxO (EraUTxO (..))
import Control.DeepSeq (NFData)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended (
  Assertion (..),
  AssertionViolation (..),
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  liftSTS,
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
  | ConwayCertsFailure (PredicateFailure (EraRule "CERTS" era))
  | ConwayTallyFailure (PredicateFailure (EraRule "TALLY" era)) -- Subtransition Failures
  deriving (Generic)

deriving instance
  ( Era era
  , Eq (PredicateFailure (EraRule "UTXOW" era))
  , Eq (PredicateFailure (EraRule "CERTS" era))
  , Eq (PredicateFailure (EraRule "TALLY" era))
  ) =>
  Eq (ConwayLedgerPredFailure era)

deriving instance
  ( Era era
  , Show (PredicateFailure (EraRule "UTXOW" era))
  , Show (PredicateFailure (EraRule "CERTS" era))
  , Show (PredicateFailure (EraRule "TALLY" era))
  ) =>
  Show (ConwayLedgerPredFailure era)

instance
  ( Era era
  , NoThunks (PredicateFailure (EraRule "UTXOW" era))
  , NoThunks (PredicateFailure (EraRule "CERTS" era))
  , NoThunks (PredicateFailure (EraRule "TALLY" era))
  ) =>
  NoThunks (ConwayLedgerPredFailure era)

instance
  ( Era era
  , NFData (PredicateFailure (EraRule "UTXOW" era))
  , NFData (PredicateFailure (EraRule "CERTS" era))
  , NFData (PredicateFailure (EraRule "TALLY" era))
  ) =>
  NFData (ConwayLedgerPredFailure era)

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "UTXOW" era))
  , EncCBOR (PredicateFailure (EraRule "CERTS" era))
  , EncCBOR (PredicateFailure (EraRule "TALLY" era))
  ) =>
  EncCBOR (ConwayLedgerPredFailure era)
  where
  encCBOR =
    encode . \case
      ConwayUtxowFailure x -> Sum (ConwayUtxowFailure @era) 1 !> To x
      ConwayCertsFailure x -> Sum (ConwayCertsFailure @era) 2 !> To x
      ConwayTallyFailure x -> Sum (ConwayTallyFailure @era) 3 !> To x

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "UTXOW" era))
  , DecCBOR (PredicateFailure (EraRule "CERTS" era))
  , DecCBOR (PredicateFailure (EraRule "TALLY" era))
  ) =>
  DecCBOR (ConwayLedgerPredFailure era)
  where
  decCBOR =
    decode $ Summands "ConwayLedgerPredFailure" $ \case
      1 -> SumD ConwayUtxowFailure <! From
      2 -> SumD ConwayCertsFailure <! From
      3 -> SumD ConwayTallyFailure <! From
      n -> Invalid n

data ConwayLedgerEvent era
  = UtxowEvent (Event (EraRule "UTXOW" era))
  | CertsEvent (Event (EraRule "CERTS" era))
  | TallyEvent (Event (EraRule "TALLY" era))

instance
  ( AlonzoEraTx era
  , ConwayEraTxBody era
  , GovernanceState era ~ ConwayGovernance era
  , Embed (EraRule "UTXOW" era) (ConwayLEDGER era)
  , Embed (EraRule "TALLY" era) (ConwayLEDGER era)
  , Embed (EraRule "CERTS" era) (ConwayLEDGER era)
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , State (EraRule "CERTS" era) ~ CertState era
  , State (EraRule "TALLY" era) ~ ConwayTallyState era
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , Environment (EraRule "CERTS" era) ~ DelegsEnv era
  , Environment (EraRule "TALLY" era) ~ TallyEnv era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , Signal (EraRule "CERTS" era) ~ Seq (TxCert era)
  , Signal (EraRule "TALLY" era) ~ Seq (GovernanceProcedure era)
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
           (LedgerState utxoSt certState) ->
              obligationCertState certState
                == utxosDeposited utxoSt
        )
    ]

-- =======================================

ledgerTransition ::
  forall (someLEDGER :: Type -> Type) era.
  ( AlonzoEraTx era
  , ConwayEraTxBody era
  , GovernanceState era ~ ConwayGovernance era
  , Signal (someLEDGER era) ~ Tx era
  , State (someLEDGER era) ~ LedgerState era
  , Environment (someLEDGER era) ~ LedgerEnv era
  , Embed (EraRule "UTXOW" era) (someLEDGER era)
  , Embed (EraRule "TALLY" era) (someLEDGER era)
  , Embed (EraRule "CERTS" era) (someLEDGER era)
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , State (EraRule "CERTS" era) ~ CertState era
  , State (EraRule "TALLY" era) ~ ConwayTallyState era
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , Environment (EraRule "TALLY" era) ~ TallyEnv era
  , Environment (EraRule "CERTS" era) ~ DelegsEnv era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , Signal (EraRule "CERTS" era) ~ Seq (TxCert era)
  , Signal (EraRule "TALLY" era) ~ Seq (GovernanceProcedure era)
  , BaseM (someLEDGER era) ~ ShelleyBase
  , STS (someLEDGER era)
  ) =>
  TransitionRule (someLEDGER era)
ledgerTransition = do
  TRC (LedgerEnv slot txIx pp account, LedgerState utxoSt certState, tx) <- judgmentContext
  let txBody = tx ^. bodyTxL

  certState' <-
    if tx ^. isValidTxL == IsValid True
      then
        trans @(EraRule "CERTS" era) $
          TRC
            ( DelegsEnv slot txIx pp tx account
            , certState
            , StrictSeq.fromStrict $ txBody ^. certsTxBodyL
            )
      else pure certState

  let dstate = certDState certState
      genCerts = dsGenDelegs dstate

  let govProcedures =
        (GovernanceVotingProcedure <$> txBody ^. votingProceduresTxBodyL)
          <> (GovernanceProposalProcedure <$> txBody ^. proposalProceduresTxBodyL)
  let govSt = utxosGovernance utxoSt
  epoch <- liftSTS $ do
    ei <- asks epochInfoPure
    epochInfoEpoch ei slot
  tallySt' <-
    trans @(EraRule "TALLY" era) $
      TRC
        ( TallyEnv (txid txBody) epoch $ cgVoterRoles govSt
        , cgTally govSt
        , StrictSeq.fromStrict govProcedures
        )

  utxoSt' <-
    trans @(EraRule "UTXOW" era) $
      TRC
        ( UtxoEnv @era slot pp certState genCerts
        , utxoSt {utxosGovernance = govSt {cgTally = tallySt'}}
        , tx
        )
  pure $ LedgerState utxoSt' certState'

instance
  ( Signable (DSIGN (EraCrypto era)) (Hash (HASH (EraCrypto era)) EraIndependentTxBody)
  , BaseM (ConwayUTXOW era) ~ ShelleyBase
  , AlonzoEraTx era
  , EraUTxO era
  , BabbageEraTxBody era
  , Embed (EraRule "UTXO" era) (ConwayUTXOW era)
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Environment (EraRule "UTXO" era) ~ UtxoEnv era
  , Script era ~ AlonzoScript era
  , TxOut era ~ BabbageTxOut era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (EraRule "UTXO" era) ~ Tx era
  , PredicateFailure (EraRule "UTXOW" era) ~ BabbageUtxowPredFailure era
  , Event (EraRule "UTXOW" era) ~ AlonzoUtxowEvent era
  , STS (ConwayUTXOW era)
  , PredicateFailure (ConwayUTXOW era) ~ BabbageUtxowPredFailure era
  , Event (ConwayUTXOW era) ~ AlonzoUtxowEvent era
  ) =>
  Embed (ConwayUTXOW era) (ConwayLEDGER era)
  where
  wrapFailed = ConwayUtxowFailure
  wrapEvent = UtxowEvent

instance
  ( EraTx era
  , ShelleyEraTxBody era
  , Embed (EraRule "CERT" era) (ConwayCERTS era)
  , State (EraRule "CERT" era) ~ CertState era
  , Environment (EraRule "CERT" era) ~ DelplEnv era
  , Signal (EraRule "CERT" era) ~ TxCert era
  , PredicateFailure (EraRule "CERTS" era) ~ ConwayCertsPredFailure era
  , Event (EraRule "CERTS" era) ~ ConwayCertsEvent era
  , EraRule "CERTS" era ~ ConwayCERTS era
  ) =>
  Embed (ConwayCERTS era) (ConwayLEDGER era)
  where
  wrapFailed = ConwayCertsFailure
  wrapEvent = CertsEvent

instance
  ( Embed (EraRule "UTXOW" era) (ConwayLEDGER era)
  , Embed (EraRule "CERTS" era) (ConwayLEDGER era)
  , Embed (EraRule "TALLY" era) (ConwayLEDGER era)
  , AlonzoEraTx era
  , ConwayEraTxBody era
  , GovernanceState era ~ ConwayGovernance era
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , Environment (EraRule "CERTS" era) ~ DelegsEnv era
  , Environment (EraRule "TALLY" era) ~ TallyEnv era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , Signal (EraRule "CERTS" era) ~ Seq (TxCert era)
  , Signal (EraRule "TALLY" era) ~ Seq (GovernanceProcedure era)
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , State (EraRule "CERTS" era) ~ CertState era
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
