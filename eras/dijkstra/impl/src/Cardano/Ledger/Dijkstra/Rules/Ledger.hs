{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.Ledger (
  LEDGER,
  DijkstraLedgerPredFailure (..),
  DijkstraLedgerEvent (..),
  shelleyToDijkstraLedgerPredFailure,
  conwayToDijkstraLedgerPredFailure,
) where

import qualified Cardano.Ledger.Allegra.Rules as Allegra
import Cardano.Ledger.Alonzo (AlonzoScript)
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded)
import Cardano.Ledger.Babbage (BabbageTxOut)
import qualified Cardano.Ledger.Babbage.Rules as Babbage
import Cardano.Ledger.BaseTypes (
  Mismatch (..),
  Relation (..),
  ShelleyBase,
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (
  ConwayEraGov (..),
  ConwayGovState,
  Proposals,
  constitutionGuardrailsScriptHashL,
  grCommitteeL,
  proposalsWithPurpose,
 )
import Cardano.Ledger.Conway.PParams (ppMaxRefScriptSizePerTxG)
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  ENTITIES,
  GOV,
  LEDGER,
  UTXOW,
 )
import Cardano.Ledger.Dijkstra.Rules.Certs ()
import Cardano.Ledger.Dijkstra.Rules.Entities (
  EntitiesEnv (..),
  EntitiesEvent,
  EntitiesPredFailure (..),
 )
import Cardano.Ledger.Dijkstra.Rules.Gov (DijkstraGovPredFailure)
import Cardano.Ledger.Dijkstra.Rules.GovCert (DijkstraGovCertPredFailure)
import Cardano.Ledger.Dijkstra.Rules.SubLedger
import Cardano.Ledger.Dijkstra.Rules.SubLedgers
import Cardano.Ledger.Dijkstra.Rules.Utxo (DijkstraUtxoEnv (..), DijkstraUtxoPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Utxow (DijkstraUtxowPredFailure)
import Cardano.Ledger.Dijkstra.TxBody
import Cardano.Ledger.Dijkstra.UTxO (DijkstraEraUTxO (..), batchNonDistinctRefScriptsSize)
import Cardano.Ledger.Rules.ValidationMode (Test, runTest)
import Cardano.Ledger.Shelley.LedgerState (
  LedgerState (..),
  UTxOState (..),
  lsUTxOStateL,
  utxosGovStateL,
  utxosUtxo,
 )
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.Slot (epochFromSlot)
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended
import Data.Sequence (Seq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Word (Word32)
import GHC.Generics (Generic (..))
import Lens.Micro
import Validation (failureUnless)

data DijkstraLedgerPredFailure era
  = DijkstraUtxowFailure (PredicateFailure (EraRule "UTXOW" era))
  | DijkstraEntitiesFailure (PredicateFailure (EraRule "ENTITIES" era))
  | DijkstraGovFailure (PredicateFailure (EraRule "GOV" era))
  | DijkstraTreasuryValueMismatch (Mismatch RelEQ Coin)
  | DijkstraTxRefScriptsSizeTooBig (Mismatch RelLTEQ Int)
  | DijkstraSubLedgersFailure (PredicateFailure (EraRule "SUBLEDGERS" era))
  deriving (Generic)

type instance EraRuleFailure "LEDGER" DijkstraEra = DijkstraLedgerPredFailure DijkstraEra

type instance EraRuleEvent "LEDGER" DijkstraEra = DijkstraLedgerEvent DijkstraEra

instance InjectRuleEvent "LEDGER" DijkstraLedgerEvent DijkstraEra

instance InjectRuleFailure "LEDGER" DijkstraLedgerPredFailure DijkstraEra

instance InjectRuleFailure "LEDGER" Conway.ConwayLedgerPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraLedgerPredFailure

instance InjectRuleFailure "LEDGER" Shelley.ShelleyLedgerPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraLedgerPredFailure

instance InjectRuleFailure "LEDGER" DijkstraUtxowPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure

instance InjectRuleFailure "LEDGER" Conway.ConwayUtxowPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" Babbage.BabbageUtxowPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" Alonzo.AlonzoUtxowPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" Shelley.ShelleyUtxowPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" DijkstraUtxoPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" Conway.ConwayUtxoPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" Babbage.BabbageUtxoPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" Alonzo.AlonzoUtxoPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" Alonzo.AlonzoUtxosPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" Conway.ConwayUtxosPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" Shelley.ShelleyUtxoPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" Allegra.AllegraUtxoPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" EntitiesPredFailure DijkstraEra where
  injectFailure = DijkstraEntitiesFailure

instance InjectRuleFailure "LEDGER" Conway.ConwayCertsPredFailure DijkstraEra where
  injectFailure = DijkstraEntitiesFailure . injectFailure

instance InjectRuleFailure "LEDGER" Conway.ConwayCertPredFailure DijkstraEra where
  injectFailure = DijkstraEntitiesFailure . injectFailure

instance InjectRuleFailure "LEDGER" Conway.ConwayDelegPredFailure DijkstraEra where
  injectFailure = DijkstraEntitiesFailure . injectFailure

instance InjectRuleFailure "LEDGER" Shelley.ShelleyPoolPredFailure DijkstraEra where
  injectFailure = DijkstraEntitiesFailure . injectFailure

instance InjectRuleFailure "LEDGER" DijkstraGovCertPredFailure DijkstraEra where
  injectFailure = DijkstraEntitiesFailure . injectFailure

instance InjectRuleFailure "LEDGER" DijkstraGovPredFailure DijkstraEra where
  injectFailure = DijkstraGovFailure . injectFailure

instance InjectRuleFailure "LEDGER" Conway.ConwayGovCertPredFailure DijkstraEra where
  injectFailure = DijkstraEntitiesFailure . injectFailure

instance InjectRuleFailure "LEDGER" Conway.ConwayGovPredFailure DijkstraEra where
  injectFailure = DijkstraGovFailure . injectFailure

instance InjectRuleFailure "LEDGER" DijkstraSubLedgersPredFailure DijkstraEra where
  injectFailure = DijkstraSubLedgersFailure . injectFailure

deriving instance
  ( Era era
  , Eq (PredicateFailure (EraRule "UTXOW" era))
  , Eq (PredicateFailure (EraRule "ENTITIES" era))
  , Eq (PredicateFailure (EraRule "GOV" era))
  , Eq (PredicateFailure (EraRule "SUBLEDGERS" era))
  ) =>
  Eq (DijkstraLedgerPredFailure era)

deriving instance
  ( Era era
  , Show (PredicateFailure (EraRule "UTXOW" era))
  , Show (PredicateFailure (EraRule "ENTITIES" era))
  , Show (PredicateFailure (EraRule "GOV" era))
  , Show (PredicateFailure (EraRule "SUBLEDGERS" era))
  ) =>
  Show (DijkstraLedgerPredFailure era)

instance
  ( Era era
  , NFData (PredicateFailure (EraRule "UTXOW" era))
  , NFData (PredicateFailure (EraRule "ENTITIES" era))
  , NFData (PredicateFailure (EraRule "GOV" era))
  , NFData (PredicateFailure (EraRule "SUBLEDGERS" era))
  ) =>
  NFData (DijkstraLedgerPredFailure era)

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "UTXOW" era))
  , EncCBOR (PredicateFailure (EraRule "ENTITIES" era))
  , EncCBOR (PredicateFailure (EraRule "GOV" era))
  , EncCBOR (PredicateFailure (EraRule "SUBLEDGERS" era))
  ) =>
  EncCBOR (DijkstraLedgerPredFailure era)
  where
  encCBOR =
    encode . \case
      DijkstraUtxowFailure x -> Sum (DijkstraUtxowFailure @era) 1 !> To x
      DijkstraEntitiesFailure x -> Sum (DijkstraEntitiesFailure @era) 2 !> To x
      DijkstraGovFailure x -> Sum (DijkstraGovFailure @era) 3 !> To x
      DijkstraTreasuryValueMismatch mm -> Sum (DijkstraTreasuryValueMismatch @era) 5 !> To mm
      DijkstraTxRefScriptsSizeTooBig mm -> Sum DijkstraTxRefScriptsSizeTooBig 6 !> To mm
      DijkstraSubLedgersFailure w -> Sum DijkstraSubLedgersFailure 9 !> To w

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "UTXOW" era))
  , DecCBOR (PredicateFailure (EraRule "ENTITIES" era))
  , DecCBOR (PredicateFailure (EraRule "GOV" era))
  , DecCBOR (PredicateFailure (EraRule "SUBLEDGERS" era))
  ) =>
  DecCBOR (DijkstraLedgerPredFailure era)
  where
  decCBOR = decode . Summands "DijkstraLedgerPredFailure" $ \case
    1 -> SumD DijkstraUtxowFailure <! From
    2 -> SumD DijkstraEntitiesFailure <! From
    3 -> SumD DijkstraGovFailure <! From
    5 -> SumD DijkstraTreasuryValueMismatch <! From
    6 -> SumD DijkstraTxRefScriptsSizeTooBig <! From
    9 -> SumD DijkstraSubLedgersFailure <! From
    n -> Invalid n

data DijkstraLedgerEvent era
  = UtxowEvent (Event (EraRule "UTXOW" era))
  | EntitiesEvent (Event (EraRule "ENTITIES" era))
  | GovEvent (Event (EraRule "GOV" era))
  | SubLedgersEvent (Event (EraRule "SUBLEDGERS" era))
  deriving (Generic)

deriving instance
  ( Eq (Event (EraRule "ENTITIES" era))
  , Eq (Event (EraRule "UTXOW" era))
  , Eq (Event (EraRule "GOV" era))
  , Eq (Event (EraRule "SUBLEDGERS" era))
  ) =>
  Eq (DijkstraLedgerEvent era)

instance
  ( NFData (Event (EraRule "ENTITIES" era))
  , NFData (Event (EraRule "UTXOW" era))
  , NFData (Event (EraRule "GOV" era))
  , NFData (Event (EraRule "SUBLEDGERS" era))
  ) =>
  NFData (DijkstraLedgerEvent era)

instance
  ( AlonzoEraTx era
  , ConwayEraTxBody era
  , ConwayEraGov era
  , DijkstraEraTxBody era
  , DijkstraEraUTxO era
  , GovState era ~ ConwayGovState era
  , Embed (EraRule "UTXOW" era) (LEDGER era)
  , Embed (EraRule "GOV" era) (LEDGER era)
  , Embed (EraRule "ENTITIES" era) (LEDGER era)
  , Embed (EraRule "SUBLEDGERS" era) (LEDGER era)
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , State (EraRule "ENTITIES" era) ~ CertState era
  , State (EraRule "GOV" era) ~ Proposals era
  , Environment (EraRule "UTXOW" era) ~ DijkstraUtxoEnv era
  , Environment (EraRule "ENTITIES" era) ~ EntitiesEnv era
  , Environment (EraRule "GOV" era) ~ Conway.GovEnv era
  , Signal (EraRule "UTXOW" era) ~ StAnnTx TopTx era
  , Signal (EraRule "ENTITIES" era) ~ Seq (TxCert era)
  , Signal (EraRule "GOV" era) ~ Conway.GovSignal era
  , Signal (EraRule "SUBLEDGERS" era) ~ [StAnnTx SubTx era]
  , ConwayEraCertState era
  , EraRule "LEDGER" era ~ LEDGER era
  , InjectRuleFailure "LEDGER" Shelley.ShelleyLedgerPredFailure era
  , InjectRuleFailure "LEDGER" Conway.ConwayLedgerPredFailure era
  , InjectRuleFailure "LEDGER" DijkstraLedgerPredFailure era
  , EraRule "SUBLEDGERS" era ~ SUBLEDGERS era
  ) =>
  STS (LEDGER era)
  where
  type State (LEDGER era) = LedgerState era
  type Signal (LEDGER era) = StAnnTx TopTx era
  type Environment (LEDGER era) = Shelley.LedgerEnv era
  type BaseM (LEDGER era) = ShelleyBase
  type PredicateFailure (LEDGER era) = DijkstraLedgerPredFailure era
  type Event (LEDGER era) = DijkstraLedgerEvent era

  initialRules = []
  transitionRules = [dijkstraLedgerTransition]

  renderAssertionViolation = Shelley.renderDepositEqualsObligationViolation

  assertions = Shelley.shelleyLedgerAssertions @era @LEDGER

validateAllRefScriptSize ::
  ( EraTx era
  , DijkstraEraTxBody era
  ) =>
  PParams era ->
  UTxO era ->
  Tx TopTx era ->
  Test (DijkstraLedgerPredFailure era)
validateAllRefScriptSize pp utxo tx =
  let totalRefScriptSize = batchNonDistinctRefScriptsSize utxo tx
      maxRefScriptSizePerTx = fromIntegral @Word32 @Int $ pp ^. ppMaxRefScriptSizePerTxG
   in failureUnless (totalRefScriptSize <= maxRefScriptSizePerTx) $
        DijkstraTxRefScriptsSizeTooBig
          Mismatch
            { mismatchSupplied = totalRefScriptSize
            , mismatchExpected = maxRefScriptSizePerTx
            }

dijkstraLedgerTransition ::
  forall era.
  ( AlonzoEraTx era
  , ConwayEraGov era
  , DijkstraEraTxBody era
  , DijkstraEraUTxO era
  , GovState era ~ ConwayGovState era
  , Embed (EraRule "UTXOW" era) (LEDGER era)
  , Embed (EraRule "GOV" era) (LEDGER era)
  , Embed (EraRule "ENTITIES" era) (LEDGER era)
  , Embed (EraRule "SUBLEDGERS" era) (LEDGER era)
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , State (EraRule "ENTITIES" era) ~ CertState era
  , State (EraRule "GOV" era) ~ Proposals era
  , Environment (EraRule "UTXOW" era) ~ DijkstraUtxoEnv era
  , Environment (EraRule "GOV" era) ~ Conway.GovEnv era
  , Environment (EraRule "ENTITIES" era) ~ EntitiesEnv era
  , Signal (EraRule "UTXOW" era) ~ StAnnTx TopTx era
  , Signal (EraRule "ENTITIES" era) ~ Seq (TxCert era)
  , Signal (EraRule "GOV" era) ~ Conway.GovSignal era
  , STS (LEDGER era)
  , EraRule "LEDGER" era ~ LEDGER era
  , EraRule "SUBLEDGERS" era ~ SUBLEDGERS era
  , InjectRuleFailure "LEDGER" Conway.ConwayLedgerPredFailure era
  , InjectRuleFailure "LEDGER" DijkstraLedgerPredFailure era
  ) =>
  TransitionRule (LEDGER era)
dijkstraLedgerTransition = do
  TRC (Shelley.LedgerEnv slot mbCurEpochNo txIx pp chainAccountState, ledgerState, stAnnTx) <-
    judgmentContext
  let tx = stAnnTx ^. txStAnnTxG

  -- Capture the original UTxO before any subtransaction processing.
  -- This is passed through the environment to UTXOW
  -- and SUBLEDGERS, and used for all witness/validation lookups.
  let originalUtxo = utxosUtxo (ledgerState ^. lsUTxOStateL)
      subStAnnTxs = subTransactionsStAnnTx stAnnTx

  -- Process all subtransactions first
  LedgerState utxoStateAfterSubLedgers certStateAfterSubLedgers <-
    trans @(EraRule "SUBLEDGERS" era) $
      TRC
        ( SubLedgerEnv
            slot
            mbCurEpochNo
            txIx
            pp
            chainAccountState
            originalUtxo
            (tx ^. isValidTxL)
        , ledgerState
        , subStAnnTxs
        )

  curEpochNo <- maybe (liftSTS $ epochFromSlot slot) pure mbCurEpochNo

  (utxoStateBeforeUtxow, certStateFinal) <-
    if tx ^. isValidTxL == IsValid True
      then do
        let txBody = tx ^. bodyTxL
        runTest $ Conway.validateTreasuryValue txBody (chainAccountState ^. casTreasuryL)
        runTest $ validateAllRefScriptSize pp originalUtxo tx

        let govState = utxoStateAfterSubLedgers ^. utxosGovStateL
            committee = govState ^. committeeGovStateL
            proposals = govState ^. proposalsGovStateL
            committeeProposals = proposalsWithPurpose grCommitteeL proposals

        certStateAfterENTITIES <-
          trans @(EraRule "ENTITIES" era) $
            TRC
              ( EntitiesEnv
                  (stAnnTx ^. plutusLegacyModeStAnnTxG)
                  (Conway.CertsEnv tx pp curEpochNo committee committeeProposals)
              , certStateAfterSubLedgers
              , StrictSeq.fromStrict $ txBody ^. certsTxBodyL
              )

        let govSignal =
              Conway.GovSignal
                { Conway.gsVotingProcedures = txBody ^. votingProceduresTxBodyL
                , Conway.gsProposalProcedures = txBody ^. proposalProceduresTxBodyL
                , Conway.gsCertificates = txBody ^. certsTxBodyL
                }
        proposalsState <-
          trans @(EraRule "GOV" era) $
            TRC
              ( Conway.GovEnv
                  (txIdTxBody txBody)
                  curEpochNo
                  pp
                  (govState ^. constitutionGovStateL . constitutionGuardrailsScriptHashL)
                  certStateAfterENTITIES
                  (govState ^. committeeGovStateL)
              , proposals
              , govSignal
              )
        pure
          ( utxoStateAfterSubLedgers
              & utxosGovStateL . proposalsGovStateL .~ proposalsState
          , certStateAfterENTITIES
          )
      else pure (utxoStateAfterSubLedgers, certStateAfterSubLedgers)

  -- Call UTXOW with DijkstraUtxoEnv, passing the original UTxO and original certState
  utxoStateFinal <-
    trans @(EraRule "UTXOW" era) $
      TRC
        ( DijkstraUtxoEnv slot pp (lsCertState ledgerState) originalUtxo
        , utxoStateBeforeUtxow
        , stAnnTx
        )
  pure $ LedgerState utxoStateFinal certStateFinal

instance
  ( AlonzoEraTx era
  , EraUTxO era
  , BabbageEraTxBody era
  , Embed (EraRule "UTXO" era) (UTXOW era)
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Environment (EraRule "UTXO" era) ~ DijkstraUtxoEnv era
  , Script era ~ AlonzoScript era
  , TxOut era ~ BabbageTxOut era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (EraRule "UTXO" era) ~ StAnnTx TopTx era
  , PredicateFailure (EraRule "UTXOW" era) ~ DijkstraUtxowPredFailure era
  , Event (EraRule "UTXOW" era) ~ Alonzo.AlonzoUtxowEvent era
  , STS (UTXOW era)
  , Event (UTXOW era) ~ Alonzo.AlonzoUtxowEvent era
  ) =>
  Embed (UTXOW era) (LEDGER era)
  where
  wrapFailed = DijkstraUtxowFailure
  wrapEvent = UtxowEvent

instance
  ( STS (LEDGER era)
  , PredicateFailure (EraRule "LEDGER" era) ~ DijkstraLedgerPredFailure era
  , Event (EraRule "LEDGER" era) ~ DijkstraLedgerEvent era
  ) =>
  Embed (LEDGER era) (Shelley.LEDGERS era)
  where
  wrapFailed = Shelley.LedgerFailure
  wrapEvent = Shelley.LedgerEvent

instance
  ( STS (GOV era)
  , PredicateFailure (EraRule "GOV" era) ~ DijkstraGovPredFailure era
  , Event (EraRule "GOV" era) ~ Conway.ConwayGovEvent era
  ) =>
  Embed (GOV era) (LEDGER era)
  where
  wrapFailed = DijkstraGovFailure
  wrapEvent = GovEvent

conwayToDijkstraLedgerPredFailure ::
  forall era.
  PredicateFailure (EraRule "ENTITIES" era) ~ EntitiesPredFailure era =>
  Conway.ConwayLedgerPredFailure era -> DijkstraLedgerPredFailure era
conwayToDijkstraLedgerPredFailure = \case
  Conway.ConwayUtxowFailure f -> DijkstraUtxowFailure f
  Conway.ConwayCertsFailure f -> DijkstraEntitiesFailure (CertsFailure f)
  Conway.ConwayGovFailure f -> DijkstraGovFailure f
  Conway.ConwayWdrlNotDelegatedToDRep kh -> DijkstraEntitiesFailure (WdrlNotDelegatedToDRep kh)
  Conway.ConwayTreasuryValueMismatch mm -> DijkstraTreasuryValueMismatch mm
  Conway.ConwayTxRefScriptsSizeTooBig mm -> DijkstraTxRefScriptsSizeTooBig mm
  Conway.ConwayMempoolFailure _ -> error "Impossible: MempoolFailure has been moved to MEMPOOL rule in Dijkstra"
  Conway.ConwayWithdrawalsMissingAccounts ws -> DijkstraEntitiesFailure (WithdrawalsMissingAccounts ws)
  Conway.ConwayIncompleteWithdrawals ws -> DijkstraEntitiesFailure (IncompleteWithdrawals ws)

shelleyToDijkstraLedgerPredFailure ::
  forall era.
  PredicateFailure (EraRule "ENTITIES" era) ~ EntitiesPredFailure era =>
  Shelley.ShelleyLedgerPredFailure era -> DijkstraLedgerPredFailure era
shelleyToDijkstraLedgerPredFailure = \case
  Shelley.UtxowFailure x -> DijkstraUtxowFailure x
  Shelley.DelegsFailure _ -> error "Impossible: DELEGS has been removed in Dijkstra"
  Shelley.ShelleyWithdrawalsMissingAccounts x -> DijkstraEntitiesFailure (WithdrawalsMissingAccounts x)
  Shelley.ShelleyIncompleteWithdrawals x -> DijkstraEntitiesFailure (IncompleteWithdrawals x)

instance
  ( STS (ENTITIES era)
  , PredicateFailure (EraRule "ENTITIES" era) ~ EntitiesPredFailure era
  , Event (EraRule "ENTITIES" era) ~ EntitiesEvent era
  ) =>
  Embed (ENTITIES era) (LEDGER era)
  where
  wrapFailed = DijkstraEntitiesFailure
  wrapEvent = EntitiesEvent

instance
  ( STS (SUBLEDGERS era)
  , PredicateFailure (EraRule "SUBLEDGERS" era) ~ DijkstraSubLedgersPredFailure era
  , Event (EraRule "SUBLEDGERS" era) ~ DijkstraSubLedgersEvent era
  ) =>
  Embed (SUBLEDGERS era) (LEDGER era)
  where
  wrapFailed = DijkstraSubLedgersFailure
  wrapEvent = SubLedgersEvent
