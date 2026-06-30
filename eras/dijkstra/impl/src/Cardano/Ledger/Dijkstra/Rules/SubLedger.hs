{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.SubLedger (
  SUBLEDGER,
  DijkstraSubLedgerPredFailure (..),
  DijkstraSubLedgerEvent (..),
  SubLedgerEnv (..),
) where

import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  SUBCERT,
  SUBCERTS,
  SUBDELEG,
  SUBENTITIES,
  SUBGOV,
  SUBGOVCERT,
  SUBLEDGER,
  SUBPOOL,
  SUBUTXO,
  SUBUTXOW,
 )
import Cardano.Ledger.Dijkstra.Rules.Gov (DijkstraGovPredFailure (..))
import Cardano.Ledger.Dijkstra.Rules.SubCerts (
  DijkstraSubCertsPredFailure (..),
  SubCertsEnv (..),
 )
import Cardano.Ledger.Dijkstra.Rules.SubDeleg (DijkstraSubDelegPredFailure)
import Cardano.Ledger.Dijkstra.Rules.SubEntities (
  SubEntitiesEvent,
  SubEntitiesPredFailure (..),
 )
import Cardano.Ledger.Dijkstra.Rules.SubGov (DijkstraSubGovEvent, DijkstraSubGovPredFailure (..))
import Cardano.Ledger.Dijkstra.Rules.SubGovCert (DijkstraSubGovCertPredFailure)
import Cardano.Ledger.Dijkstra.Rules.SubPool (DijkstraSubPoolEvent, DijkstraSubPoolPredFailure)
import Cardano.Ledger.Dijkstra.Rules.SubUtxo (SubUtxoEnv (..))
import Cardano.Ledger.Dijkstra.Rules.SubUtxow (
  DijkstraSubUtxowEvent (..),
  DijkstraSubUtxowPredFailure (..),
 )
import Cardano.Ledger.Dijkstra.Rules.Utxow (DijkstraUtxowPredFailure (..))
import Cardano.Ledger.Dijkstra.TxCert
import Cardano.Ledger.Rules.ValidationMode (runTest)
import Cardano.Ledger.Shelley.LedgerState
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.Slot (epochFromSlot)
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended (
  BaseM,
  Embed (..),
  Environment,
  Event,
  PredicateFailure,
  STS,
  Signal,
  State,
  TRC (TRC),
  TransitionRule,
  judgmentContext,
  liftSTS,
  trans,
  transitionRules,
 )
import qualified Data.Sequence.Strict as StrictSeq
import GHC.Generics (Generic)
import Lens.Micro

data SubLedgerEnv era = SubLedgerEnv
  { sleSlotNo :: SlotNo
  , sleEpochNo :: Maybe EpochNo
  , sleTxIx :: TxIx
  , slePParams :: PParams era
  , sleAccount :: ChainAccountState
  , sleOriginalUtxo :: UTxO era
  , sleTopTxIsValid :: IsValid
  }

data DijkstraSubLedgerPredFailure era
  = SubUtxowFailure (PredicateFailure (EraRule "SUBUTXOW" era))
  | SubEntitiesFailure (PredicateFailure (EraRule "SUBENTITIES" era))
  | SubGovFailure (PredicateFailure (EraRule "SUBGOV" era))
  | SubTreasuryValueMismatch (Mismatch RelEQ Coin)
  deriving (Generic)

deriving stock instance
  ( Eq (PredicateFailure (EraRule "SUBGOV" era))
  , Eq (PredicateFailure (EraRule "SUBENTITIES" era))
  , Eq (PredicateFailure (EraRule "SUBUTXOW" era))
  ) =>
  Eq (DijkstraSubLedgerPredFailure era)

deriving stock instance
  ( Show (PredicateFailure (EraRule "SUBGOV" era))
  , Show (PredicateFailure (EraRule "SUBENTITIES" era))
  , Show (PredicateFailure (EraRule "SUBUTXOW" era))
  ) =>
  Show (DijkstraSubLedgerPredFailure era)

instance
  ( NFData (PredicateFailure (EraRule "SUBGOV" era))
  , NFData (PredicateFailure (EraRule "SUBENTITIES" era))
  , NFData (PredicateFailure (EraRule "SUBUTXOW" era))
  ) =>
  NFData (DijkstraSubLedgerPredFailure era)

type instance EraRuleFailure "SUBLEDGER" DijkstraEra = DijkstraSubLedgerPredFailure DijkstraEra

type instance EraRuleEvent "SUBLEDGER" DijkstraEra = DijkstraSubLedgerEvent DijkstraEra

instance InjectRuleFailure "SUBLEDGER" DijkstraSubLedgerPredFailure DijkstraEra

instance InjectRuleFailure "SUBLEDGER" DijkstraSubGovPredFailure DijkstraEra where
  injectFailure = SubGovFailure

instance InjectRuleFailure "SUBLEDGER" DijkstraSubUtxowPredFailure DijkstraEra where
  injectFailure = SubUtxowFailure

instance InjectRuleFailure "SUBLEDGER" SubEntitiesPredFailure DijkstraEra where
  injectFailure = SubEntitiesFailure

instance InjectRuleFailure "SUBLEDGER" DijkstraSubCertsPredFailure DijkstraEra where
  injectFailure = SubEntitiesFailure . SubCertsFailure

instance InjectRuleFailure "SUBLEDGER" Conway.ConwayLedgerPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraSubLedgerPredFailure

data DijkstraSubLedgerEvent era
  = SubEntitiesEvent (Event (EraRule "SUBENTITIES" era))
  | SubGovEvent (Event (EraRule "SUBGOV" era))
  | SubUtxowEvent (Event (EraRule "SUBUTXOW" era))
  deriving (Generic)

deriving instance
  ( Eq (Event (EraRule "SUBENTITIES" era))
  , Eq (Event (EraRule "SUBGOV" era))
  , Eq (Event (EraRule "SUBUTXOW" era))
  ) =>
  Eq (DijkstraSubLedgerEvent era)

instance
  ( NFData (Event (EraRule "SUBENTITIES" era))
  , NFData (Event (EraRule "SUBGOV" era))
  , NFData (Event (EraRule "SUBUTXOW" era))
  ) =>
  NFData (DijkstraSubLedgerEvent era)

instance InjectRuleEvent "SUBLEDGER" DijkstraSubLedgerEvent DijkstraEra

instance
  ( EraTx era
  , ConwayEraTxBody era
  , ConwayEraGov era
  , ConwayEraCertState era
  , EraRule "SUBLEDGER" era ~ SUBLEDGER era
  , EraRule "SUBGOV" era ~ SUBGOV era
  , EraRule "SUBUTXO" era ~ SUBUTXO era
  , EraRule "SUBUTXOW" era ~ SUBUTXOW era
  , EraRule "SUBENTITIES" era ~ SUBENTITIES era
  , EraRule "SUBCERTS" era ~ SUBCERTS era
  , EraRule "SUBCERT" era ~ SUBCERT era
  , EraRule "SUBDELEG" era ~ SUBDELEG era
  , EraRule "SUBPOOL" era ~ SUBPOOL era
  , EraRule "SUBGOVCERT" era ~ SUBGOVCERT era
  , Embed (EraRule "SUBGOV" era) (SUBLEDGER era)
  , Embed (EraRule "SUBUTXOW" era) (SUBLEDGER era)
  , Embed (EraRule "SUBENTITIES" era) (SUBLEDGER era)
  , InjectRuleEvent "SUBPOOL" Shelley.PoolEvent era
  , InjectRuleEvent "SUBPOOL" DijkstraSubPoolEvent era
  , InjectRuleFailure "SUBPOOL" Shelley.ShelleyPoolPredFailure era
  , InjectRuleFailure "SUBPOOL" DijkstraSubPoolPredFailure era
  , InjectRuleFailure "SUBGOVCERT" DijkstraSubGovCertPredFailure era
  , InjectRuleFailure "SUBGOVCERT" Conway.ConwayGovCertPredFailure era
  , InjectRuleFailure "SUBDELEG" Conway.ConwayDelegPredFailure era
  , InjectRuleFailure "SUBDELEG" DijkstraSubDelegPredFailure era
  , InjectRuleFailure "SUBLEDGER" Conway.ConwayLedgerPredFailure era
  , InjectRuleFailure "SUBUTXOW" Alonzo.AlonzoUtxowPredFailure era
  , TxCert era ~ DijkstraTxCert era
  ) =>
  STS (SUBLEDGER era)
  where
  type State (SUBLEDGER era) = LedgerState era
  type Signal (SUBLEDGER era) = StAnnTx SubTx era
  type Environment (SUBLEDGER era) = SubLedgerEnv era
  type BaseM (SUBLEDGER era) = ShelleyBase
  type PredicateFailure (SUBLEDGER era) = DijkstraSubLedgerPredFailure era
  type Event (SUBLEDGER era) = DijkstraSubLedgerEvent era

  transitionRules = [dijkstraSubLedgersTransition @era]

dijkstraSubLedgersTransition ::
  forall era.
  ( EraTx era
  , ConwayEraTxBody era
  , ConwayEraGov era
  , EraRule "SUBLEDGER" era ~ SUBLEDGER era
  , EraRule "SUBGOV" era ~ SUBGOV era
  , EraRule "SUBUTXOW" era ~ SUBUTXOW era
  , EraRule "SUBENTITIES" era ~ SUBENTITIES era
  , Embed (EraRule "SUBGOV" era) (SUBLEDGER era)
  , Embed (EraRule "SUBUTXOW" era) (SUBLEDGER era)
  , Embed (EraRule "SUBENTITIES" era) (SUBLEDGER era)
  , InjectRuleFailure "SUBLEDGER" Conway.ConwayLedgerPredFailure era
  , STS (EraRule "SUBLEDGER" era)
  ) =>
  TransitionRule (EraRule "SUBLEDGER" era)
dijkstraSubLedgersTransition = do
  TRC
    ( SubLedgerEnv slot mbCurEpochNo _ pp chainAccountState originalUtxo topIsValid
      , ledgerState@(LedgerState utxoState certState)
      , stAnnTx
      ) <-
    judgmentContext

  let tx = stAnnTx ^. txStAnnTxG
  curEpochNo <- maybe (liftSTS $ epochFromSlot slot) pure mbCurEpochNo
  let txBody = tx ^. bodyTxL
  let govState = utxoState ^. utxosGovStateL
  let committee = govState ^. committeeGovStateL
  let proposals = govState ^. proposalsGovStateL

  runTest $ Conway.validateTreasuryValue txBody (chainAccountState ^. casTreasuryL)

  certStateAfterSubEntities <-
    trans @(EraRule "SUBENTITIES" era) $
      TRC
        ( SubCertsEnv tx pp curEpochNo committee (proposalsWithPurpose grCommitteeL proposals)
        , certState
        , StrictSeq.fromStrict $ txBody ^. certsTxBodyL
        )
  let govEnv =
        Conway.GovEnv
          (txIdTxBody txBody)
          curEpochNo
          pp
          (govState ^. constitutionGovStateL . constitutionGuardrailsScriptHashL)
          certStateAfterSubEntities
          committee
  let govSignal =
        Conway.GovSignal
          { Conway.gsVotingProcedures = txBody ^. votingProceduresTxBodyL
          , Conway.gsProposalProcedures = txBody ^. proposalProceduresTxBodyL
          , Conway.gsCertificates = txBody ^. certsTxBodyL
          }
  proposalsState <-
    trans @(EraRule "SUBGOV" era) $
      TRC
        ( govEnv
        , proposals
        , govSignal
        )

  utxoStateAfterSubUtxow <-
    trans @(EraRule "SUBUTXOW" era) $
      TRC
        ( SubUtxoEnv slot pp certState originalUtxo topIsValid
        , utxoState
        , stAnnTx
        )
  pure $
    ledgerState
      & lsUTxOStateL .~ utxoStateAfterSubUtxow
      & lsUTxOStateL . utxosGovStateL . proposalsGovStateL .~ proposalsState
      & lsCertStateL .~ certStateAfterSubEntities

instance
  ( STS (SUBGOV era)
  , PredicateFailure (EraRule "SUBGOV" era) ~ DijkstraSubGovPredFailure era
  , Event (EraRule "SUBGOV" era) ~ DijkstraSubGovEvent era
  ) =>
  Embed (SUBGOV era) (SUBLEDGER era)
  where
  wrapFailed = SubGovFailure
  wrapEvent = SubGovEvent

instance
  ( STS (SUBUTXOW era)
  , PredicateFailure (EraRule "SUBUTXOW" era) ~ DijkstraSubUtxowPredFailure era
  , Event (EraRule "SUBUTXOW" era) ~ DijkstraSubUtxowEvent era
  ) =>
  Embed (SUBUTXOW era) (SUBLEDGER era)
  where
  wrapFailed = SubUtxowFailure
  wrapEvent = SubUtxowEvent

instance
  ( STS (SUBENTITIES era)
  , PredicateFailure (EraRule "SUBENTITIES" era) ~ SubEntitiesPredFailure era
  , Event (EraRule "SUBENTITIES" era) ~ SubEntitiesEvent era
  ) =>
  Embed (SUBENTITIES era) (SUBLEDGER era)
  where
  wrapFailed = SubEntitiesFailure
  wrapEvent = SubEntitiesEvent

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "SUBUTXOW" era))
  , EncCBOR (PredicateFailure (EraRule "SUBENTITIES" era))
  , EncCBOR (PredicateFailure (EraRule "SUBGOV" era))
  ) =>
  EncCBOR (DijkstraSubLedgerPredFailure era)
  where
  encCBOR =
    encode . \case
      SubUtxowFailure x -> Sum (SubUtxowFailure @era) 1 !> To x
      SubEntitiesFailure x -> Sum (SubEntitiesFailure @era) 2 !> To x
      SubGovFailure x -> Sum (SubGovFailure @era) 3 !> To x
      SubTreasuryValueMismatch mm -> Sum (SubTreasuryValueMismatch @era) 5 !> To mm

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "SUBUTXOW" era))
  , DecCBOR (PredicateFailure (EraRule "SUBENTITIES" era))
  , DecCBOR (PredicateFailure (EraRule "SUBGOV" era))
  ) =>
  DecCBOR (DijkstraSubLedgerPredFailure era)
  where
  decCBOR = decode . Summands "DijkstraSubLedgerPredFailure" $ \case
    1 -> SumD SubUtxowFailure <! From
    2 -> SumD SubEntitiesFailure <! From
    3 -> SumD SubGovFailure <! From
    5 -> SumD SubTreasuryValueMismatch <! From
    n -> Invalid n

conwayToDijkstraSubLedgerPredFailure ::
  forall era.
  ( InjectRuleFailure "SUBUTXOW" DijkstraUtxowPredFailure era
  , PredicateFailure (EraRule "UTXOW" era) ~ DijkstraUtxowPredFailure era
  , InjectRuleFailure "SUBENTITIES" Conway.ConwayCertsPredFailure era
  , PredicateFailure (EraRule "SUBENTITIES" era) ~ SubEntitiesPredFailure era
  , PredicateFailure (EraRule "CERTS" era) ~ Conway.ConwayCertsPredFailure era
  , InjectRuleFailure "SUBGOV" DijkstraGovPredFailure era
  , PredicateFailure (EraRule "GOV" era) ~ DijkstraGovPredFailure era
  ) =>
  Conway.ConwayLedgerPredFailure era ->
  DijkstraSubLedgerPredFailure era
conwayToDijkstraSubLedgerPredFailure = \case
  Conway.ConwayUtxowFailure f -> SubUtxowFailure (injectFailure @"SUBUTXOW" f)
  Conway.ConwayCertsFailure f -> SubEntitiesFailure (injectFailure @"SUBENTITIES" f)
  Conway.ConwayGovFailure f -> SubGovFailure (injectFailure @"SUBGOV" f)
  Conway.ConwayWdrlNotDelegatedToDRep x -> SubEntitiesFailure (SubWdrlNotDelegatedToDRep x)
  Conway.ConwayWithdrawalsMissingAccounts x -> SubEntitiesFailure (SubWithdrawalsMissingAccounts x)
  Conway.ConwayTreasuryValueMismatch x -> SubTreasuryValueMismatch x
  Conway.ConwayTxRefScriptsSizeTooBig _ -> error "Impossible: `ConwayTxRefScriptsSizeTooBig` for SUBLEDGER"
  Conway.ConwayMempoolFailure _ -> error "Impossible: `ConwayMempoolFailure` for SUBLEDGER"
  Conway.ConwayIncompleteWithdrawals _ -> error "Impossible: `ConwayIncompleteWithdrawals` for SUBLEDGER"
