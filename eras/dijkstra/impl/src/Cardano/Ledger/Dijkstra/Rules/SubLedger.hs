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
  DijkstraSUBLEDGER,
  DijkstraSubLedgerPredFailure (..),
  DijkstraSubLedgerEvent (..),
) where

import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules (
  ConwayDelegPredFailure,
  ConwayGovCertPredFailure,
  ConwayGovEvent,
  ConwayGovPredFailure,
  GovEnv (..),
  GovSignal (..),
  gsCertificates,
  gsProposalProcedures,
  gsVotingProcedures,
 )
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  DijkstraSUBCERT,
  DijkstraSUBCERTS,
  DijkstraSUBDELEG,
  DijkstraSUBGOV,
  DijkstraSUBGOVCERT,
  DijkstraSUBLEDGER,
  DijkstraSUBPOOL,
  DijkstraSUBUTXO,
  DijkstraSUBUTXOS,
  DijkstraSUBUTXOW,
 )
import Cardano.Ledger.Dijkstra.Rules.SubCerts (DijkstraSubCertsPredFailure (..), SubCertsEnv (..))
import Cardano.Ledger.Dijkstra.Rules.SubDeleg (DijkstraSubDelegPredFailure)
import Cardano.Ledger.Dijkstra.Rules.SubGov (DijkstraSubGovEvent, DijkstraSubGovPredFailure (..))
import Cardano.Ledger.Dijkstra.Rules.SubGovCert (DijkstraSubGovCertPredFailure)
import Cardano.Ledger.Dijkstra.Rules.SubPool (DijkstraSubPoolEvent, DijkstraSubPoolPredFailure)
import Cardano.Ledger.Dijkstra.Rules.SubUtxow (DijkstraSubUtxowPredFailure (..))
import Cardano.Ledger.Dijkstra.TxCert
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (
  LedgerEnv (..),
  PoolEvent,
  ShelleyPoolPredFailure,
  UtxoEnv (..),
  epochFromSlot,
 )
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
import Data.List.NonEmpty (NonEmpty)
import Data.Map.NonEmpty (NonEmptyMap)
import qualified Data.Sequence.Strict as StrictSeq
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

data DijkstraSubLedgerPredFailure era
  = SubUtxowFailure (PredicateFailure (EraRule "SUBUTXOW" era))
  | SubCertsFailure (PredicateFailure (EraRule "SUBCERTS" era))
  | SubGovFailure (PredicateFailure (EraRule "SUBGOV" era))
  | SubWdrlNotDelegatedToDRep (NonEmpty (KeyHash Staking))
  | SubTreasuryValueMismatch (Mismatch RelEQ Coin)
  | SubTxRefScriptsSizeTooBig (Mismatch RelLTEQ Int)
  | SubWithdrawalsMissingAccounts Withdrawals
  | SubIncompleteWithdrawals (NonEmptyMap AccountAddress (Mismatch RelEQ Coin))
  deriving (Generic)

deriving stock instance
  ( Eq (PredicateFailure (EraRule "SUBGOV" era))
  , Eq (PredicateFailure (EraRule "SUBCERTS" era))
  , Eq (PredicateFailure (EraRule "SUBUTXOW" era))
  ) =>
  Eq (DijkstraSubLedgerPredFailure era)

deriving stock instance
  ( Show (PredicateFailure (EraRule "SUBGOV" era))
  , Show (PredicateFailure (EraRule "SUBCERTS" era))
  , Show (PredicateFailure (EraRule "SUBUTXOW" era))
  ) =>
  Show (DijkstraSubLedgerPredFailure era)

instance
  ( NoThunks (PredicateFailure (EraRule "SUBGOV" era))
  , NoThunks (PredicateFailure (EraRule "SUBCERTS" era))
  , NoThunks (PredicateFailure (EraRule "SUBUTXOW" era))
  ) =>
  NoThunks (DijkstraSubLedgerPredFailure era)

instance
  ( NFData (PredicateFailure (EraRule "SUBGOV" era))
  , NFData (PredicateFailure (EraRule "SUBCERTS" era))
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

instance InjectRuleFailure "SUBLEDGER" DijkstraSubCertsPredFailure DijkstraEra where
  injectFailure = SubCertsFailure

data DijkstraSubLedgerEvent era
  = SubCertsEvent (Event (EraRule "SUBCERTS" era))
  | SubGovEvent (Event (EraRule "SUBGOV" era))
  | SubUtxowEvent (Event (EraRule "SUBUTXOW" era))
  deriving (Generic)

deriving instance
  ( Eq (Event (EraRule "SUBCERTS" era))
  , Eq (Event (EraRule "SUBGOV" era))
  , Eq (Event (EraRule "SUBUTXOW" era))
  ) =>
  Eq (DijkstraSubLedgerEvent era)

instance
  ( NFData (Event (EraRule "SUBCERTS" era))
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
  , EraRule "SUBLEDGER" era ~ DijkstraSUBLEDGER era
  , EraRule "SUBGOV" era ~ DijkstraSUBGOV era
  , EraRule "SUBUTXO" era ~ DijkstraSUBUTXO era
  , EraRule "SUBUTXOW" era ~ DijkstraSUBUTXOW era
  , EraRule "SUBUTXOS" era ~ DijkstraSUBUTXOS era
  , EraRule "SUBCERTS" era ~ DijkstraSUBCERTS era
  , EraRule "SUBCERT" era ~ DijkstraSUBCERT era
  , EraRule "SUBDELEG" era ~ DijkstraSUBDELEG era
  , EraRule "SUBPOOL" era ~ DijkstraSUBPOOL era
  , EraRule "SUBGOVCERT" era ~ DijkstraSUBGOVCERT era
  , Embed (EraRule "SUBGOV" era) (DijkstraSUBLEDGER era)
  , Embed (EraRule "SUBUTXOW" era) (DijkstraSUBLEDGER era)
  , Embed (EraRule "SUBCERTS" era) (DijkstraSUBCERTS era)
  , InjectRuleEvent "SUBPOOL" PoolEvent era
  , InjectRuleEvent "SUBPOOL" DijkstraSubPoolEvent era
  , InjectRuleFailure "SUBPOOL" ShelleyPoolPredFailure era
  , InjectRuleFailure "SUBPOOL" DijkstraSubPoolPredFailure era
  , InjectRuleFailure "SUBGOVCERT" DijkstraSubGovCertPredFailure era
  , InjectRuleFailure "SUBGOVCERT" ConwayGovCertPredFailure era
  , InjectRuleFailure "SUBDELEG" ConwayDelegPredFailure era
  , InjectRuleFailure "SUBDELEG" DijkstraSubDelegPredFailure era
  , TxCert era ~ DijkstraTxCert era
  ) =>
  STS (DijkstraSUBLEDGER era)
  where
  type State (DijkstraSUBLEDGER era) = LedgerState era
  type Signal (DijkstraSUBLEDGER era) = Tx SubTx era
  type Environment (DijkstraSUBLEDGER era) = LedgerEnv era
  type BaseM (DijkstraSUBLEDGER era) = ShelleyBase
  type PredicateFailure (DijkstraSUBLEDGER era) = DijkstraSubLedgerPredFailure era
  type Event (DijkstraSUBLEDGER era) = DijkstraSubLedgerEvent era

  transitionRules = [dijkstraSubLedgersTransition @era]

dijkstraSubLedgersTransition ::
  forall era.
  ( EraTx era
  , ConwayEraTxBody era
  , ConwayEraGov era
  , ConwayEraCertState era
  , EraRule "SUBLEDGER" era ~ DijkstraSUBLEDGER era
  , EraRule "SUBGOV" era ~ DijkstraSUBGOV era
  , EraRule "SUBUTXOW" era ~ DijkstraSUBUTXOW era
  , EraRule "SUBCERTS" era ~ DijkstraSUBCERTS era
  , EraRule "SUBCERT" era ~ DijkstraSUBCERT era
  , EraRule "SUBDELEG" era ~ DijkstraSUBDELEG era
  , EraRule "SUBPOOL" era ~ DijkstraSUBPOOL era
  , EraRule "SUBGOVCERT" era ~ DijkstraSUBGOVCERT era
  , Embed (EraRule "SUBGOV" era) (DijkstraSUBLEDGER era)
  , Embed (EraRule "SUBUTXOW" era) (DijkstraSUBLEDGER era)
  , InjectRuleEvent "SUBPOOL" PoolEvent era
  , InjectRuleEvent "SUBPOOL" DijkstraSubPoolEvent era
  , InjectRuleFailure "SUBPOOL" ShelleyPoolPredFailure era
  , InjectRuleFailure "SUBPOOL" DijkstraSubPoolPredFailure era
  , InjectRuleFailure "SUBGOVCERT" DijkstraSubGovCertPredFailure era
  , InjectRuleFailure "SUBGOVCERT" ConwayGovCertPredFailure era
  , InjectRuleFailure "SUBDELEG" ConwayDelegPredFailure era
  , InjectRuleFailure "SUBDELEG" DijkstraSubDelegPredFailure era
  , STS (EraRule "SUBLEDGER" era)
  , TxCert era ~ DijkstraTxCert era
  ) =>
  TransitionRule (EraRule "SUBLEDGER" era)
dijkstraSubLedgersTransition = do
  TRC
    ( LedgerEnv slot mbCurEpochNo _ pp _
      , ledgerState
      , tx
      ) <-
    judgmentContext

  curEpochNo <- maybe (liftSTS $ epochFromSlot slot) pure mbCurEpochNo
  let txBody = tx ^. bodyTxL
  let govState = ledgerState ^. lsUTxOStateL . utxosGovStateL
  let committee = govState ^. committeeGovStateL
  let proposals = govState ^. proposalsGovStateL
  certStateAfterSubCerts <-
    trans @(EraRule "SUBCERTS" era) $
      TRC
        ( SubCertsEnv tx pp curEpochNo committee (proposalsWithPurpose grCommitteeL proposals)
        , ledgerState ^. lsCertStateL
        , StrictSeq.fromStrict $ txBody ^. certsTxBodyL
        )
  let govEnv =
        GovEnv
          (txIdTxBody txBody)
          curEpochNo
          pp
          (govState ^. constitutionGovStateL . constitutionGuardrailsScriptHashL)
          certStateAfterSubCerts
          committee
  let govSignal =
        GovSignal
          { gsVotingProcedures = txBody ^. votingProceduresTxBodyL
          , gsProposalProcedures = txBody ^. proposalProceduresTxBodyL
          , gsCertificates = txBody ^. certsTxBodyL
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
        ( UtxoEnv @era slot pp (ledgerState ^. lsCertStateL)
        , ledgerState ^. lsUTxOStateL
        , tx
        )
  pure $
    ledgerState
      & lsUTxOStateL .~ utxoStateAfterSubUtxow
      & lsUTxOStateL . utxosGovStateL . proposalsGovStateL .~ proposalsState
      & lsCertStateL .~ certStateAfterSubCerts

instance
  ( ConwayEraGov era
  , ConwayEraCertState era
  , ConwayEraPParams era
  , ConwayEraTxCert era
  , EraRule "SUBGOV" era ~ DijkstraSUBGOV era
  , InjectRuleEvent "SUBGOV" DijkstraSubGovEvent era
  , InjectRuleEvent "SUBGOV" ConwayGovEvent era
  , InjectRuleFailure "SUBGOV" DijkstraSubGovPredFailure era
  , InjectRuleFailure "SUBGOV" ConwayGovPredFailure era
  ) =>
  Embed (DijkstraSUBGOV era) (DijkstraSUBLEDGER era)
  where
  wrapFailed = SubGovFailure
  wrapEvent = SubGovEvent

instance
  ( ConwayEraGov era
  , ConwayEraCertState era
  , ConwayEraTxBody era
  , EraPlutusContext era
  , EraRule "SUBUTXO" era ~ DijkstraSUBUTXO era
  , EraRule "SUBUTXOW" era ~ DijkstraSUBUTXOW era
  , EraRule "SUBUTXOS" era ~ DijkstraSUBUTXOS era
  ) =>
  Embed (DijkstraSUBUTXOW era) (DijkstraSUBLEDGER era)
  where
  wrapFailed = SubUtxowFailure
  wrapEvent = SubUtxowEvent

instance
  ( ConwayEraGov era
  , ConwayEraPParams era
  , ConwayEraCertState era
  , EraRule "SUBCERTS" era ~ DijkstraSUBCERTS era
  , EraRule "SUBCERT" era ~ DijkstraSUBCERT era
  , EraRule "SUBDELEG" era ~ DijkstraSUBDELEG era
  , EraRule "SUBPOOL" era ~ DijkstraSUBPOOL era
  , EraRule "SUBGOVCERT" era ~ DijkstraSUBGOVCERT era
  , InjectRuleEvent "SUBPOOL" DijkstraSubPoolEvent era
  , InjectRuleEvent "SUBPOOL" PoolEvent era
  , InjectRuleFailure "SUBPOOL" DijkstraSubPoolPredFailure era
  , InjectRuleFailure "SUBPOOL" ShelleyPoolPredFailure era
  , InjectRuleFailure "SUBGOVCERT" DijkstraSubGovCertPredFailure era
  , InjectRuleFailure "SUBGOVCERT" ConwayGovCertPredFailure era
  , InjectRuleFailure "SUBDELEG" ConwayDelegPredFailure era
  , InjectRuleFailure "SUBDELEG" DijkstraSubDelegPredFailure era
  , TxCert era ~ DijkstraTxCert era
  ) =>
  Embed (DijkstraSUBCERTS era) (DijkstraSUBLEDGER era)
  where
  wrapFailed = SubCertsFailure
  wrapEvent = SubCertsEvent

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "SUBUTXOW" era))
  , EncCBOR (PredicateFailure (EraRule "SUBCERTS" era))
  , EncCBOR (PredicateFailure (EraRule "SUBGOV" era))
  ) =>
  EncCBOR (DijkstraSubLedgerPredFailure era)
  where
  encCBOR =
    encode . \case
      SubUtxowFailure x -> Sum (SubUtxowFailure @era) 1 !> To x
      SubCertsFailure x -> Sum (SubCertsFailure @era) 2 !> To x
      SubGovFailure x -> Sum (SubGovFailure @era) 3 !> To x
      SubWdrlNotDelegatedToDRep x -> Sum (SubWdrlNotDelegatedToDRep @era) 4 !> To x
      SubTreasuryValueMismatch mm -> Sum (SubTreasuryValueMismatch @era) 5 !> To mm
      SubTxRefScriptsSizeTooBig mm -> Sum SubTxRefScriptsSizeTooBig 6 !> To mm
      SubWithdrawalsMissingAccounts w -> Sum SubWithdrawalsMissingAccounts 7 !> To w
      SubIncompleteWithdrawals w -> Sum SubIncompleteWithdrawals 8 !> To w

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "SUBUTXOW" era))
  , DecCBOR (PredicateFailure (EraRule "SUBCERTS" era))
  , DecCBOR (PredicateFailure (EraRule "SUBGOV" era))
  ) =>
  DecCBOR (DijkstraSubLedgerPredFailure era)
  where
  decCBOR = decode . Summands "DijkstraSubLedgerPredFailure" $ \case
    1 -> SumD SubUtxowFailure <! From
    2 -> SumD SubCertsFailure <! From
    3 -> SumD SubGovFailure <! From
    4 -> SumD SubWdrlNotDelegatedToDRep <! From
    5 -> SumD SubTreasuryValueMismatch <! From
    6 -> SumD SubTxRefScriptsSizeTooBig <! From
    7 -> SumD SubWithdrawalsMissingAccounts <! From
    8 -> SumD SubIncompleteWithdrawals <! From
    n -> Invalid n
