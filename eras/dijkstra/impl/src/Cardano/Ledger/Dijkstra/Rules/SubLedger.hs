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
) where

import Cardano.Ledger.BaseTypes (
  ShelleyBase,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules (
  GovEnv (..),
  GovSignal (..),
  gsCertificates,
  gsProposalProcedures,
  gsVotingProcedures,
 )
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  DijkstraSUBGOV,
  DijkstraSUBLEDGER,
  DijkstraSUBUTXO,
  DijkstraSUBUTXOS,
  DijkstraSUBUTXOW,
 )
import Cardano.Ledger.Dijkstra.Rules.SubGov (DijkstraSubGovPredFailure (..))
import Cardano.Ledger.Dijkstra.Rules.SubUtxow (DijkstraSubUtxowPredFailure (..))
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (
  LedgerEnv (..),
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
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

data DijkstraSubLedgerPredFailure era
  = SubUtxowFailure (PredicateFailure (EraRule "SUBUTXOW" era))
  | SubGovFailure (PredicateFailure (EraRule "SUBGOV" era))
  deriving (Generic)

deriving stock instance
  ( Eq (PredicateFailure (EraRule "SUBGOV" era))
  , Eq (PredicateFailure (EraRule "SUBUTXOW" era))
  ) =>
  Eq (DijkstraSubLedgerPredFailure era)

deriving stock instance
  ( Show (PredicateFailure (EraRule "SUBGOV" era))
  , Show (PredicateFailure (EraRule "SUBUTXOW" era))
  ) =>
  Show (DijkstraSubLedgerPredFailure era)

instance
  ( NoThunks (PredicateFailure (EraRule "SUBGOV" era))
  , NoThunks (PredicateFailure (EraRule "SUBUTXOW" era))
  ) =>
  NoThunks (DijkstraSubLedgerPredFailure era)

instance
  ( NFData (PredicateFailure (EraRule "SUBGOV" era))
  , NFData (PredicateFailure (EraRule "SUBUTXOW" era))
  ) =>
  NFData (DijkstraSubLedgerPredFailure era)

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "SUBGOV" era))
  , EncCBOR (PredicateFailure (EraRule "SUBUTXOW" era))
  ) =>
  EncCBOR (DijkstraSubLedgerPredFailure era)
  where
  encCBOR =
    encode . \case
      SubUtxowFailure e -> Sum (SubUtxowFailure @era) 1 !> To e
      SubGovFailure e -> Sum (SubGovFailure @era) 2 !> To e

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "SUBGOV" era))
  , DecCBOR (PredicateFailure (EraRule "SUBUTXOW" era))
  ) =>
  DecCBOR (DijkstraSubLedgerPredFailure era)
  where
  decCBOR = decode . Summands "DijkstraSubLedgerPredFailure" $ \case
    1 -> SumD SubUtxowFailure <! From
    2 -> SumD SubGovFailure <! From
    n -> Invalid n

type instance EraRuleFailure "SUBLEDGER" DijkstraEra = DijkstraSubLedgerPredFailure DijkstraEra

type instance EraRuleEvent "SUBLEDGER" DijkstraEra = VoidEraRule "SUBLEDGER" DijkstraEra

instance InjectRuleFailure "SUBLEDGER" DijkstraSubLedgerPredFailure DijkstraEra

instance InjectRuleFailure "SUBLEDGER" DijkstraSubGovPredFailure DijkstraEra where
  injectFailure = SubGovFailure

instance InjectRuleFailure "SUBLEDGER" DijkstraSubUtxowPredFailure DijkstraEra where
  injectFailure = SubUtxowFailure

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
  , Embed (EraRule "SUBGOV" era) (DijkstraSUBLEDGER era)
  , Embed (EraRule "SUBUTXOW" era) (DijkstraSUBLEDGER era)
  ) =>
  STS (DijkstraSUBLEDGER era)
  where
  type State (DijkstraSUBLEDGER era) = LedgerState era
  type Signal (DijkstraSUBLEDGER era) = Tx SubTx era
  type Environment (DijkstraSUBLEDGER era) = LedgerEnv era
  type BaseM (DijkstraSUBLEDGER era) = ShelleyBase
  type PredicateFailure (DijkstraSUBLEDGER era) = DijkstraSubLedgerPredFailure era
  type Event (DijkstraSUBLEDGER era) = Void

  transitionRules = [dijkstraSubLedgersTransition @era]

dijkstraSubLedgersTransition ::
  forall era.
  ( EraTx era
  , ConwayEraTxBody era
  , ConwayEraGov era
  , EraRule "SUBLEDGER" era ~ DijkstraSUBLEDGER era
  , EraRule "SUBGOV" era ~ DijkstraSUBGOV era
  , EraRule "SUBUTXOW" era ~ DijkstraSUBUTXOW era
  , Embed (EraRule "SUBGOV" era) (DijkstraSUBLEDGER era)
  , Embed (EraRule "SUBUTXOW" era) (DijkstraSUBLEDGER era)
  , STS (EraRule "SUBLEDGER" era)
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
  let govEnv =
        GovEnv
          (txIdTxBody txBody)
          curEpochNo
          pp
          (govState ^. constitutionGovStateL . constitutionGuardrailsScriptHashL)
          (ledgerState ^. lsCertStateL)
          (govState ^. committeeGovStateL)
  let govSignal =
        GovSignal
          { gsVotingProcedures = txBody ^. votingProceduresTxBodyL
          , gsProposalProcedures = txBody ^. proposalProceduresTxBodyL
          , gsCertificates = txBody ^. certsTxBodyL
          }
  proposals <-
    trans @(EraRule "SUBGOV" era) $
      TRC
        ( govEnv
        , govState ^. proposalsGovStateL
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
      & lsUTxOStateL . utxosGovStateL . proposalsGovStateL .~ proposals

instance
  ( ConwayEraGov era
  , ConwayEraCertState era
  , EraRule "SUBGOV" era ~ DijkstraSUBGOV era
  ) =>
  Embed (DijkstraSUBGOV era) (DijkstraSUBLEDGER era)
  where
  wrapFailed = SubGovFailure
  wrapEvent = absurd

instance
  ( ConwayEraGov era
  , ConwayEraCertState era
  , EraRule "SUBUTXO" era ~ DijkstraSUBUTXO era
  , EraRule "SUBUTXOW" era ~ DijkstraSUBUTXOW era
  , EraRule "SUBUTXOS" era ~ DijkstraSUBUTXOS era
  ) =>
  Embed (DijkstraSUBUTXOW era) (DijkstraSUBLEDGER era)
  where
  wrapFailed = SubUtxowFailure
  wrapEvent = absurd
