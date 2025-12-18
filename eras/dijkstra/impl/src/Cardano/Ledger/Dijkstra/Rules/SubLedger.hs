{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
 )
import Cardano.Ledger.Dijkstra.Rules.SubGov (DijkstraSubGovPredFailure (..))
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (LedgerEnv (..), epochFromSlot)
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
  = SubGovFailure (PredicateFailure (EraRule "SUBGOV" era))
  deriving (Generic)

deriving stock instance
  Eq (PredicateFailure (EraRule "SUBGOV" era)) => Eq (DijkstraSubLedgerPredFailure era)

deriving stock instance
  Show (PredicateFailure (EraRule "SUBGOV" era)) => Show (DijkstraSubLedgerPredFailure era)

instance
  NoThunks (PredicateFailure (EraRule "SUBGOV" era)) =>
  NoThunks (DijkstraSubLedgerPredFailure era)

instance NFData (PredicateFailure (EraRule "SUBGOV" era)) => NFData (DijkstraSubLedgerPredFailure era)

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "SUBGOV" era))
  ) =>
  EncCBOR (DijkstraSubLedgerPredFailure era)
  where
  encCBOR (SubGovFailure e) = encCBOR e

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "SUBGOV" era))
  ) =>
  DecCBOR (DijkstraSubLedgerPredFailure era)
  where
  decCBOR = SubGovFailure <$> decCBOR

type instance EraRuleFailure "SUBLEDGER" DijkstraEra = DijkstraSubLedgerPredFailure DijkstraEra

type instance EraRuleEvent "SUBLEDGER" DijkstraEra = VoidEraRule "SUBLEDGER" DijkstraEra

instance InjectRuleFailure "SUBLEDGER" DijkstraSubLedgerPredFailure DijkstraEra

instance InjectRuleFailure "SUBLEDGER" DijkstraSubGovPredFailure DijkstraEra where
  injectFailure = SubGovFailure

instance
  ( EraTx era
  , ConwayEraTxBody era
  , ConwayEraGov era
  , ConwayEraCertState era
  , EraRule "SUBLEDGER" era ~ DijkstraSUBLEDGER era
  , EraRule "SUBGOV" era ~ DijkstraSUBGOV era
  , Embed (EraRule "SUBGOV" era) (DijkstraSUBLEDGER era)
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
  , Embed (EraRule "SUBGOV" era) (DijkstraSUBLEDGER era)
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
  pure $ ledgerState & lsUTxOStateL . utxosGovStateL . proposalsGovStateL .~ proposals

instance
  ( ConwayEraGov era
  , ConwayEraCertState era
  , EraRule "SUBGOV" era ~ DijkstraSUBGOV era
  ) =>
  Embed (DijkstraSUBGOV era) (DijkstraSUBLEDGER era)
  where
  wrapFailed = SubGovFailure
  wrapEvent = absurd
