{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

module Cardano.Ledger.Dijkstra.Rules.Mempool (
  DijkstraMEMPOOL,
  DijkstraMempoolPredFailure (..),
  DijkstraMempoolEvent (..),
) where

import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext)
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Conway.Governance (
  ConwayEraGov,
  ConwayGovState,
  Proposals,
 )
import Cardano.Ledger.Conway.Rules (
  CertsEnv,
  ConwayDelegPredFailure,
  ConwayGovCertPredFailure,
  ConwayGovEvent,
  ConwayGovPredFailure,
  ConwayLedgerPredFailure (ConwayMempoolFailure),
  GovEnv,
  GovSignal,
 )
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  DijkstraLEDGER,
  DijkstraMEMPOOL,
  DijkstraSUBCERT,
  DijkstraSUBCERTS,
  DijkstraSUBDELEG,
  DijkstraSUBGOV,
  DijkstraSUBGOVCERT,
  DijkstraSUBLEDGER,
  DijkstraSUBLEDGERS,
  DijkstraSUBPOOL,
  DijkstraSUBUTXO,
  DijkstraSUBUTXOS,
  DijkstraSUBUTXOW,
 )
import Cardano.Ledger.Dijkstra.Rules.Ledger (
  DijkstraLedgerPredFailure (..),
  conwayToDijkstraLedgerPredFailure,
 )
import Cardano.Ledger.Dijkstra.Rules.SubDeleg (DijkstraSubDelegPredFailure)
import Cardano.Ledger.Dijkstra.Rules.SubGov (DijkstraSubGovEvent, DijkstraSubGovPredFailure)
import Cardano.Ledger.Dijkstra.Rules.SubGovCert (DijkstraSubGovCertPredFailure)
import Cardano.Ledger.Dijkstra.Rules.SubLedgers (DijkstraSubLedgersPredFailure (..))
import Cardano.Ledger.Dijkstra.Rules.SubPool (DijkstraSubPoolEvent, DijkstraSubPoolPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Utxo (DijkstraUtxoPredFailure)
import Cardano.Ledger.Dijkstra.State
import Cardano.Ledger.Dijkstra.TxBody (DijkstraEraTxBody)
import Cardano.Ledger.Dijkstra.TxCert
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (
  LedgerEnv (..),
  PoolEvent,
  ShelleyLedgerPredFailure,
  ShelleyPoolPredFailure,
  UtxoEnv,
 )
import Control.DeepSeq (NFData)
import Control.State.Transition (
  BaseM,
  Environment,
  Event,
  PredicateFailure,
  STS (..),
  Signal,
  State,
  TRC (TRC),
  TransitionRule,
  judgmentContext,
  transitionRules,
  whenFailureFreeDefault,
  (?!),
 )
import Control.State.Transition.Extended (Embed (..), trans)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import Data.Text (Text)
import GHC.Generics (Generic (..))
import Lens.Micro ((^.))

data DijkstraMempoolPredFailure era
  = LedgerFailure (PredicateFailure (EraRule "LEDGER" era))
  | MempoolFailure Text
  | AllInputsAreSpent
  deriving (Generic)

type instance EraRuleFailure "MEMPOOL" DijkstraEra = DijkstraMempoolPredFailure DijkstraEra

type instance EraRuleEvent "MEMPOOL" DijkstraEra = DijkstraMempoolEvent DijkstraEra

instance InjectRuleFailure "MEMPOOL" DijkstraMempoolPredFailure DijkstraEra

instance InjectRuleFailure "MEMPOOL" ConwayLedgerPredFailure DijkstraEra where
  injectFailure = \case
    ConwayMempoolFailure "All inputs are spent. Transaction has probably already been included" -> AllInputsAreSpent
    ConwayMempoolFailure predFailureMessage -> MempoolFailure predFailureMessage
    otherLedgerFailure -> LedgerFailure $ conwayToDijkstraLedgerPredFailure otherLedgerFailure

instance InjectRuleFailure "MEMPOOL" DijkstraUtxoPredFailure DijkstraEra where
  injectFailure = LedgerFailure . injectFailure

deriving instance
  Eq (PredicateFailure (EraRule "LEDGER" era)) =>
  Eq (DijkstraMempoolPredFailure era)

deriving instance
  Show (PredicateFailure (EraRule "LEDGER" era)) =>
  Show (DijkstraMempoolPredFailure era)

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  EncCBOR (DijkstraMempoolPredFailure era)
  where
  encCBOR =
    encode . \case
      LedgerFailure x -> Sum (LedgerFailure @era) 1 !> To x
      MempoolFailure t -> Sum MempoolFailure 2 !> To t
      AllInputsAreSpent -> Sum AllInputsAreSpent 3

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  DecCBOR (DijkstraMempoolPredFailure era)
  where
  decCBOR = decode . Summands "DijkstraMempoolPredFailure" $ \case
    1 -> SumD (LedgerFailure @era) <! From
    2 -> SumD MempoolFailure <! From
    3 -> SumD AllInputsAreSpent
    n -> Invalid n

newtype DijkstraMempoolEvent era
  = LedgerEvent (Event (EraRule "LEDGER" era))
  deriving (Generic)

deriving instance
  Eq (Event (EraRule "LEDGER" era)) =>
  Eq (DijkstraMempoolEvent era)

instance
  NFData (Event (EraRule "LEDGER" era)) =>
  NFData (DijkstraMempoolEvent era)

instance
  ( EraTx era
  , ConwayEraTxBody era
  , ConwayEraGov era
  , ConwayEraCertState era
  , EraStake era
  , EraCertState era
  , Embed (EraRule "LEDGER" era) (DijkstraMEMPOOL era)
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Eq (PredicateFailure (EraRule "CERTS" era))
  , Eq (PredicateFailure (EraRule "GOV" era))
  , Eq (PredicateFailure (EraRule "UTXOW" era))
  , Eq (PredicateFailure (EraRule "SUBLEDGERS" era))
  , Show (PredicateFailure (EraRule "CERTS" era))
  , Show (PredicateFailure (EraRule "GOV" era))
  , Show (PredicateFailure (EraRule "UTXOW" era))
  , Show (PredicateFailure (EraRule "SUBLEDGERS" era))
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , Tx TopTx era ~ Signal (EraRule "LEDGER" era)
  , EraRuleFailure "SUBLEDGERS" era ~ DijkstraSubLedgersPredFailure era
  ) =>
  STS (DijkstraMEMPOOL era)
  where
  type State (DijkstraMEMPOOL era) = LedgerState era
  type Signal (DijkstraMEMPOOL era) = Tx TopTx era
  type Environment (DijkstraMEMPOOL era) = LedgerEnv era
  type BaseM (DijkstraMEMPOOL era) = ShelleyBase
  type PredicateFailure (DijkstraMEMPOOL era) = DijkstraMempoolPredFailure era
  type Event (DijkstraMEMPOOL era) = DijkstraMempoolEvent era

  transitionRules = [mempoolTransition @era]

mempoolTransition ::
  forall era.
  ( EraTx era
  , Embed (EraRule "LEDGER" era) (DijkstraMEMPOOL era)
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , Tx TopTx era ~ Signal (EraRule "LEDGER" era)
  ) =>
  TransitionRule (DijkstraMEMPOOL era)
mempoolTransition = do
  TRC trc@(_ledgerEnv, ledgerState, tx) <-
    judgmentContext

  -- This rule only gets invoked on transactions within the mempool.
  -- Add checks here that sanitize undesired transactions.

  -- Detect whether the transaction is probably a duplicate
  let
    inputs = tx ^. bodyTxL . inputsTxBodyL
    UTxO utxo = ledgerState ^. utxoG
    notAllSpent = any (`Map.member` utxo) inputs
  notAllSpent
    ?! AllInputsAreSpent

  -- Continue with LEDGER rules if the transaction is not a duplicate,
  whenFailureFreeDefault ledgerState $ do
    trans @(EraRule "LEDGER" era) $ TRC trc

instance
  ( AlonzoEraTx era
  , ConwayEraCertState era
  , DijkstraEraTxBody era
  , ConwayEraGov era
  , EraPlutusContext era
  , GovState era ~ ConwayGovState era
  , Embed (EraRule "CERTS" era) (DijkstraLEDGER era)
  , Embed (EraRule "GOV" era) (DijkstraLEDGER era)
  , Embed (EraRule "UTXOW" era) (DijkstraLEDGER era)
  , Environment (EraRule "CERTS" era) ~ CertsEnv era
  , Environment (EraRule "GOV" era) ~ GovEnv era
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , State (EraRule "CERTS" era) ~ CertState era
  , State (EraRule "GOV" era) ~ Proposals era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , Signal (EraRule "CERTS" era) ~ Seq (TxCert era)
  , Signal (EraRule "GOV" era) ~ GovSignal era
  , Signal (EraRule "UTXOW" era) ~ Tx TopTx era
  , EraRule "LEDGER" era ~ DijkstraLEDGER era
  , EraRule "SUBLEDGERS" era ~ DijkstraSUBLEDGERS era
  , EraRule "SUBLEDGER" era ~ DijkstraSUBLEDGER era
  , EraRule "SUBGOV" era ~ DijkstraSUBGOV era
  , EraRule "SUBUTXO" era ~ DijkstraSUBUTXO era
  , EraRule "SUBUTXOS" era ~ DijkstraSUBUTXOS era
  , EraRule "SUBUTXOW" era ~ DijkstraSUBUTXOW era
  , EraRule "SUBCERTS" era ~ DijkstraSUBCERTS era
  , EraRule "SUBCERT" era ~ DijkstraSUBCERT era
  , EraRule "SUBDELEG" era ~ DijkstraSUBDELEG era
  , EraRule "SUBPOOL" era ~ DijkstraSUBPOOL era
  , EraRule "SUBGOVCERT" era ~ DijkstraSUBGOVCERT era
  , InjectRuleFailure "LEDGER" ShelleyLedgerPredFailure era
  , InjectRuleFailure "LEDGER" ConwayLedgerPredFailure era
  , InjectRuleFailure "LEDGER" DijkstraLedgerPredFailure era
  , InjectRuleEvent "SUBPOOL" DijkstraSubPoolEvent era
  , InjectRuleEvent "SUBPOOL" PoolEvent era
  , InjectRuleFailure "SUBPOOL" DijkstraSubPoolPredFailure era
  , InjectRuleFailure "SUBPOOL" ShelleyPoolPredFailure era
  , InjectRuleFailure "SUBGOVCERT" DijkstraSubGovCertPredFailure era
  , InjectRuleFailure "SUBGOVCERT" ConwayGovCertPredFailure era
  , InjectRuleFailure "DELEG" ConwayDelegPredFailure era
  , InjectRuleFailure "SUBDELEG" ConwayDelegPredFailure era
  , InjectRuleFailure "SUBDELEG" DijkstraSubDelegPredFailure era
  , InjectRuleEvent "SUBGOV" DijkstraSubGovEvent era
  , InjectRuleEvent "SUBGOV" ConwayGovEvent era
  , InjectRuleFailure "SUBGOV" DijkstraSubGovPredFailure era
  , InjectRuleFailure "SUBGOV" ConwayGovPredFailure era
  , TxCert era ~ DijkstraTxCert era
  ) =>
  Embed (DijkstraLEDGER era) (DijkstraMEMPOOL era)
  where
  wrapFailed = LedgerFailure
  wrapEvent = LedgerEvent
