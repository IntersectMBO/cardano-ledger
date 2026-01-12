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
  DijkstraLEDGER,
  DijkstraLedgerPredFailure (..),
  DijkstraLedgerEvent (..),
  shelleyToDijkstraLedgerPredFailure,
  conwayToDijkstraLedgerPredFailure,
) where

import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Alonzo (AlonzoScript)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
  AlonzoUtxowEvent,
  AlonzoUtxowPredFailure,
 )
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded)
import Cardano.Ledger.Babbage (BabbageTxOut)
import Cardano.Ledger.Babbage.Rules (
  BabbageUtxoPredFailure,
  BabbageUtxowPredFailure,
 )
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
 )
import Cardano.Ledger.Conway.Rules (
  CertEnv,
  CertsEnv (..),
  ConwayCERTS,
  ConwayCertEvent (..),
  ConwayCertPredFailure (..),
  ConwayCertsEvent (..),
  ConwayCertsPredFailure (..),
  ConwayDELEG,
  ConwayDelegPredFailure,
  ConwayGovCertPredFailure,
  ConwayGovPredFailure,
  ConwayLedgerPredFailure,
  ConwayUtxoPredFailure,
  ConwayUtxosPredFailure,
  ConwayUtxowPredFailure,
  GovEnv (..),
  GovSignal (..),
  conwayLedgerTransitionTRC,
 )
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Dijkstra.Era (
  DijkstraEra,
  DijkstraGOV,
  DijkstraLEDGER,
  DijkstraSUBCERT,
  DijkstraSUBCERTS,
  DijkstraSUBDELEG,
  DijkstraSUBGOV,
  DijkstraSUBGOVCERT,
  DijkstraSUBPOOL,
  DijkstraSUBUTXO,
  DijkstraSUBUTXOS,
  DijkstraSUBUTXOW,
  DijkstraUTXOW,
 )
import Cardano.Ledger.Dijkstra.Rules.Certs ()
import Cardano.Ledger.Dijkstra.Rules.Gov (DijkstraGovPredFailure)
import Cardano.Ledger.Dijkstra.Rules.GovCert (DijkstraGovCertPredFailure)
import Cardano.Ledger.Dijkstra.Rules.SubLedger
import Cardano.Ledger.Dijkstra.Rules.SubLedgers
import Cardano.Ledger.Dijkstra.Rules.Utxo (DijkstraUtxoPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Utxow (DijkstraUtxowPredFailure)
import Cardano.Ledger.Dijkstra.TxBody
import Cardano.Ledger.Dijkstra.TxCert
import Cardano.Ledger.Shelley.LedgerState (
  LedgerState (..),
  UTxOState (..),
 )
import Cardano.Ledger.Shelley.Rules (
  LedgerEnv (..),
  ShelleyLEDGERS,
  ShelleyLedgerPredFailure (..),
  ShelleyLedgersEvent (LedgerEvent),
  ShelleyLedgersPredFailure (LedgerFailure),
  ShelleyPoolPredFailure,
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure,
  UtxoEnv (..),
  renderDepositEqualsObligationViolation,
  shelleyLedgerAssertions,
 )
import Cardano.Ledger.TxIn (TxId, TxIn (..))
import Control.DeepSeq (NFData)
import Control.State.Transition.Extended
import Data.List.NonEmpty (NonEmpty)
import Data.Map.NonEmpty (NonEmptyMap)
import qualified Data.Map.Strict as Map
import qualified Data.OMap.Strict as OMap
import Data.Sequence (Seq)
import qualified Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import qualified Data.Set.NonEmpty as NES
import GHC.Generics (Generic (..))
import Lens.Micro
import NoThunks.Class (NoThunks (..))

data DijkstraLedgerPredFailure era
  = DijkstraUtxowFailure (PredicateFailure (EraRule "UTXOW" era))
  | DijkstraCertsFailure (PredicateFailure (EraRule "CERTS" era))
  | DijkstraGovFailure (PredicateFailure (EraRule "GOV" era))
  | DijkstraWdrlNotDelegatedToDRep (NonEmpty (KeyHash Staking))
  | DijkstraTreasuryValueMismatch (Mismatch RelEQ Coin)
  | DijkstraTxRefScriptsSizeTooBig (Mismatch RelLTEQ Int)
  | DijkstraWithdrawalsMissingAccounts Withdrawals
  | DijkstraIncompleteWithdrawals (NonEmptyMap RewardAccount (Mismatch RelEQ Coin))
  | DijkstraSubLedgersFailure (PredicateFailure (EraRule "SUBLEDGERS" era))
  | DijkstraSpendingOutputFromSameTx (NonEmptyMap TxId (NonEmptySet TxIn))
  deriving (Generic)

type instance EraRuleFailure "LEDGER" DijkstraEra = DijkstraLedgerPredFailure DijkstraEra

type instance EraRuleEvent "LEDGER" DijkstraEra = DijkstraLedgerEvent DijkstraEra

instance InjectRuleEvent "LEDGER" DijkstraLedgerEvent DijkstraEra

instance InjectRuleFailure "LEDGER" DijkstraLedgerPredFailure DijkstraEra

instance InjectRuleFailure "LEDGER" ConwayLedgerPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraLedgerPredFailure

instance InjectRuleFailure "LEDGER" ShelleyLedgerPredFailure DijkstraEra where
  injectFailure = shelleyToDijkstraLedgerPredFailure

instance InjectRuleFailure "LEDGER" DijkstraUtxowPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure

instance InjectRuleFailure "LEDGER" ConwayUtxowPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" BabbageUtxowPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AlonzoUtxowPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" DijkstraUtxoPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ConwayUtxoPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" BabbageUtxoPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AlonzoUtxoPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ConwayUtxosPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AllegraUtxoPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ConwayCertsPredFailure DijkstraEra where
  injectFailure = DijkstraCertsFailure

instance InjectRuleFailure "LEDGER" ConwayCertPredFailure DijkstraEra where
  injectFailure = DijkstraCertsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ConwayDelegPredFailure DijkstraEra where
  injectFailure = DijkstraCertsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyPoolPredFailure DijkstraEra where
  injectFailure = DijkstraCertsFailure . injectFailure

instance InjectRuleFailure "LEDGER" DijkstraGovCertPredFailure DijkstraEra where
  injectFailure = DijkstraCertsFailure . injectFailure

instance InjectRuleFailure "LEDGER" DijkstraGovPredFailure DijkstraEra where
  injectFailure = DijkstraGovFailure . injectFailure

instance InjectRuleFailure "LEDGER" ConwayGovCertPredFailure DijkstraEra where
  injectFailure = DijkstraCertsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ConwayGovPredFailure DijkstraEra where
  injectFailure = DijkstraGovFailure . injectFailure

instance InjectRuleFailure "LEDGER" DijkstraSubLedgersPredFailure DijkstraEra where
  injectFailure = DijkstraSubLedgersFailure . injectFailure

deriving instance
  ( Era era
  , Eq (PredicateFailure (EraRule "UTXOW" era))
  , Eq (PredicateFailure (EraRule "CERTS" era))
  , Eq (PredicateFailure (EraRule "GOV" era))
  , Eq (PredicateFailure (EraRule "SUBLEDGERS" era))
  ) =>
  Eq (DijkstraLedgerPredFailure era)

deriving instance
  ( Era era
  , Show (PredicateFailure (EraRule "UTXOW" era))
  , Show (PredicateFailure (EraRule "CERTS" era))
  , Show (PredicateFailure (EraRule "GOV" era))
  , Show (PredicateFailure (EraRule "SUBLEDGERS" era))
  ) =>
  Show (DijkstraLedgerPredFailure era)

instance
  ( Era era
  , NoThunks (PredicateFailure (EraRule "UTXOW" era))
  , NoThunks (PredicateFailure (EraRule "CERTS" era))
  , NoThunks (PredicateFailure (EraRule "GOV" era))
  , NoThunks (PredicateFailure (EraRule "SUBLEDGERS" era))
  ) =>
  NoThunks (DijkstraLedgerPredFailure era)

instance
  ( Era era
  , NFData (PredicateFailure (EraRule "UTXOW" era))
  , NFData (PredicateFailure (EraRule "CERTS" era))
  , NFData (PredicateFailure (EraRule "GOV" era))
  , NFData (PredicateFailure (EraRule "SUBLEDGERS" era))
  ) =>
  NFData (DijkstraLedgerPredFailure era)

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "UTXOW" era))
  , EncCBOR (PredicateFailure (EraRule "CERTS" era))
  , EncCBOR (PredicateFailure (EraRule "GOV" era))
  , EncCBOR (PredicateFailure (EraRule "SUBLEDGERS" era))
  ) =>
  EncCBOR (DijkstraLedgerPredFailure era)
  where
  encCBOR =
    encode . \case
      DijkstraUtxowFailure x -> Sum (DijkstraUtxowFailure @era) 1 !> To x
      DijkstraCertsFailure x -> Sum (DijkstraCertsFailure @era) 2 !> To x
      DijkstraGovFailure x -> Sum (DijkstraGovFailure @era) 3 !> To x
      DijkstraWdrlNotDelegatedToDRep x -> Sum (DijkstraWdrlNotDelegatedToDRep @era) 4 !> To x
      DijkstraTreasuryValueMismatch mm -> Sum (DijkstraTreasuryValueMismatch @era) 5 !> To mm
      DijkstraTxRefScriptsSizeTooBig mm -> Sum DijkstraTxRefScriptsSizeTooBig 6 !> To mm
      DijkstraWithdrawalsMissingAccounts w -> Sum DijkstraWithdrawalsMissingAccounts 7 !> To w
      DijkstraIncompleteWithdrawals w -> Sum DijkstraIncompleteWithdrawals 8 !> To w
      DijkstraSubLedgersFailure w -> Sum DijkstraSubLedgersFailure 9 !> To w
      DijkstraSpendingOutputFromSameTx txIds -> Sum DijkstraSpendingOutputFromSameTx 10 !> To txIds

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "UTXOW" era))
  , DecCBOR (PredicateFailure (EraRule "CERTS" era))
  , DecCBOR (PredicateFailure (EraRule "GOV" era))
  , DecCBOR (PredicateFailure (EraRule "SUBLEDGERS" era))
  ) =>
  DecCBOR (DijkstraLedgerPredFailure era)
  where
  decCBOR = decode . Summands "DijkstraLedgerPredFailure" $ \case
    1 -> SumD DijkstraUtxowFailure <! From
    2 -> SumD DijkstraCertsFailure <! From
    3 -> SumD DijkstraGovFailure <! From
    4 -> SumD DijkstraWdrlNotDelegatedToDRep <! From
    5 -> SumD DijkstraTreasuryValueMismatch <! From
    6 -> SumD DijkstraTxRefScriptsSizeTooBig <! From
    7 -> SumD DijkstraWithdrawalsMissingAccounts <! From
    8 -> SumD DijkstraIncompleteWithdrawals <! From
    9 -> SumD DijkstraSubLedgersFailure <! From
    10 -> SumD DijkstraSpendingOutputFromSameTx <! From
    n -> Invalid n

data DijkstraLedgerEvent era
  = UtxowEvent (Event (EraRule "UTXOW" era))
  | CertsEvent (Event (EraRule "CERTS" era))
  | GovEvent (Event (EraRule "GOV" era))
  | SubLedgersEvent (Event (EraRule "SUBLEDGERS" era))
  deriving (Generic)

deriving instance
  ( Eq (Event (EraRule "CERTS" era))
  , Eq (Event (EraRule "UTXOW" era))
  , Eq (Event (EraRule "GOV" era))
  , Eq (Event (EraRule "SUBLEDGERS" era))
  ) =>
  Eq (DijkstraLedgerEvent era)

instance
  ( NFData (Event (EraRule "CERTS" era))
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
  , GovState era ~ ConwayGovState era
  , Embed (EraRule "UTXOW" era) (DijkstraLEDGER era)
  , Embed (EraRule "GOV" era) (DijkstraLEDGER era)
  , Embed (EraRule "CERTS" era) (DijkstraLEDGER era)
  , Embed (EraRule "SUBLEDGERS" era) (DijkstraLEDGER era)
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , State (EraRule "CERTS" era) ~ CertState era
  , State (EraRule "GOV" era) ~ Proposals era
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , Environment (EraRule "CERTS" era) ~ CertsEnv era
  , Environment (EraRule "GOV" era) ~ GovEnv era
  , Signal (EraRule "UTXOW" era) ~ Tx TopTx era
  , Signal (EraRule "CERTS" era) ~ Seq (TxCert era)
  , Signal (EraRule "GOV" era) ~ GovSignal era
  , ConwayEraCertState era
  , EraRule "LEDGER" era ~ DijkstraLEDGER era
  , InjectRuleFailure "LEDGER" ShelleyLedgerPredFailure era
  , InjectRuleFailure "LEDGER" ConwayLedgerPredFailure era
  , InjectRuleFailure "LEDGER" DijkstraLedgerPredFailure era
  , EraRule "SUBLEDGERS" era ~ DijkstraSUBLEDGERS era
  ) =>
  STS (DijkstraLEDGER era)
  where
  type State (DijkstraLEDGER era) = LedgerState era
  type Signal (DijkstraLEDGER era) = Tx TopTx era
  type Environment (DijkstraLEDGER era) = LedgerEnv era
  type BaseM (DijkstraLEDGER era) = ShelleyBase
  type PredicateFailure (DijkstraLEDGER era) = DijkstraLedgerPredFailure era
  type Event (DijkstraLEDGER era) = DijkstraLedgerEvent era

  initialRules = []
  transitionRules = [dijkstraLedgerTransition]

  renderAssertionViolation = renderDepositEqualsObligationViolation

  assertions = shelleyLedgerAssertions @era @DijkstraLEDGER

-- | A transaction should not be able to spend its own outputs.
-- Finds all spendable inputs in the entire transaction that are sub-transaction outputs (TxIds).
spentSubTxOutputs ::
  (EraTx era, DijkstraEraTxBody era) => Tx TopTx era -> Map.Map TxId (NonEmptySet TxIn)
spentSubTxOutputs tx =
  filterBadInputs subTxs <> filterBadInputs (Map.singleton (txIdTx tx) tx)
  where
    subTxs = OMap.toMap $ tx ^. bodyTxL . subTransactionsTxBodyL
    subTxIds = Map.keysSet subTxs -- None of these should be present as a spendable input in the entire transaction
    filterBadInputs :: EraTx era => Map.Map TxId (Tx l era) -> Map.Map TxId (NonEmptySet TxIn)
    filterBadInputs = Map.mapMaybe $ \curTx -> do
      let spendableInputs = curTx ^. bodyTxL . spendableInputsTxBodyF
      NES.fromSet $
        Set.filter (\(TxIn txId _) -> txId `Set.member` subTxIds) spendableInputs

dijkstraLedgerTransition ::
  forall era.
  ( AlonzoEraTx era
  , ConwayEraCertState era
  , ConwayEraGov era
  , DijkstraEraTxBody era
  , GovState era ~ ConwayGovState era
  , Embed (EraRule "UTXOW" era) (DijkstraLEDGER era)
  , Embed (EraRule "GOV" era) (DijkstraLEDGER era)
  , Embed (EraRule "CERTS" era) (DijkstraLEDGER era)
  , Embed (EraRule "SUBLEDGERS" era) (DijkstraLEDGER era)
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , State (EraRule "CERTS" era) ~ CertState era
  , State (EraRule "GOV" era) ~ Proposals era
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , Environment (EraRule "GOV" era) ~ GovEnv era
  , Environment (EraRule "CERTS" era) ~ CertsEnv era
  , Signal (EraRule "UTXOW" era) ~ Tx TopTx era
  , Signal (EraRule "CERTS" era) ~ Seq (TxCert era)
  , Signal (EraRule "GOV" era) ~ GovSignal era
  , STS (DijkstraLEDGER era)
  , EraRule "LEDGER" era ~ DijkstraLEDGER era
  , EraRule "SUBLEDGERS" era ~ DijkstraSUBLEDGERS era
  , InjectRuleFailure "LEDGER" ShelleyLedgerPredFailure era
  , InjectRuleFailure "LEDGER" ConwayLedgerPredFailure era
  ) =>
  TransitionRule (DijkstraLEDGER era)
dijkstraLedgerTransition = do
  TRC (env, ledgerState, tx) <- judgmentContext

  failOnNonEmptyMap (spentSubTxOutputs tx) DijkstraSpendingOutputFromSameTx

  ledgerStateAfterSubledgers <-
    trans @(EraRule "SUBLEDGERS" era) $
      TRC (env, ledgerState, tx ^. bodyTxL . subTransactionsTxBodyL)
  conwayLedgerTransitionTRC (TRC (env, ledgerStateAfterSubledgers, tx))

instance
  ( AlonzoEraTx era
  , EraUTxO era
  , BabbageEraTxBody era
  , Embed (EraRule "UTXO" era) (DijkstraUTXOW era)
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Environment (EraRule "UTXO" era) ~ UtxoEnv era
  , Script era ~ AlonzoScript era
  , TxOut era ~ BabbageTxOut era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (EraRule "UTXO" era) ~ Tx TopTx era
  , PredicateFailure (EraRule "UTXOW" era) ~ DijkstraUtxowPredFailure era
  , Event (EraRule "UTXOW" era) ~ AlonzoUtxowEvent era
  , STS (DijkstraUTXOW era)
  , Event (DijkstraUTXOW era) ~ AlonzoUtxowEvent era
  ) =>
  Embed (DijkstraUTXOW era) (DijkstraLEDGER era)
  where
  wrapFailed = DijkstraUtxowFailure
  wrapEvent = UtxowEvent

instance
  ( Embed (EraRule "UTXOW" era) (DijkstraLEDGER era)
  , Embed (EraRule "CERTS" era) (DijkstraLEDGER era)
  , Embed (EraRule "GOV" era) (DijkstraLEDGER era)
  , Embed (EraRule "SUBLEDGERS" era) (DijkstraSUBLEDGERS era)
  , ConwayEraGov era
  , AlonzoEraTx era
  , ConwayEraPParams era
  , DijkstraEraTxBody era
  , GovState era ~ ConwayGovState era
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , Environment (EraRule "CERTS" era) ~ CertsEnv era
  , Signal (EraRule "UTXOW" era) ~ Tx TopTx era
  , Signal (EraRule "CERTS" era) ~ Seq (TxCert era)
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , State (EraRule "CERTS" era) ~ CertState era
  , EraRule "GOV" era ~ DijkstraGOV era
  , ConwayEraCertState era
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
  , InjectRuleFailure "LEDGER" DijkstraSubLedgersPredFailure era
  , TxCert era ~ DijkstraTxCert era
  ) =>
  Embed (DijkstraLEDGER era) (ShelleyLEDGERS era)
  where
  wrapFailed = LedgerFailure
  wrapEvent = LedgerEvent

instance
  ( ConwayEraTxCert era
  , ConwayEraPParams era
  , ConwayEraGov era
  , EraRule "GOV" era ~ DijkstraGOV era
  , InjectRuleFailure "GOV" ConwayGovPredFailure era
  , InjectRuleFailure "GOV" DijkstraGovPredFailure era
  , ConwayEraCertState era
  ) =>
  Embed (DijkstraGOV era) (DijkstraLEDGER era)
  where
  wrapFailed = DijkstraGovFailure
  wrapEvent = GovEvent

conwayToDijkstraLedgerPredFailure ::
  forall era. ConwayLedgerPredFailure era -> DijkstraLedgerPredFailure era
conwayToDijkstraLedgerPredFailure = \case
  Conway.ConwayUtxowFailure f -> DijkstraUtxowFailure f
  Conway.ConwayCertsFailure f -> DijkstraCertsFailure f
  Conway.ConwayGovFailure f -> DijkstraGovFailure f
  Conway.ConwayWdrlNotDelegatedToDRep kh -> DijkstraWdrlNotDelegatedToDRep kh
  Conway.ConwayTreasuryValueMismatch mm -> DijkstraTreasuryValueMismatch mm
  Conway.ConwayTxRefScriptsSizeTooBig mm -> DijkstraTxRefScriptsSizeTooBig mm
  Conway.ConwayMempoolFailure _ -> error "Impossible: MempoolFailure has been moved to MEMPOOL rule in Dijkstra"
  Conway.ConwayWithdrawalsMissingAccounts ws -> DijkstraWithdrawalsMissingAccounts ws
  Conway.ConwayIncompleteWithdrawals ws -> DijkstraIncompleteWithdrawals ws

shelleyToDijkstraLedgerPredFailure ::
  forall era. ShelleyLedgerPredFailure era -> DijkstraLedgerPredFailure era
shelleyToDijkstraLedgerPredFailure = \case
  UtxowFailure x -> DijkstraUtxowFailure x
  DelegsFailure _ -> error "Impossible: DELEGS has ben removed in Dijkstra"
  ShelleyWithdrawalsMissingAccounts x -> DijkstraWithdrawalsMissingAccounts x
  ShelleyIncompleteWithdrawals x -> DijkstraIncompleteWithdrawals x

instance
  ( EraTx era
  , ConwayEraTxBody era
  , ConwayEraPParams era
  , ConwayEraGov era
  , Embed (EraRule "CERT" era) (ConwayCERTS era)
  , State (EraRule "CERT" era) ~ CertState era
  , Environment (EraRule "CERT" era) ~ CertEnv era
  , Signal (EraRule "CERT" era) ~ TxCert era
  , PredicateFailure (EraRule "CERT" era) ~ ConwayCertPredFailure era
  , EraRuleFailure "CERT" era ~ ConwayCertPredFailure era
  , EraRule "CERTS" era ~ ConwayCERTS era
  , ConwayEraCertState era
  ) =>
  Embed (ConwayCERTS era) (DijkstraLEDGER era)
  where
  wrapFailed = DijkstraCertsFailure
  wrapEvent = CertsEvent

instance
  ( EraPParams era
  , EraRule "DELEG" era ~ ConwayDELEG era
  , PredicateFailure (EraRule "CERTS" era) ~ ConwayCertsPredFailure era
  , PredicateFailure (EraRule "CERT" era) ~ ConwayCertPredFailure era
  , Event (EraRule "CERTS" era) ~ ConwayCertsEvent era
  , Event (EraRule "CERT" era) ~ ConwayCertEvent era
  , ConwayEraCertState era
  ) =>
  Embed (ConwayDELEG era) (DijkstraLEDGER era)
  where
  wrapFailed = DijkstraCertsFailure . CertFailure . DelegFailure
  wrapEvent = CertsEvent . CertEvent . DelegEvent

instance
  ( EraTx era
  , ConwayEraTxBody era
  , ConwayEraGov era
  , ConwayEraCertState era
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
  , Event (EraRule "LEDGER" era) ~ DijkstraLedgerEvent era
  , TxCert era ~ DijkstraTxCert era
  ) =>
  Embed (DijkstraSUBLEDGERS era) (DijkstraLEDGER era)
  where
  wrapFailed = DijkstraSubLedgersFailure
  wrapEvent = SubLedgersEvent
