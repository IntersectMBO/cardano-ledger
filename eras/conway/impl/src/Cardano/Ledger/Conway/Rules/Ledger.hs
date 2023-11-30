{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Cardano.Ledger.Address (RewardAcnt (..))
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxowEvent)
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript)
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded (..))
import Cardano.Ledger.Babbage.Rules (BabbageUtxowPredFailure)
import Cardano.Ledger.Babbage.Tx (IsValid (..))
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes (ShelleyBase, StrictMaybe (SJust, SNothing), epochInfoPure)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Block (txid)
import Cardano.Ledger.CertState (certDStateL, dsGenDelegsL)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayCERTS, ConwayGOV, ConwayLEDGER, ConwayUTXOW)
import Cardano.Ledger.Conway.Governance (
  ConwayEraGov (..),
  ConwayGovState (..),
  GovProcedures (..),
  enactStateGovStateL,
  ensPrevGovActionIdsChildrenL,
  ensPrevGovActionIdsL,
  proposalsGovStateL,
 )
import Cardano.Ledger.Conway.PParams (ConwayEraPParams)
import Cardano.Ledger.Conway.Rules.Cert (CertEnv)
import Cardano.Ledger.Conway.Rules.Certs (CertsEnv (CertsEnv), ConwayCertsEvent, ConwayCertsPredFailure)
import Cardano.Ledger.Conway.Rules.Gov (ConwayGovEvent (..), ConwayGovPredFailure, GovEnv (..), GovRuleState (..))
import Cardano.Ledger.Conway.Tx (AlonzoEraTx (..))
import Cardano.Ledger.Conway.TxBody (ConwayEraTxBody (..), currentTreasuryValueTxBodyL)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Shelley.LedgerState (
  CertState (..),
  DState (..),
  LedgerState (..),
  UTxOState (..),
  asTreasuryL,
  utxosGovStateL,
 )
import Cardano.Ledger.Shelley.Rules (
  LedgerEnv (..),
  ShelleyLEDGERS,
  ShelleyLedgersEvent (..),
  ShelleyLedgersPredFailure (..),
  UtxoEnv (..),
  renderDepositEqualsObligationViolation,
  shelleyLedgerAssertions,
 )
import Cardano.Ledger.Slot (epochInfoEpoch)
import Cardano.Ledger.UMap (UView (..), dRepMap)
import qualified Cardano.Ledger.UMap as UMap
import Cardano.Ledger.UTxO (EraUTxO (..))
import Control.DeepSeq (NFData)
import Control.Monad (when)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended (
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  judgmentContext,
  liftSTS,
  trans,
  (?!),
 )
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic (..))
import Lens.Micro
import NoThunks.Class (NoThunks (..))

data ConwayLedgerPredFailure era
  = ConwayUtxowFailure (PredicateFailure (EraRule "UTXOW" era))
  | ConwayCertsFailure (PredicateFailure (EraRule "CERTS" era))
  | ConwayGovFailure (PredicateFailure (EraRule "GOV" era)) -- Subtransition Failures
  | ConwayWdrlNotDelegatedToDRep (Set (Credential 'Staking (EraCrypto era)))
  | ConwayTreasuryValueMismatch
      -- | Actual
      Coin
      -- | Submitted in transaction
      Coin
  deriving (Generic)

deriving instance
  ( Era era
  , Eq (PredicateFailure (EraRule "UTXOW" era))
  , Eq (PredicateFailure (EraRule "CERTS" era))
  , Eq (PredicateFailure (EraRule "GOV" era))
  ) =>
  Eq (ConwayLedgerPredFailure era)

deriving instance
  ( Era era
  , Show (PredicateFailure (EraRule "UTXOW" era))
  , Show (PredicateFailure (EraRule "CERTS" era))
  , Show (PredicateFailure (EraRule "GOV" era))
  ) =>
  Show (ConwayLedgerPredFailure era)

instance
  ( Era era
  , NoThunks (PredicateFailure (EraRule "UTXOW" era))
  , NoThunks (PredicateFailure (EraRule "CERTS" era))
  , NoThunks (PredicateFailure (EraRule "GOV" era))
  ) =>
  NoThunks (ConwayLedgerPredFailure era)

instance
  ( Era era
  , NFData (PredicateFailure (EraRule "UTXOW" era))
  , NFData (PredicateFailure (EraRule "CERTS" era))
  , NFData (PredicateFailure (EraRule "GOV" era))
  ) =>
  NFData (ConwayLedgerPredFailure era)

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "UTXOW" era))
  , EncCBOR (PredicateFailure (EraRule "CERTS" era))
  , EncCBOR (PredicateFailure (EraRule "GOV" era))
  ) =>
  EncCBOR (ConwayLedgerPredFailure era)
  where
  encCBOR =
    encode . \case
      ConwayUtxowFailure x -> Sum (ConwayUtxowFailure @era) 1 !> To x
      ConwayCertsFailure x -> Sum (ConwayCertsFailure @era) 2 !> To x
      ConwayGovFailure x -> Sum (ConwayGovFailure @era) 3 !> To x
      ConwayWdrlNotDelegatedToDRep x ->
        Sum (ConwayWdrlNotDelegatedToDRep @era) 4 !> To x
      ConwayTreasuryValueMismatch actual submitted ->
        Sum (ConwayTreasuryValueMismatch @era) 5 !> To actual !> To submitted

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "UTXOW" era))
  , DecCBOR (PredicateFailure (EraRule "CERTS" era))
  , DecCBOR (PredicateFailure (EraRule "GOV" era))
  ) =>
  DecCBOR (ConwayLedgerPredFailure era)
  where
  decCBOR =
    decode $ Summands "ConwayLedgerPredFailure" $ \case
      1 -> SumD ConwayUtxowFailure <! From
      2 -> SumD ConwayCertsFailure <! From
      3 -> SumD ConwayGovFailure <! From
      4 -> SumD ConwayWdrlNotDelegatedToDRep <! From
      5 -> SumD ConwayTreasuryValueMismatch <! From <! From
      n -> Invalid n

data ConwayLedgerEvent era
  = UtxowEvent (Event (EraRule "UTXOW" era))
  | CertsEvent (Event (EraRule "CERTS" era))
  | GovEvent (Event (EraRule "GOV" era))

instance
  ( AlonzoEraTx era
  , ConwayEraTxBody era
  , ConwayEraGov era
  , GovState era ~ ConwayGovState era
  , Embed (EraRule "UTXOW" era) (ConwayLEDGER era)
  , Embed (EraRule "GOV" era) (ConwayLEDGER era)
  , Embed (EraRule "CERTS" era) (ConwayLEDGER era)
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , State (EraRule "CERTS" era) ~ CertState era
  , State (EraRule "GOV" era) ~ GovRuleState era
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , Environment (EraRule "CERTS" era) ~ CertsEnv era
  , Environment (EraRule "GOV" era) ~ GovEnv era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , Signal (EraRule "CERTS" era) ~ Seq (TxCert era)
  , Signal (EraRule "GOV" era) ~ GovProcedures era
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

  renderAssertionViolation = renderDepositEqualsObligationViolation

  assertions = shelleyLedgerAssertions @era @ConwayLEDGER

-- =======================================

ledgerTransition ::
  forall (someLEDGER :: Type -> Type) era.
  ( AlonzoEraTx era
  , ConwayEraTxBody era
  , ConwayEraGov era
  , GovState era ~ ConwayGovState era
  , Signal (someLEDGER era) ~ Tx era
  , State (someLEDGER era) ~ LedgerState era
  , Environment (someLEDGER era) ~ LedgerEnv era
  , PredicateFailure (someLEDGER era) ~ ConwayLedgerPredFailure era
  , Embed (EraRule "UTXOW" era) (someLEDGER era)
  , Embed (EraRule "GOV" era) (someLEDGER era)
  , Embed (EraRule "CERTS" era) (someLEDGER era)
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , State (EraRule "CERTS" era) ~ CertState era
  , State (EraRule "GOV" era) ~ GovRuleState era
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , Environment (EraRule "GOV" era) ~ GovEnv era
  , Environment (EraRule "CERTS" era) ~ CertsEnv era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , Signal (EraRule "CERTS" era) ~ Seq (TxCert era)
  , Signal (EraRule "GOV" era) ~ GovProcedures era
  , BaseM (someLEDGER era) ~ ShelleyBase
  , STS (someLEDGER era)
  ) =>
  TransitionRule (someLEDGER era)
ledgerTransition = do
  TRC (LedgerEnv slot _txIx pp account, LedgerState utxoState certState, tx) <- judgmentContext

  let actualTreasuryValue = account ^. asTreasuryL
   in case tx ^. bodyTxL . currentTreasuryValueTxBodyL of
        SNothing -> pure ()
        SJust submittedTreasuryValue ->
          submittedTreasuryValue
            == actualTreasuryValue
              ?! ConwayTreasuryValueMismatch actualTreasuryValue submittedTreasuryValue

  currentEpoch <- liftSTS $ do
    ei <- asks epochInfoPure
    epochInfoEpoch ei slot

  let txBody = tx ^. bodyTxL

  (utxoState', certStateAfterCERTS) <-
    if tx ^. isValidTxL == IsValid True
      then do
        certStateAfterCERTS <-
          trans @(EraRule "CERTS" era) $
            TRC
              ( CertsEnv tx pp slot currentEpoch
              , certState
              , StrictSeq.fromStrict $ txBody ^. certsTxBodyL
              )
        let wdrlAddrs = Map.keysSet . unWithdrawals $ tx ^. bodyTxL . withdrawalsTxBodyL
            wdrlCreds = Set.map getRwdCred wdrlAddrs
            dUnified = dsUnified $ certDState certStateAfterCERTS
            delegatedAddrs = DRepUView dUnified

        -- TODO enable this check once delegation is fully implemented in cardano-api
        when False $ do
          all (`UMap.member` delegatedAddrs) wdrlCreds
            ?! ConwayWdrlNotDelegatedToDRep (wdrlCreds Set.\\ Map.keysSet (dRepMap dUnified))

        -- Votes and proposals from signal tx
        let govProcedures =
              GovProcedures
                { gpVotingProcedures = txBody ^. votingProceduresTxBodyL
                , gpProposalProcedures = txBody ^. proposalProceduresTxBodyL
                }
        GovRuleState govActionsState' prevGovActionIdsChildren <-
          trans @(EraRule "GOV" era) $
            TRC
              ( GovEnv
                  (txid txBody)
                  currentEpoch
                  pp
                  (utxoState ^. utxosGovStateL . enactStateGovStateL . ensPrevGovActionIdsL)
              , GovRuleState
                  (utxoState ^. utxosGovStateL . proposalsGovStateL)
                  (utxoState ^. utxosGovStateL . enactStateGovStateL . ensPrevGovActionIdsChildrenL)
              , govProcedures
              )
        let utxoState' =
              utxoState
                & utxosGovStateL . proposalsGovStateL .~ govActionsState'
                & utxosGovStateL . enactStateGovStateL . ensPrevGovActionIdsChildrenL .~ prevGovActionIdsChildren
        pure (utxoState', certStateAfterCERTS)
      else pure (utxoState, certState)

  utxoState'' <-
    trans @(EraRule "UTXOW" era) $
      TRC
        -- Pass to UTXOW the unmodified CertState in its Environment, so it can process
        -- refunds of deposits for deregistering stake credentials and DReps.
        -- The modified CertState (certStateAfterCERTS) has these already removed from its UMap.
        ( UtxoEnv @era slot pp certState (certState ^. certDStateL . dsGenDelegsL)
        , utxoState'
        , tx
        )
  pure $ LedgerState utxoState'' certStateAfterCERTS

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
  , ConwayEraTxBody era
  , ConwayEraPParams era
  , ConwayEraGov era
  , Embed (EraRule "CERT" era) (ConwayCERTS era)
  , State (EraRule "CERT" era) ~ CertState era
  , Environment (EraRule "CERT" era) ~ CertEnv era
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
  , Embed (EraRule "GOV" era) (ConwayLEDGER era)
  , ConwayEraGov era
  , AlonzoEraTx era
  , ConwayEraTxBody era
  , ConwayEraPParams era
  , GovState era ~ ConwayGovState era
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , Environment (EraRule "CERTS" era) ~ CertsEnv era
  , Environment (EraRule "GOV" era) ~ GovEnv era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , Signal (EraRule "CERTS" era) ~ Seq (TxCert era)
  , Signal (EraRule "GOV" era) ~ GovProcedures era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , State (EraRule "CERTS" era) ~ CertState era
  , State (EraRule "GOV" era) ~ GovRuleState era
  , PredicateFailure (EraRule "LEDGER" era) ~ ConwayLedgerPredFailure era
  , Event (EraRule "LEDGER" era) ~ ConwayLedgerEvent era
  , EraGov era
  ) =>
  Embed (ConwayLEDGER era) (ShelleyLEDGERS era)
  where
  wrapFailed = LedgerFailure
  wrapEvent = LedgerEvent

instance
  ( ConwayEraPParams era
  , BaseM (ConwayLEDGER era) ~ ShelleyBase
  , PredicateFailure (EraRule "GOV" era) ~ ConwayGovPredFailure era
  , Event (EraRule "GOV" era) ~ ConwayGovEvent era
  ) =>
  Embed (ConwayGOV era) (ConwayLEDGER era)
  where
  wrapFailed = ConwayGovFailure
  wrapEvent = GovEvent
