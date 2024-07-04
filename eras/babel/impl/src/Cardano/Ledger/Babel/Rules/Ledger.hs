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

module Cardano.Ledger.Babel.Rules.Ledger (
  BabelLEDGER,
  BabelLedgerPredFailure (..),
  BabelLedgerEvent (..),
) where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..))
import Cardano.Crypto.Hash.Class (Hash)
import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxosPredFailure,
  AlonzoUtxowEvent,
 )
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript)
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded (..))
import Cardano.Ledger.Babbage.Rules (
  BabbageUtxoPredFailure,
 )
import Cardano.Ledger.Babbage.Tx (IsValid (..))
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.Babel.Core (BabelEraTxBody)
import Cardano.Ledger.Babel.Era (
  BabelEra,
  BabelLEDGER,
  BabelUTXOW,
 )
import Cardano.Ledger.Babel.LedgerState.Types (BabelLedgerState)
import Cardano.Ledger.Babel.Rules.Cert ()
import Cardano.Ledger.Babel.Rules.Certs ()
import Cardano.Ledger.Babel.Rules.Deleg ()
import Cardano.Ledger.Babel.Rules.Gov ()
import Cardano.Ledger.Babel.Rules.GovCert ()
import Cardano.Ledger.Babel.Rules.Utxo (BabelUtxoPredFailure)
import Cardano.Ledger.Babel.Rules.Utxos (BabelUtxosPredFailure)
import Cardano.Ledger.Babel.Rules.Utxow (BabelUtxowPredFailure)
import Cardano.Ledger.BaseTypes (ShelleyBase, StrictMaybe (..), epochInfoPure)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (
  ConwayEraGov (..),
  ConwayGovState (..),
  GovProcedures (..),
  Proposals,
  constitutionScriptL,
  pRootsL,
  proposalsGovStateL,
  toPrevGovActionIds,
 )
import Cardano.Ledger.Conway.Rules (
  CertEnv,
  CertsEnv (CertsEnv),
  ConwayCERTS,
  ConwayCertPredFailure,
  ConwayCertsEvent,
  ConwayCertsPredFailure,
  ConwayDelegPredFailure,
  ConwayGOV,
  ConwayGovCertPredFailure,
  ConwayGovEvent,
  ConwayGovPredFailure,
  GovEnv (GovEnv),
 )
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Shelley.LedgerState (
  CertState (..),
  DState (..),
  HasLedgerState (..),
  UTxOState (..),
  asTreasuryL,
  certVStateL,
  utxosGovStateL,
  vsCommitteeStateL,
 )
import Cardano.Ledger.Shelley.Rules (
  LedgerEnv (..),
  ShelleyPoolPredFailure,
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure,
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
import Lens.Micro as L
import NoThunks.Class (NoThunks (..))

data BabelLedgerPredFailure era
  = BabelUtxowFailure (PredicateFailure (EraRule "UTXOW" era))
  | BabelCertsFailure (PredicateFailure (EraRule "CERTS" era))
  | BabelGovFailure (PredicateFailure (EraRule "GOV" era))
  | BabelWdrlNotDelegatedToDRep (Set (Credential 'Staking (EraCrypto era)))
  | BabelTreasuryValueMismatch
      -- | Actual
      Coin
      -- | Submitted in transaction
      Coin
  deriving (Generic)

type instance EraRuleFailure "LEDGER" (BabelEra c) = BabelLedgerPredFailure (BabelEra c)

type instance EraRuleEvent "LEDGER" (BabelEra c) = BabelLedgerEvent (BabelEra c)

instance InjectRuleFailure "LEDGER" BabelLedgerPredFailure (BabelEra c)

instance InjectRuleFailure "LEDGER" BabelUtxowPredFailure (BabelEra c) where
  injectFailure = BabelUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure (BabelEra c) where
  injectFailure = BabelUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" BabelUtxoPredFailure (BabelEra c) where
  injectFailure = BabelUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" BabbageUtxoPredFailure (BabelEra c) where
  injectFailure = BabelUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure (BabelEra c) where
  injectFailure = BabelUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" BabelUtxosPredFailure (BabelEra c) where
  injectFailure = BabelUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure (BabelEra c) where
  injectFailure = BabelUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ConwayCertsPredFailure (BabelEra c) where
  injectFailure = BabelCertsFailure

instance InjectRuleFailure "LEDGER" ConwayCertPredFailure (BabelEra c) where
  injectFailure = BabelCertsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ConwayDelegPredFailure (BabelEra c) where
  injectFailure = BabelCertsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyPoolPredFailure (BabelEra c) where
  injectFailure = BabelCertsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ConwayGovCertPredFailure (BabelEra c) where
  injectFailure = BabelCertsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ConwayGovPredFailure (BabelEra c) where
  injectFailure = BabelGovFailure . injectFailure

deriving instance
  ( Era era
  , Eq (PredicateFailure (EraRule "UTXOW" era))
  , Eq (PredicateFailure (EraRule "CERTS" era))
  , Eq (PredicateFailure (EraRule "GOV" era))
  ) =>
  Eq (BabelLedgerPredFailure era)

deriving instance
  ( Era era
  , Show (PredicateFailure (EraRule "UTXOW" era))
  , Show (PredicateFailure (EraRule "CERTS" era))
  , Show (PredicateFailure (EraRule "GOV" era))
  ) =>
  Show (BabelLedgerPredFailure era)

instance
  ( Era era
  , NoThunks (PredicateFailure (EraRule "UTXOW" era))
  , NoThunks (PredicateFailure (EraRule "CERTS" era))
  , NoThunks (PredicateFailure (EraRule "GOV" era))
  ) =>
  NoThunks (BabelLedgerPredFailure era)

instance
  ( Era era
  , NFData (PredicateFailure (EraRule "UTXOW" era))
  , NFData (PredicateFailure (EraRule "CERTS" era))
  , NFData (PredicateFailure (EraRule "GOV" era))
  ) =>
  NFData (BabelLedgerPredFailure era)

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "UTXOW" era))
  , EncCBOR (PredicateFailure (EraRule "CERTS" era))
  , EncCBOR (PredicateFailure (EraRule "GOV" era))
  ) =>
  EncCBOR (BabelLedgerPredFailure era)
  where
  encCBOR =
    encode . \case
      BabelUtxowFailure x -> Sum (BabelUtxowFailure @era) 1 !> To x
      BabelCertsFailure x -> Sum (BabelCertsFailure @era) 2 !> To x
      BabelGovFailure x -> Sum (BabelGovFailure @era) 3 !> To x
      BabelWdrlNotDelegatedToDRep x ->
        Sum (BabelWdrlNotDelegatedToDRep @era) 4 !> To x
      BabelTreasuryValueMismatch actual submitted ->
        Sum (BabelTreasuryValueMismatch @era) 5 !> To actual !> To submitted

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "UTXOW" era))
  , DecCBOR (PredicateFailure (EraRule "CERTS" era))
  , DecCBOR (PredicateFailure (EraRule "GOV" era))
  ) =>
  DecCBOR (BabelLedgerPredFailure era)
  where
  decCBOR =
    decode $ Summands "BabelLedgerPredFailure" $ \case
      1 -> SumD BabelUtxowFailure <! From
      2 -> SumD BabelCertsFailure <! From
      3 -> SumD BabelGovFailure <! From
      4 -> SumD BabelWdrlNotDelegatedToDRep <! From
      5 -> SumD BabelTreasuryValueMismatch <! From <! From
      n -> Invalid n

data BabelLedgerEvent era
  = UtxowEvent (Event (EraRule "UTXOW" era))
  | CertsEvent (Event (EraRule "CERTS" era))
  | GovEvent (Event (EraRule "GOV" era))
  deriving (Generic)

deriving instance
  ( Eq (Event (EraRule "CERTS" era))
  , Eq (Event (EraRule "UTXOW" era))
  , Eq (Event (EraRule "GOV" era))
  ) =>
  Eq (BabelLedgerEvent era)

instance
  ( NFData (Event (EraRule "CERTS" era))
  , NFData (Event (EraRule "UTXOW" era))
  , NFData (Event (EraRule "GOV" era))
  ) =>
  NFData (BabelLedgerEvent era)

instance
  ( AlonzoEraTx era
  , BabelEraTxBody era
  , ConwayEraGov era
  , GovState era ~ ConwayGovState era
  , Embed (EraRule "UTXOW" era) (BabelLEDGER era)
  , Embed (EraRule "GOV" era) (BabelLEDGER era)
  , Embed (EraRule "CERTS" era) (BabelLEDGER era)
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , State (EraRule "CERTS" era) ~ CertState era
  , State (EraRule "GOV" era) ~ Proposals era
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , Environment (EraRule "CERTS" era) ~ CertsEnv era
  , Environment (EraRule "GOV" era) ~ GovEnv era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , Signal (EraRule "CERTS" era) ~ Seq (TxCert era)
  , Signal (EraRule "GOV" era) ~ GovProcedures era
  , HasLedgerState era
  , EraLedgerState era ~ BabelLedgerState era
  ) =>
  STS (BabelLEDGER era)
  where
  type State (BabelLEDGER era) = BabelLedgerState era
  type Signal (BabelLEDGER era) = Tx era
  type Environment (BabelLEDGER era) = LedgerEnv era
  type BaseM (BabelLEDGER era) = ShelleyBase
  type PredicateFailure (BabelLEDGER era) = BabelLedgerPredFailure era
  type Event (BabelLEDGER era) = BabelLedgerEvent era

  initialRules = []
  transitionRules = [ledgerTransition @BabelLEDGER]

  renderAssertionViolation = renderDepositEqualsObligationViolation

  assertions = shelleyLedgerAssertions @era @BabelLEDGER

-- =======================================

ledgerTransition ::
  forall (someLEDGER :: Type -> Type) era.
  ( AlonzoEraTx era
  , BabelEraTxBody era
  , ConwayEraGov era
  , GovState era ~ ConwayGovState era
  , Signal (someLEDGER era) ~ Tx era
  , State (someLEDGER era) ~ BabelLedgerState era
  , Environment (someLEDGER era) ~ LedgerEnv era
  , PredicateFailure (someLEDGER era) ~ BabelLedgerPredFailure era
  , Embed (EraRule "UTXOW" era) (someLEDGER era)
  , Embed (EraRule "GOV" era) (someLEDGER era)
  , Embed (EraRule "CERTS" era) (someLEDGER era)
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , State (EraRule "CERTS" era) ~ CertState era
  , State (EraRule "GOV" era) ~ Proposals era
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
  TRC (LedgerEnv slot _txIx pp account, st, tx) <- judgmentContext

  let actualTreasuryValue = account ^. asTreasuryL
   in case tx ^. bodyTxL . currentTreasuryValueTxBodyL of
        SNothing -> pure ()
        SJust submittedTreasuryValue ->
          submittedTreasuryValue
            == actualTreasuryValue
              ?! BabelTreasuryValueMismatch actualTreasuryValue submittedTreasuryValue

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
              , st ^. hlsCertStateL
              , StrictSeq.fromStrict $ txBody ^. certsTxBodyL
              )
        let wdrlAddrs = Map.keysSet . unWithdrawals $ tx ^. bodyTxL . withdrawalsTxBodyL
            wdrlCreds = Set.map raCredential wdrlAddrs
            dUnified = dsUnified $ certDState certStateAfterCERTS
            delegatedAddrs = DRepUView dUnified

        -- TODO enable this check once delegation is fully implemented in cardano-api
        when False $ do
          all (`UMap.member` delegatedAddrs) wdrlCreds
            ?! BabelWdrlNotDelegatedToDRep (wdrlCreds Set.\\ Map.keysSet (dRepMap dUnified))

        -- Votes and proposals from signal tx
        let govProcedures =
              GovProcedures
                { gpVotingProcedures = txBody ^. votingProceduresTxBodyL
                , gpProposalProcedures = txBody ^. proposalProceduresTxBodyL
                }
        proposalsState <-
          trans @(EraRule "GOV" era) $
            TRC
              ( GovEnv
                  (txIdTxBody txBody)
                  currentEpoch
                  pp
                  (utxoState ^. utxosGovStateL . proposalsGovStateL . pRootsL . L.to toPrevGovActionIds)
                  (utxoState ^. utxosGovStateL . constitutionGovStateL . constitutionScriptL)
                  (certState ^. certVStateL . vsCommitteeStateL)
              , utxoState ^. utxosGovStateL . proposalsGovStateL
              , govProcedures
              )
        let utxoState' =
              utxoState
                & utxosGovStateL
                . proposalsGovStateL
                .~ proposalsState
        pure (utxoState', certStateAfterCERTS)
      else pure (utxoState, certState)

  utxoState'' <-
    trans @(EraRule "UTXOW" era) $
      TRC
        -- Pass to UTXOW the unmodified CertState in its Environment,
        -- so it can process refunds of deposits for deregistering
        -- stake credentials and DReps. The modified CertState
        -- (certStateAfterCERTS) has these already removed from its
        -- UMap.
        ( UtxoEnv @era slot pp certState
        , utxoState'
        , tx
        )
  pure $ LedgerState utxoState'' certStateAfterCERTS

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
  Embed (ConwayCERTS era) (BabelLEDGER era)
  where
  wrapFailed = BabelCertsFailure
  wrapEvent = CertsEvent

instance
  ( ConwayEraPParams era
  , BaseM (BabelLEDGER era) ~ ShelleyBase
  , PredicateFailure (EraRule "GOV" era) ~ ConwayGovPredFailure era
  , Event (EraRule "GOV" era) ~ ConwayGovEvent era
  , EraRule "GOV" era ~ ConwayGOV era
  , InjectRuleFailure "GOV" ConwayGovPredFailure era
  ) =>
  Embed (ConwayGOV era) (BabelLEDGER era)
  where
  wrapFailed = BabelGovFailure
  wrapEvent = GovEvent

instance
  ( Signable (DSIGN (EraCrypto era)) (Hash (HASH (EraCrypto era)) EraIndependentTxBody)
  , BaseM (BabelUTXOW era) ~ ShelleyBase
  , AlonzoEraTx era
  , EraUTxO era
  , BabbageEraTxBody era
  , Embed (EraRule "UTXO" era) (BabelUTXOW era)
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Environment (EraRule "UTXO" era) ~ UtxoEnv era
  , Script era ~ AlonzoScript era
  , TxOut era ~ BabbageTxOut era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (EraRule "UTXO" era) ~ Tx era
  , PredicateFailure (EraRule "UTXOW" era) ~ BabelUtxowPredFailure era
  , Event (EraRule "UTXOW" era) ~ AlonzoUtxowEvent era
  , STS (BabelUTXOW era)
  , PredicateFailure (BabelUTXOW era) ~ BabelUtxowPredFailure era
  , Event (BabelUTXOW era) ~ AlonzoUtxowEvent era
  ) =>
  Embed (BabelUTXOW era) (BabelLEDGER era)
  where
  wrapFailed = BabelUtxowFailure
  wrapEvent = UtxowEvent
