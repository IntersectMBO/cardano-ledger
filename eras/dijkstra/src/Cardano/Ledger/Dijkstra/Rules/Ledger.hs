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
  maxRefScriptSizePerTx,
) where

import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.Allegra.Rules (AllegraUtxoPredFailure)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
  AlonzoUtxowEvent,
  AlonzoUtxowPredFailure,
 )
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript)
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded (..))
import Cardano.Ledger.Babbage.Rules (
  BabbageUtxoPredFailure,
  BabbageUtxowPredFailure,
 )
import Cardano.Ledger.Babbage.Tx (IsValid (..))
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes (
  Mismatch (..),
  Relation (..),
  ShelleyBase,
  StrictMaybe (..),
  swapMismatch,
  unswapMismatch,
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Era (
  DijkstraCERTS,
  DijkstraDELEG,
  DijkstraEra,
  DijkstraGOV,
  DijkstraLEDGER,
  DijkstraUTXOW,
 )
import Cardano.Ledger.Dijkstra.Governance (
  DijkstraEraGov (..),
  DijkstraGovState,
  Proposals,
  constitutionScriptL,
  grCommitteeL,
  proposalsGovStateL,
  proposalsWithPurpose,
 )
import Cardano.Ledger.Dijkstra.Rules.Cert (CertEnv, DijkstraCertEvent (..), DijkstraCertPredFailure (..))
import Cardano.Ledger.Dijkstra.Rules.Certs (
  CertsEnv (CertsEnv),
  DijkstraCertsEvent (..),
  DijkstraCertsPredFailure (..),
 )
import Cardano.Ledger.Dijkstra.Rules.Deleg (DijkstraDelegPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Gov (
  DijkstraGovEvent (..),
  DijkstraGovPredFailure,
  GovEnv (..),
  GovSignal (..),
 )
import Cardano.Ledger.Dijkstra.Rules.GovCert (DijkstraGovCertPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Utxo (DijkstraUtxoPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Utxos (DijkstraUtxosPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Utxow (DijkstraUtxowPredFailure)
import Cardano.Ledger.Dijkstra.State
import Cardano.Ledger.Dijkstra.UTxO (txNonDistinctRefScriptsSize)
import Cardano.Ledger.Credential (Credential (..), credKeyHash)
import qualified Cardano.Ledger.Shelley.HardForks as HF (bootstrapPhase)
import Cardano.Ledger.Shelley.LedgerState (
  LedgerState (..),
  UTxOState (..),
  utxosGovStateL,
 )
import Cardano.Ledger.Shelley.Rules (
  LedgerEnv (..),
  ShelleyLEDGERS,
  ShelleyLedgersEvent (..),
  ShelleyLedgersPredFailure (..),
  ShelleyPoolPredFailure,
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure,
  UtxoEnv (..),
  renderDepositEqualsObligationViolation,
  shelleyLedgerAssertions,
 )
import Cardano.Ledger.Slot (epochFromSlot)
import Cardano.Ledger.UMap (UView (..))
import qualified Cardano.Ledger.UMap as UMap
import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Control.State.Transition.Extended (
  Embed (..),
  STS (..),
  TRC (..),
  TransitionRule,
  failOnNonEmpty,
  judgmentContext,
  liftSTS,
  trans,
  (?!),
 )
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics (Generic (..))
import Lens.Micro as L
import NoThunks.Class (NoThunks (..))

data DijkstraLedgerPredFailure era
  = DijkstraUtxowFailure (PredicateFailure (EraRule "UTXOW" era))
  | DijkstraCertsFailure (PredicateFailure (EraRule "CERTS" era))
  | DijkstraGovFailure (PredicateFailure (EraRule "GOV" era))
  | DijkstraWdrlNotDelegatedToDRep (NonEmpty (KeyHash 'Staking))
  | DijkstraTreasuryValueMismatch (Mismatch 'RelEQ Coin) -- The serialisation order is in reverse
  | DijkstraTxRefScriptsSizeTooBig (Mismatch 'RelLTEQ Int)
  | DijkstraMempoolFailure Text
  deriving (Generic)

-- | In the next era this will become a proper protocol parameter. For now this is a hard
-- coded limit on the total number of bytes of reference scripts that a transaction can
-- use.
maxRefScriptSizePerTx :: Int
maxRefScriptSizePerTx = 200 * 1024 -- 200KiB

type instance EraRuleFailure "LEDGER" DijkstraEra = DijkstraLedgerPredFailure DijkstraEra

type instance EraRuleEvent "LEDGER" DijkstraEra = DijkstraLedgerEvent DijkstraEra

instance InjectRuleFailure "LEDGER" DijkstraLedgerPredFailure DijkstraEra

instance InjectRuleFailure "LEDGER" DijkstraUtxowPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure

instance InjectRuleFailure "LEDGER" BabbageUtxowPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AlonzoUtxowPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" DijkstraUtxoPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" BabbageUtxoPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AlonzoUtxoPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" DijkstraUtxosPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AllegraUtxoPredFailure DijkstraEra where
  injectFailure = DijkstraUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" DijkstraCertsPredFailure DijkstraEra where
  injectFailure = DijkstraCertsFailure

instance InjectRuleFailure "LEDGER" DijkstraCertPredFailure DijkstraEra where
  injectFailure = DijkstraCertsFailure . injectFailure

instance InjectRuleFailure "LEDGER" DijkstraDelegPredFailure DijkstraEra where
  injectFailure = DijkstraCertsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyPoolPredFailure DijkstraEra where
  injectFailure = DijkstraCertsFailure . injectFailure

instance InjectRuleFailure "LEDGER" DijkstraGovCertPredFailure DijkstraEra where
  injectFailure = DijkstraCertsFailure . injectFailure

instance InjectRuleFailure "LEDGER" DijkstraGovPredFailure DijkstraEra where
  injectFailure = DijkstraGovFailure

deriving instance
  ( Era era
  , Eq (PredicateFailure (EraRule "UTXOW" era))
  , Eq (PredicateFailure (EraRule "CERTS" era))
  , Eq (PredicateFailure (EraRule "GOV" era))
  ) =>
  Eq (DijkstraLedgerPredFailure era)

deriving instance
  ( Era era
  , Show (PredicateFailure (EraRule "UTXOW" era))
  , Show (PredicateFailure (EraRule "CERTS" era))
  , Show (PredicateFailure (EraRule "GOV" era))
  ) =>
  Show (DijkstraLedgerPredFailure era)

instance
  ( Era era
  , NoThunks (PredicateFailure (EraRule "UTXOW" era))
  , NoThunks (PredicateFailure (EraRule "CERTS" era))
  , NoThunks (PredicateFailure (EraRule "GOV" era))
  ) =>
  NoThunks (DijkstraLedgerPredFailure era)

instance
  ( Era era
  , NFData (PredicateFailure (EraRule "UTXOW" era))
  , NFData (PredicateFailure (EraRule "CERTS" era))
  , NFData (PredicateFailure (EraRule "GOV" era))
  ) =>
  NFData (DijkstraLedgerPredFailure era)

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "UTXOW" era))
  , EncCBOR (PredicateFailure (EraRule "CERTS" era))
  , EncCBOR (PredicateFailure (EraRule "GOV" era))
  ) =>
  EncCBOR (DijkstraLedgerPredFailure era)
  where
  encCBOR =
    encode . \case
      DijkstraUtxowFailure x -> Sum (DijkstraUtxowFailure @era) 1 !> To x
      DijkstraCertsFailure x -> Sum (DijkstraCertsFailure @era) 2 !> To x
      DijkstraGovFailure x -> Sum (DijkstraGovFailure @era) 3 !> To x
      DijkstraWdrlNotDelegatedToDRep x -> Sum (DijkstraWdrlNotDelegatedToDRep @era) 4 !> To x
      DijkstraTreasuryValueMismatch mm ->
        Sum (DijkstraTreasuryValueMismatch @era . unswapMismatch) 5 !> ToGroup (swapMismatch mm)
      DijkstraTxRefScriptsSizeTooBig mm -> Sum DijkstraTxRefScriptsSizeTooBig 6 !> ToGroup mm
      DijkstraMempoolFailure t -> Sum DijkstraMempoolFailure 7 !> To t

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "UTXOW" era))
  , DecCBOR (PredicateFailure (EraRule "CERTS" era))
  , DecCBOR (PredicateFailure (EraRule "GOV" era))
  ) =>
  DecCBOR (DijkstraLedgerPredFailure era)
  where
  decCBOR = decode . Summands "DijkstraLedgerPredFailure" $ \case
    1 -> SumD DijkstraUtxowFailure <! From
    2 -> SumD DijkstraCertsFailure <! From
    3 -> SumD DijkstraGovFailure <! From
    4 -> SumD DijkstraWdrlNotDelegatedToDRep <! From
    5 -> SumD DijkstraTreasuryValueMismatch <! mapCoder unswapMismatch FromGroup
    6 -> SumD DijkstraTxRefScriptsSizeTooBig <! FromGroup
    7 -> SumD DijkstraMempoolFailure <! From
    n -> Invalid n

data DijkstraLedgerEvent era
  = UtxowEvent (Event (EraRule "UTXOW" era))
  | CertsEvent (Event (EraRule "CERTS" era))
  | GovEvent (Event (EraRule "GOV" era))
  deriving (Generic)

deriving instance
  ( Eq (Event (EraRule "CERTS" era))
  , Eq (Event (EraRule "UTXOW" era))
  , Eq (Event (EraRule "GOV" era))
  ) =>
  Eq (DijkstraLedgerEvent era)

instance
  ( NFData (Event (EraRule "CERTS" era))
  , NFData (Event (EraRule "UTXOW" era))
  , NFData (Event (EraRule "GOV" era))
  ) =>
  NFData (DijkstraLedgerEvent era)

instance
  ( AlonzoEraTx era
  , DijkstraEraTxBody era
  , DijkstraEraGov era
  , GovState era ~ DijkstraGovState era
  , Embed (EraRule "UTXOW" era) (DijkstraLEDGER era)
  , Embed (EraRule "GOV" era) (DijkstraLEDGER era)
  , Embed (EraRule "CERTS" era) (DijkstraLEDGER era)
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , State (EraRule "CERTS" era) ~ CertState era
  , State (EraRule "GOV" era) ~ Proposals era
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , Environment (EraRule "CERTS" era) ~ CertsEnv era
  , Environment (EraRule "GOV" era) ~ GovEnv era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , Signal (EraRule "CERTS" era) ~ Seq (TxCert era)
  , Signal (EraRule "GOV" era) ~ GovSignal era
  , EraCertState era
  ) =>
  STS (DijkstraLEDGER era)
  where
  type State (DijkstraLEDGER era) = LedgerState era
  type Signal (DijkstraLEDGER era) = Tx era
  type Environment (DijkstraLEDGER era) = LedgerEnv era
  type BaseM (DijkstraLEDGER era) = ShelleyBase
  type PredicateFailure (DijkstraLEDGER era) = DijkstraLedgerPredFailure era
  type Event (DijkstraLEDGER era) = DijkstraLedgerEvent era

  initialRules = []
  transitionRules = [ledgerTransition @DijkstraLEDGER]

  renderAssertionViolation = renderDepositEqualsObligationViolation

  assertions = shelleyLedgerAssertions @era @DijkstraLEDGER

-- =======================================

ledgerTransition ::
  forall (someLEDGER :: Type -> Type) era.
  ( AlonzoEraTx era
  , DijkstraEraTxBody era
  , DijkstraEraGov era
  , GovState era ~ DijkstraGovState era
  , Signal (someLEDGER era) ~ Tx era
  , State (someLEDGER era) ~ LedgerState era
  , Environment (someLEDGER era) ~ LedgerEnv era
  , PredicateFailure (someLEDGER era) ~ DijkstraLedgerPredFailure era
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
  , Signal (EraRule "GOV" era) ~ GovSignal era
  , BaseM (someLEDGER era) ~ ShelleyBase
  , STS (someLEDGER era)
  , EraCertState era
  ) =>
  TransitionRule (someLEDGER era)
ledgerTransition = do
  TRC
    ( LedgerEnv slot mbCurEpochNo _txIx pp chainAccountState
      , LedgerState utxoState certState
      , tx
      ) <-
    judgmentContext

  curEpochNo <- maybe (liftSTS $ epochFromSlot slot) pure mbCurEpochNo

  (utxoState', certStateAfterCERTS) <-
    if tx ^. isValidTxL == IsValid True
      then do
        let txBody = tx ^. bodyTxL
            actualTreasuryValue = chainAccountState ^. casTreasuryL
        case txBody ^. currentTreasuryValueTxBodyL of
          SNothing -> pure ()
          SJust submittedTreasuryValue ->
            submittedTreasuryValue
              == actualTreasuryValue
                ?! DijkstraTreasuryValueMismatch
                  ( Mismatch
                      { mismatchSupplied = submittedTreasuryValue
                      , mismatchExpected = actualTreasuryValue
                      }
                  )

        let totalRefScriptSize = txNonDistinctRefScriptsSize (utxoState ^. utxoL) tx
        totalRefScriptSize
          <= maxRefScriptSizePerTx
            ?! DijkstraTxRefScriptsSizeTooBig
              ( Mismatch
                  { mismatchSupplied = totalRefScriptSize
                  , mismatchExpected = maxRefScriptSizePerTx
                  }
              )

        let govState = utxoState ^. utxosGovStateL
            committee = govState ^. committeeGovStateL
            proposals = govState ^. proposalsGovStateL
            committeeProposals = proposalsWithPurpose grCommitteeL proposals

        -- Starting with version 10, we don't allow withdrawals into RewardAcounts that are
        -- KeyHashes and not delegated to Dreps.
        --
        -- We also need to make sure we are using the certState before certificates are applied,
        -- because otherwise it would not be possible to unregister a reward account and withdraw
        -- all funds from it in the same transaction.
        unless (HF.bootstrapPhase (pp ^. ppProtocolVersionL)) $ do
          let dUnified = certState ^. certDStateL . dsUnifiedL
              wdrls = unWithdrawals $ tx ^. bodyTxL . withdrawalsTxBodyL
              delegatedAddrs = DRepUView dUnified
              wdrlsKeyHashes =
                Set.fromList
                  [kh | (ra, _) <- Map.toList wdrls, Just kh <- [credKeyHash $ raCredential ra]]
              nonExistentDelegations =
                Set.filter (not . (`UMap.member` delegatedAddrs) . KeyHashObj) wdrlsKeyHashes
          failOnNonEmpty nonExistentDelegations DijkstraWdrlNotDelegatedToDRep

        certStateAfterCERTS <-
          trans @(EraRule "CERTS" era) $
            TRC
              ( CertsEnv tx pp curEpochNo committee committeeProposals
              , certState
              , StrictSeq.fromStrict $ txBody ^. certsTxBodyL
              )

        -- Votes and proposals from signal tx
        let govSignal =
              GovSignal
                { gsVotingProcedures = txBody ^. votingProceduresTxBodyL
                , gsProposalProcedures = txBody ^. proposalProceduresTxBodyL
                , gsCertificates = txBody ^. certsTxBodyL
                }
        proposalsState <-
          trans @(EraRule "GOV" era) $
            TRC
              ( GovEnv
                  (txIdTxBody txBody)
                  curEpochNo
                  pp
                  (govState ^. constitutionGovStateL . constitutionScriptL)
                  certStateAfterCERTS
              , proposals
              , govSignal
              )
        let utxoState' =
              utxoState
                & utxosGovStateL . proposalsGovStateL .~ proposalsState
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
  ( BaseM (DijkstraUTXOW era) ~ ShelleyBase
  , AlonzoEraTx era
  , EraUTxO era
  , BabbageEraTxBody era
  , Embed (EraRule "UTXO" era) (DijkstraUTXOW era)
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Environment (EraRule "UTXO" era) ~ UtxoEnv era
  , Script era ~ AlonzoScript era
  , TxOut era ~ BabbageTxOut era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (EraRule "UTXO" era) ~ Tx era
  , PredicateFailure (EraRule "UTXOW" era) ~ DijkstraUtxowPredFailure era
  , Event (EraRule "UTXOW" era) ~ AlonzoUtxowEvent era
  , STS (DijkstraUTXOW era)
  , PredicateFailure (DijkstraUTXOW era) ~ DijkstraUtxowPredFailure era
  , Event (DijkstraUTXOW era) ~ AlonzoUtxowEvent era
  ) =>
  Embed (DijkstraUTXOW era) (DijkstraLEDGER era)
  where
  wrapFailed = DijkstraUtxowFailure
  wrapEvent = UtxowEvent

instance
  ( EraTx era
  , DijkstraEraTxBody era
  , DijkstraEraPParams era
  , DijkstraEraGov era
  , Embed (EraRule "CERT" era) (DijkstraCERTS era)
  , State (EraRule "CERT" era) ~ CertState era
  , Environment (EraRule "CERT" era) ~ CertEnv era
  , Signal (EraRule "CERT" era) ~ TxCert era
  , PredicateFailure (EraRule "CERTS" era) ~ DijkstraCertsPredFailure era
  , Event (EraRule "CERTS" era) ~ DijkstraCertsEvent era
  , EraRule "CERTS" era ~ DijkstraCERTS era
  , EraCertState era
  , DijkstraEraCertState era
  ) =>
  Embed (DijkstraCERTS era) (DijkstraLEDGER era)
  where
  wrapFailed = DijkstraCertsFailure
  wrapEvent = CertsEvent

instance
  ( Embed (EraRule "UTXOW" era) (DijkstraLEDGER era)
  , Embed (EraRule "CERTS" era) (DijkstraLEDGER era)
  , Embed (EraRule "GOV" era) (DijkstraLEDGER era)
  , DijkstraEraGov era
  , AlonzoEraTx era
  , DijkstraEraTxBody era
  , DijkstraEraPParams era
  , GovState era ~ DijkstraGovState era
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , Environment (EraRule "CERTS" era) ~ CertsEnv era
  , Environment (EraRule "GOV" era) ~ GovEnv era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , Signal (EraRule "CERTS" era) ~ Seq (TxCert era)
  , Signal (EraRule "GOV" era) ~ GovSignal era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , State (EraRule "CERTS" era) ~ CertState era
  , State (EraRule "GOV" era) ~ Proposals era
  , EraRule "GOV" era ~ DijkstraGOV era
  , PredicateFailure (EraRule "LEDGER" era) ~ DijkstraLedgerPredFailure era
  , Event (EraRule "LEDGER" era) ~ DijkstraLedgerEvent era
  , EraGov era
  , EraCertState era
  ) =>
  Embed (DijkstraLEDGER era) (ShelleyLEDGERS era)
  where
  wrapFailed = LedgerFailure
  wrapEvent = LedgerEvent

instance
  ( DijkstraEraTxCert era
  , DijkstraEraPParams era
  , BaseM (DijkstraLEDGER era) ~ ShelleyBase
  , PredicateFailure (EraRule "GOV" era) ~ DijkstraGovPredFailure era
  , Event (EraRule "GOV" era) ~ DijkstraGovEvent era
  , EraRule "GOV" era ~ DijkstraGOV era
  , InjectRuleFailure "GOV" DijkstraGovPredFailure era
  , EraCertState era
  , DijkstraEraCertState era
  ) =>
  Embed (DijkstraGOV era) (DijkstraLEDGER era)
  where
  wrapFailed = DijkstraGovFailure
  wrapEvent = GovEvent

instance
  ( EraPParams era
  , EraRule "DELEG" era ~ DijkstraDELEG era
  , PredicateFailure (EraRule "CERTS" era) ~ DijkstraCertsPredFailure era
  , PredicateFailure (EraRule "CERT" era) ~ DijkstraCertPredFailure era
  , Event (EraRule "CERTS" era) ~ DijkstraCertsEvent era
  , Event (EraRule "CERT" era) ~ DijkstraCertEvent era
  , EraCertState era
  , DijkstraEraCertState era
  ) =>
  Embed (DijkstraDELEG era) (DijkstraLEDGER era)
  where
  wrapFailed = DijkstraCertsFailure . CertFailure . DelegFailure
  wrapEvent = CertsEvent . CertEvent . DelegEvent
