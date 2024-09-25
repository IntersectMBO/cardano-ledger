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

module Cardano.Ledger.Conway.Rules.Ledger (
  ConwayLEDGER,
  ConwayLedgerPredFailure (..),
  ConwayLedgerEvent (..),
  maxRefScriptSizePerTx,
) where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..))
import Cardano.Crypto.Hash.Class (Hash)
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
import Cardano.Ledger.BaseTypes (ShelleyBase, StrictMaybe (..), epochInfoPure)
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Core hiding (proposals)
import Cardano.Ledger.Conway.Era (
  ConwayCERTS,
  ConwayDELEG,
  ConwayEra,
  ConwayGOV,
  ConwayLEDGER,
  ConwayMEMPOOL,
  ConwayUTXOW,
 )
import Cardano.Ledger.Conway.Governance (
  ConwayEraGov (..),
  ConwayGovState,
  Proposals,
  constitutionScriptL,
  grCommitteeL,
  proposalsGovStateL,
  proposalsWithPurpose,
 )
import Cardano.Ledger.Conway.Rules.Cert (CertEnv, ConwayCertEvent (..), ConwayCertPredFailure (..))
import Cardano.Ledger.Conway.Rules.Certs (
  CertsEnv (CertsEnv),
  ConwayCertsEvent (..),
  ConwayCertsPredFailure (..),
 )
import Cardano.Ledger.Conway.Rules.Deleg (ConwayDelegPredFailure)
import Cardano.Ledger.Conway.Rules.Gov (
  ConwayGovEvent (..),
  ConwayGovPredFailure,
  GovEnv (..),
  GovSignal (..),
 )
import Cardano.Ledger.Conway.Rules.GovCert (ConwayGovCertPredFailure)
import Cardano.Ledger.Conway.Rules.Mempool (ConwayMempoolEvent (..), ConwayMempoolPredFailure (..))
import Cardano.Ledger.Conway.Rules.Utxo (ConwayUtxoPredFailure)
import Cardano.Ledger.Conway.Rules.Utxos (ConwayUtxosPredFailure)
import Cardano.Ledger.Conway.Rules.Utxow (ConwayUtxowPredFailure)
import Cardano.Ledger.Conway.UTxO (txNonDistinctRefScriptsSize)
import Cardano.Ledger.Credential (Credential (..), credKeyHash)
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import qualified Cardano.Ledger.Shelley.HardForks as HF (bootstrapPhase)
import Cardano.Ledger.Shelley.LedgerState (
  CertState (..),
  DState (..),
  LedgerState (..),
  UTxOState (..),
  asTreasuryL,
  utxosGovStateL,
  utxosUtxoL,
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
import Cardano.Ledger.Slot (epochInfoEpoch)
import Cardano.Ledger.UMap (UView (..))
import qualified Cardano.Ledger.UMap as UMap
import Cardano.Ledger.UTxO (EraUTxO (..))
import Control.DeepSeq (NFData)
import Control.Monad (unless, void, when)
import Control.Monad.Trans.Reader (asks)
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

data ConwayLedgerPredFailure era
  = ConwayUtxowFailure (PredicateFailure (EraRule "UTXOW" era))
  | ConwayCertsFailure (PredicateFailure (EraRule "CERTS" era))
  | ConwayGovFailure (PredicateFailure (EraRule "GOV" era))
  | ConwayWdrlNotDelegatedToDRep (NonEmpty (KeyHash 'Staking (EraCrypto era)))
  | ConwayTreasuryValueMismatch
      -- | Actual
      Coin
      -- | Submitted in transaction
      Coin
  | ConwayTxRefScriptsSizeTooBig
      -- | Computed sum of reference script size
      Int
      -- | Maximum allowed total reference script size
      Int
  | ConwayMempoolFailure Text
  deriving (Generic)

-- | In the next era this will become a proper protocol parameter. For now this is a hard
-- coded limit on the total number of bytes of reference scripts that a transaction can
-- use.
maxRefScriptSizePerTx :: Int
maxRefScriptSizePerTx = 200 * 1024 -- 200KiB

type instance EraRuleFailure "LEDGER" (ConwayEra c) = ConwayLedgerPredFailure (ConwayEra c)

type instance EraRuleEvent "LEDGER" (ConwayEra c) = ConwayLedgerEvent (ConwayEra c)

instance InjectRuleFailure "LEDGER" ConwayLedgerPredFailure (ConwayEra c)

instance InjectRuleFailure "LEDGER" ConwayUtxowPredFailure (ConwayEra c) where
  injectFailure = ConwayUtxowFailure

instance InjectRuleFailure "LEDGER" BabbageUtxowPredFailure (ConwayEra c) where
  injectFailure = ConwayUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AlonzoUtxowPredFailure (ConwayEra c) where
  injectFailure = ConwayUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure (ConwayEra c) where
  injectFailure = ConwayUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ConwayUtxoPredFailure (ConwayEra c) where
  injectFailure = ConwayUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" BabbageUtxoPredFailure (ConwayEra c) where
  injectFailure = ConwayUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AlonzoUtxoPredFailure (ConwayEra c) where
  injectFailure = ConwayUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure (ConwayEra c) where
  injectFailure = ConwayUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ConwayUtxosPredFailure (ConwayEra c) where
  injectFailure = ConwayUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure (ConwayEra c) where
  injectFailure = ConwayUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AllegraUtxoPredFailure (ConwayEra c) where
  injectFailure = ConwayUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ConwayCertsPredFailure (ConwayEra c) where
  injectFailure = ConwayCertsFailure

instance InjectRuleFailure "LEDGER" ConwayCertPredFailure (ConwayEra c) where
  injectFailure = ConwayCertsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ConwayDelegPredFailure (ConwayEra c) where
  injectFailure = ConwayCertsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyPoolPredFailure (ConwayEra c) where
  injectFailure = ConwayCertsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ConwayGovCertPredFailure (ConwayEra c) where
  injectFailure = ConwayCertsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ConwayGovPredFailure (ConwayEra c) where
  injectFailure = ConwayGovFailure

instance InjectRuleFailure "LEDGER" ConwayMempoolPredFailure (ConwayEra c) where
  injectFailure (ConwayMempoolPredFailure t) = ConwayMempoolFailure t

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
      ConwayTxRefScriptsSizeTooBig x y -> Sum ConwayTxRefScriptsSizeTooBig 6 !> To x !> To y
      ConwayMempoolFailure t -> Sum ConwayMempoolFailure 7 !> To t

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
      6 -> SumD ConwayTxRefScriptsSizeTooBig <! From <! From
      7 -> SumD ConwayMempoolFailure <! From
      n -> Invalid n

data ConwayLedgerEvent era
  = UtxowEvent (Event (EraRule "UTXOW" era))
  | CertsEvent (Event (EraRule "CERTS" era))
  | GovEvent (Event (EraRule "GOV" era))
  | MempoolEvent (Event (EraRule "MEMPOOL" era))
  deriving (Generic)

deriving instance
  ( Eq (Event (EraRule "CERTS" era))
  , Eq (Event (EraRule "UTXOW" era))
  , Eq (Event (EraRule "GOV" era))
  , Eq (Event (EraRule "MEMPOOL" era))
  ) =>
  Eq (ConwayLedgerEvent era)

instance
  ( NFData (Event (EraRule "CERTS" era))
  , NFData (Event (EraRule "UTXOW" era))
  , NFData (Event (EraRule "GOV" era))
  , NFData (Event (EraRule "MEMPOOL" era))
  ) =>
  NFData (ConwayLedgerEvent era)

instance
  ( AlonzoEraTx era
  , ConwayEraTxBody era
  , ConwayEraGov era
  , GovState era ~ ConwayGovState era
  , Embed (EraRule "UTXOW" era) (ConwayLEDGER era)
  , Embed (EraRule "GOV" era) (ConwayLEDGER era)
  , Embed (EraRule "CERTS" era) (ConwayLEDGER era)
  , Embed (EraRule "MEMPOOL" era) (ConwayLEDGER era)
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , State (EraRule "CERTS" era) ~ CertState era
  , State (EraRule "GOV" era) ~ Proposals era
  , State (EraRule "MEMPOOL" era) ~ LedgerState era
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , Environment (EraRule "CERTS" era) ~ CertsEnv era
  , Environment (EraRule "GOV" era) ~ GovEnv era
  , Environment (EraRule "MEMPOOL" era) ~ LedgerEnv era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , Signal (EraRule "CERTS" era) ~ Seq (TxCert era)
  , Signal (EraRule "GOV" era) ~ GovSignal era
  , Signal (EraRule "MEMPOOL" era) ~ Tx era
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
  , Embed (EraRule "MEMPOOL" era) (someLEDGER era)
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , State (EraRule "CERTS" era) ~ CertState era
  , State (EraRule "GOV" era) ~ Proposals era
  , State (EraRule "MEMPOOL" era) ~ LedgerState era
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , Environment (EraRule "GOV" era) ~ GovEnv era
  , Environment (EraRule "CERTS" era) ~ CertsEnv era
  , Environment (EraRule "MEMPOOL" era) ~ LedgerEnv era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , Signal (EraRule "CERTS" era) ~ Seq (TxCert era)
  , Signal (EraRule "GOV" era) ~ GovSignal era
  , Signal (EraRule "MEMPOOL" era) ~ Tx era
  , BaseM (someLEDGER era) ~ ShelleyBase
  , STS (someLEDGER era)
  ) =>
  TransitionRule (someLEDGER era)
ledgerTransition = do
  TRC (le@(LedgerEnv slot _txIx pp account mempool), ls@(LedgerState utxoState certState), tx) <-
    judgmentContext

  when mempool $
    void $
      trans @(EraRule "MEMPOOL" era) $
        TRC (le, ls, tx)

  currentEpoch <- liftSTS $ do
    ei <- asks epochInfoPure
    epochInfoEpoch ei slot

  (utxoState', certStateAfterCERTS) <-
    if tx ^. isValidTxL == IsValid True
      then do
        let txBody = tx ^. bodyTxL
            actualTreasuryValue = account ^. asTreasuryL
        case txBody ^. currentTreasuryValueTxBodyL of
          SNothing -> pure ()
          SJust submittedTreasuryValue ->
            submittedTreasuryValue
              == actualTreasuryValue
                ?! ConwayTreasuryValueMismatch actualTreasuryValue submittedTreasuryValue

        let totalRefScriptSize = txNonDistinctRefScriptsSize (utxoState ^. utxosUtxoL) tx
        totalRefScriptSize
          <= maxRefScriptSizePerTx
            ?! ConwayTxRefScriptsSizeTooBig totalRefScriptSize maxRefScriptSizePerTx

        let govState = utxoState ^. utxosGovStateL
            committee = govState ^. committeeGovStateL
            proposals = govState ^. proposalsGovStateL
            committeeProposals = proposalsWithPurpose grCommitteeL proposals
        certStateAfterCERTS <-
          trans @(EraRule "CERTS" era) $
            TRC
              ( CertsEnv tx pp slot currentEpoch committee committeeProposals
              , certState
              , StrictSeq.fromStrict $ txBody ^. certsTxBodyL
              )

        -- Starting with version 10, we don't allow withdrawals into RewardAcounts that are KeyHashes and not delegated to Dreps
        unless (HF.bootstrapPhase (pp ^. ppProtocolVersionL)) $ do
          let dUnified = dsUnified $ certDState certStateAfterCERTS
              wdrls = unWithdrawals $ tx ^. bodyTxL . withdrawalsTxBodyL
              delegatedAddrs = DRepUView dUnified
              wdrlsKeyHashes =
                Set.fromList
                  [kh | (ra, _) <- Map.toList wdrls, Just kh <- [credKeyHash $ raCredential ra]]
              nonExistentDelegations =
                Set.filter (not . (`UMap.member` delegatedAddrs) . KeyHashObj) wdrlsKeyHashes
          failOnNonEmpty nonExistentDelegations ConwayWdrlNotDelegatedToDRep

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
                  currentEpoch
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
  , PredicateFailure (EraRule "UTXOW" era) ~ ConwayUtxowPredFailure era
  , Event (EraRule "UTXOW" era) ~ AlonzoUtxowEvent era
  , STS (ConwayUTXOW era)
  , PredicateFailure (ConwayUTXOW era) ~ ConwayUtxowPredFailure era
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
  , Embed (EraRule "MEMPOOL" era) (ConwayLEDGER era)
  , ConwayEraGov era
  , AlonzoEraTx era
  , ConwayEraTxBody era
  , ConwayEraPParams era
  , GovState era ~ ConwayGovState era
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , Environment (EraRule "CERTS" era) ~ CertsEnv era
  , Environment (EraRule "GOV" era) ~ GovEnv era
  , Environment (EraRule "MEMPOOL" era) ~ LedgerEnv era
  , Signal (EraRule "UTXOW" era) ~ Tx era
  , Signal (EraRule "CERTS" era) ~ Seq (TxCert era)
  , Signal (EraRule "GOV" era) ~ GovSignal era
  , Signal (EraRule "MEMPOOL" era) ~ Tx era
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , State (EraRule "CERTS" era) ~ CertState era
  , State (EraRule "GOV" era) ~ Proposals era
  , State (EraRule "MEMPOOL" era) ~ LedgerState era
  , EraRule "GOV" era ~ ConwayGOV era
  , PredicateFailure (EraRule "LEDGER" era) ~ ConwayLedgerPredFailure era
  , Event (EraRule "LEDGER" era) ~ ConwayLedgerEvent era
  , EraGov era
  ) =>
  Embed (ConwayLEDGER era) (ShelleyLEDGERS era)
  where
  wrapFailed = LedgerFailure
  wrapEvent = LedgerEvent

instance
  ( ConwayEraTxCert era
  , ConwayEraPParams era
  , BaseM (ConwayLEDGER era) ~ ShelleyBase
  , PredicateFailure (EraRule "GOV" era) ~ ConwayGovPredFailure era
  , Event (EraRule "GOV" era) ~ ConwayGovEvent era
  , EraRule "GOV" era ~ ConwayGOV era
  , InjectRuleFailure "GOV" ConwayGovPredFailure era
  ) =>
  Embed (ConwayGOV era) (ConwayLEDGER era)
  where
  wrapFailed = ConwayGovFailure
  wrapEvent = GovEvent

instance
  ( EraPParams era
  , EraRule "DELEG" era ~ ConwayDELEG era
  , PredicateFailure (EraRule "CERTS" era) ~ ConwayCertsPredFailure era
  , PredicateFailure (EraRule "CERT" era) ~ ConwayCertPredFailure era
  , Event (EraRule "CERTS" era) ~ ConwayCertsEvent era
  , Event (EraRule "CERT" era) ~ ConwayCertEvent era
  ) =>
  Embed (ConwayDELEG era) (ConwayLEDGER era)
  where
  wrapFailed = ConwayCertsFailure . CertFailure . DelegFailure
  wrapEvent = CertsEvent . CertEvent . DelegEvent

instance
  ( EraGov era
  , EraTx era
  , EraRule "MEMPOOL" era ~ ConwayMEMPOOL era
  , PredicateFailure (EraRule "MEMPOOL" era) ~ ConwayMempoolPredFailure era
  , Event (EraRule "MEMPOOL" era) ~ ConwayMempoolEvent era
  ) =>
  Embed (ConwayMEMPOOL era) (ConwayLEDGER era)
  where
  wrapFailed (ConwayMempoolPredFailure t) = ConwayMempoolFailure t
  wrapEvent = MempoolEvent
