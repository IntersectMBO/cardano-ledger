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
  shelleyToConwayLedgerPredFailure,
  conwayLedgerTransition,
  conwayLedgerTransitionTRC,
) where

import Cardano.Ledger.Address (AccountAddress, accountAddressCredentialL)
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
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (
  ConwayCERTS,
  ConwayDELEG,
  ConwayEra,
  ConwayGOV,
  ConwayLEDGER,
  ConwayUTXOW,
  hardforkConwayBootstrapPhase,
  hardforkConwayMoveWithdrawalsAndDRepChecksToLedgerRule,
 )
import Cardano.Ledger.Conway.Governance (
  ConwayEraGov (..),
  ConwayGovState,
  Proposals,
  constitutionGuardrailsScriptHashL,
  grCommitteeL,
  proposalsGovStateL,
  proposalsWithPurpose,
 )
import Cardano.Ledger.Conway.PParams (ConwayEraPParams (..))
import Cardano.Ledger.Conway.Rules.Cert (CertEnv, ConwayCertEvent (..), ConwayCertPredFailure (..))
import Cardano.Ledger.Conway.Rules.Certs (
  CertsEnv (CertsEnv),
  ConwayCertsEvent (..),
  ConwayCertsPredFailure (..),
  updateDormantDRepExpiries,
  updateVotingDRepExpiries,
 )
import Cardano.Ledger.Conway.Rules.Deleg (ConwayDelegPredFailure)
import Cardano.Ledger.Conway.Rules.Gov (
  ConwayGovPredFailure,
  GovEnv (..),
  GovSignal (..),
 )
import Cardano.Ledger.Conway.Rules.GovCert (ConwayGovCertPredFailure)
import Cardano.Ledger.Conway.Rules.Utxo (ConwayUtxoPredFailure)
import Cardano.Ledger.Conway.Rules.Utxos (ConwayUtxosPredFailure)
import Cardano.Ledger.Conway.Rules.Utxow (ConwayUtxowPredFailure)
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Conway.UTxO (txNonDistinctRefScriptsSize)
import Cardano.Ledger.Credential (Credential (..), credKeyHash)
import Cardano.Ledger.Shelley.LedgerState (
  LedgerState (..),
  UTxOState (..),
  utxosGovStateL,
 )
import Cardano.Ledger.Shelley.Rules (
  LedgerEnv (..),
  ShelleyLEDGERS,
  ShelleyLedgerPredFailure (..),
  ShelleyLedgersEvent (..),
  ShelleyLedgersPredFailure (..),
  ShelleyPoolPredFailure,
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure,
  UtxoEnv (..),
  renderDepositEqualsObligationViolation,
  shelleyLedgerAssertions,
  testIncompleteAndMissingWithdrawals,
 )
import Cardano.Ledger.Slot (epochFromSlot)
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
import Data.Map.NonEmpty (NonEmptyMap)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Sequence (Seq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Text (Text)
import Data.Word (Word32)
import GHC.Generics (Generic (..))
import Lens.Micro as L
import NoThunks.Class (NoThunks (..))

data ConwayLedgerPredFailure era
  = ConwayUtxowFailure (PredicateFailure (EraRule "UTXOW" era))
  | ConwayCertsFailure (PredicateFailure (EraRule "CERTS" era))
  | ConwayGovFailure (PredicateFailure (EraRule "GOV" era))
  | ConwayWdrlNotDelegatedToDRep (NonEmpty (KeyHash Staking))
  | ConwayTreasuryValueMismatch (Mismatch RelEQ Coin) -- The serialisation order is in reverse
  | ConwayTxRefScriptsSizeTooBig (Mismatch RelLTEQ Int)
  | ConwayMempoolFailure Text
  | ConwayWithdrawalsMissingAccounts Withdrawals
  | ConwayIncompleteWithdrawals (NonEmptyMap AccountAddress (Mismatch RelEQ Coin))
  deriving (Generic)

type instance EraRuleFailure "LEDGER" ConwayEra = ConwayLedgerPredFailure ConwayEra

type instance EraRuleEvent "LEDGER" ConwayEra = ConwayLedgerEvent ConwayEra

instance InjectRuleFailure "LEDGER" ConwayLedgerPredFailure ConwayEra

instance InjectRuleFailure "LEDGER" ShelleyLedgerPredFailure ConwayEra where
  injectFailure = shelleyToConwayLedgerPredFailure

instance InjectRuleFailure "LEDGER" ConwayUtxowPredFailure ConwayEra where
  injectFailure = ConwayUtxowFailure

instance InjectRuleFailure "LEDGER" BabbageUtxowPredFailure ConwayEra where
  injectFailure = ConwayUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AlonzoUtxowPredFailure ConwayEra where
  injectFailure = ConwayUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure ConwayEra where
  injectFailure = ConwayUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ConwayUtxoPredFailure ConwayEra where
  injectFailure = ConwayUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" BabbageUtxoPredFailure ConwayEra where
  injectFailure = ConwayUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AlonzoUtxoPredFailure ConwayEra where
  injectFailure = ConwayUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure ConwayEra where
  injectFailure = ConwayUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure ConwayEra where
  injectFailure = ConwayUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" AllegraUtxoPredFailure ConwayEra where
  injectFailure = ConwayUtxowFailure . injectFailure

instance InjectRuleFailure "LEDGER" ConwayCertsPredFailure ConwayEra where
  injectFailure = ConwayCertsFailure

instance InjectRuleFailure "LEDGER" ConwayCertPredFailure ConwayEra where
  injectFailure = ConwayCertsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ConwayDelegPredFailure ConwayEra where
  injectFailure = ConwayCertsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ShelleyPoolPredFailure ConwayEra where
  injectFailure = ConwayCertsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ConwayGovCertPredFailure ConwayEra where
  injectFailure = ConwayCertsFailure . injectFailure

instance InjectRuleFailure "LEDGER" ConwayGovPredFailure ConwayEra where
  injectFailure = ConwayGovFailure

instance InjectRuleFailure "LEDGER" ConwayUtxosPredFailure ConwayEra where
  injectFailure = ConwayUtxowFailure . injectFailure

shelleyToConwayLedgerPredFailure ::
  forall era. ShelleyLedgerPredFailure era -> ConwayLedgerPredFailure era
shelleyToConwayLedgerPredFailure = \case
  UtxowFailure x -> ConwayUtxowFailure x
  DelegsFailure _ -> error "Impossible: DELEGS has ben removed in Conway"
  ShelleyWithdrawalsMissingAccounts x -> ConwayWithdrawalsMissingAccounts x
  ShelleyIncompleteWithdrawals x -> ConwayIncompleteWithdrawals x

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
      ConwayWdrlNotDelegatedToDRep x -> Sum (ConwayWdrlNotDelegatedToDRep @era) 4 !> To x
      ConwayTreasuryValueMismatch mm ->
        Sum (ConwayTreasuryValueMismatch @era . unswapMismatch) 5 !> ToGroup (swapMismatch mm)
      ConwayTxRefScriptsSizeTooBig mm -> Sum ConwayTxRefScriptsSizeTooBig 6 !> ToGroup mm
      ConwayMempoolFailure t -> Sum ConwayMempoolFailure 7 !> To t
      ConwayWithdrawalsMissingAccounts w -> Sum ConwayWithdrawalsMissingAccounts 8 !> To w
      ConwayIncompleteWithdrawals w -> Sum ConwayIncompleteWithdrawals 9 !> To w

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "UTXOW" era))
  , DecCBOR (PredicateFailure (EraRule "CERTS" era))
  , DecCBOR (PredicateFailure (EraRule "GOV" era))
  ) =>
  DecCBOR (ConwayLedgerPredFailure era)
  where
  decCBOR = decode . Summands "ConwayLedgerPredFailure" $ \case
    1 -> SumD ConwayUtxowFailure <! From
    2 -> SumD ConwayCertsFailure <! From
    3 -> SumD ConwayGovFailure <! From
    4 -> SumD ConwayWdrlNotDelegatedToDRep <! From
    5 -> SumD ConwayTreasuryValueMismatch <! mapCoder unswapMismatch FromGroup
    6 -> SumD ConwayTxRefScriptsSizeTooBig <! FromGroup
    7 -> SumD ConwayMempoolFailure <! From
    8 -> SumD ConwayWithdrawalsMissingAccounts <! From
    9 -> SumD ConwayIncompleteWithdrawals <! From
    n -> Invalid n

data ConwayLedgerEvent era
  = UtxowEvent (Event (EraRule "UTXOW" era))
  | CertsEvent (Event (EraRule "CERTS" era))
  | GovEvent (Event (EraRule "GOV" era))
  deriving (Generic)

deriving instance
  ( Eq (Event (EraRule "CERTS" era))
  , Eq (Event (EraRule "UTXOW" era))
  , Eq (Event (EraRule "GOV" era))
  ) =>
  Eq (ConwayLedgerEvent era)

instance
  ( NFData (Event (EraRule "CERTS" era))
  , NFData (Event (EraRule "UTXOW" era))
  , NFData (Event (EraRule "GOV" era))
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
  , EraRule "LEDGER" era ~ ConwayLEDGER era
  , InjectRuleFailure "LEDGER" ShelleyLedgerPredFailure era
  , InjectRuleFailure "LEDGER" ConwayLedgerPredFailure era
  ) =>
  STS (ConwayLEDGER era)
  where
  type State (ConwayLEDGER era) = LedgerState era
  type Signal (ConwayLEDGER era) = Tx TopTx era
  type Environment (ConwayLEDGER era) = LedgerEnv era
  type BaseM (ConwayLEDGER era) = ShelleyBase
  type PredicateFailure (ConwayLEDGER era) = ConwayLedgerPredFailure era
  type Event (ConwayLEDGER era) = ConwayLedgerEvent era

  initialRules = []
  transitionRules = [conwayLedgerTransition @ConwayLEDGER]

  renderAssertionViolation = renderDepositEqualsObligationViolation

  assertions = shelleyLedgerAssertions @era @ConwayLEDGER

-- =======================================

conwayLedgerTransitionTRC ::
  forall (someLEDGER :: Type -> Type) era.
  ( AlonzoEraTx era
  , ConwayEraTxBody era
  , ConwayEraGov era
  , GovState era ~ ConwayGovState era
  , Signal (someLEDGER era) ~ Tx TopTx era
  , State (someLEDGER era) ~ LedgerState era
  , Environment (someLEDGER era) ~ LedgerEnv era
  , Embed (EraRule "UTXOW" era) (someLEDGER era)
  , Embed (EraRule "GOV" era) (someLEDGER era)
  , Embed (EraRule "CERTS" era) (someLEDGER era)
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , State (EraRule "CERTS" era) ~ CertState era
  , State (EraRule "GOV" era) ~ Proposals era
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , Environment (EraRule "GOV" era) ~ GovEnv era
  , Environment (EraRule "CERTS" era) ~ CertsEnv era
  , Signal (EraRule "UTXOW" era) ~ Tx TopTx era
  , Signal (EraRule "CERTS" era) ~ Seq (TxCert era)
  , Signal (EraRule "GOV" era) ~ GovSignal era
  , BaseM (someLEDGER era) ~ ShelleyBase
  , STS (someLEDGER era)
  , ConwayEraCertState era
  , EraRule "LEDGER" era ~ someLEDGER era
  , InjectRuleFailure "LEDGER" ShelleyLedgerPredFailure era
  , InjectRuleFailure "LEDGER" ConwayLedgerPredFailure era
  ) =>
  TRC (someLEDGER era) ->
  TransitionRule (someLEDGER era)
conwayLedgerTransitionTRC
  ( TRC
      ( LedgerEnv slot mbCurEpochNo _txIx pp chainAccountState
        , LedgerState utxoState certState
        , tx
        )
    ) = do
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
                  ?! (injectFailure . ConwayTreasuryValueMismatch)
                    ( Mismatch
                        { mismatchSupplied = submittedTreasuryValue
                        , mismatchExpected = actualTreasuryValue
                        }
                    )

          let
            totalRefScriptSize = txNonDistinctRefScriptsSize (utxoState ^. utxoL) tx
            maxRefScriptSizePerTx = fromIntegral @Word32 @Int $ pp ^. ppMaxRefScriptSizePerTxG
          totalRefScriptSize
            <= maxRefScriptSizePerTx
              ?! injectFailure
                ( ConwayTxRefScriptsSizeTooBig
                    Mismatch
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
          -- because otherwise it would not be possible to unregister an account address and withdraw
          -- all funds from it in the same transaction.
          unless (hardforkConwayBootstrapPhase (pp ^. ppProtocolVersionL)) $ do
            let accounts = certState ^. certDStateL . accountsL
                wdrls = unWithdrawals $ tx ^. bodyTxL . withdrawalsTxBodyL
                wdrlsKeyHashes =
                  [ kh | (ra, _) <- Map.toList wdrls, Just kh <- [credKeyHash $ ra ^. accountAddressCredentialL]
                  ]
                isNotDRepDelegated keyHash = isNothing $ do
                  accountState <- lookupAccountState (KeyHashObj keyHash) accounts
                  accountState ^. dRepDelegationAccountStateL
                nonExistentDelegations =
                  filter isNotDRepDelegated wdrlsKeyHashes
            failOnNonEmpty nonExistentDelegations (injectFailure . ConwayWdrlNotDelegatedToDRep)

          certState' <-
            if hardforkConwayMoveWithdrawalsAndDRepChecksToLedgerRule $ pp ^. ppProtocolVersionL
              then do
                let withdrawals = tx ^. bodyTxL . withdrawalsTxBodyL
                testIncompleteAndMissingWithdrawals (certState ^. certDStateL . accountsL) withdrawals
                pure $
                  certState
                    & updateDormantDRepExpiries tx curEpochNo
                    & updateVotingDRepExpiries tx curEpochNo (pp ^. ppDRepActivityL)
                    & certDStateL . accountsL %~ drainAccounts withdrawals
              else pure certState

          certStateAfterCERTS <-
            trans @(EraRule "CERTS" era) $
              TRC
                ( CertsEnv tx pp curEpochNo committee committeeProposals
                , certState'
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
                    (govState ^. constitutionGovStateL . constitutionGuardrailsScriptHashL)
                    certStateAfterCERTS
                    (govState ^. committeeGovStateL)
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

conwayLedgerTransition ::
  forall (someLEDGER :: Type -> Type) era.
  ( AlonzoEraTx era
  , ConwayEraTxBody era
  , ConwayEraGov era
  , GovState era ~ ConwayGovState era
  , Signal (someLEDGER era) ~ Tx TopTx era
  , State (someLEDGER era) ~ LedgerState era
  , Environment (someLEDGER era) ~ LedgerEnv era
  , Embed (EraRule "UTXOW" era) (someLEDGER era)
  , Embed (EraRule "GOV" era) (someLEDGER era)
  , Embed (EraRule "CERTS" era) (someLEDGER era)
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , State (EraRule "CERTS" era) ~ CertState era
  , State (EraRule "GOV" era) ~ Proposals era
  , Environment (EraRule "UTXOW" era) ~ UtxoEnv era
  , Environment (EraRule "GOV" era) ~ GovEnv era
  , Environment (EraRule "CERTS" era) ~ CertsEnv era
  , Signal (EraRule "UTXOW" era) ~ Tx TopTx era
  , Signal (EraRule "CERTS" era) ~ Seq (TxCert era)
  , Signal (EraRule "GOV" era) ~ GovSignal era
  , BaseM (someLEDGER era) ~ ShelleyBase
  , STS (someLEDGER era)
  , ConwayEraCertState era
  , EraRule "LEDGER" era ~ someLEDGER era
  , InjectRuleFailure "LEDGER" ShelleyLedgerPredFailure era
  , InjectRuleFailure "LEDGER" ConwayLedgerPredFailure era
  ) =>
  TransitionRule (someLEDGER era)
conwayLedgerTransition = judgmentContext >>= conwayLedgerTransitionTRC

instance
  ( AlonzoEraTx era
  , EraUTxO era
  , BabbageEraTxBody era
  , Embed (EraRule "UTXO" era) (ConwayUTXOW era)
  , State (EraRule "UTXO" era) ~ UTxOState era
  , Environment (EraRule "UTXO" era) ~ UtxoEnv era
  , Script era ~ AlonzoScript era
  , TxOut era ~ BabbageTxOut era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (EraRule "UTXO" era) ~ Tx TopTx era
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
  , PredicateFailure (EraRule "CERT" era) ~ ConwayCertPredFailure era
  , EraRuleFailure "CERT" era ~ ConwayCertPredFailure era
  , EraRule "CERTS" era ~ ConwayCERTS era
  , ConwayEraCertState era
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
  , Signal (EraRule "UTXOW" era) ~ Tx TopTx era
  , Signal (EraRule "CERTS" era) ~ Seq (TxCert era)
  , State (EraRule "UTXOW" era) ~ UTxOState era
  , State (EraRule "CERTS" era) ~ CertState era
  , EraRule "GOV" era ~ ConwayGOV era
  , ConwayEraCertState era
  , EraRule "LEDGER" era ~ ConwayLEDGER era
  , InjectRuleFailure "LEDGER" ShelleyLedgerPredFailure era
  , InjectRuleFailure "LEDGER" ConwayLedgerPredFailure era
  , InjectRuleFailure "LEDGER" ConwayCertsPredFailure era
  ) =>
  Embed (ConwayLEDGER era) (ShelleyLEDGERS era)
  where
  wrapFailed = LedgerFailure
  wrapEvent = LedgerEvent

instance
  ( ConwayEraTxCert era
  , ConwayEraPParams era
  , ConwayEraGov era
  , EraRule "GOV" era ~ ConwayGOV era
  , InjectRuleFailure "GOV" ConwayGovPredFailure era
  , ConwayEraCertState era
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
  , ConwayEraCertState era
  ) =>
  Embed (ConwayDELEG era) (ConwayLEDGER era)
  where
  wrapFailed = ConwayCertsFailure . CertFailure . DelegFailure
  wrapEvent = CertsEvent . CertEvent . DelegEvent
