{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.ImpTest (
  module Test.Cardano.Ledger.Babbage.ImpTest,
  ConwayEraImp,
  enactConstitution,
  enactTreasuryWithdrawals,
  submitGovAction,
  submitGovAction_,
  submitGovActions,
  submitProposal,
  submitAndExpireProposalToMakeReward,
  submitProposal_,
  submitProposals,
  submitFailingProposal,
  trySubmitGovAction,
  trySubmitGovActions,
  trySubmitProposal,
  trySubmitProposals,
  mkConstitutionProposal,
  mkProposal,
  mkProposalWithRewardAccount,
  mkTreasuryWithdrawalsGovAction,
  submitTreasuryWithdrawals,
  submitVote,
  submitVote_,
  submitYesVote_,
  submitFailingVote,
  trySubmitVote,
  genRegTxCert,
  genUnRegTxCert,
  registerDRep,
  unRegisterDRep,
  updateDRep,
  delegateToDRep,
  setupSingleDRep,
  setupDRepWithoutStake,
  setupPoolWithStake,
  setupPoolWithoutStake,
  conwayModifyPParams,
  getProposals,
  getEnactState,
  getGovActionState,
  lookupGovActionState,
  expectPresentGovActionId,
  expectMissingGovActionId,
  getRatifyEnv,
  calculateDRepAcceptedRatio,
  calculatePoolAcceptedRatio,
  calculateCommitteeAcceptedRatio,
  logAcceptedRatio,
  isDRepAccepted,
  isSpoAccepted,
  isCommitteeAccepted,
  getCommitteeMembers,
  getConstitution,
  registerInitialCommittee,
  logRatificationChecks,
  resignCommitteeColdKey,
  registerCommitteeHotKey,
  registerCommitteeHotKeys,
  logCurPParams,
  submitCommitteeElection,
  electBasicCommittee,
  proposalsShowDebug,
  getGovPolicy,
  submitFailingGovAction,
  submitGovActionForest,
  submitGovActionTree,
  getProposalsForest,
  logProposalsForest,
  logProposalsForestDiff,
  getCCExpiry,
  ccShouldBeExpired,
  ccShouldNotBeExpired,
  ccShouldBeResigned,
  ccShouldNotBeResigned,
  getLastEnactedCommittee,
  getLastEnactedConstitution,
  submitParameterChange,
  mkMinFeeUpdateGovAction,
  mkParameterChangeGovAction,
  mkUpdateCommitteeProposal,
  submitUpdateCommittee,
  expectCommitteeMemberPresence,
  expectCommitteeMemberAbsence,
  getLastEnactedParameterChange,
  getLastEnactedHardForkInitiation,
  getConstitutionProposals,
  getParameterChangeProposals,
  expectNumDormantEpochs,
  submitConstitution,
  isDRepExpired,
  expectDRepExpiry,
  expectActualDRepExpiry,
  expectDRepNotRegistered,
  expectCurrentProposals,
  expectNoCurrentProposals,
  expectPulserProposals,
  expectNoPulserProposals,
  currentProposalsShouldContain,
  ifBootstrap,
  whenBootstrap,
  whenPostBootstrap,
  submitYesVoteCCs_,
  donateToTreasury,
  expectMembers,
  showConwayTxBalance,
  logConwayTxBalance,
  submitBootstrapAware,
  submitBootstrapAwareFailingVote,
  submitBootstrapAwareFailingProposal,
  submitBootstrapAwareFailingProposal_,
  SubmitFailureExpectation (..),
  FailBoth (..),
  delegateSPORewardAddressToDRep_,
  getCommittee,
) where

import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.Allegra.Scripts (Timelock)
import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  EpochNo (..),
  ShelleyBase,
  StrictMaybe (..),
  UnitInterval,
  addEpochInterval,
  binOpEpochNo,
  inject,
  textToUrl,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Conway (ConwayEra, hardforkConwayBootstrapPhase)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams (UpgradeConwayPParams (..))
import Cardano.Ledger.Conway.Rules (
  ConwayCertPredFailure (..),
  ConwayCertsPredFailure (..),
  ConwayDelegPredFailure (..),
  ConwayLedgerPredFailure (..),
  EnactSignal,
  committeeAccepted,
  committeeAcceptedRatio,
  dRepAccepted,
  dRepAcceptedRatio,
  prevActionAsExpected,
  spoAccepted,
  spoAcceptedRatio,
  validCommitteeTerm,
  withdrawalCanWithdraw,
 )
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Conway.TxCert (Delegatee (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep
import Cardano.Ledger.Plutus.Language (Language (..), SLanguage (..), hashPlutusScript)
import Cardano.Ledger.PoolParams (PoolParams (..), ppRewardAccount)
import Cardano.Ledger.Shelley.LedgerState (
  curPParamsEpochStateL,
  epochStateGovStateL,
  epochStatePoolParamsL,
  esLStateL,
  lsCertStateL,
  lsUTxOStateL,
  nesELL,
  nesEpochStateL,
  nesEsL,
  nesPdL,
  newEpochStateGovStateL,
  produced,
  utxosGovStateL,
 )
import Cardano.Ledger.Shelley.Rules (ShelleyDelegPredFailure)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.TxIn (TxId (..))
import Cardano.Ledger.Val (Val (..), (<->))
import Control.Monad (forM)
import Control.Monad.Trans.Fail.String (errorFail)
import Control.State.Transition.Extended (STS (..))
import Data.Bifunctor (bimap)
import Data.Default (Default (..))
import Data.Foldable (Foldable (..))
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Tree
import qualified GHC.Exts as GHC (fromList)
import Lens.Micro
import Prettyprinter (align, hsep, viaShow, vsep)
import Test.Cardano.Ledger.Babbage.ImpTest
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.Era
import Test.Cardano.Ledger.Conway.TreeDiff (tableDoc)
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus (testingCostModel)
import Test.Cardano.Ledger.Plutus.Guardrail (guardrailScript)

-- | Modify the PParams in the current state with the given function
conwayModifyPParams ::
  ConwayEraGov era =>
  (PParams era -> PParams era) ->
  ImpTestM era ()
conwayModifyPParams f = modifyNES $ \nes ->
  nes
    & nesEsL . curPParamsEpochStateL %~ f
    & newEpochStateGovStateL . drepPulsingStateGovStateL %~ modifyDRepPulser
  where
    modifyDRepPulser pulser =
      case finishDRepPulser pulser of
        (snapshot, ratifyState) ->
          DRComplete snapshot (ratifyState & rsEnactStateL . ensCurPParamsL %~ f)

instance ShelleyEraImp ConwayEra where
  initGenesis = do
    kh1 <- freshKeyHash
    kh2 <- freshKeyHash
    let
      ccExpiryEpochNo = addEpochInterval (impEraStartEpochNo @ConwayEra) (EpochInterval 15)
      committee = Committee [(KeyHashObj kh1, ccExpiryEpochNo), (KeyHashObj kh2, ccExpiryEpochNo)] (1 %! 1)
      constitutionAnchor =
        Anchor
          { anchorUrl = errorFail $ textToUrl 128 "https://cardano-constitution.crypto"
          , anchorDataHash = hashAnnotated (AnchorData "Cardano Constitution Content")
          }
      guardrailScriptHash = hashPlutusScript guardrailScript
    pure
      ConwayGenesis
        { cgUpgradePParams =
            UpgradeConwayPParams
              { ucppPoolVotingThresholds =
                  PoolVotingThresholds
                    { pvtMotionNoConfidence = 51 %! 100
                    , pvtCommitteeNormal = 65 %! 100
                    , pvtCommitteeNoConfidence = 65 %! 100
                    , pvtHardForkInitiation = 51 %! 100
                    , pvtPPSecurityGroup = 51 %! 100
                    }
              , ucppDRepVotingThresholds =
                  DRepVotingThresholds
                    { dvtMotionNoConfidence = 51 %! 100
                    , dvtCommitteeNormal = 65 %! 100
                    , dvtCommitteeNoConfidence = 65 %! 100
                    , dvtUpdateToConstitution = 65 %! 100
                    , dvtHardForkInitiation = 51 %! 100
                    , dvtPPNetworkGroup = 51 %! 100
                    , dvtPPEconomicGroup = 51 %! 100
                    , dvtPPTechnicalGroup = 51 %! 100
                    , dvtPPGovGroup = 75 %! 100
                    , dvtTreasuryWithdrawal = 51 %! 100
                    }
              , ucppCommitteeMinSize = 1
              , ucppCommitteeMaxTermLength = EpochInterval 20
              , ucppGovActionLifetime = EpochInterval 30
              , ucppGovActionDeposit = Coin 123
              , ucppDRepDeposit = Coin 70_000_000
              , ucppDRepActivity = EpochInterval 100
              , ucppMinFeeRefScriptCostPerByte = 15 %! 1
              , ucppPlutusV3CostModel = testingCostModel PlutusV3
              }
        , cgConstitution = Constitution constitutionAnchor (SJust guardrailScriptHash)
        , cgCommittee = committee
        , cgDelegs = mempty
        , cgInitialDReps = mempty
        }

  impSatisfyNativeScript = impAllegraSatisfyNativeScript

  modifyPParams = conwayModifyPParams

  fixupTx = babbageFixupTx
  expectTxSuccess = impBabbageExpectTxSuccess

instance MaryEraImp ConwayEra

instance AlonzoEraImp ConwayEra where
  scriptTestContexts =
    plutusTestScripts SPlutusV1
      <> plutusTestScripts SPlutusV2
      <> plutusTestScripts SPlutusV3

class
  ( AlonzoEraImp era
  , ConwayEraTest era
  , ConwayEraTxCert era
  , STS (EraRule "ENACT" era)
  , BaseM (EraRule "ENACT" era) ~ ShelleyBase
  , State (EraRule "ENACT" era) ~ EnactState era
  , Signal (EraRule "ENACT" era) ~ EnactSignal era
  , Environment (EraRule "ENACT" era) ~ ()
  , NativeScript era ~ Timelock era
  , GovState era ~ ConwayGovState era
  ) =>
  ConwayEraImp era

instance ConwayEraImp ConwayEra

registerInitialCommittee ::
  (HasCallStack, ConwayEraImp era) =>
  ImpTestM era (NonEmpty (Credential 'HotCommitteeRole))
registerInitialCommittee = do
  committeeMembers <- Set.toList <$> getCommitteeMembers
  case committeeMembers of
    x : xs -> registerCommitteeHotKeys (KeyHashObj <$> freshKeyHash) $ x NE.:| xs
    [] -> error "Expected an initial committee"

-- | Submit a transaction that registers a new DRep and return the keyhash
-- belonging to that DRep
registerDRep :: ConwayEraImp era => ImpTestM era (KeyHash 'DRepRole)
registerDRep = do
  -- Register a DRep
  khDRep <- freshKeyHash
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  submitTxAnn_ "Register DRep" $
    mkBasicTx mkBasicTxBody
      & bodyTxL . certsTxBodyL
        .~ SSeq.singleton
          ( RegDRepTxCert
              (KeyHashObj khDRep)
              (pp ^. ppDRepDepositL)
              SNothing
          )
  dreps <- getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL
  dreps `shouldSatisfy` Map.member (KeyHashObj khDRep)
  pure khDRep

-- | Submit a transaction that unregisters a given DRep
unRegisterDRep ::
  forall era.
  ( ShelleyEraImp era
  , ConwayEraTxCert era
  , ConwayEraCertState era
  ) =>
  Credential 'DRepRole ->
  ImpTestM era ()
unRegisterDRep drep = do
  drepState <- getDRepState drep
  let refund = drepDeposit drepState
  submitTxAnn_ "UnRegister DRep" $
    mkBasicTx mkBasicTxBody
      & bodyTxL . certsTxBodyL
        .~ SSeq.singleton (UnRegDRepTxCert drep refund)

genUnRegTxCert ::
  forall era.
  ( ShelleyEraImp era
  , ConwayEraTxCert era
  ) =>
  Credential 'Staking ->
  ImpTestM era (TxCert era)
genUnRegTxCert stakingCredential = do
  accounts <- getsNES (nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL)
  case lookupAccountState stakingCredential accounts of
    Nothing -> pure $ UnRegTxCert stakingCredential
    Just accountState ->
      elements
        [ UnRegTxCert stakingCredential
        , UnRegDepositTxCert stakingCredential (fromCompact (accountState ^. depositAccountStateL))
        ]

genRegTxCert ::
  forall era.
  ( ShelleyEraImp era
  , ConwayEraTxCert era
  ) =>
  Credential 'Staking ->
  ImpTestM era (TxCert era)
genRegTxCert stakingCredential =
  oneof
    [ pure $ RegTxCert stakingCredential
    , RegDepositTxCert stakingCredential
        <$> getsNES (nesEsL . curPParamsEpochStateL . ppKeyDepositL)
    ]

-- | Submit a transaction that updates a given DRep
updateDRep ::
  forall era.
  ( ShelleyEraImp era
  , ConwayEraTxCert era
  ) =>
  Credential 'DRepRole ->
  ImpTestM era ()
updateDRep drep = do
  mAnchor <- arbitrary
  submitTxAnn_ "Update DRep" $
    mkBasicTx mkBasicTxBody
      & bodyTxL . certsTxBodyL
        .~ SSeq.singleton (UpdateDRepTxCert drep mAnchor)

-- | In contrast to `setupSingleDRep`, this function does not make a UTxO entry
-- that could count as delegated stake to the DRep
setupDRepWithoutStake ::
  ConwayEraImp era =>
  ImpTestM era (KeyHash 'DRepRole, KeyHash 'Staking)
setupDRepWithoutStake = do
  drepKH <- registerDRep
  delegatorKH <- freshKeyHash
  deposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
  submitTxAnn_ "Delegate to DRep" $
    mkBasicTx mkBasicTxBody
      & bodyTxL . certsTxBodyL
        .~ SSeq.fromList
          [ RegDepositDelegTxCert
              (KeyHashObj delegatorKH)
              (DelegVote (DRepCredential $ KeyHashObj drepKH))
              deposit
          ]
  pure (drepKH, delegatorKH)

-- | Registers a new DRep, registers its stake credentials and delegates the specified amount of ADA to it.
setupSingleDRep ::
  ConwayEraImp era =>
  Integer ->
  ImpTestM era (Credential 'DRepRole, Credential 'Staking, KeyPair 'Payment)
setupSingleDRep stake = do
  drepKH <- registerDRep
  delegatorKH <- freshKeyHash
  deposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
  let tx =
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ SSeq.fromList [RegDepositTxCert (KeyHashObj delegatorKH) deposit]
  submitTx_ tx
  spendingKP <-
    delegateToDRep (KeyHashObj delegatorKH) (Coin stake) (DRepCredential (KeyHashObj drepKH))
  pure (KeyHashObj drepKH, KeyHashObj delegatorKH, spendingKP)

delegateToDRep ::
  ConwayEraImp era =>
  Credential 'Staking ->
  Coin ->
  DRep ->
  ImpTestM era (KeyPair 'Payment)
delegateToDRep cred stake dRep = do
  (_, spendingKP) <- freshKeyPair

  submitTxAnn_ "Delegate to DRep" $
    mkBasicTx mkBasicTxBody
      & bodyTxL . outputsTxBodyL
        .~ SSeq.singleton (mkBasicTxOut (mkAddr spendingKP cred) (inject stake))
      & bodyTxL . certsTxBodyL
        .~ SSeq.fromList [DelegTxCert cred (DelegVote dRep)]
  pure spendingKP

getDRepState ::
  (HasCallStack, ConwayEraCertState era) =>
  Credential 'DRepRole ->
  ImpTestM era DRepState
getDRepState dRepCred = do
  drepsState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL
  case Map.lookup dRepCred drepsState of
    Nothing -> error $ "Expected for DRep " ++ show dRepCred ++ " to be present in the CertState"
    Just state -> pure state

-- | Sets up a stake pool with coin delegated to it.
--
-- NOTE: This uses the `RegDepositDelegTxCert` for delegating, so it has to be
-- in Conway. The Shelley version of this function would have to separately
-- register the staking credential and then delegate it.
setupPoolWithStake ::
  (ShelleyEraImp era, ConwayEraTxCert era) =>
  Coin ->
  ImpTestM era (KeyHash 'StakePool, Credential 'Payment, Credential 'Staking)
setupPoolWithStake delegCoin = do
  khPool <- freshKeyHash
  registerPool khPool
  credDelegatorPayment <- KeyHashObj <$> freshKeyHash
  credDelegatorStaking <- KeyHashObj <$> freshKeyHash
  sendCoinTo_ (mkAddr credDelegatorPayment credDelegatorStaking) delegCoin
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  submitTxAnn_ "Delegate to stake pool" $
    mkBasicTx mkBasicTxBody
      & bodyTxL . certsTxBodyL
        .~ SSeq.fromList
          [ RegDepositDelegTxCert
              credDelegatorStaking
              (DelegStake khPool)
              (pp ^. ppKeyDepositL)
          ]
  pure (khPool, credDelegatorPayment, credDelegatorStaking)

setupPoolWithoutStake ::
  (ShelleyEraImp era, ConwayEraTxCert era) =>
  ImpTestM era (KeyHash 'StakePool, Credential 'Staking)
setupPoolWithoutStake = do
  khPool <- freshKeyHash
  registerPool khPool
  credDelegatorStaking <- KeyHashObj <$> freshKeyHash
  deposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
  submitTxAnn_ "Delegate to stake pool" $
    mkBasicTx mkBasicTxBody
      & bodyTxL . certsTxBodyL
        .~ SSeq.fromList
          [ RegDepositDelegTxCert
              credDelegatorStaking
              (DelegStake khPool)
              deposit
          ]
  pure (khPool, credDelegatorStaking)

-- | Submits a transaction with a Vote for the given governance action as
-- some voter
submitVote ::
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  , HasCallStack
  ) =>
  Vote ->
  Voter ->
  GovActionId ->
  ImpTestM era TxId
submitVote vote voter gaId = trySubmitVote vote voter gaId >>= expectRightDeep

-- | Submits a transaction that votes "Yes" for the given governance action as
-- some voter
submitYesVote_ ::
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  , HasCallStack
  ) =>
  Voter ->
  GovActionId ->
  ImpTestM era ()
submitYesVote_ voter gaId = void $ submitVote VoteYes voter gaId

submitVote_ ::
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  , HasCallStack
  ) =>
  Vote ->
  Voter ->
  GovActionId ->
  ImpTestM era ()
submitVote_ vote voter gaId = void $ submitVote vote voter gaId

submitFailingVote ::
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  , HasCallStack
  ) =>
  Voter ->
  GovActionId ->
  NonEmpty (PredicateFailure (EraRule "LEDGER" era)) ->
  ImpTestM era ()
submitFailingVote voter gaId expectedFailure =
  trySubmitVote VoteYes voter gaId >>= (`shouldBeLeftExpr` expectedFailure)

-- | Submits a transaction that votes "Yes" for the given governance action as
-- some voter, and expects an `Either` result.
trySubmitVote ::
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  ) =>
  Vote ->
  Voter ->
  GovActionId ->
  ImpTestM era (Either (NonEmpty (PredicateFailure (EraRule "LEDGER" era))) TxId)
trySubmitVote vote voter gaId =
  fmap (bimap fst txIdTx) $
    trySubmitTx $
      mkBasicTx mkBasicTxBody
        & bodyTxL . votingProceduresTxBodyL
          .~ VotingProcedures
            ( Map.singleton
                voter
                ( Map.singleton
                    gaId
                    ( VotingProcedure
                        { vProcVote = vote
                        , vProcAnchor = SNothing
                        }
                    )
                )
            )

submitProposal_ ::
  (ShelleyEraImp era, ConwayEraTxBody era, HasCallStack) =>
  ProposalProcedure era ->
  ImpTestM era ()
submitProposal_ = void . submitProposal

submitProposal ::
  (ShelleyEraImp era, ConwayEraTxBody era, HasCallStack) =>
  ProposalProcedure era ->
  ImpTestM era GovActionId
submitProposal proposal = trySubmitProposal proposal >>= expectRightExpr

submitProposals ::
  (ShelleyEraImp era, ConwayEraGov era, ConwayEraTxBody era, HasCallStack) =>
  NE.NonEmpty (ProposalProcedure era) ->
  ImpTestM era (NE.NonEmpty GovActionId)
submitProposals proposals = do
  curEpochNo <- getsNES nesELL
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  tx <- trySubmitProposals proposals >>= expectRightExpr
  let txId = txIdTx tx
      proposalsWithGovActionId =
        NE.zipWith (\idx p -> (GovActionId txId (GovActionIx idx), p)) (0 NE.:| [1 ..]) proposals
  forM proposalsWithGovActionId $ \(govActionId, proposal) -> do
    govActionState <- getGovActionState govActionId
    govActionState
      `shouldBeExpr` GovActionState
        { gasId = govActionId
        , gasCommitteeVotes = mempty
        , gasDRepVotes = mempty
        , gasStakePoolVotes = mempty
        , gasProposalProcedure = proposal
        , gasProposedIn = curEpochNo
        , gasExpiresAfter = addEpochInterval curEpochNo (pp ^. ppGovActionLifetimeL)
        }
    pure govActionId

-- | Submits a transaction that proposes the given proposal
trySubmitProposal ::
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  ) =>
  ProposalProcedure era ->
  ImpTestM era (Either (NonEmpty (PredicateFailure (EraRule "LEDGER" era))) GovActionId)
trySubmitProposal proposal = do
  res <- trySubmitProposals (pure proposal)
  pure $ case res of
    Right tx ->
      Right
        GovActionId
          { gaidTxId = txIdTx tx
          , gaidGovActionIx = GovActionIx 0
          }
    Left (err, _) -> Left err

trySubmitProposals ::
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  ) =>
  NE.NonEmpty (ProposalProcedure era) ->
  ImpTestM era (Either (NonEmpty (PredicateFailure (EraRule "LEDGER" era)), Tx era) (Tx era))
trySubmitProposals proposals = do
  trySubmitTx $
    mkBasicTx mkBasicTxBody
      & bodyTxL . proposalProceduresTxBodyL .~ GHC.fromList (toList proposals)

submitFailingProposal ::
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  , HasCallStack
  ) =>
  ProposalProcedure era ->
  NonEmpty (PredicateFailure (EraRule "LEDGER" era)) ->
  ImpTestM era ()
submitFailingProposal proposal expectedFailure =
  trySubmitProposal proposal >>= (`shouldBeLeftExpr` expectedFailure)

-- | Submits a transaction that proposes the given governance action. For proposing
-- multiple actions in the same transaciton use `trySubmitGovActions` instead.
trySubmitGovAction ::
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  ) =>
  GovAction era ->
  ImpTestM era (Either (NonEmpty (PredicateFailure (EraRule "LEDGER" era))) GovActionId)
trySubmitGovAction ga = do
  let mkGovActionId tx = GovActionId (txIdTx tx) (GovActionIx 0)
  bimap fst mkGovActionId <$> trySubmitGovActions (pure ga)

submitAndExpireProposalToMakeReward ::
  ConwayEraImp era =>
  Credential 'Staking ->
  ImpTestM era ()
submitAndExpireProposalToMakeReward stakingC = do
  rewardAccount <- getRewardAccountFor stakingC
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  let
    EpochInterval lifetime = pp ^. ppGovActionLifetimeL
    deposit = pp ^. ppGovActionDepositL
  gai <-
    submitProposal $
      ProposalProcedure
        { pProcDeposit = deposit
        , pProcReturnAddr = rewardAccount
        , pProcGovAction = InfoAction
        , pProcAnchor = def
        }
  passNEpochs $ 2 + fromIntegral lifetime
  expectMissingGovActionId gai

-- | Submits a transaction that proposes the given governance action
trySubmitGovActions ::
  (ShelleyEraImp era, ConwayEraTxBody era) =>
  NE.NonEmpty (GovAction era) ->
  ImpTestM era (Either (NonEmpty (PredicateFailure (EraRule "LEDGER" era)), Tx era) (Tx era))
trySubmitGovActions gas = do
  proposals <- traverse mkProposal gas
  trySubmitProposals proposals

mkProposalWithRewardAccount ::
  (ShelleyEraImp era, ConwayEraTxBody era) =>
  GovAction era ->
  RewardAccount ->
  ImpTestM era (ProposalProcedure era)
mkProposalWithRewardAccount ga rewardAccount = do
  deposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionDepositL
  anchor <- arbitrary
  pure
    ProposalProcedure
      { pProcDeposit = deposit
      , pProcReturnAddr = rewardAccount
      , pProcGovAction = ga
      , pProcAnchor = anchor
      }

mkProposal ::
  (ShelleyEraImp era, ConwayEraTxBody era) =>
  GovAction era ->
  ImpTestM era (ProposalProcedure era)
mkProposal ga = do
  rewardAccount <- registerRewardAccount
  mkProposalWithRewardAccount ga rewardAccount

submitGovAction ::
  forall era.
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  , HasCallStack
  ) =>
  GovAction era ->
  ImpTestM era GovActionId
submitGovAction ga = do
  gaId NE.:| _ <- submitGovActions (pure ga)
  pure gaId

submitGovAction_ ::
  forall era.
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  , HasCallStack
  ) =>
  GovAction era ->
  ImpTestM era ()
submitGovAction_ = void . submitGovAction

submitGovActions ::
  forall era.
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  , HasCallStack
  ) =>
  NE.NonEmpty (GovAction era) ->
  ImpTestM era (NE.NonEmpty GovActionId)
submitGovActions gas = do
  tx <- trySubmitGovActions gas >>= expectRightExpr
  let txId = txIdTx tx
  pure $ NE.zipWith (\idx _ -> GovActionId txId (GovActionIx idx)) (0 NE.:| [1 ..]) gas

mkTreasuryWithdrawalsGovAction ::
  ConwayEraGov era =>
  [(RewardAccount, Coin)] ->
  ImpTestM era (GovAction era)
mkTreasuryWithdrawalsGovAction wdrls =
  TreasuryWithdrawals (Map.fromList wdrls) <$> getGovPolicy

submitTreasuryWithdrawals ::
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  , ConwayEraGov era
  ) =>
  [(RewardAccount, Coin)] ->
  ImpTestM era GovActionId
submitTreasuryWithdrawals wdrls =
  mkTreasuryWithdrawalsGovAction wdrls >>= submitGovAction

enactTreasuryWithdrawals ::
  ConwayEraImp era =>
  [(RewardAccount, Coin)] ->
  Credential 'DRepRole ->
  NonEmpty (Credential 'HotCommitteeRole) ->
  ImpTestM era GovActionId
enactTreasuryWithdrawals withdrawals dRep cms = do
  gaId <- submitTreasuryWithdrawals withdrawals
  submitYesVote_ (DRepVoter dRep) gaId
  submitYesVoteCCs_ cms gaId
  passNEpochs 2
  pure gaId

submitParameterChange ::
  ConwayEraImp era =>
  StrictMaybe GovActionId ->
  PParamsUpdate era ->
  ImpTestM era GovActionId
submitParameterChange parent ppu =
  mkParameterChangeGovAction parent ppu >>= submitGovAction

mkParameterChangeGovAction ::
  ConwayEraImp era =>
  StrictMaybe GovActionId ->
  PParamsUpdate era ->
  ImpTestM era (GovAction era)
mkParameterChangeGovAction parent ppu =
  ParameterChange (GovPurposeId <$> parent) ppu <$> getGovPolicy

mkMinFeeUpdateGovAction ::
  ConwayEraImp era =>
  StrictMaybe GovActionId ->
  ImpTestM era (GovAction era)
mkMinFeeUpdateGovAction p = do
  minFeeValue <- uniformRM (30, 1000)
  mkParameterChangeGovAction p (def & ppuMinFeeAL .~ SJust (Coin minFeeValue))

getGovPolicy :: ConwayEraGov era => ImpTestM era (StrictMaybe ScriptHash)
getGovPolicy =
  getsNES $
    nesEpochStateL . epochStateGovStateL . constitutionGovStateL . constitutionScriptL

submitFailingGovAction ::
  forall era.
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  , HasCallStack
  ) =>
  GovAction era ->
  NonEmpty (PredicateFailure (EraRule "LEDGER" era)) ->
  ImpTestM era ()
submitFailingGovAction ga expectedFailure = trySubmitGovAction ga >>= (`shouldBeLeftExpr` expectedFailure)

getEnactState :: ConwayEraGov era => ImpTestM era (EnactState era)
getEnactState = mkEnactState <$> getsNES (nesEsL . epochStateGovStateL)

getProposals :: ConwayEraGov era => ImpTestM era (Proposals era)
getProposals = getsNES $ newEpochStateGovStateL . proposalsGovStateL

logProposalsForest :: (ConwayEraGov era, HasCallStack) => ImpTestM era ()
logProposalsForest = do
  proposals <- getProposals
  logDoc $ proposalsShowDebug proposals True

getCommitteeMembers ::
  ConwayEraImp era =>
  ImpTestM era (Set.Set (Credential 'ColdCommitteeRole))
getCommitteeMembers = do
  committee <- getsNES $ newEpochStateGovStateL . committeeGovStateL
  pure $ Map.keysSet $ foldMap' committeeMembers committee

getLastEnactedCommittee ::
  ConwayEraGov era => ImpTestM era (StrictMaybe (GovPurposeId 'CommitteePurpose))
getLastEnactedCommittee = do
  ps <- getProposals
  pure $ ps ^. pRootsL . grCommitteeL . prRootL

getConstitution ::
  ConwayEraImp era =>
  ImpTestM era (Constitution era)
getConstitution = getsNES $ newEpochStateGovStateL . constitutionGovStateL

getLastEnactedConstitution ::
  ConwayEraGov era => ImpTestM era (StrictMaybe (GovPurposeId 'ConstitutionPurpose))
getLastEnactedConstitution = do
  ps <- getProposals
  pure $ ps ^. pRootsL . grConstitutionL . prRootL

getLastEnactedParameterChange ::
  ConwayEraGov era => ImpTestM era (StrictMaybe (GovPurposeId 'PParamUpdatePurpose))
getLastEnactedParameterChange = do
  ps <- getProposals
  pure $ ps ^. pRootsL . grPParamUpdateL . prRootL

getLastEnactedHardForkInitiation ::
  ConwayEraGov era => ImpTestM era (StrictMaybe (GovPurposeId 'HardForkPurpose))
getLastEnactedHardForkInitiation = do
  ps <- getProposals
  pure $ ps ^. pRootsL . grHardForkL . prRootL

getConstitutionProposals ::
  ConwayEraGov era =>
  ImpTestM
    era
    ( Map.Map
        (GovPurposeId 'ConstitutionPurpose)
        (PEdges (GovPurposeId 'ConstitutionPurpose))
    )
getConstitutionProposals = do
  ps <- getProposals
  pure $ ps ^. pGraphL . grConstitutionL . pGraphNodesL

getParameterChangeProposals ::
  ConwayEraGov era =>
  ImpTestM
    era
    ( Map.Map
        (GovPurposeId 'PParamUpdatePurpose)
        (PEdges (GovPurposeId 'PParamUpdatePurpose))
    )
getParameterChangeProposals = do
  ps <- getProposals
  pure $ ps ^. pGraphL . grPParamUpdateL . pGraphNodesL

logProposalsForestDiff ::
  ( Era era
  , ToExpr (PParamsHKD StrictMaybe era)
  , HasCallStack
  ) =>
  Proposals era ->
  Proposals era ->
  ImpTestM era ()
logProposalsForestDiff pf1 pf2 = logDoc $ vsep ["Proposals Forest Diff:", diffExpr pf1 pf2]

-- | Looks up the governance action state corresponding to the governance action id
lookupGovActionState ::
  ConwayEraGov era =>
  GovActionId ->
  ImpTestM era (Maybe (GovActionState era))
lookupGovActionState aId = proposalsLookupId aId <$> getProposals

-- | Looks up the governance action state corresponding to the governance action id
getGovActionState ::
  (HasCallStack, ConwayEraGov era) =>
  GovActionId ->
  ImpTestM era (GovActionState era)
getGovActionState govActionId =
  impAnn "Expecting an action state" $ do
    lookupGovActionState govActionId >>= \case
      Nothing ->
        assertFailure $ "Could not find action state for govActionId: " <> show govActionId
      Just govActionState -> pure govActionState

expectPresentGovActionId ::
  (HasCallStack, ConwayEraGov era) =>
  GovActionId ->
  ImpTestM era ()
expectPresentGovActionId govActionId = void $ getGovActionState govActionId

expectMissingGovActionId ::
  (HasCallStack, ConwayEraGov era) =>
  GovActionId ->
  ImpTestM era ()
expectMissingGovActionId govActionId =
  impAnn "Expecting for gov action state to be missing" $ do
    lookupGovActionState govActionId >>= \case
      Just _ ->
        expectationFailure $ "Found gov action state for govActionId: " <> ansiExprString govActionId
      Nothing -> pure ()

-- | Builds a RatifyEnv from the current state
getRatifyEnv :: (ConwayEraGov era, ConwayEraCertState era) => ImpTestM era (RatifyEnv era)
getRatifyEnv = do
  eNo <- getsNES nesELL
  instantStake <- getsNES instantStakeG
  poolDistr <- getsNES nesPdL
  drepDistr <- getsNES $ nesEsL . epochStateDRepPulsingStateL . psDRepDistrG
  drepState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL
  committeeState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL
  accounts <- getsNES (nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL)
  poolPs <- getsNES $ nesEsL . epochStatePoolParamsL
  pure
    RatifyEnv
      { reStakePoolDistr = poolDistr
      , reInstantStake = instantStake
      , reDRepState = drepState
      , reDRepDistr = drepDistr
      , reCurrentEpoch = eNo - 1
      , reCommitteeState = committeeState
      , reAccounts = accounts
      , rePoolParams = poolPs
      }

ccShouldNotBeExpired ::
  (HasCallStack, ConwayEraGov era) =>
  Credential 'ColdCommitteeRole ->
  ImpTestM era ()
ccShouldNotBeExpired coldC = do
  curEpochNo <- getsNES nesELL
  ccExpiryEpochNo <- getCCExpiry coldC
  curEpochNo `shouldSatisfy` (<= ccExpiryEpochNo)

ccShouldBeExpired ::
  (HasCallStack, ConwayEraGov era) =>
  Credential 'ColdCommitteeRole ->
  ImpTestM era ()
ccShouldBeExpired coldC = do
  curEpochNo <- getsNES nesELL
  ccExpiryEpochNo <- getCCExpiry coldC
  curEpochNo `shouldSatisfy` (> ccExpiryEpochNo)

getCCExpiry ::
  (HasCallStack, ConwayEraGov era) =>
  Credential 'ColdCommitteeRole ->
  ImpTestM era EpochNo
getCCExpiry coldC = do
  committee <- getsNES $ nesEsL . epochStateGovStateL . committeeGovStateL
  case committee of
    SNothing -> assertFailure "There is no committee"
    SJust Committee {committeeMembers} ->
      case Map.lookup coldC committeeMembers of
        Nothing -> assertFailure $ "Committee not found for cold credential: " <> show coldC
        Just epochNo -> pure epochNo

-- | Test the resignation status for a CC cold key to be resigned
ccShouldBeResigned ::
  (HasCallStack, ConwayEraCertState era) => Credential 'ColdCommitteeRole -> ImpTestM era ()
ccShouldBeResigned coldK = do
  committeeCreds <-
    getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL . csCommitteeCredsL
  authHk <$> Map.lookup coldK committeeCreds `shouldBe` Just Nothing

-- | Test the resignation status for a CC cold key to not be resigned
ccShouldNotBeResigned ::
  (HasCallStack, ConwayEraCertState era) => Credential 'ColdCommitteeRole -> ImpTestM era ()
ccShouldNotBeResigned coldK = do
  committeeCreds <-
    getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL . csCommitteeCredsL
  (Map.lookup coldK committeeCreds >>= authHk) `shouldSatisfy` isJust

authHk :: CommitteeAuthorization -> Maybe (Credential 'HotCommitteeRole)
authHk (CommitteeHotCredential hk) = Just hk
authHk _ = Nothing

-- | Calculates the ratio of DReps that have voted for the governance action
calculateDRepAcceptedRatio ::
  forall era.
  (HasCallStack, ConwayEraGov era, ConwayEraCertState era) =>
  GovActionId ->
  ImpTestM era Rational
calculateDRepAcceptedRatio gaId = do
  ratEnv <- getRatifyEnv
  gas <- getGovActionState gaId
  pure $
    dRepAcceptedRatio @era
      ratEnv
      (gas ^. gasDRepVotesL)
      (gasAction gas)

-- | Calculates the ratio of Committee members that have voted for the governance
-- action
calculateCommitteeAcceptedRatio ::
  forall era.
  (HasCallStack, ConwayEraGov era, ConwayEraCertState era) =>
  GovActionId ->
  ImpTestM era Rational
calculateCommitteeAcceptedRatio gaId = do
  eNo <- getsNES nesELL
  RatifyEnv {reCommitteeState} <- getRatifyEnv
  GovActionState {gasCommitteeVotes} <- getGovActionState gaId
  committee <- getsNES $ nesEsL . epochStateGovStateL . committeeGovStateL
  let
    members = foldMap' (committeeMembers @era) committee
  pure $
    committeeAcceptedRatio
      members
      gasCommitteeVotes
      reCommitteeState
      eNo

calculatePoolAcceptedRatio ::
  (ConwayEraGov era, ConwayEraCertState era) => GovActionId -> ImpTestM era Rational
calculatePoolAcceptedRatio gaId = do
  ratEnv <- getRatifyEnv
  gas <- getGovActionState gaId
  pv <- getProtVer
  pure $ spoAcceptedRatio ratEnv gas pv

-- | Logs the ratios of accepted votes per category
logAcceptedRatio ::
  (HasCallStack, ConwayEraGov era, ConwayEraCertState era) => GovActionId -> ImpTestM era ()
logAcceptedRatio aId = do
  dRepRatio <- calculateDRepAcceptedRatio aId
  committeeRatio <- calculateCommitteeAcceptedRatio aId
  spoRatio <- calculatePoolAcceptedRatio aId
  logDoc $
    tableDoc
      (Just "ACCEPTED RATIOS")
      [ ("DRep accepted ratio:", viaShow dRepRatio)
      , ("Committee accepted ratio:", viaShow committeeRatio)
      , ("SPO accepted ratio:", viaShow spoRatio)
      ]

getRatifyEnvAndState ::
  (ConwayEraGov era, ConwayEraCertState era) => ImpTestM era (RatifyEnv era, RatifyState era)
getRatifyEnvAndState = do
  ratifyEnv <- getRatifyEnv
  enactState <- getEnactState
  let ratifyState =
        RatifyState
          { rsEnactState = enactState
          , rsEnacted = mempty
          , rsExpired = mempty
          , rsDelayed = False
          }
  pure (ratifyEnv, ratifyState)

-- | Checks whether the governance action has enough DRep votes to be accepted in the next
-- epoch. (Note that no other checks except DRep votes are used)
isDRepAccepted ::
  (HasCallStack, ConwayEraGov era, ConwayEraPParams era, ConwayEraCertState era) =>
  GovActionId ->
  ImpTestM era Bool
isDRepAccepted gaId = do
  (ratifyEnv, ratifyState) <- getRatifyEnvAndState
  action <- getGovActionState gaId
  pure $ dRepAccepted ratifyEnv ratifyState action

isSpoAccepted ::
  (HasCallStack, ConwayEraGov era, ConwayEraPParams era, ConwayEraCertState era) =>
  GovActionId ->
  ImpTestM era Bool
isSpoAccepted gaId = do
  (ratifyEnv, ratifyState) <- getRatifyEnvAndState
  action <- getGovActionState gaId
  pure $ spoAccepted ratifyEnv ratifyState action

isCommitteeAccepted ::
  (HasCallStack, ConwayEraGov era, ConwayEraPParams era, ConwayEraCertState era) =>
  GovActionId ->
  ImpTestM era Bool
isCommitteeAccepted gaId = do
  (ratifyEnv, ratifyState) <- getRatifyEnvAndState
  action <- getGovActionState gaId
  pure $ committeeAccepted ratifyEnv ratifyState action

-- | Logs the results of each check required to make the governance action pass
logRatificationChecks ::
  (ConwayEraGov era, ConwayEraPParams era, HasCallStack, ConwayEraCertState era) =>
  GovActionId ->
  ImpTestM era ()
logRatificationChecks gaId = do
  mbyGas <- lookupGovActionState gaId
  case mbyGas of
    Nothing -> logText $ "Goveranance action not found: " <> T.pack (show gaId)
    Just gas@GovActionState {gasCommitteeVotes, gasDRepVotes} -> do
      let govAction = gasAction gas
      ens@EnactState {..} <- getEnactState
      committee <- getsNES $ nesEsL . epochStateGovStateL . committeeGovStateL
      ratEnv@RatifyEnv {reCurrentEpoch} <- getRatifyEnv
      let ratSt = RatifyState ens mempty mempty False
      curTreasury <- getsNES treasuryL
      currentEpoch <- getsNES nesELL
      pv <- getProtVer
      let
        members = foldMap' committeeMembers committee
        committeeState = reCommitteeState ratEnv
      curPParams <- getsNES $ nesEsL . epochStateGovStateL . curPParamsGovStateL
      logDoc $
        tableDoc
          (Just "RATIFICATION CHECKS")
          [ ("prevActionAsExpected:", viaShow $ prevActionAsExpected gas ensPrevGovActionIds)
          , ("validCommitteeTerm:", viaShow $ validCommitteeTerm govAction curPParams currentEpoch)
          , ("notDelayed:", "??")
          , ("withdrawalCanWithdraw:", viaShow $ withdrawalCanWithdraw govAction curTreasury)
          ,
            ( "committeeAccepted:"
            , hsep
                [ viaShow $ committeeAccepted ratEnv ratSt gas
                , "["
                , "To Pass:"
                , viaShow $ committeeAcceptedRatio members gasCommitteeVotes committeeState currentEpoch
                , ">="
                , viaShow $ votingCommitteeThreshold reCurrentEpoch ratSt committeeState (gasAction gas)
                , "]"
                ]
            )
          ,
            ( "spoAccepted:"
            , hsep
                [ viaShow $ spoAccepted ratEnv ratSt gas
                , "["
                , "To Pass:"
                , viaShow $ spoAcceptedRatio ratEnv gas pv
                , ">="
                , viaShow $ votingStakePoolThreshold ratSt (gasAction gas)
                , "]"
                ]
            )
          ,
            ( "dRepAccepted:"
            , hsep
                [ viaShow $ dRepAccepted ratEnv ratSt gas
                , "["
                , "To Pass:"
                , viaShow $ dRepAcceptedRatio ratEnv gasDRepVotes (gasAction gas)
                , ">="
                , viaShow $ votingDRepThreshold ratSt (gasAction gas)
                , "]"
                ]
            )
          ]

-- | Submits a transaction that registers a hot key for the given cold key.
-- Returns the hot key hash.
registerCommitteeHotKey ::
  (ShelleyEraImp era, ConwayEraTxCert era) =>
  Credential 'ColdCommitteeRole ->
  ImpTestM era (Credential 'HotCommitteeRole)
registerCommitteeHotKey coldKey = do
  hotKey NE.:| [] <- registerCommitteeHotKeys (KeyHashObj <$> freshKeyHash) $ pure coldKey
  pure hotKey

registerCommitteeHotKeys ::
  (ShelleyEraImp era, ConwayEraTxCert era) =>
  -- | Hot Credential generator
  ImpTestM era (Credential 'HotCommitteeRole) ->
  NonEmpty (Credential 'ColdCommitteeRole) ->
  ImpTestM era (NonEmpty (Credential 'HotCommitteeRole))
registerCommitteeHotKeys genHotCred coldKeys = do
  keys <- forM coldKeys (\coldKey -> (,) coldKey <$> genHotCred)
  submitTxAnn_ "Registering Committee Hot keys" $
    mkBasicTx mkBasicTxBody
      & bodyTxL . certsTxBodyL
        .~ SSeq.fromList (map (uncurry AuthCommitteeHotKeyTxCert) (toList keys))
  pure $ fmap snd keys

-- | Submits a transaction that resigns the cold key. Prior to resignation if there was
-- hot credential authorization for this committee member it will be returned.
resignCommitteeColdKey ::
  (ShelleyEraImp era, ConwayEraTxCert era, ConwayEraCertState era) =>
  Credential 'ColdCommitteeRole ->
  StrictMaybe Anchor ->
  ImpTestM era (Maybe (Credential 'HotCommitteeRole))
resignCommitteeColdKey coldKey anchor = do
  committeAuthorizations <-
    getsNES $
      nesEsL
        . esLStateL
        . lsCertStateL
        . certVStateL
        . vsCommitteeStateL
        . csCommitteeCredsL
  submitTxAnn_ "Resigning Committee Cold key" $
    mkBasicTx mkBasicTxBody
      & bodyTxL . certsTxBodyL
        .~ SSeq.singleton (ResignCommitteeColdTxCert coldKey anchor)
  pure $ do
    CommitteeHotCredential hotCred <- Map.lookup coldKey committeAuthorizations
    pure hotCred

submitCommitteeElection ::
  forall era.
  ( HasCallStack
  , ConwayEraImp era
  ) =>
  StrictMaybe (GovPurposeId 'CommitteePurpose) ->
  Credential 'DRepRole ->
  Set.Set (Credential 'ColdCommitteeRole) ->
  Map.Map (Credential 'ColdCommitteeRole) EpochNo ->
  ImpTestM era (GovPurposeId 'CommitteePurpose)
submitCommitteeElection prevGovId drep toRemove toAdd = impAnn "Electing committee" $ do
  let
    committeeAction =
      UpdateCommittee
        prevGovId
        toRemove
        toAdd
        (1 %! 2)
  gaidCommitteeProp <- submitGovAction committeeAction
  submitYesVote_ (DRepVoter drep) gaidCommitteeProp
  pure (GovPurposeId gaidCommitteeProp)

electBasicCommittee ::
  forall era.
  ( HasCallStack
  , ConwayEraImp era
  ) =>
  ImpTestM
    era
    ( Credential 'DRepRole
    , Credential 'HotCommitteeRole
    , GovPurposeId 'CommitteePurpose
    )
electBasicCommittee = do
  logString "Setting up a DRep"
  (drep, _, _) <- setupSingleDRep 1_000_000
  (spoC, _, _) <- setupPoolWithStake $ Coin 1_000_000

  logString "Registering committee member"
  coldCommitteeC <- KeyHashObj <$> freshKeyHash
  startEpochNo <- getsNES nesELL
  let
    committeeAction =
      UpdateCommittee
        SNothing
        mempty
        (Map.singleton coldCommitteeC (addEpochInterval startEpochNo (EpochInterval 10)))
        (1 %! 2)
  (gaidCommitteeProp NE.:| _) <-
    submitGovActions
      [ committeeAction
      , UpdateCommittee SNothing mempty mempty (1 %! 10)
      ]
  submitYesVote_ (DRepVoter drep) gaidCommitteeProp
  submitYesVote_ (StakePoolVoter spoC) gaidCommitteeProp
  passNEpochs 2
  committeeMembers <- getCommitteeMembers
  impAnn "The committee should be enacted" $
    committeeMembers `shouldSatisfy` Set.member coldCommitteeC
  hotCommitteeC <- registerCommitteeHotKey coldCommitteeC
  pure (drep, hotCommitteeC, GovPurposeId gaidCommitteeProp)

logCurPParams ::
  ( EraGov era
  , ToExpr (PParamsHKD Identity era)
  , HasCallStack
  ) =>
  ImpTestM era ()
logCurPParams = do
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  logDoc $
    vsep
      [ ""
      , "----- Current PParams -----"
      , ansiExpr pp
      , "---------------------------"
      , ""
      ]

proposalsShowDebug :: Proposals era -> Bool -> Doc AnsiStyle
proposalsShowDebug ps showRoots =
  align . vsep $
    [ ""
    , "----- Proposals -----"
    , "Size"
    , viaShow $ proposalsSize ps
    , "OMap"
    , viaShow $ proposalsIds ps
    , ""
    , "Roots"
    , "> PParamUpdate"
    , viaShow $ ps ^. pRootsL . grPParamUpdateL
    , "> HardFork"
    , viaShow $ ps ^. pRootsL . grHardForkL
    , "> Committee"
    , viaShow $ ps ^. pRootsL . grCommitteeL
    , "> Constitution"
    , viaShow $ ps ^. pRootsL . grConstitutionL
    ]
      <> ( if showRoots
             then
               [ "Hierarchy"
               , ">> PParamUpdate"
               , viaShow $ ps ^. pGraphL . grPParamUpdateL . pGraphNodesL
               , ">> HardFork"
               , viaShow $ ps ^. pGraphL . grHardForkL . pGraphNodesL
               , ">> Committee"
               , viaShow $ ps ^. pGraphL . grCommitteeL . pGraphNodesL
               , ">> Constitution"
               , viaShow $ ps ^. pGraphL . grConstitutionL . pGraphNodesL
               ]
             else mempty
         )
      <> ["----- Proposals End -----"]

getProposalsForest ::
  ConwayEraGov era =>
  ImpTestM era (Forest (StrictMaybe GovActionId))
getProposalsForest = do
  ps <- getProposals
  pure
    [ Node (mkRoot grPParamUpdateL ps) $ mkForest grPParamUpdateL ps
    , Node (mkRoot grHardForkL ps) $ mkForest grHardForkL ps
    , Node (mkRoot grCommitteeL ps) $ mkForest grCommitteeL ps
    , Node (mkRoot grConstitutionL ps) $ mkForest grConstitutionL ps
    ]
  where
    mkRoot ::
      Lens' (GovRelation PRoot) (PRoot (GovPurposeId p)) ->
      Proposals era ->
      StrictMaybe GovActionId
    mkRoot rootL ps = fmap unGovPurposeId $ ps ^. pRootsL . rootL . prRootL
    mkForest ::
      (forall f. Lens' (GovRelation f) (f (GovPurposeId p))) ->
      Proposals era ->
      Forest (StrictMaybe GovActionId)
    mkForest forestL ps =
      let h = ps ^. pGraphL . forestL . pGraphNodesL
          s = toList $ proposalsIds ps
          getOrderedChildren cs = filter (`Set.member` Set.map unGovPurposeId cs) s
          go c = (SJust c, getOrderedChildren $ h Map.! GovPurposeId c ^. peChildrenL)
       in unfoldForest go (getOrderedChildren $ ps ^. pRootsL . forestL . prChildrenL)

submitGovActionTree ::
  (StrictMaybe GovActionId -> ImpTestM era GovActionId) ->
  StrictMaybe GovActionId ->
  Tree () ->
  ImpTestM era (Tree GovActionId)
submitGovActionTree submitAction p tree =
  unfoldTreeM go $ fmap (const p) tree
  where
    go (Node parent children) = do
      n <- submitAction parent
      pure (n, fmap (\(Node _child subtree) -> Node (SJust n) subtree) children)

submitGovActionForest ::
  (StrictMaybe GovActionId -> ImpTestM era GovActionId) ->
  StrictMaybe GovActionId ->
  Forest () ->
  ImpTestM era (Forest GovActionId)
submitGovActionForest submitAction p forest =
  unfoldForestM go $ fmap (fmap $ const p) forest
  where
    go (Node parent children) = do
      n <- submitAction parent
      pure (n, fmap (\(Node _child subtree) -> Node (SJust n) subtree) children)

enactConstitution ::
  forall era.
  ( ConwayEraImp era
  , HasCallStack
  ) =>
  StrictMaybe (GovPurposeId 'ConstitutionPurpose) ->
  Constitution era ->
  Credential 'DRepRole ->
  NonEmpty (Credential 'HotCommitteeRole) ->
  ImpTestM era GovActionId
enactConstitution prevGovId constitution dRep committeeMembers = impAnn "Enacting constitution" $ do
  let action = NewConstitution prevGovId constitution
  govId <- submitGovAction action
  submitYesVote_ (DRepVoter dRep) govId
  submitYesVoteCCs_ committeeMembers govId
  logRatificationChecks govId
  passNEpochs 2
  enactedConstitution <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
  enactedConstitution `shouldBe` constitution
  pure govId

expectNumDormantEpochs :: (HasCallStack, ConwayEraCertState era) => EpochNo -> ImpTestM era ()
expectNumDormantEpochs expected = do
  nd <-
    getsNES $
      nesEsL . esLStateL . lsCertStateL . certVStateL . vsNumDormantEpochsL
  nd `shouldBeExpr` expected

mkConstitutionProposal ::
  ConwayEraImp era =>
  StrictMaybe (GovPurposeId 'ConstitutionPurpose) ->
  ImpTestM era (ProposalProcedure era, Constitution era)
mkConstitutionProposal prevGovId = do
  constitution <- arbitrary
  (,constitution) <$> mkProposal (NewConstitution prevGovId constitution)

submitConstitution ::
  forall era.
  ConwayEraImp era =>
  StrictMaybe (GovPurposeId 'ConstitutionPurpose) ->
  ImpTestM era GovActionId
submitConstitution prevGovId = do
  (proposal, _) <- mkConstitutionProposal prevGovId
  submitProposal proposal

expectDRepNotRegistered ::
  (HasCallStack, ConwayEraCertState era) =>
  Credential 'DRepRole ->
  ImpTestM era ()
expectDRepNotRegistered drep = do
  dsMap <- getsNES (nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL)
  Map.lookup drep dsMap `shouldBe` Nothing

isDRepExpired ::
  (HasCallStack, ConwayEraCertState era) =>
  Credential 'DRepRole ->
  ImpTestM era Bool
isDRepExpired drep = do
  vState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL
  currentEpoch <- getsNES nesELL
  case Map.lookup drep $ vState ^. vsDRepsL of
    Nothing -> error $ unlines ["DRep not found", show drep]
    Just drep' ->
      pure $
        binOpEpochNo (+) (vState ^. vsNumDormantEpochsL) (drep' ^. drepExpiryL)
          < currentEpoch

expectDRepExpiry ::
  (HasCallStack, ConwayEraCertState era) =>
  Credential 'DRepRole ->
  EpochNo ->
  ImpTestM era ()
expectDRepExpiry drep expected = do
  dsMap <- getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL
  let ds = fromJust $ Map.lookup drep dsMap
  drepExpiry ds `shouldBe` expected

expectActualDRepExpiry ::
  (HasCallStack, ConwayEraCertState era) =>
  Credential 'DRepRole ->
  EpochNo ->
  ImpTestM era ()
expectActualDRepExpiry drep expected = do
  vState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL
  let actualDRepExpiry = fromJust $ vsActualDRepExpiry drep vState
  actualDRepExpiry `shouldBe` expected

currentProposalsShouldContain ::
  ( HasCallStack
  , ConwayEraGov era
  ) =>
  GovActionId ->
  ImpTestM era ()
currentProposalsShouldContain gai =
  currentProposalIds >>= flip shouldContain [gai] . toList

expectCurrentProposals :: (HasCallStack, ConwayEraGov era) => ImpTestM era ()
expectCurrentProposals = do
  props <- currentProposalIds
  assertBool "Expected proposals in current gov state" (not (SSeq.null props))

expectNoCurrentProposals :: (HasCallStack, ConwayEraImp era) => ImpTestM era ()
expectNoCurrentProposals = do
  proposals <- getProposals
  case proposalsActions proposals of
    Empty -> pure ()
    xs -> assertFailure $ "Expected no active proposals, but got:\n" <> show (toExpr xs)

expectPulserProposals :: (HasCallStack, ConwayEraGov era) => ImpTestM era ()
expectPulserProposals = do
  props <- lastEpochProposals
  assertBool "Expected proposals in the pulser" (not (SSeq.null props))

expectNoPulserProposals :: (HasCallStack, ConwayEraGov era) => ImpTestM era ()
expectNoPulserProposals = do
  props <- lastEpochProposals
  assertBool "Expected no proposals in the pulser" (SSeq.null props)

currentProposalIds ::
  ConwayEraGov era => ImpTestM era (SSeq.StrictSeq GovActionId)
currentProposalIds = proposalsIds <$> getsNES (newEpochStateGovStateL . proposalsGovStateL)

lastEpochProposals ::
  forall era.
  ConwayEraGov era =>
  ImpTestM era (SSeq.StrictSeq GovActionId)
lastEpochProposals =
  fmap (gasId @era) . psProposals
    <$> getsNES
      ( nesEsL
          . esLStateL
          . lsUTxOStateL
          . utxosGovStateL
          . drepPulsingStateGovStateL
          . pulsingStateSnapshotL
      )

pulsingStateSnapshotL ::
  (EraStake era, ConwayEraAccounts era) =>
  Lens' (DRepPulsingState era) (PulsingSnapshot era)
pulsingStateSnapshotL = lens getter setter
  where
    getter (DRComplete x _) = x
    getter state = fst (finishDRepPulser state)
    setter (DRComplete _ y) snap = DRComplete snap y
    setter state snap = DRComplete snap $ snd $ finishDRepPulser state

whenBootstrap :: EraGov era => ImpTestM era () -> ImpTestM era ()
whenBootstrap = whenMajorVersion @9

whenPostBootstrap :: EraGov era => ImpTestM era () -> ImpTestM era ()
whenPostBootstrap = whenMajorVersionAtLeast @10

ifBootstrap :: EraGov era => ImpTestM era a -> ImpTestM era a -> ImpTestM era a
ifBootstrap inBootstrap outOfBootstrap = do
  pv <- getProtVer
  if hardforkConwayBootstrapPhase pv then inBootstrap else outOfBootstrap

submitYesVoteCCs_ ::
  forall era f.
  (ConwayEraImp era, Foldable f) =>
  f (Credential 'HotCommitteeRole) ->
  GovActionId ->
  ImpTestM era ()
submitYesVoteCCs_ committeeMembers govId =
  mapM_ (\c -> submitYesVote_ (CommitteeVoter c) govId) committeeMembers

mkUpdateCommitteeProposal ::
  ConwayEraImp era =>
  -- | Set the parent. When Nothing is supplied latest parent will be used.
  Maybe (StrictMaybe (GovPurposeId 'CommitteePurpose)) ->
  -- | CC members to remove
  Set.Set (Credential 'ColdCommitteeRole) ->
  -- | CC members to add
  [(Credential 'ColdCommitteeRole, EpochInterval)] ->
  UnitInterval ->
  ImpTestM era (ProposalProcedure era)
mkUpdateCommitteeProposal mParent ccsToRemove ccsToAdd threshold = do
  nes <- getsNES id
  let
    curEpochNo = nes ^. nesELL
    rootCommittee = nes ^. newEpochStateGovStateL . cgsProposalsL . pRootsL . grCommitteeL
    parent = fromMaybe (prRoot rootCommittee) mParent
    newCommitteMembers =
      Map.fromList [(cc, addEpochInterval curEpochNo lifetime) | (cc, lifetime) <- ccsToAdd]
  mkProposal $ UpdateCommittee parent ccsToRemove newCommitteMembers threshold

submitUpdateCommittee ::
  ConwayEraImp era =>
  -- | Set the parent. When Nothing is supplied latest parent will be used.
  Maybe (StrictMaybe (GovPurposeId 'CommitteePurpose)) ->
  -- | CC members to remove
  Set.Set (Credential 'ColdCommitteeRole) ->
  -- | CC members to add
  [(Credential 'ColdCommitteeRole, EpochInterval)] ->
  UnitInterval ->
  ImpTestM era GovActionId
submitUpdateCommittee mParent ccsToRemove ccsToAdd threshold =
  mkUpdateCommitteeProposal mParent ccsToRemove ccsToAdd threshold >>= submitProposal

expectCommitteeMemberPresence ::
  (HasCallStack, ConwayEraGov era) => Credential 'ColdCommitteeRole -> ImpTestM era ()
expectCommitteeMemberPresence cc = do
  SJust committee <- getsNES $ newEpochStateGovStateL . committeeGovStateL
  assertBool ("Expected Committee Member: " ++ show cc ++ " to be present in the committee") $
    Map.member cc (committee ^. committeeMembersL)

expectCommitteeMemberAbsence ::
  (HasCallStack, ConwayEraGov era) => Credential 'ColdCommitteeRole -> ImpTestM era ()
expectCommitteeMemberAbsence cc = do
  SJust committee <- getsNES $ newEpochStateGovStateL . committeeGovStateL
  assertBool ("Expected Committee Member: " ++ show cc ++ " to be absent from the committee") $
    Map.notMember cc (committee ^. committeeMembersL)

donateToTreasury :: ConwayEraImp era => Coin -> ImpTestM era ()
donateToTreasury amount =
  impAnn ("Donation to treasury in the amount of: " ++ show amount) $ do
    treasuryStart <- getsNES treasuryL
    submitTx_ $ mkBasicTx (mkBasicTxBody & treasuryDonationTxBodyL .~ amount)
    treasuryEndEpoch0 <- getsNES treasuryL
    -- Actual donation happens on the epoch boundary
    treasuryStart `shouldBe` treasuryEndEpoch0
    passEpoch
    treasuryEndEpoch1 <- getsNES treasuryL
    treasuryEndEpoch1 <-> treasuryStart `shouldBe` amount

expectMembers ::
  (HasCallStack, ConwayEraGov era) =>
  Set.Set (Credential 'ColdCommitteeRole) ->
  ImpTestM era ()
expectMembers expKhs = do
  committee <- getsNES $ newEpochStateGovStateL . committeeGovStateL
  let members = Map.keysSet $ foldMap' committeeMembers committee
  impAnn "Expecting committee members" $ members `shouldBe` expKhs

showConwayTxBalance ::
  ( EraUTxO era
  , ConwayEraTxBody era
  , ConwayEraCertState era
  ) =>
  PParams era ->
  CertState era ->
  UTxO era ->
  Tx era ->
  String
showConwayTxBalance pp certState utxo tx =
  unlines
    [ "Consumed:"
    , "\tInputs:     \t" <> show (coin inputs)
    , "\tRefunds:    \t" <> show refunds
    , "\tWithdrawals \t" <> show withdrawals
    , "\tTotal:      \t" <> (show . coin $ consumed pp certState utxo txBody)
    , ""
    , "Produced:"
    , "\tOutputs:   \t" <> show (coin $ sumAllValue (txBody ^. outputsTxBodyL))
    , "\tDonations: \t" <> show (txBody ^. treasuryDonationTxBodyL)
    , "\tDeposits:  \t" <> show (getTotalDepositsTxBody pp isRegPoolId txBody)
    , "\tFees:      \t" <> show (txBody ^. feeTxBodyL)
    , "\tTotal:     \t" <> (show . coin $ produced pp certState txBody)
    ]
  where
    txBody = tx ^. bodyTxL
    inputs = sumUTxO (txInsFilter utxo (txBody ^. inputsTxBodyL))
    refunds =
      getTotalRefundsTxBody
        pp
        (lookupDepositDState $ certState ^. certDStateL)
        (lookupDepositVState $ certState ^. certVStateL)
        txBody
    isRegPoolId = (`Map.member` (certState ^. certPStateL . psStakePoolParamsL))
    withdrawals = fold . unWithdrawals $ txBody ^. withdrawalsTxBodyL

logConwayTxBalance ::
  ( EraUTxO era
  , EraGov era
  , ConwayEraTxBody era
  , ConwayEraCertState era
  ) =>
  Tx era ->
  ImpTestM era ()
logConwayTxBalance tx = do
  pp <- getsPParams id
  certState <- getsNES $ nesEsL . esLStateL . lsCertStateL
  utxo <- getsNES utxoL
  logString $ showConwayTxBalance pp certState utxo tx

submitBootstrapAwareFailingVote ::
  ConwayEraImp era =>
  Vote ->
  Voter ->
  GovActionId ->
  SubmitFailureExpectation era ->
  ImpTestM era ()
submitBootstrapAwareFailingVote vote voter gaId =
  submitBootstrapAware
    (submitVote_ vote voter gaId)
    (submitFailingVote voter gaId)

submitBootstrapAwareFailingProposal ::
  ConwayEraImp era =>
  ProposalProcedure era ->
  SubmitFailureExpectation era ->
  ImpTestM era (Maybe GovActionId)
submitBootstrapAwareFailingProposal proposal =
  submitBootstrapAware
    (Just <$> submitProposal proposal)
    ((Nothing <$) . submitFailingProposal proposal)

submitBootstrapAwareFailingProposal_ ::
  ConwayEraImp era =>
  ProposalProcedure era ->
  SubmitFailureExpectation era ->
  ImpTestM era ()
submitBootstrapAwareFailingProposal_ p = void . submitBootstrapAwareFailingProposal p

data SubmitFailureExpectation era
  = FailBootstrap (NE.NonEmpty (PredicateFailure (EraRule "LEDGER" era)))
  | FailPostBootstrap (NE.NonEmpty (PredicateFailure (EraRule "LEDGER" era)))
  | FailBootstrapAndPostBootstrap (FailBoth era)

data FailBoth era = FailBoth
  { bootstrapFailures :: NE.NonEmpty (PredicateFailure (EraRule "LEDGER" era))
  , postBootstrapFailures :: NE.NonEmpty (PredicateFailure (EraRule "LEDGER" era))
  }

submitBootstrapAware ::
  EraGov era =>
  ImpTestM era a ->
  (NE.NonEmpty (PredicateFailure (EraRule "LEDGER" era)) -> ImpTestM era a) ->
  SubmitFailureExpectation era ->
  ImpTestM era a
submitBootstrapAware action failAction =
  \case
    FailBootstrap failures ->
      ifBootstrap
        (failAction failures)
        action
    FailPostBootstrap failures ->
      ifBootstrap
        action
        (failAction failures)
    FailBootstrapAndPostBootstrap (FailBoth bFailures pBFailures) ->
      ifBootstrap
        (failAction bFailures)
        (failAction pBFailures)

delegateSPORewardAddressToDRep_ ::
  ConwayEraImp era =>
  KeyHash 'StakePool ->
  Coin ->
  DRep ->
  ImpTestM era ()
delegateSPORewardAddressToDRep_ kh stake drep = do
  pp <- getRatifyEnv >>= expectJust . Map.lookup kh . rePoolParams
  void $
    delegateToDRep
      (raCredential . ppRewardAccount $ pp)
      stake
      drep

-- Partial implementation used for checking predicate failures
instance InjectRuleFailure "LEDGER" ShelleyDelegPredFailure ConwayEra where
  injectFailure = ConwayCertsFailure . injectFailure

instance InjectRuleFailure "CERTS" ShelleyDelegPredFailure ConwayEra where
  injectFailure = CertFailure . injectFailure

instance InjectRuleFailure "CERT" ShelleyDelegPredFailure ConwayEra where
  injectFailure = DelegFailure . injectFailure

instance InjectRuleFailure "DELEG" ShelleyDelegPredFailure ConwayEra where
  injectFailure (Shelley.StakeKeyAlreadyRegisteredDELEG c) = StakeKeyRegisteredDELEG c
  injectFailure (Shelley.StakeKeyNotRegisteredDELEG c) = StakeKeyNotRegisteredDELEG c
  injectFailure (Shelley.StakeKeyNonZeroAccountBalanceDELEG c) = StakeKeyHasNonZeroRewardAccountBalanceDELEG c
  injectFailure _ = error "Cannot inject ShelleyDelegPredFailure into ConwayEra"

getCommittee :: ConwayEraGov era => ImpTestM era (StrictMaybe (Committee era))
getCommittee = getsNES $ nesEsL . epochStateGovStateL . committeeGovStateL
