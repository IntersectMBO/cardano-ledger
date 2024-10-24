{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
  submitTreasuryWithdrawals,
  submitVote,
  submitVote_,
  submitYesVote_,
  submitFailingVote,
  trySubmitVote,
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
  electCommittee,
  electBasicCommittee,
  proposalsShowDebug,
  getGovPolicy,
  submitFailingGovAction,
  submitGovActionForest,
  submitGovActionTree,
  getProposalsForest,
  logProposalsForest,
  logProposalsForestDiff,
  constitutionShouldBe,
  getCCExpiry,
  ccShouldBeExpired,
  ccShouldNotBeExpired,
  ccShouldBeResigned,
  ccShouldNotBeResigned,
  getLastEnactedCommittee,
  getLastEnactedConstitution,
  submitParameterChange,
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
  minorFollow,
  majorFollow,
  cantFollow,
  getsPParams,
  currentProposalsShouldContain,
  withImpStateWithProtVer,
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
) where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), Ed25519DSIGN, Signable)
import Cardano.Crypto.Hash.Blake2b (Blake2b_224)
import Cardano.Crypto.Hash.Class (Hash)
import Cardano.Ledger.Address (Addr (..), RewardAccount (..))
import Cardano.Ledger.Allegra.Scripts (Timelock)
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript)
import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  EpochNo (..),
  Network (..),
  ProtVer (..),
  ShelleyBase,
  StrictMaybe (..),
  UnitInterval,
  Version,
  addEpochInterval,
  binOpEpochNo,
  hashAnchorData,
  inject,
  succVersion,
  textToUrl,
 )
import Cardano.Ledger.CertState (
  CertState,
  CommitteeAuthorization (..),
  certPStateL,
  csCommitteeCredsL,
  psStakePoolParamsL,
  vsActualDRepExpiry,
  vsNumDormantEpochsL,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core hiding (proposals)
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams (UpgradeConwayPParams (..))
import Cardano.Ledger.Conway.Rules (
  ConwayGovEvent,
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
import Cardano.Ledger.Conway.Tx (AlonzoTx)
import Cardano.Ledger.Conway.TxCert (Delegatee (..))
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.DRep
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Plutus.Language (Language (..), SLanguage (..))
import qualified Cardano.Ledger.Shelley.HardForks as HardForks (bootstrapPhase)
import Cardano.Ledger.Shelley.LedgerState (
  IncrementalStake (..),
  asTreasuryL,
  certVStateL,
  consumed,
  curPParamsEpochStateL,
  epochStateGovStateL,
  epochStatePoolParamsL,
  esAccountStateL,
  esLStateL,
  lsCertStateL,
  lsUTxOStateL,
  nesELL,
  nesEpochStateL,
  nesEsL,
  nesPdL,
  newEpochStateGovStateL,
  produced,
  unifiedL,
  utxosGovStateL,
  utxosStakeDistrL,
  utxosUtxoL,
  vsCommitteeStateL,
  vsDRepsL,
 )
import Cardano.Ledger.TxIn (TxId (..))
import Cardano.Ledger.UMap (dRepMap)
import Cardano.Ledger.UTxO (EraUTxO, UTxO, balance, sumAllValue, txInsFilter)
import Cardano.Ledger.Val (Val (..), (<->))
import Control.Monad (forM)
import Control.Monad.Trans.Fail.String (errorFail)
import Control.State.Transition.Extended (STS (..))
import Data.Bifunctor (bimap)
import Data.Default.Class (Default (..))
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
import Test.Cardano.Ledger.Conway.TreeDiff (tableDoc)
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkCred)
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus (testingCostModel)

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

withImpStateWithProtVer ::
  forall era.
  ConwayEraImp era =>
  Version ->
  SpecWith (ImpTestState era) ->
  Spec
withImpStateWithProtVer ver = do
  withImpStateModified $
    impNESL
      . nesEsL
      . esLStateL
      . lsUTxOStateL
      . utxosGovStateL
      . cgsCurPParamsL
      . ppProtocolVersionL
      .~ ProtVer ver 0

instance
  ( Crypto c
  , NFData (SigDSIGN (DSIGN c))
  , NFData (VerKeyDSIGN (DSIGN c))
  , ADDRHASH c ~ Blake2b_224
  , DSIGN c ~ Ed25519DSIGN
  , Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)
  , Eq (ConwayGovEvent (ConwayEra c))
  ) =>
  ShelleyEraImp (ConwayEra c)
  where
  initGenesis = do
    kh1 <- freshKeyHash
    kh2 <- freshKeyHash
    let
      ccExpiryEpochNo = addEpochInterval (impEraStartEpochNo @(ConwayEra c)) (EpochInterval 15)
      committee = Committee [(KeyHashObj kh1, ccExpiryEpochNo), (KeyHashObj kh2, ccExpiryEpochNo)] (1 %! 1)
      constitutionAnchor =
        Anchor
          { anchorUrl = errorFail $ textToUrl 128 "https://cardano-constitution.crypto"
          , anchorDataHash = hashAnchorData (AnchorData "Cardano Constitution Content")
          }
    pure
      ConwayGenesis
        { cgUpgradePParams =
            UpgradeConwayPParams
              { ucppPoolVotingThresholds =
                  PoolVotingThresholds
                    { pvtMotionNoConfidence = 51 %! 100
                    , pvtCommitteeNormal = 51 %! 100
                    , pvtCommitteeNoConfidence = 51 %! 100
                    , pvtHardForkInitiation = 51 %! 100
                    , pvtPPSecurityGroup = 51 %! 100
                    }
              , ucppDRepVotingThresholds =
                  DRepVotingThresholds
                    { dvtMotionNoConfidence = 51 %! 100
                    , dvtCommitteeNormal = 51 %! 100
                    , dvtCommitteeNoConfidence = 51 %! 100
                    , dvtUpdateToConstitution = 51 %! 100
                    , dvtHardForkInitiation = 51 %! 100
                    , dvtPPNetworkGroup = 51 %! 100
                    , dvtPPEconomicGroup = 51 %! 100
                    , dvtPPTechnicalGroup = 51 %! 100
                    , dvtPPGovGroup = 51 %! 100
                    , dvtTreasuryWithdrawal = 51 %! 100
                    }
              , ucppCommitteeMinSize = 1
              , ucppCommitteeMaxTermLength = EpochInterval 20
              , ucppGovActionLifetime = EpochInterval 30
              , ucppGovActionDeposit = Coin 123
              , ucppDRepDeposit = Coin 70_000_000
              , ucppDRepActivity = EpochInterval 100
              , ucppMinFeeRefScriptCostPerByte = 15 %! 1
              , -- TODO: Replace with correct cost model.
                ucppPlutusV3CostModel = testingCostModel PlutusV3
              }
        , cgConstitution = Constitution constitutionAnchor SNothing
        , cgCommittee = committee
        , cgDelegs = mempty
        , cgInitialDReps = mempty
        }

  impSatisfyNativeScript = impAllegraSatisfyNativeScript

  modifyPParams = conwayModifyPParams

  fixupTx = alonzoFixupTx

instance
  ( Crypto c
  , NFData (SigDSIGN (DSIGN c))
  , NFData (VerKeyDSIGN (DSIGN c))
  , ADDRHASH c ~ Blake2b_224
  , DSIGN c ~ Ed25519DSIGN
  , Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)
  ) =>
  MaryEraImp (ConwayEra c)

instance ShelleyEraImp (ConwayEra c) => AlonzoEraImp (ConwayEra c) where
  scriptTestContexts =
    plutusTestScripts SPlutusV1
      <> plutusTestScripts SPlutusV2
      <> plutusTestScripts SPlutusV3

class
  ( AlonzoEraImp era
  , ConwayEraGov era
  , ConwayEraTxBody era
  , ConwayEraTxCert era
  , ConwayEraPParams era
  , STS (EraRule "ENACT" era)
  , BaseM (EraRule "ENACT" era) ~ ShelleyBase
  , State (EraRule "ENACT" era) ~ EnactState era
  , Signal (EraRule "ENACT" era) ~ EnactSignal era
  , Environment (EraRule "ENACT" era) ~ ()
  , NativeScript era ~ Timelock era
  , Script era ~ AlonzoScript era
  , GovState era ~ ConwayGovState era
  ) =>
  ConwayEraImp era

instance
  ( Crypto c
  , NFData (SigDSIGN (DSIGN c))
  , NFData (VerKeyDSIGN (DSIGN c))
  , ADDRHASH c ~ Blake2b_224
  , DSIGN c ~ Ed25519DSIGN
  , Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)
  ) =>
  ConwayEraImp (ConwayEra c)

registerInitialCommittee ::
  (HasCallStack, ConwayEraImp era) =>
  ImpTestM era (NonEmpty (Credential 'HotCommitteeRole (EraCrypto era)))
registerInitialCommittee = do
  committeeMembers <- Set.toList <$> getCommitteeMembers
  case committeeMembers of
    x : xs -> registerCommitteeHotKeys (KeyHashObj <$> freshKeyHash) $ x NE.:| xs
    [] -> error "Expected an initial committee"

-- | Submit a transaction that registers a new DRep and return the keyhash
-- belonging to that DRep
registerDRep :: ConwayEraImp era => ImpTestM era (KeyHash 'DRepRole (EraCrypto era))
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
  ) =>
  Credential 'DRepRole (EraCrypto era) ->
  ImpTestM era ()
unRegisterDRep drep = do
  drepState <- lookupDRepState drep
  let refund = drepDeposit drepState
  submitTxAnn_ "UnRegister DRep" $
    mkBasicTx mkBasicTxBody
      & bodyTxL . certsTxBodyL
        .~ SSeq.singleton (UnRegDRepTxCert drep refund)

-- | Submit a transaction that updates a given DRep
updateDRep ::
  forall era.
  ( ShelleyEraImp era
  , ConwayEraTxCert era
  ) =>
  Credential 'DRepRole (EraCrypto era) ->
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
  ImpTestM
    era
    ( KeyHash 'DRepRole (EraCrypto era)
    , KeyHash 'Staking (EraCrypto era)
    )
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
  ImpTestM
    era
    ( Credential 'DRepRole (EraCrypto era)
    , Credential 'Staking (EraCrypto era)
    , KeyPair 'Payment (EraCrypto era)
    )
setupSingleDRep stake = do
  drepKH <- registerDRep
  delegatorKH <- freshKeyHash
  deposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
  let tx =
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ SSeq.fromList
              [ RegDepositTxCert
                  (KeyHashObj delegatorKH)
                  deposit
              ]
  submitTx_ tx
  spendingKP <-
    delegateToDRep (KeyHashObj delegatorKH) (Coin stake) (DRepCredential (KeyHashObj drepKH))
  pure (KeyHashObj drepKH, KeyHashObj delegatorKH, spendingKP)

delegateToDRep ::
  ConwayEraImp era =>
  Credential 'Staking (EraCrypto era) ->
  Coin ->
  DRep (EraCrypto era) ->
  ImpTestM
    era
    (KeyPair 'Payment (EraCrypto era))
delegateToDRep cred stake dRep = do
  (_, spendingKP) <- freshKeyPair
  let addr = Addr Testnet (mkCred spendingKP) (StakeRefBase cred)
  submitTxAnn_ "Delegate to DRep" $
    mkBasicTx mkBasicTxBody
      & bodyTxL . outputsTxBodyL
        .~ SSeq.singleton
          ( mkBasicTxOut
              addr
              (inject stake)
          )
      & bodyTxL . certsTxBodyL
        .~ SSeq.fromList
          [ DelegTxCert
              cred
              (DelegVote dRep)
          ]
  pure spendingKP

lookupDRepState ::
  HasCallStack =>
  Credential 'DRepRole (EraCrypto era) ->
  ImpTestM era (DRepState (EraCrypto era))
lookupDRepState dRepCred = do
  drepsState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL
  case Map.lookup dRepCred drepsState of
    Nothing -> error $ "Expected for DRep " ++ show dRepCred ++ " to be present in the CertState"
    Just state -> pure state

getsPParams :: EraGov era => Lens' (PParams era) a -> ImpTestM era a
getsPParams f = getsNES $ nesEsL . curPParamsEpochStateL . f

-- | Sets up a stake pool with coin delegated to it.
--
-- NOTE: This uses the `RegDepositDelegTxCert` for delegating, so it has to be
-- in Conway. The Shelley version of this function would have to separately
-- register the staking credential and then delegate it.
setupPoolWithStake ::
  (ShelleyEraImp era, ConwayEraTxCert era) =>
  Coin ->
  ImpTestM
    era
    ( KeyHash 'StakePool (EraCrypto era)
    , Credential 'Payment (EraCrypto era)
    , Credential 'Staking (EraCrypto era)
    )
setupPoolWithStake delegCoin = do
  khPool <- freshKeyHash
  registerPool khPool
  credDelegatorPayment <- KeyHashObj <$> freshKeyHash
  credDelegatorStaking <- KeyHashObj <$> freshKeyHash
  void $
    sendCoinTo
      (Addr Testnet credDelegatorPayment (StakeRefBase credDelegatorStaking))
      delegCoin
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
  ImpTestM
    era
    ( KeyHash 'StakePool (EraCrypto era)
    , Credential 'Staking (EraCrypto era)
    )
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
  Voter (EraCrypto era) ->
  GovActionId (EraCrypto era) ->
  ImpTestM era (TxId (EraCrypto era))
submitVote vote voter gaId = trySubmitVote vote voter gaId >>= expectRightDeep

-- | Submits a transaction that votes "Yes" for the given governance action as
-- some voter
submitYesVote_ ::
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  , HasCallStack
  ) =>
  Voter (EraCrypto era) ->
  GovActionId (EraCrypto era) ->
  ImpTestM era ()
submitYesVote_ voter gaId = void $ submitVote VoteYes voter gaId

submitVote_ ::
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  , HasCallStack
  ) =>
  Vote ->
  Voter (EraCrypto era) ->
  GovActionId (EraCrypto era) ->
  ImpTestM era ()
submitVote_ vote voter gaId = void $ submitVote vote voter gaId

submitFailingVote ::
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  , HasCallStack
  ) =>
  Voter (EraCrypto era) ->
  GovActionId (EraCrypto era) ->
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
  Voter (EraCrypto era) ->
  GovActionId (EraCrypto era) ->
  ImpTestM
    era
    ( Either
        (NonEmpty (PredicateFailure (EraRule "LEDGER" era)))
        (TxId (EraCrypto era))
    )
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
  ImpTestM era (GovActionId (EraCrypto era))
submitProposal proposal = trySubmitProposal proposal >>= expectRightExpr

submitProposals ::
  (ShelleyEraImp era, ConwayEraGov era, ConwayEraTxBody era, HasCallStack) =>
  NE.NonEmpty (ProposalProcedure era) ->
  ImpTestM era (NE.NonEmpty (GovActionId (EraCrypto era)))
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
  ImpTestM
    era
    ( Either
        (NonEmpty (PredicateFailure (EraRule "LEDGER" era)))
        (GovActionId (EraCrypto era))
    )
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
  ImpTestM
    era
    ( Either
        (NonEmpty (PredicateFailure (EraRule "LEDGER" era)))
        (GovActionId (EraCrypto era))
    )
trySubmitGovAction ga = do
  let mkGovActionId tx = GovActionId (txIdTx tx) (GovActionIx 0)
  bimap fst mkGovActionId <$> trySubmitGovActions (pure ga)

submitAndExpireProposalToMakeReward ::
  ConwayEraImp era =>
  Credential 'Staking (EraCrypto era) ->
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
  RewardAccount (EraCrypto era) ->
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
  ImpTestM era (GovActionId (EraCrypto era))
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
  ImpTestM era (NE.NonEmpty (GovActionId (EraCrypto era)))
submitGovActions gas = do
  tx <- trySubmitGovActions gas >>= expectRightExpr
  let txId = txIdTx tx
  pure $ NE.zipWith (\idx _ -> GovActionId txId (GovActionIx idx)) (0 NE.:| [1 ..]) gas

submitTreasuryWithdrawals ::
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  , ConwayEraGov era
  ) =>
  [(RewardAccount (EraCrypto era), Coin)] ->
  ImpTestM era (GovActionId (EraCrypto era))
submitTreasuryWithdrawals wdrls = do
  policy <- getGovPolicy
  submitGovAction $ TreasuryWithdrawals (Map.fromList wdrls) policy

enactTreasuryWithdrawals ::
  ConwayEraImp era =>
  [(RewardAccount (EraCrypto era), Coin)] ->
  Credential 'DRepRole (EraCrypto era) ->
  NonEmpty (Credential 'HotCommitteeRole (EraCrypto era)) ->
  ImpTestM era (GovActionId (EraCrypto era))
enactTreasuryWithdrawals withdrawals dRep cms = do
  gaId <- submitTreasuryWithdrawals withdrawals
  submitYesVote_ (DRepVoter dRep) gaId
  submitYesVoteCCs_ cms gaId
  passNEpochs 2
  pure gaId

submitParameterChange ::
  ConwayEraImp era =>
  StrictMaybe (GovActionId (EraCrypto era)) ->
  PParamsUpdate era ->
  ImpTestM era (GovActionId (EraCrypto era))
submitParameterChange parent ppu = do
  policy <- getGovPolicy
  submitGovAction $ ParameterChange (GovPurposeId <$> parent) ppu policy

getGovPolicy :: ConwayEraGov era => ImpTestM era (StrictMaybe (ScriptHash (EraCrypto era)))
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
  ImpTestM era (Set.Set (Credential 'ColdCommitteeRole (EraCrypto era)))
getCommitteeMembers = do
  committee <- getsNES $ newEpochStateGovStateL . committeeGovStateL
  pure $ Map.keysSet $ foldMap' committeeMembers committee

getLastEnactedCommittee ::
  ConwayEraGov era => ImpTestM era (StrictMaybe (GovPurposeId 'CommitteePurpose era))
getLastEnactedCommittee = do
  ps <- getProposals
  pure $ ps ^. pRootsL . grCommitteeL . prRootL

getConstitution ::
  ConwayEraImp era =>
  ImpTestM era (Constitution era)
getConstitution = getsNES $ newEpochStateGovStateL . constitutionGovStateL

getLastEnactedConstitution ::
  ConwayEraGov era => ImpTestM era (StrictMaybe (GovPurposeId 'ConstitutionPurpose era))
getLastEnactedConstitution = do
  ps <- getProposals
  pure $ ps ^. pRootsL . grConstitutionL . prRootL

getLastEnactedParameterChange ::
  ConwayEraGov era => ImpTestM era (StrictMaybe (GovPurposeId 'PParamUpdatePurpose era))
getLastEnactedParameterChange = do
  ps <- getProposals
  pure $ ps ^. pRootsL . grPParamUpdateL . prRootL

getLastEnactedHardForkInitiation ::
  ConwayEraGov era => ImpTestM era (StrictMaybe (GovPurposeId 'HardForkPurpose era))
getLastEnactedHardForkInitiation = do
  ps <- getProposals
  pure $ ps ^. pRootsL . grHardForkL . prRootL

getConstitutionProposals ::
  ConwayEraGov era =>
  ImpTestM
    era
    ( Map.Map
        (GovPurposeId 'ConstitutionPurpose era)
        (PEdges (GovPurposeId 'ConstitutionPurpose era))
    )
getConstitutionProposals = do
  ps <- getProposals
  pure $ ps ^. pGraphL . grConstitutionL . pGraphNodesL

getParameterChangeProposals ::
  ConwayEraGov era =>
  ImpTestM
    era
    ( Map.Map
        (GovPurposeId 'PParamUpdatePurpose era)
        (PEdges (GovPurposeId 'PParamUpdatePurpose era))
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
  GovActionId (EraCrypto era) ->
  ImpTestM era (Maybe (GovActionState era))
lookupGovActionState aId = proposalsLookupId aId <$> getProposals

-- | Looks up the governance action state corresponding to the governance action id
getGovActionState ::
  (HasCallStack, ConwayEraGov era) =>
  GovActionId (EraCrypto era) ->
  ImpTestM era (GovActionState era)
getGovActionState govActionId =
  impAnn "Expecting an action state" $ do
    lookupGovActionState govActionId >>= \case
      Nothing ->
        assertFailure $ "Could not find action state for govActionId: " <> show govActionId
      Just govActionState -> pure govActionState

expectPresentGovActionId ::
  (HasCallStack, ConwayEraGov era) =>
  GovActionId (EraCrypto era) ->
  ImpTestM era ()
expectPresentGovActionId govActionId = void $ getGovActionState govActionId

expectMissingGovActionId ::
  (HasCallStack, ConwayEraGov era) =>
  GovActionId (EraCrypto era) ->
  ImpTestM era ()
expectMissingGovActionId govActionId =
  impAnn "Expecting for gov action state to be missing" $ do
    lookupGovActionState govActionId >>= \case
      Just _ ->
        expectationFailure $ "Found gov action state for govActionId: " <> ansiExprString govActionId
      Nothing -> pure ()

-- | Builds a RatifyEnv from the current state
getRatifyEnv :: ConwayEraGov era => ImpTestM era (RatifyEnv era)
getRatifyEnv = do
  eNo <- getsNES nesELL
  stakeDistr <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosStakeDistrL
  poolDistr <- getsNES nesPdL
  drepDistr <- getsNES $ nesEsL . epochStateDRepPulsingStateL . psDRepDistrG
  drepState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL
  committeeState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL
  umap <- getsNES $ unifiedL
  poolPs <- getsNES $ nesEsL . epochStatePoolParamsL
  pure
    RatifyEnv
      { reStakePoolDistr = poolDistr
      , reStakeDistr = credMap stakeDistr
      , reDRepState = drepState
      , reDRepDistr = drepDistr
      , reCurrentEpoch = eNo - 1
      , reCommitteeState = committeeState
      , reDelegatees = dRepMap umap
      , rePoolParams = poolPs
      }

ccShouldNotBeExpired ::
  (HasCallStack, ConwayEraGov era) =>
  Credential 'ColdCommitteeRole (EraCrypto era) ->
  ImpTestM era ()
ccShouldNotBeExpired coldC = do
  curEpochNo <- getsNES nesELL
  ccExpiryEpochNo <- getCCExpiry coldC
  curEpochNo `shouldSatisfy` (<= ccExpiryEpochNo)

ccShouldBeExpired ::
  (HasCallStack, ConwayEraGov era) =>
  Credential 'ColdCommitteeRole (EraCrypto era) ->
  ImpTestM era ()
ccShouldBeExpired coldC = do
  curEpochNo <- getsNES nesELL
  ccExpiryEpochNo <- getCCExpiry coldC
  curEpochNo `shouldSatisfy` (> ccExpiryEpochNo)

getCCExpiry ::
  (HasCallStack, ConwayEraGov era) =>
  Credential 'ColdCommitteeRole (EraCrypto era) ->
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
  HasCallStack => Credential 'ColdCommitteeRole (EraCrypto era) -> ImpTestM era ()
ccShouldBeResigned coldK = do
  committeeCreds <-
    getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL . csCommitteeCredsL
  authHk <$> Map.lookup coldK committeeCreds `shouldBe` Just Nothing

-- | Test the resignation status for a CC cold key to not be resigned
ccShouldNotBeResigned ::
  HasCallStack => Credential 'ColdCommitteeRole (EraCrypto era) -> ImpTestM era ()
ccShouldNotBeResigned coldK = do
  committeeCreds <-
    getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL . csCommitteeCredsL
  (Map.lookup coldK committeeCreds >>= authHk) `shouldSatisfy` isJust

authHk :: CommitteeAuthorization c -> Maybe (Credential 'HotCommitteeRole c)
authHk (CommitteeHotCredential hk) = Just hk
authHk _ = Nothing

-- | Calculates the ratio of DReps that have voted for the governance action
calculateDRepAcceptedRatio ::
  forall era.
  (HasCallStack, ConwayEraGov era) =>
  GovActionId (EraCrypto era) ->
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
  (HasCallStack, ConwayEraGov era) =>
  GovActionId (EraCrypto era) ->
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
  ConwayEraGov era => GovActionId (EraCrypto era) -> ImpTestM era Rational
calculatePoolAcceptedRatio gaId = do
  ratEnv <- getRatifyEnv
  gas <- getGovActionState gaId
  pv <- getProtVer
  pure $ spoAcceptedRatio ratEnv gas pv

-- | Logs the ratios of accepted votes per category
logAcceptedRatio ::
  (HasCallStack, ConwayEraGov era) => GovActionId (EraCrypto era) -> ImpTestM era ()
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

getRatifyEnvAndState :: ConwayEraGov era => ImpTestM era (RatifyEnv era, RatifyState era)
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
  (HasCallStack, ConwayEraGov era, ConwayEraPParams era) =>
  GovActionId (EraCrypto era) ->
  ImpTestM era Bool
isDRepAccepted gaId = do
  (ratifyEnv, ratifyState) <- getRatifyEnvAndState
  action <- getGovActionState gaId
  pure $ dRepAccepted ratifyEnv ratifyState action

isSpoAccepted ::
  (HasCallStack, ConwayEraGov era, ConwayEraPParams era) =>
  GovActionId (EraCrypto era) ->
  ImpTestM era Bool
isSpoAccepted gaId = do
  (ratifyEnv, ratifyState) <- getRatifyEnvAndState
  action <- getGovActionState gaId
  pure $ spoAccepted ratifyEnv ratifyState action

isCommitteeAccepted ::
  (HasCallStack, ConwayEraGov era, ConwayEraPParams era) =>
  GovActionId (EraCrypto era) ->
  ImpTestM era Bool
isCommitteeAccepted gaId = do
  (ratifyEnv, ratifyState) <- getRatifyEnvAndState
  action <- getGovActionState gaId
  pure $ committeeAccepted ratifyEnv ratifyState action

-- | Logs the results of each check required to make the governance action pass
logRatificationChecks ::
  (ConwayEraGov era, ConwayEraPParams era, HasCallStack) =>
  GovActionId (EraCrypto era) ->
  ImpTestM era ()
logRatificationChecks gaId = do
  gas@GovActionState {gasCommitteeVotes, gasDRepVotes} <- getGovActionState gaId
  let govAction = gasAction gas
  ens@EnactState {..} <- getEnactState
  committee <- getsNES $ nesEsL . epochStateGovStateL . committeeGovStateL
  ratEnv@RatifyEnv {reCurrentEpoch} <- getRatifyEnv
  let ratSt = RatifyState ens mempty mempty False
  curTreasury <- getsNES $ nesEsL . esAccountStateL . asTreasuryL
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
  Credential 'ColdCommitteeRole (EraCrypto era) ->
  ImpTestM era (Credential 'HotCommitteeRole (EraCrypto era))
registerCommitteeHotKey coldKey = do
  hotKey NE.:| [] <- registerCommitteeHotKeys (KeyHashObj <$> freshKeyHash) $ pure coldKey
  pure hotKey

registerCommitteeHotKeys ::
  (ShelleyEraImp era, ConwayEraTxCert era) =>
  -- | Hot Credential generator
  ImpTestM era (Credential 'HotCommitteeRole (EraCrypto era)) ->
  NonEmpty (Credential 'ColdCommitteeRole (EraCrypto era)) ->
  ImpTestM era (NonEmpty (Credential 'HotCommitteeRole (EraCrypto era)))
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
  (ShelleyEraImp era, ConwayEraTxCert era) =>
  Credential 'ColdCommitteeRole (EraCrypto era) ->
  StrictMaybe (Anchor (EraCrypto era)) ->
  ImpTestM era (Maybe (Credential 'HotCommitteeRole (EraCrypto era)))
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

electCommittee ::
  forall era.
  ( HasCallStack
  , ConwayEraImp era
  ) =>
  StrictMaybe (GovPurposeId 'CommitteePurpose era) ->
  Credential 'DRepRole (EraCrypto era) ->
  Set.Set (Credential 'ColdCommitteeRole (EraCrypto era)) ->
  Map.Map (Credential 'ColdCommitteeRole (EraCrypto era)) EpochNo ->
  ImpTestM era (GovPurposeId 'CommitteePurpose era)
electCommittee prevGovId drep toRemove toAdd = impAnn "Electing committee" $ do
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
    ( Credential 'DRepRole (EraCrypto era)
    , Credential 'HotCommitteeRole (EraCrypto era)
    , GovPurposeId 'CommitteePurpose era
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

proposalsShowDebug :: Era era => Proposals era -> Bool -> Doc AnsiStyle
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
  ImpTestM era (Forest (StrictMaybe (GovActionId (EraCrypto era))))
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
      Lens' (GovRelation PRoot era) (PRoot (GovPurposeId p era)) ->
      Proposals era ->
      StrictMaybe (GovActionId (EraCrypto era))
    mkRoot rootL ps = fmap unGovPurposeId $ ps ^. pRootsL . rootL . prRootL
    mkForest ::
      (forall f. Lens' (GovRelation f era) (f (GovPurposeId p era))) ->
      Proposals era ->
      Forest (StrictMaybe (GovActionId (EraCrypto era)))
    mkForest forestL ps =
      let h = ps ^. pGraphL . forestL . pGraphNodesL
          s = toList $ proposalsIds ps
          getOrderedChildren cs = filter (`Set.member` Set.map unGovPurposeId cs) s
          go c = (SJust c, getOrderedChildren $ h Map.! GovPurposeId c ^. peChildrenL)
       in unfoldForest go (getOrderedChildren $ ps ^. pRootsL . forestL . prChildrenL)

submitGovActionTree ::
  (StrictMaybe (GovActionId (EraCrypto era)) -> ImpTestM era (GovActionId (EraCrypto era))) ->
  StrictMaybe (GovActionId (EraCrypto era)) ->
  Tree () ->
  ImpTestM era (Tree (GovActionId (EraCrypto era)))
submitGovActionTree submitAction p tree =
  unfoldTreeM go $ fmap (const p) tree
  where
    go (Node parent children) = do
      n <- submitAction parent
      pure (n, fmap (\(Node _child subtree) -> Node (SJust n) subtree) children)

submitGovActionForest ::
  (StrictMaybe (GovActionId (EraCrypto era)) -> ImpTestM era (GovActionId (EraCrypto era))) ->
  StrictMaybe (GovActionId (EraCrypto era)) ->
  Forest () ->
  ImpTestM era (Forest (GovActionId (EraCrypto era)))
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
  StrictMaybe (GovPurposeId 'ConstitutionPurpose era) ->
  Constitution era ->
  Credential 'DRepRole (EraCrypto era) ->
  NonEmpty (Credential 'HotCommitteeRole (EraCrypto era)) ->
  ImpTestM era (GovActionId (EraCrypto era))
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

-- | Asserts that the URL of the current constitution is equal to the given
-- string
constitutionShouldBe :: (HasCallStack, ConwayEraGov era) => String -> ImpTestM era ()
constitutionShouldBe cUrl = do
  Constitution {constitutionAnchor = Anchor {anchorUrl}} <-
    getsNES $ newEpochStateGovStateL . constitutionGovStateL
  anchorUrl `shouldBe` errorFail (textToUrl 128 $ T.pack cUrl)

expectNumDormantEpochs :: HasCallStack => EpochNo -> ImpTestM era ()
expectNumDormantEpochs expected = do
  nd <-
    getsNES $
      nesEsL . esLStateL . lsCertStateL . certVStateL . vsNumDormantEpochsL
  nd `shouldBeExpr` expected

mkConstitutionProposal ::
  ConwayEraImp era =>
  StrictMaybe (GovPurposeId 'ConstitutionPurpose era) ->
  ImpTestM era (ProposalProcedure era, Constitution era)
mkConstitutionProposal prevGovId = do
  constitution <- arbitrary
  (,constitution) <$> mkProposal (NewConstitution prevGovId constitution)

submitConstitution ::
  forall era.
  ConwayEraImp era =>
  StrictMaybe (GovPurposeId 'ConstitutionPurpose era) ->
  ImpTestM era (GovActionId (EraCrypto era))
submitConstitution prevGovId = do
  (proposal, _) <- mkConstitutionProposal prevGovId
  submitProposal proposal

expectDRepNotRegistered ::
  HasCallStack =>
  Credential 'DRepRole (EraCrypto era) ->
  ImpTestM era ()
expectDRepNotRegistered drep = do
  dsMap <- getsNES (nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL)
  Map.lookup drep dsMap `shouldBe` Nothing

isDRepExpired ::
  HasCallStack =>
  Credential 'DRepRole (EraCrypto era) ->
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
  HasCallStack =>
  Credential 'DRepRole (EraCrypto era) ->
  EpochNo ->
  ImpTestM era ()
expectDRepExpiry drep expected = do
  dsMap <- getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL
  let ds = fromJust $ Map.lookup drep dsMap
  drepExpiry ds `shouldBe` expected

expectActualDRepExpiry ::
  HasCallStack =>
  Credential 'DRepRole (EraCrypto era) ->
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
  GovActionId (EraCrypto era) ->
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
  ConwayEraGov era => ImpTestM era (SSeq.StrictSeq (GovActionId (EraCrypto era)))
currentProposalIds = proposalsIds <$> getsNES (newEpochStateGovStateL . proposalsGovStateL)

lastEpochProposals ::
  forall era.
  ConwayEraGov era =>
  ImpTestM era (SSeq.StrictSeq (GovActionId (EraCrypto era)))
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

pulsingStateSnapshotL :: Lens' (DRepPulsingState era) (PulsingSnapshot era)
pulsingStateSnapshotL = lens getter setter
  where
    getter (DRComplete x _) = x
    getter state = fst (finishDRepPulser state)
    setter (DRComplete _ y) snap = DRComplete snap y
    setter state snap = DRComplete snap $ snd $ finishDRepPulser state

-- | A legal ProtVer that differs in the minor Version
minorFollow :: ProtVer -> ProtVer
minorFollow (ProtVer x y) = ProtVer x (y + 1)

-- | A legal ProtVer that moves to the next major Version
majorFollow :: ProtVer -> ProtVer
majorFollow pv@(ProtVer x _) = case succVersion x of
  Just x' -> ProtVer x' 0
  Nothing -> error ("The last major version can't be incremented. " ++ show pv)

-- | An illegal ProtVer that skips 3 minor versions
cantFollow :: ProtVer -> ProtVer
cantFollow (ProtVer x y) = ProtVer x (y + 3)

whenBootstrap :: EraGov era => ImpTestM era () -> ImpTestM era ()
whenBootstrap a = do
  pv <- getProtVer
  when (HardForks.bootstrapPhase pv) a

whenPostBootstrap :: EraGov era => ImpTestM era () -> ImpTestM era ()
whenPostBootstrap a = do
  pv <- getProtVer
  unless (HardForks.bootstrapPhase pv) a

ifBootstrap :: EraGov era => ImpTestM era a -> ImpTestM era a -> ImpTestM era a
ifBootstrap inBootstrap outOfBootstrap = do
  pv <- getProtVer
  if HardForks.bootstrapPhase pv then inBootstrap else outOfBootstrap

submitYesVoteCCs_ ::
  forall era f.
  (ConwayEraImp era, Foldable f) =>
  f (Credential 'HotCommitteeRole (EraCrypto era)) ->
  GovActionId (EraCrypto era) ->
  ImpTestM era ()
submitYesVoteCCs_ committeeMembers govId =
  mapM_ (\c -> submitYesVote_ (CommitteeVoter c) govId) committeeMembers

submitUpdateCommittee ::
  ConwayEraImp era =>
  -- | Set the parent. When Nothing is supplied latest parent will be used.
  Maybe (StrictMaybe (GovPurposeId 'CommitteePurpose era)) ->
  -- | CC members to remove
  Set.Set (Credential 'ColdCommitteeRole (EraCrypto era)) ->
  -- | CC members to add
  [(Credential 'ColdCommitteeRole (EraCrypto era), EpochInterval)] ->
  UnitInterval ->
  ImpTestM era (GovActionId (EraCrypto era))
submitUpdateCommittee mParent ccsToRemove ccsToAdd threshold = do
  nes <- getsNES id
  let
    curEpochNo = nes ^. nesELL
    rootCommittee = nes ^. newEpochStateGovStateL . cgsProposalsL . pRootsL . grCommitteeL
    parent = fromMaybe (prRoot rootCommittee) mParent
    newCommitteMembers =
      Map.fromList [(cc, addEpochInterval curEpochNo lifetime) | (cc, lifetime) <- ccsToAdd]
  submitGovAction $ UpdateCommittee parent ccsToRemove newCommitteMembers threshold

expectCommitteeMemberPresence ::
  (HasCallStack, ConwayEraGov era) => Credential 'ColdCommitteeRole (EraCrypto era) -> ImpTestM era ()
expectCommitteeMemberPresence cc = do
  SJust committee <- getsNES $ newEpochStateGovStateL . committeeGovStateL
  assertBool ("Expected Committee Member: " ++ show cc ++ " to be present in the committee") $
    Map.member cc (committee ^. committeeMembersL)

expectCommitteeMemberAbsence ::
  (HasCallStack, ConwayEraGov era) => Credential 'ColdCommitteeRole (EraCrypto era) -> ImpTestM era ()
expectCommitteeMemberAbsence cc = do
  SJust committee <- getsNES $ newEpochStateGovStateL . committeeGovStateL
  assertBool ("Expected Committee Member: " ++ show cc ++ " to be absent from the committee") $
    Map.notMember cc (committee ^. committeeMembersL)

donateToTreasury :: ConwayEraImp era => Coin -> ImpTestM era ()
donateToTreasury amount =
  impAnn ("Donation to treasury in the amount of: " ++ show amount) $ do
    treasuryStart <- getsNES $ nesEsL . esAccountStateL . asTreasuryL
    submitTx_ $ mkBasicTx (mkBasicTxBody & treasuryDonationTxBodyL .~ amount)
    treasuryEndEpoch0 <- getsNES $ nesEsL . esAccountStateL . asTreasuryL
    -- Actual donation happens on the epoch boundary
    treasuryStart `shouldBe` treasuryEndEpoch0
    passEpoch
    treasuryEndEpoch1 <- getsNES $ nesEsL . esAccountStateL . asTreasuryL
    treasuryEndEpoch1 <-> treasuryStart `shouldBe` amount

expectMembers ::
  (HasCallStack, ConwayEraGov era) =>
  Set.Set (Credential 'ColdCommitteeRole (EraCrypto era)) -> ImpTestM era ()
expectMembers expKhs = do
  committee <- getsNES $ newEpochStateGovStateL . committeeGovStateL
  let members = Map.keysSet $ foldMap' committeeMembers committee
  impAnn "Expecting committee members" $ members `shouldBe` expKhs

showConwayTxBalance ::
  ( EraUTxO era
  , ConwayEraTxBody era
  , Tx era ~ AlonzoTx era
  ) =>
  PParams era ->
  CertState era ->
  UTxO era ->
  AlonzoTx era ->
  String
showConwayTxBalance pp certState utxo tx =
  unlines
    [ "Consumed:   \t"
    , "\tInputs:     \t" <> show (coin inputs)
    , -- , "Refunds:    \t" <> show refunds
      "\tWithdrawals \t" <> show withdrawals
    , "\tTotal:      \t" <> (show . coin $ consumed pp certState utxo txBody)
    , ""
    , "Produced:  \t"
    , "\tOutputs:   \t" <> show (coin $ sumAllValue (txBody ^. outputsTxBodyL))
    , "\tDonations: \t" <> show (txBody ^. treasuryDonationTxBodyL)
    , "\tDeposits:  \t" <> show (getTotalDepositsTxBody pp isRegPoolId txBody)
    , "\tFees:      \t" <> show (txBody ^. feeTxBodyL)
    , "\tTotal:     \t" <> (show . coin $ produced pp certState txBody)
    ]
  where
    -- lookupStakingDeposit c = certState ^. certPStateL . psStakePoolParamsL
    -- lookupDRepDeposit c = undefined
    txBody = tx ^. bodyTxL
    inputs = balance (txInsFilter utxo (txBody ^. inputsTxBodyL))
    -- refunds = getTotalRefundsTxBody pp lookupStakingDeposit lookupDRepDeposit txBody
    isRegPoolId = (`Map.member` (certState ^. certPStateL . psStakePoolParamsL))
    withdrawals = fold . unWithdrawals $ txBody ^. withdrawalsTxBodyL

logConwayTxBalance ::
  ( EraUTxO era
  , EraGov era
  , ConwayEraTxBody era
  , Tx era ~ AlonzoTx era
  ) =>
  AlonzoTx era ->
  ImpTestM era ()
logConwayTxBalance tx = do
  pp <- getsPParams id
  certState <- getsNES $ nesEsL . esLStateL . lsCertStateL
  utxo <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosUtxoL
  logString $ showConwayTxBalance pp certState utxo tx

submitBootstrapAwareFailingVote ::
  ConwayEraImp era =>
  Vote ->
  Voter (EraCrypto era) ->
  GovActionId (EraCrypto era) ->
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
  ImpTestM era (Maybe (GovActionId (EraCrypto era)))
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
