{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.ImpTest (
  module ImpTest,
  ConwayEraImp,
  enactConstitution,
  submitGovAction,
  submitGovAction_,
  submitGovActions,
  submitProposal,
  submitProposal_,
  submitProposals,
  submitFailingProposal,
  trySubmitGovAction,
  trySubmitGovActions,
  trySubmitProposal,
  trySubmitProposals,
  submitTreasuryWithdrawals,
  submitVote,
  submitVote_,
  submitYesVote_,
  submitFailingVote,
  trySubmitVote,
  registerDRep,
  setupSingleDRep,
  setupPoolWithStake,
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
  logRatificationChecks,
  resignCommitteeColdKey,
  registerCommitteeHotKey,
  logCurPParams,
  electCommittee,
  electBasicCommittee,
  proposalsShowDebug,
  getGovPolicy,
  submitFailingGovAction,
  submitInitConstitutionGovAction,
  submitChildConstitutionGovAction,
  submitConstitutionGovAction,
  submitConstitutionGovActionTree,
  submitConstitutionGovActionForest,
  getProposalsForest,
  logProposalsForest,
  logProposalsForestDiff,
  constitutionShouldBe,
  ccShouldBeResigned,
  ccShouldNotBeResigned,
  getLastEnactedCommittee,
  getLastEnactedConstitution,
  submitParameterChange,
  getLastEnactedParameterChange,
  getLastEnactedHardForkInitiation,
  getConstitutionProposals,
  getParameterChangeProposals,
  expectNumDormantEpochs,
  submitConstitution,
  expectExtraDRepExpiry,
  expectCurrentProposals,
  expectNoCurrentProposals,
  expectPulserProposals,
  expectNoPulserProposals,
  minorFollow,
  majorFollow,
  cantFollow,
) where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), Ed25519DSIGN, Signable)
import Cardano.Crypto.Hash.Class (Hash)
import Cardano.Ledger.Address (Addr (..), RewardAccount (..))
import Cardano.Ledger.Allegra.Scripts (Timelock)
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript)
import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  EpochNo,
  Network (..),
  ProtVer (..),
  ShelleyBase,
  StrictMaybe (..),
  addEpochInterval,
  inject,
  succVersion,
  textToUrl,
 )
import Cardano.Ledger.CertState (
  CommitteeAuthorization (..),
  csCommitteeCredsL,
  vsNumDormantEpochsL,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core hiding (proposals)
import Cardano.Ledger.Conway.Governance
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
import Cardano.Ledger.Conway.TxCert (
  ConwayEraTxCert (..),
  Delegatee (..),
  pattern AuthCommitteeHotKeyTxCert,
  pattern RegDRepTxCert,
  pattern RegDepositDelegTxCert,
  pattern ResignCommitteeColdTxCert,
 )
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.DRep
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Plutus.Language (SLanguage (..))
import Cardano.Ledger.Shelley.LedgerState (
  IncrementalStake (..),
  asTreasuryL,
  certVStateL,
  curPParamsEpochStateL,
  epochStateGovStateL,
  esAccountStateL,
  esLStateL,
  lsCertStateL,
  lsUTxOStateL,
  nesELL,
  nesEpochStateL,
  nesEsL,
  nesPdL,
  newEpochStateGovStateL,
  utxosGovStateL,
  utxosStakeDistrL,
  vsCommitteeStateL,
  vsDRepsL,
 )
import Cardano.Ledger.TxIn (TxId (..))
import Cardano.Ledger.Val (Val (..))
import Control.Monad (forM)
import Control.State.Transition.Extended (STS (..))
import Data.Default.Class (Default (..))
import Data.Foldable (Foldable (..))
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust)
import Data.Maybe.Strict (isSJust)
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Tree
import qualified GHC.Exts as GHC (fromList)
import Lens.Micro
import Test.Cardano.Ledger.Allegra.ImpTest (impAllegraSatisfyNativeScript)
import Test.Cardano.Ledger.Alonzo.ImpTest as ImpTest
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.TreeDiff ()
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkAddr)
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
import Test.Cardano.Ledger.Imp.Common

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

instance
  ( Crypto c
  , NFData (SigDSIGN (DSIGN c))
  , NFData (VerKeyDSIGN (DSIGN c))
  , DSIGN c ~ Ed25519DSIGN
  , Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)
  , Eq (ConwayGovEvent (ConwayEra c))
  ) =>
  ShelleyEraImp (ConwayEra c)
  where
  initImpNES =
    let nes =
          initAlonzoImpNES
            & nesEsL . curPParamsEpochStateL . ppDRepActivityL .~ EpochInterval 100
            & nesEsL . curPParamsEpochStateL . ppGovActionLifetimeL .~ EpochInterval 30
        epochState = nes ^. nesEsL
        ratifyState =
          def
            & rsEnactStateL .~ mkEnactState (epochState ^. epochStateGovStateL)
     in nes & nesEsL .~ setCompleteDRepPulsingState def ratifyState epochState

  impSatisfyNativeScript = impAllegraSatisfyNativeScript

  modifyPParams = conwayModifyPParams

  fixupTx = alonzoFixupTx

instance ShelleyEraImp (ConwayEra c) => AlonzoEraImp (ConwayEra c) where
  scriptTestContexts =
    plutusTestScripts SPlutusV1
      <> plutusTestScripts SPlutusV2
      <> plutusTestScripts SPlutusV3

class
  ( AlonzoEraImp era
  , ConwayEraGov era
  , ConwayEraTxBody era
  , STS (EraRule "ENACT" era)
  , BaseM (EraRule "ENACT" era) ~ ShelleyBase
  , State (EraRule "ENACT" era) ~ EnactState era
  , Signal (EraRule "ENACT" era) ~ EnactSignal era
  , Environment (EraRule "ENACT" era) ~ ()
  , NativeScript era ~ Timelock era
  , Script era ~ AlonzoScript era
  ) =>
  ConwayEraImp era

instance
  ( Crypto c
  , NFData (SigDSIGN (DSIGN c))
  , NFData (VerKeyDSIGN (DSIGN c))
  , DSIGN c ~ Ed25519DSIGN
  , Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)
  ) =>
  ConwayEraImp (ConwayEra c)

-- | Submit a transaction that registers a new DRep and return the keyhash
-- belonging to that DRep
registerDRep ::
  forall era.
  ( ShelleyEraImp era
  , ConwayEraTxCert era
  ) =>
  ImpTestM era (KeyHash 'DRepRole (EraCrypto era))
registerDRep = do
  -- Register a DRep
  khDRep <- freshKeyHash
  submitTxAnn_ "Register DRep" $
    mkBasicTx mkBasicTxBody
      & bodyTxL . certsTxBodyL
        .~ SSeq.singleton
          ( RegDRepTxCert
              (KeyHashObj khDRep)
              zero
              SNothing
          )
  dreps <- getsNES @era $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL
  dreps `shouldSatisfy` Map.member (KeyHashObj khDRep)
  pure khDRep

-- | Registers a new DRep and delegates 1 ADA to it. Returns the keyhash of the
-- DRep
setupSingleDRep ::
  forall era.
  ( ConwayEraTxCert era
  , ShelleyEraImp era
  ) =>
  Integer ->
  ImpTestM
    era
    ( KeyHash 'DRepRole (EraCrypto era)
    , KeyHash 'Staking (EraCrypto era)
    , KeyPair 'Payment (EraCrypto era)
    )
setupSingleDRep stake = do
  drepKH <- registerDRep
  delegatorKH <- freshKeyHash
  delegatorKP <- lookupKeyPair delegatorKH
  spendingKP <- lookupKeyPair =<< freshKeyHash
  submitTxAnn_ "Delegate to DRep" $
    mkBasicTx mkBasicTxBody
      & bodyTxL . outputsTxBodyL
        .~ SSeq.singleton
          ( mkBasicTxOut
              (mkAddr (spendingKP, delegatorKP))
              (inject $ Coin stake)
          )
      & bodyTxL . certsTxBodyL
        .~ SSeq.fromList
          [ mkRegDepositDelegTxCert @era
              (KeyHashObj delegatorKH)
              (DelegVote (DRepCredential $ KeyHashObj drepKH))
              zero
          ]
  pure (drepKH, delegatorKH, spendingKP)

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
  khPool <- registerPool
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
  fmap (fmap txIdTx) $
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
    Left err -> Left err

trySubmitProposals ::
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  ) =>
  NE.NonEmpty (ProposalProcedure era) ->
  ImpTestM era (Either (NonEmpty (PredicateFailure (EraRule "LEDGER" era))) (Tx era))
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
  fmap mkGovActionId <$> trySubmitGovActions (pure ga)

-- | Submits a transaction that proposes the given governance action
trySubmitGovActions ::
  (ShelleyEraImp era, ConwayEraTxBody era) =>
  NE.NonEmpty (GovAction era) ->
  ImpTestM era (Either (NonEmpty (PredicateFailure (EraRule "LEDGER" era))) (Tx era))
trySubmitGovActions gas = do
  deposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionDepositL
  rewardAccount <- registerRewardAccount
  proposals <- forM gas $ \ga -> do
    pure
      ProposalProcedure
        { pProcDeposit = deposit
        , pProcReturnAddr = rewardAccount
        , pProcGovAction = ga
        , pProcAnchor = def
        }
  trySubmitProposals proposals

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

submitParameterChange ::
  ConwayEraImp era =>
  StrictMaybe (GovPurposeId 'PParamUpdatePurpose era) ->
  PParamsUpdate era ->
  ImpTestM era (GovActionId (EraCrypto era))
submitParameterChange parent ppu = do
  policy <- getGovPolicy
  submitGovAction $ ParameterChange parent ppu policy

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
getProposals = getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . proposalsGovStateL

logProposalsForest :: ConwayEraGov era => ImpTestM era ()
logProposalsForest = do
  proposals <- getProposals
  logEntry $ proposalsShowDebug proposals True

getLastEnactedCommittee :: ConwayEraGov era => ImpTestM era (StrictMaybe (GovPurposeId 'CommitteePurpose era))
getLastEnactedCommittee = do
  ps <- getProposals
  pure $ ps ^. pRootsL . grCommitteeL . prRootL

getLastEnactedConstitution :: ConwayEraGov era => ImpTestM era (StrictMaybe (GovPurposeId 'ConstitutionPurpose era))
getLastEnactedConstitution = do
  ps <- getProposals
  pure $ ps ^. pRootsL . grConstitutionL . prRootL

getLastEnactedParameterChange :: ConwayEraGov era => ImpTestM era (StrictMaybe (GovPurposeId 'PParamUpdatePurpose era))
getLastEnactedParameterChange = do
  ps <- getProposals
  pure $ ps ^. pRootsL . grPParamUpdateL . prRootL

getLastEnactedHardForkInitiation :: ConwayEraGov era => ImpTestM era (StrictMaybe (GovPurposeId 'HardForkPurpose era))
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
  (Era era, ToExpr (PParamsHKD StrictMaybe era)) =>
  Proposals era ->
  Proposals era ->
  ImpTestM era ()
logProposalsForestDiff pf1 pf2 = logEntry $ unlines ["Proposals Forest Diff:", diffExpr pf1 pf2]

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
        expectationFailure $ "Found gov action state for govActionId: " <> show govActionId
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
  pure
    RatifyEnv
      { reStakePoolDistr = poolDistr
      , reStakeDistr = credMap stakeDistr
      , reDRepState = drepState
      , reDRepDistr = drepDistr
      , reCurrentEpoch = eNo - 1
      , reCommitteeState = committeeState
      }

-- | Test the resignation status for a CC cold key to be resigned
ccShouldBeResigned :: HasCallStack => Credential 'ColdCommitteeRole (EraCrypto era) -> ImpTestM era ()
ccShouldBeResigned coldK = do
  committeeCreds <- getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL . csCommitteeCredsL
  authHk <$> Map.lookup coldK committeeCreds `shouldBe` Just Nothing

-- | Test the resignation status for a CC cold key to not be resigned
ccShouldNotBeResigned :: HasCallStack => Credential 'ColdCommitteeRole (EraCrypto era) -> ImpTestM era ()
ccShouldNotBeResigned coldK = do
  committeeCreds <- getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL . csCommitteeCredsL
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

calculatePoolAcceptedRatio :: ConwayEraGov era => GovActionId (EraCrypto era) -> ImpTestM era Rational
calculatePoolAcceptedRatio gaId = do
  ratEnv <- getRatifyEnv
  gas <- getGovActionState gaId
  pure $ spoAcceptedRatio ratEnv gas

-- | Logs the ratios of accepted votes per category
logAcceptedRatio :: (HasCallStack, ConwayEraGov era) => GovActionId (EraCrypto era) -> ImpTestM era ()
logAcceptedRatio aId = do
  dRepRatio <- calculateDRepAcceptedRatio aId
  committeeRatio <- calculateCommitteeAcceptedRatio aId
  spoRatio <- calculatePoolAcceptedRatio aId
  logEntry $
    unlines
      [ ""
      , "----- ACCEPTED RATIOS -----"
      , "DRep accepted ratio:\t\t" <> show dRepRatio
      , "Committee accepted ratio:\t" <> show committeeRatio
      , "SPO accepted ratio:\t\t" <> show spoRatio
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
-- epoch. (Note that no other checks execept DRep votes is used)
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

-- | Logs the results of each check required to make the governance action pass
logRatificationChecks ::
  (ConwayEraGov era, ConwayEraPParams era) =>
  GovActionId (EraCrypto era) ->
  ImpTestM era ()
logRatificationChecks gaId = do
  gas@GovActionState {gasCommitteeVotes, gasDRepVotes} <- getGovActionState gaId
  let govAction = gasAction gas
  ens@EnactState {..} <- getEnactState
  committee <- getsNES $ nesEsL . epochStateGovStateL . committeeGovStateL
  ratEnv <- getRatifyEnv
  let ratSt = RatifyState ens mempty mempty False
  curTreasury <- getsNES $ nesEsL . esAccountStateL . asTreasuryL
  currentEpoch <- getsNES nesELL
  let
    members = foldMap' committeeMembers committee
    committeeState = reCommitteeState ratEnv
  curPParams <- getsNES $ nesEsL . epochStateGovStateL . curPParamsGovStateL
  logEntry $
    unlines
      [ "----- RATIFICATION CHECKS -----"
      , "prevActionAsExpected:\t" <> show (prevActionAsExpected gas ensPrevGovActionIds)
      , "validCommitteeTerm:\t" <> show (validCommitteeTerm govAction curPParams currentEpoch)
      , "notDelayed:\t\t??"
      , "withdrawalCanWithdraw:\t" <> show (withdrawalCanWithdraw govAction curTreasury)
      , "committeeAccepted:\t"
          <> show (committeeAccepted ratEnv ratSt gas)
          <> " [ To Pass: "
          <> show (committeeAcceptedRatio members gasCommitteeVotes committeeState currentEpoch)
          <> " >= "
          <> show (votingCommitteeThreshold ratSt (gasAction gas))
          <> " ]"
      , "spoAccepted:\t\t"
          <> show (spoAccepted ratEnv ratSt gas)
          <> " [ To Pass: "
          <> show (spoAcceptedRatio ratEnv gas)
          <> " >= "
          <> show (votingStakePoolThreshold ratSt (gasAction gas))
          <> " ]"
      , "dRepAccepted:\t\t"
          <> show (dRepAccepted ratEnv ratSt gas)
          <> " [ To Pass: "
          <> show (dRepAcceptedRatio ratEnv gasDRepVotes (gasAction gas))
          <> " >= "
          <> show (votingDRepThreshold ratSt (gasAction gas))
          <> " ]"
      , ""
      ]

-- | Submits a transaction that registers a hot key for the given cold key.
-- Returns the hot key hash.
registerCommitteeHotKey ::
  (ShelleyEraImp era, ConwayEraTxCert era) =>
  Credential 'ColdCommitteeRole (EraCrypto era) ->
  ImpTestM era (Credential 'HotCommitteeRole (EraCrypto era))
registerCommitteeHotKey coldKey = do
  hotKey <- KeyHashObj <$> freshKeyHash
  submitTxAnn_ "Registering Committee Hot key" $
    mkBasicTx mkBasicTxBody
      & bodyTxL . certsTxBodyL
        .~ SSeq.singleton (AuthCommitteeHotKeyTxCert coldKey hotKey)
  pure hotKey

-- | Submits a transaction that resigns the cold key
resignCommitteeColdKey ::
  (ShelleyEraImp era, ConwayEraTxCert era) =>
  Credential 'ColdCommitteeRole (EraCrypto era) ->
  StrictMaybe (Anchor (EraCrypto era)) ->
  ImpTestM era ()
resignCommitteeColdKey coldKey anchor = do
  submitTxAnn_ "Resigning Committee Cold key" $
    mkBasicTx mkBasicTxBody
      & bodyTxL . certsTxBodyL
        .~ SSeq.singleton (ResignCommitteeColdTxCert coldKey anchor)

electCommittee ::
  forall era.
  ( HasCallStack
  , ConwayEraImp era
  ) =>
  StrictMaybe (GovPurposeId 'CommitteePurpose era) ->
  KeyHash 'DRepRole (EraCrypto era) ->
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
  submitYesVote_ (DRepVoter $ KeyHashObj drep) gaidCommitteeProp
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
  logEntry "Setting up PParams and DRep"
  modifyPParams $ \pp ->
    pp
      & ppDRepVotingThresholdsL
        %~ ( \dvt ->
              dvt
                { dvtCommitteeNormal = 1 %! 1
                , dvtCommitteeNoConfidence = 1 %! 2
                , dvtUpdateToConstitution = 1 %! 2
                }
           )
      & ppCommitteeMaxTermLengthL .~ EpochInterval 20
      & ppGovActionLifetimeL .~ EpochInterval 2
      & ppGovActionDepositL .~ Coin 123
  (khDRep, _, _) <- setupSingleDRep 1_000_000

  logEntry "Registering committee member"
  khCommitteeMember <- KeyHashObj <$> freshKeyHash
  let
    committeeAction =
      UpdateCommittee
        SNothing
        mempty
        (Map.singleton khCommitteeMember 20)
        (1 %! 2)
  (gaidCommitteeProp NE.:| _) <-
    submitGovActions
      [ committeeAction
      , UpdateCommittee SNothing mempty mempty (1 %! 10)
      ]
  submitYesVote_ (DRepVoter $ KeyHashObj khDRep) gaidCommitteeProp

  let
    assertNoCommittee = do
      committee <-
        getsNES $
          nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . committeeGovStateL
      impAnn "There should not be a committee" $ committee `shouldBe` SNothing
  logRatificationChecks gaidCommitteeProp
  assertNoCommittee

  passEpoch
  logRatificationChecks gaidCommitteeProp
  assertNoCommittee
  passEpoch
  do
    committee <-
      getsNES $
        nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . committeeGovStateL
    impAnn "There should be a committee" $ committee `shouldSatisfy` isSJust

  credCommitteeMemberHot <- registerCommitteeHotKey khCommitteeMember
  pure (KeyHashObj khDRep, credCommitteeMemberHot, GovPurposeId gaidCommitteeProp)

logCurPParams :: (EraGov era, ToExpr (PParamsHKD Identity era)) => ImpTestM era ()
logCurPParams = do
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  logEntry $
    unlines
      [ ""
      , "----- Current PParams -----"
      , showExpr pp
      , "---------------------------"
      , ""
      ]

proposalsShowDebug :: Era era => Proposals era -> Bool -> String
proposalsShowDebug ps showRoots =
  unlines $
    [ ""
    , "----- Proposals -----"
    , "Size"
    , show $ proposalsSize ps
    , "OMap"
    , show $ proposalsIds ps
    , ""
    , "Roots"
    , "> PParamUpdate"
    , show $ ps ^. pRootsL . grPParamUpdateL
    , "> HardFork"
    , show $ ps ^. pRootsL . grHardForkL
    , "> Committee"
    , show $ ps ^. pRootsL . grCommitteeL
    , "> Constitution"
    , show $ ps ^. pRootsL . grConstitutionL
    ]
      <> ( if showRoots
            then
              [ "Hierarchy"
              , ">> PParamUpdate"
              , show $ ps ^. pGraphL . grPParamUpdateL . pGraphNodesL
              , ">> HardFork"
              , show $ ps ^. pGraphL . grHardForkL . pGraphNodesL
              , ">> Committee"
              , show $ ps ^. pGraphL . grCommitteeL . pGraphNodesL
              , ">> Constitution"
              , show $ ps ^. pGraphL . grConstitutionL . pGraphNodesL
              ]
            else mempty
         )
      <> ["----- Proposals End -----"]

submitInitConstitutionGovAction ::
  (ShelleyEraImp era, ConwayEraTxBody era) =>
  ImpTestM era (GovActionId (EraCrypto era))
submitInitConstitutionGovAction = do
  submitConstitutionGovAction SNothing

submitChildConstitutionGovAction ::
  (ShelleyEraImp era, ConwayEraTxBody era) =>
  GovActionId (EraCrypto era) ->
  ImpTestM era (GovActionId (EraCrypto era))
submitChildConstitutionGovAction gai =
  submitConstitutionGovAction $ SJust $ GovPurposeId gai

submitConstitutionGovAction ::
  (ShelleyEraImp era, ConwayEraTxBody era) =>
  StrictMaybe (GovPurposeId 'ConstitutionPurpose era) ->
  ImpTestM era (GovActionId (EraCrypto era))
submitConstitutionGovAction pgai = do
  constitutionHash <- freshSafeHash
  let constitutionAction =
        NewConstitution
          pgai
          ( Constitution
              ( Anchor
                  (fromJust $ textToUrl 64 "constitution.dummy.0")
                  constitutionHash
              )
              SNothing
          )
  submitGovAction constitutionAction

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

submitConstitutionGovActionTree ::
  (ShelleyEraImp era, ConwayEraTxBody era) =>
  StrictMaybe (GovActionId (EraCrypto era)) ->
  Tree () ->
  ImpTestM era (Tree (GovActionId (EraCrypto era)))
submitConstitutionGovActionTree p tree =
  unfoldTreeM go $ fmap (const p) tree
  where
    go (Node parent children) = do
      n <- submitConstitutionGovAction $ GovPurposeId <$> parent
      pure (n, fmap (\(Node _child subtree) -> Node (SJust n) subtree) children)

submitConstitutionGovActionForest ::
  ConwayEraImp era =>
  StrictMaybe (GovActionId (EraCrypto era)) ->
  Forest () ->
  ImpTestM era (Forest (GovActionId (EraCrypto era)))
submitConstitutionGovActionForest p forest =
  unfoldForestM go $ fmap (fmap $ const p) forest
  where
    go (Node parent children) = do
      n <- submitConstitutionGovAction $ GovPurposeId <$> parent
      pure (n, fmap (\(Node _child subtree) -> Node (SJust n) subtree) children)

enactConstitution ::
  forall era.
  ( ConwayEraImp era
  , HasCallStack
  ) =>
  StrictMaybe (GovPurposeId 'ConstitutionPurpose era) ->
  Constitution era ->
  Credential 'DRepRole (EraCrypto era) ->
  Credential 'HotCommitteeRole (EraCrypto era) ->
  ImpTestM era (GovActionId (EraCrypto era))
enactConstitution prevGovId constitution dRep committeeMember = impAnn "Enacting constitution" $ do
  let action = NewConstitution prevGovId constitution
  govId <- submitGovAction action
  submitYesVote_ (DRepVoter dRep) govId
  submitYesVote_ (CommitteeVoter committeeMember) govId
  logRatificationChecks govId
  passNEpochs 2
  enactedConstitution <-
    getsNES $
      nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . constitutionGovStateL
  enactedConstitution `shouldBe` constitution
  pure govId

-- | Asserts that the URL of the current constitution is equal to the given
-- string
constitutionShouldBe :: (HasCallStack, ConwayEraGov era) => String -> ImpTestM era ()
constitutionShouldBe cUrl = do
  Constitution {constitutionAnchor = Anchor {anchorUrl}} <-
    getsNES $
      nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . constitutionGovStateL
  anchorUrl `shouldBe` fromJust (textToUrl 64 $ T.pack cUrl)

expectNumDormantEpochs :: HasCallStack => EpochNo -> ImpTestM era ()
expectNumDormantEpochs expected = do
  nd <-
    getsNES $
      nesEsL . esLStateL . lsCertStateL . certVStateL . vsNumDormantEpochsL
  nd `shouldBeExpr` expected

submitConstitution ::
  forall era.
  ConwayEraImp era =>
  StrictMaybe (GovPurposeId 'ConstitutionPurpose era) ->
  ImpTestM era (GovActionId (EraCrypto era), Constitution era)
submitConstitution prevGovId = do
  constitution <- arbitrary
  let constitutionAction =
        NewConstitution
          prevGovId
          constitution
  govActionId <- submitGovAction constitutionAction
  pure (govActionId, constitution)

expectExtraDRepExpiry ::
  (HasCallStack, EraGov era, ConwayEraPParams era) =>
  KeyHash 'DRepRole (EraCrypto era) ->
  EpochNo ->
  ImpTestM era ()
expectExtraDRepExpiry kh expected = do
  drepActivity <-
    getsNES $
      nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . curPParamsGovStateL . ppDRepActivityL
  dsMap <-
    getsNES $
      nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL
  let ds = Map.lookup (KeyHashObj kh) dsMap
  (^. drepExpiryL)
    <$> ds
      `shouldBe` Just (addEpochInterval expected drepActivity)

expectCurrentProposals :: (HasCallStack, ConwayEraGov era) => ImpTestM era ()
expectCurrentProposals = do
  props <- currentProposalIds
  assertBool "Expected proposals in current gov state" (not (SSeq.null props))

expectNoCurrentProposals :: (HasCallStack, ConwayEraGov era) => ImpTestM era ()
expectNoCurrentProposals = do
  props <- currentProposalIds
  assertBool "Expected no proposals in current gov state" (SSeq.null props)

expectPulserProposals :: (HasCallStack, ConwayEraGov era) => ImpTestM era ()
expectPulserProposals = do
  props <- lastEpochProposals
  assertBool "Expected proposals in the pulser" (not (SSeq.null props))

expectNoPulserProposals :: (HasCallStack, ConwayEraGov era) => ImpTestM era ()
expectNoPulserProposals = do
  props <- lastEpochProposals
  assertBool "Expected no proposals in the pulser" (SSeq.null props)

currentProposalIds :: ConwayEraGov era => ImpTestM era (SSeq.StrictSeq (GovActionId (EraCrypto era)))
currentProposalIds =
  proposalsIds
    <$> getsNES (nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . proposalsGovStateL)

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
