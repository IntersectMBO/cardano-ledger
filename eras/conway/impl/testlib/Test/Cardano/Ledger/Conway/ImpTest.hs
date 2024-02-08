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
  calculateCommitteeAcceptedRatio,
  logAcceptedRatio,
  canGovActionBeDRepAccepted,
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
) where

import Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm (..), Signable)
import Cardano.Crypto.Hash.Class (Hash)
import Cardano.Ledger.Address (Addr (..), RewardAccount (..))
import Cardano.Ledger.Allegra.Scripts (Timelock)
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript)
import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  EpochNo,
  Network (..),
  ShelleyBase,
  StrictMaybe (..),
  inject,
  textToUrl,
 )
import Cardano.Ledger.CertState (DRep (..), csCommitteeCredsL)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules (
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
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
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
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust)
import Data.Maybe.Strict (isSJust)
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Tree
import qualified GHC.Exts as GHC (fromList)
import Lens.Micro (Lens', (%~), (&), (.~), (^.))
import Test.Cardano.Ledger.Allegra.ImpTest (impAllegraSatisfyNativeScript)
import Test.Cardano.Ledger.Alonzo.ImpTest as ImpTest
import Test.Cardano.Ledger.Babbage.ImpTest (babbageFixupTx)
import Test.Cardano.Ledger.Conway.TreeDiff ()
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)
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
  , Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)
  ) =>
  ShelleyEraImp (ConwayEra c)
  where
  initImpNES rootCoin =
    let nes =
          initAlonzoImpNES rootCoin
            & nesEsL . curPParamsEpochStateL . ppDRepActivityL .~ EpochInterval 100
            & nesEsL . curPParamsEpochStateL . ppGovActionLifetimeL .~ EpochInterval 30
        epochState = nes ^. nesEsL
        ratifyState =
          def
            & rsEnactStateL .~ mkEnactState (epochState ^. epochStateGovStateL)
     in nes & nesEsL .~ setCompleteDRepPulsingState def ratifyState epochState

  impSatisfyNativeScript = impAllegraSatisfyNativeScript

  modifyPParams = conwayModifyPParams

  fixupTx = babbageFixupTx

class
  ( ShelleyEraImp era
  , ConwayEraGov era
  , ConwayEraTxBody era
  , STS (EraRule "ENACT" era)
  , BaseM (EraRule "ENACT" era) ~ ShelleyBase
  , State (EraRule "ENACT" era) ~ EnactState era
  , Signal (EraRule "ENACT" era) ~ EnactSignal era
  , Environment (EraRule "ENACT" era) ~ ()
  , NativeScript era ~ Timelock era
  , Script era ~ AlonzoScript era
  , ToExpr (PredicateFailure (EraRule "CERTS" era))
  ) =>
  ConwayEraImp era

instance
  ( Crypto c
  , NFData (SigDSIGN (DSIGN c))
  , NFData (VerKeyDSIGN (DSIGN c))
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
  ImpTestM era (KeyHash 'DRepRole (EraCrypto era))
setupSingleDRep = do
  khDRep <- registerDRep

  khDelegator <- freshKeyHash
  kpDelegator <- lookupKeyPair khDelegator
  kpSpending <- lookupKeyPair =<< freshKeyHash
  submitTxAnn_ "Delegate to DRep" $
    mkBasicTx mkBasicTxBody
      & bodyTxL . outputsTxBodyL
        .~ SSeq.singleton
          ( mkBasicTxOut
              (mkAddr (kpSpending, kpDelegator))
              (inject $ Coin 1_000_000)
          )
      & bodyTxL . certsTxBodyL
        .~ SSeq.fromList
          [ mkRegDepositDelegTxCert @era
              (KeyHashObj khDelegator)
              (DelegVote (DRepCredential $ KeyHashObj khDRep))
              zero
          ]
  pure khDRep

-- | Sets up a stake pool with coin delegated to it.
--
-- NOTE: This uses the `RegDepositDelegTxCert` for delegating, so it has to be
-- in Conway. The Shelley version of this function would have to separately
-- register the staking credential and then delegate it.
setupPoolWithStake ::
  (ShelleyEraImp era, ConwayEraTxCert era) =>
  Coin ->
  ImpTestM era (KeyHash 'StakePool (EraCrypto era))
setupPoolWithStake delegCoin = do
  khPool <- registerPool
  credDelegatorPayment <- KeyHashObj <$> freshKeyHash
  credDelegatorStaking <- KeyHashObj <$> freshKeyHash
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
  pure khPool

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
  [PredicateFailure (EraRule "LEDGER" era)] ->
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
        [PredicateFailure (EraRule "LEDGER" era)]
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
  (ShelleyEraImp era, ConwayEraTxBody era, HasCallStack) =>
  NE.NonEmpty (ProposalProcedure era) ->
  ImpTestM era (NE.NonEmpty (GovActionId (EraCrypto era)))
submitProposals proposals = do
  tx <- trySubmitProposals proposals >>= expectRightExpr
  let txId = txIdTx tx
  pure $ NE.zipWith (\ix _ -> GovActionId txId (GovActionIx ix)) (0 NE.:| [1 ..]) proposals

-- | Submits a transaction that proposes the given proposal
trySubmitProposal ::
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  ) =>
  ProposalProcedure era ->
  ImpTestM
    era
    ( Either
        [PredicateFailure (EraRule "LEDGER" era)]
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
  ImpTestM era (Either [PredicateFailure (EraRule "LEDGER" era)] (Tx era))
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
  [PredicateFailure (EraRule "LEDGER" era)] ->
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
        [PredicateFailure (EraRule "LEDGER" era)]
        (GovActionId (EraCrypto era))
    )
trySubmitGovAction ga = do
  let mkGovActionId tx = GovActionId (txIdTx tx) (GovActionIx 0)
  fmap mkGovActionId <$> trySubmitGovActions (pure ga)

-- | Submits a transaction that proposes the given governance action
trySubmitGovActions ::
  (ShelleyEraImp era, ConwayEraTxBody era) =>
  NE.NonEmpty (GovAction era) ->
  ImpTestM era (Either [PredicateFailure (EraRule "LEDGER" era)] (Tx era))
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
  pure $ NE.zipWith (\ix _ -> GovActionId txId (GovActionIx ix)) (0 NE.:| [1 ..]) gas

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
  [PredicateFailure (EraRule "LEDGER" era)] ->
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
  Map.lookup coldK committeeCreds `shouldBe` Just Nothing

-- | Test the resignation status for a CC cold key to not be resigned
ccShouldNotBeResigned :: HasCallStack => Credential 'ColdCommitteeRole (EraCrypto era) -> ImpTestM era ()
ccShouldNotBeResigned coldK = do
  committeeCreds <- getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL . csCommitteeCredsL
  Map.lookup coldK committeeCreds `shouldSatisfy` maybe False isJust

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

-- | Checks whether the governance action has enough DRep votes to be accepted in the next
-- epoch. (Note that no other checks execept DRep votes is used)
canGovActionBeDRepAccepted ::
  (HasCallStack, ConwayEraGov era, ConwayEraPParams era) =>
  GovActionId (EraCrypto era) ->
  ImpTestM era Bool
canGovActionBeDRepAccepted gaId = do
  eNo <- getsNES nesELL
  stakeDistr <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosStakeDistrL
  poolDistr <- getsNES nesPdL
  drepDistr <- getsNES $ nesEsL . epochStateDRepPulsingStateL . psDRepDistrG
  drepState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL
  action <- getGovActionState gaId
  enactSt <- getEnactState
  committeeState <- getsNES $ nesEsL . esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL
  let
    ratEnv =
      RatifyEnv
        { reStakePoolDistr = poolDistr
        , reStakeDistr = credMap stakeDistr
        , reDRepState = drepState
        , reDRepDistr = drepDistr
        , reCurrentEpoch = eNo
        , reCommitteeState = committeeState
        }
    ratSt =
      RatifyState
        { rsEnactState = enactSt
        , rsEnacted = mempty
        , rsExpired = mempty
        , rsDelayed = False
        }
  pure $ dRepAccepted ratEnv ratSt action

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
  ImpTestM era ()
resignCommitteeColdKey coldKey = do
  submitTxAnn_ "Resigning Committee Cold key" $
    mkBasicTx mkBasicTxBody
      & bodyTxL . certsTxBodyL
        .~ SSeq.singleton (ResignCommitteeColdTxCert coldKey SNothing)

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
        .~ def
          { dvtCommitteeNormal = 1 %! 1
          , dvtCommitteeNoConfidence = 1 %! 2
          , dvtUpdateToConstitution = 1 %! 2
          }
      & ppCommitteeMaxTermLengthL .~ EpochInterval 20
      & ppGovActionLifetimeL .~ EpochInterval 2
      & ppGovActionDepositL .~ Coin 123
  khDRep <- setupSingleDRep

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

-- | Asserts that the URL of the current constitution is equal to the given
-- string
constitutionShouldBe :: (HasCallStack, ConwayEraGov era) => String -> ImpTestM era ()
constitutionShouldBe cUrl = do
  Constitution {constitutionAnchor = Anchor {anchorUrl}} <-
    getsNES $
      nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . constitutionGovStateL
  anchorUrl `shouldBe` fromJust (textToUrl 64 $ T.pack cUrl)
