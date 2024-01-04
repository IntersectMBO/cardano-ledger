{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conway.Imp.GovSpec where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.CertState (vsNumDormantEpochsL)
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules (ConwayGovPredFailure (..))
import Cardano.Ledger.Credential (Credential (KeyHashObj))
import Cardano.Ledger.DRep (drepExpiryL)
import Cardano.Ledger.Keys (
  KeyHash,
  KeyRole (..),
 )
import Cardano.Ledger.Plutus (Language (..))
import Cardano.Ledger.Shelley.LedgerState
import Control.Monad (replicateM_)
import Control.State.Transition.Extended (PredicateFailure)
import Data.Default.Class (Default (..))
import Data.Foldable (Foldable (..), traverse_)
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysSucceeds)
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ( ConwayEraImp era
  , GovState era ~ ConwayGovState era
  , Inject (ConwayGovPredFailure era) (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  SpecWith (ImpTestState era)
spec =
  describe "GOV" $ do
    describe "Hardfork is the first one (doesn't have a PrevGovActionId) " $ do
      it "Hardfork minorFollow" (firstHardForkFollows minorFollow)
      it "Hardfork majorFollow" (firstHardForkFollows majorFollow)
      it "Hardfork cantFollow" firstHardForkCantFollow
    describe "Hardfork is the second one (has a PrevGovActionId)" $ do
      it "Hardfork minorFollow" (secondHardForkFollows minorFollow)
      it "Hardfork majorFollow" (secondHardForkFollows majorFollow)
      it "Hardfork cantFollow" secondHardForkCantFollow
    describe "Voting" $ do
      context "fails for" $ do
        it "expired gov-actions" $ do
          -- Voting after the 3rd epoch should fail
          modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
          khDRep <- setupSingleDRep
          (govActionId, _) <- submitConstitution SNothing
          passEpoch
          passEpoch
          passEpoch
          let voter = DRepVoter $ KeyHashObj khDRep
          submitFailingVote
            voter
            govActionId
            [ inject $
                VotingOnExpiredGovAction @era $
                  Map.singleton govActionId voter
            ]
        it "non-existent gov-actions" $ do
          khDRep <- setupSingleDRep
          (govActionId, _) <- submitConstitution SNothing
          let voter = DRepVoter $ KeyHashObj khDRep
              dummyGaid = govActionId {gaidGovActionIx = GovActionIx 99} -- non-existent `GovActionId`
          submitFailingVote
            voter
            dummyGaid
            [inject $ GovActionsDoNotExist @era $ Set.singleton dummyGaid]

    describe "Constitution proposals" $ do
      context "accepted for" $
        it "empty PrevGovId before the first constitution is enacted" $ do
          --  Initial proposal does not need a PrevGovActionId but after it is enacted, the
          --  following ones are not
          _ <- submitConstitution SNothing
          -- Until the first proposal is enacted all proposals with empty PrevGovActionIds are valid
          void $ submitConstitution SNothing

      context "rejected for" $ do
        it "empty PrevGovId after the first constitution was enacted" $ do
          (govActionId, _) <- submitConstitution SNothing
          modifyNES $ \nes ->
            nes
              & nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgEnactStateL . ensPrevConstitutionL
                .~ SJust (PrevGovActionId govActionId) -- Add first proposal to PrevGovActionIds in enacted state
              & nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgProposalsL
                .~ def -- Remove all proposals, so that the lookup only succeeds for enacted state.
                -- Once a proposal with a purpose has been enacted, following proposals can no
                -- longer have empty PrevGovActionIds
          constitution <- newConstitution
          let invalidNewConstitutionGovAction =
                NewConstitution
                  SNothing
                  constitution
          invalidNewConstitutionProposal <- proposalWithRewardAccount invalidNewConstitutionGovAction
          submitFailingProposal
            invalidNewConstitutionProposal
            [ inject $ InvalidPrevGovActionId invalidNewConstitutionProposal
            ]
        it "invalid index in PrevGovActionId" $ do
          (govActionId, _) <- submitConstitution SNothing
          constitution <- newConstitution
          let invalidPrevGovActionId =
                -- Expected Ix = 0
                PrevGovActionId (govActionId {gaidGovActionIx = GovActionIx 1})
              invalidNewConstitutionGovAction =
                NewConstitution
                  (SJust invalidPrevGovActionId)
                  constitution
          invalidNewConstitutionProposal <- proposalWithRewardAccount invalidNewConstitutionGovAction
          submitFailingProposal
            invalidNewConstitutionProposal
            [ inject $ InvalidPrevGovActionId invalidNewConstitutionProposal
            ]
        it "valid PrevGovActionId but invalid purpose" $ do
          (govActionId, _) <- submitConstitution SNothing
          let invalidNoConfidenceAction =
                NoConfidence $ SJust $ PrevGovActionId govActionId
          invalidNoConfidenceProposal <- proposalWithRewardAccount invalidNoConfidenceAction

          submitFailingProposal
            invalidNoConfidenceProposal
            [ inject $ InvalidPrevGovActionId invalidNoConfidenceProposal
            ]

    describe "Constitution" $ do
      it "submitted successfully with valid PrevGovActionId" $ do
        modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 1

        curConstitution <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
        initialPulser <- getsNES $ newEpochStateGovStateL . cgDRepPulsingStateL
        initialEnactState <- getsNES $ newEpochStateGovStateL . cgEnactStateL

        (govActionId, _) <- submitConstitution SNothing
        curConstitution' <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
        impAnn "Constitution has not been enacted yet" $
          curConstitution' `shouldBe` curConstitution

        ConwayGovState expectedProposals expectedEnactState expectedPulser <- getsNES newEpochStateGovStateL

        impAnn "EnactState reflects the submitted governance action" $ do
          let enactStateWithChildren =
                initialEnactState
                  & ensPrevGovActionIdsChildrenL . pgacConstitutionL %~ Set.insert (PrevGovActionId govActionId)
          expectedEnactState `shouldBe` enactStateWithChildren

        impAnn "Proposals contain the submitted proposal" $
          expectedProposals `shouldSatisfy` \props -> govActionId `elem` proposalsIds props

        impAnn "Pulser has not changed" $
          expectedPulser `shouldBe` initialPulser

        passEpoch >> passEpoch
        impAnn "Proposal gets removed after expiry" $ do
          ConwayGovState _ _ pulser <- getsNES newEpochStateGovStateL
          let ratifyState = extractDRepPulsingState pulser
          rsRemoved ratifyState `shouldBe` Set.singleton govActionId

      it "submitted and enacted when voted on" $ do
        (dRep, committeeMember) <- electBasicCommittee
        (govActionId, constitution) <- submitConstitution SNothing

        ConwayGovState proposalsBeforeVotes enactStateBeforeVotes pulserBeforeVotes <- getsNES newEpochStateGovStateL
        submitYesVote_ (DRepVoter dRep) govActionId
        submitYesVote_ (CommitteeVoter committeeMember) govActionId
        ConwayGovState proposalsAfterVotes enactStateAfterVotes pulserAfterVotes <- getsNES newEpochStateGovStateL

        impAnn "Votes are recorded in the proposals" $ do
          let proposalsWithVotes =
                proposalsAddVote
                  (CommitteeVoter committeeMember)
                  VoteYes
                  govActionId
                  ( proposalsAddVote
                      (DRepVoter dRep)
                      VoteYes
                      govActionId
                      proposalsBeforeVotes
                  )
          proposalsAfterVotes `shouldBe` proposalsWithVotes

        impAnn "EnactState has not changed" $
          enactStateAfterVotes `shouldBe` enactStateBeforeVotes

        impAnn "Pulser has not changed" $
          pulserAfterVotes `shouldBe` pulserBeforeVotes

        passEpoch

        impAnn "New constitution is not enacted after one epoch" $ do
          constitutionAfterOneEpoch <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
          constitutionAfterOneEpoch `shouldBe` def

        impAnn "Pulser should reflect the constitution to be enacted" $ do
          ConwayGovState _ _ pulser <- getsNES newEpochStateGovStateL
          let ratifyState = extractDRepPulsingState pulser
          rsEnacted ratifyState `shouldBe` Set.singleton govActionId
          rsEnactState ratifyState ^. ensConstitutionL `shouldBe` constitution

        passEpoch

        impAnn "Constitution is enacted after two epochs" $ do
          curConstitution <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
          curConstitution `shouldBe` constitution

        impAnn "Pulser is reset" $ do
          ConwayGovState _ _ pulser <- getsNES newEpochStateGovStateL
          let pulserRatifyState = extractDRepPulsingState pulser
          rsEnacted pulserRatifyState `shouldBe` Set.empty
          enactState <- getsNES $ newEpochStateGovStateL . cgEnactStateL
          rsEnactState pulserRatifyState `shouldBe` enactState

      xit "policy is respected by proposals" $ do
        let
          scriptHash = hashScript @era $ alwaysSucceeds @'PlutusV3 0
          wrongScriptHash = hashScript @era $ alwaysSucceeds @'PlutusV3 1
        modifyNES $
          nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgEnactStateL . ensConstitutionL
            .~ Constitution def (SJust scriptHash)
        pp <- getsNES $ nesEsL . curPParamsEpochStateL
        impAnn "ParameterChange with correct policy succeeds" $ do
          let
            pparamsUpdate =
              def
                & ppuCommitteeMinSizeL .~ SJust 1
          rewardAccount <- registerRewardAccount
          void $
            submitProposal
              ProposalProcedure
                { pProcReturnAddr = rewardAccount
                , pProcGovAction = ParameterChange SNothing pparamsUpdate (SJust scriptHash)
                , pProcDeposit = pp ^. ppGovActionDepositL
                , pProcAnchor = def
                }

        impAnn "TreasuryWithdrawals with correct policy succeeds" $ do
          rewardAccount <- registerRewardAccount
          let
            withdrawals =
              Map.fromList
                [ (rewardAccount, Coin 1000)
                ]
          void $
            submitProposal
              ProposalProcedure
                { pProcReturnAddr = rewardAccount
                , pProcGovAction = TreasuryWithdrawals withdrawals (SJust scriptHash)
                , pProcDeposit = pp ^. ppGovActionDepositL
                , pProcAnchor = def
                }

        impAnn "ParameterChange with invalid policy succeeds" $ do
          rewardAccount <- registerRewardAccount
          let
            pparamsUpdate =
              def
                & ppuCommitteeMinSizeL .~ SJust 2
          res <-
            trySubmitProposal
              ProposalProcedure
                { pProcReturnAddr = rewardAccount
                , pProcGovAction = ParameterChange SNothing pparamsUpdate (SJust wrongScriptHash)
                , pProcDeposit = pp ^. ppGovActionDepositL
                , pProcAnchor = def
                }
          res `shouldBeLeft` []

        impAnn "TreasuryWithdrawals with invalid policy succeeds" $ do
          rewardAccount <- registerRewardAccount
          let
            withdrawals =
              Map.fromList
                [ (rewardAccount, Coin 1000)
                ]
          res <-
            trySubmitProposal
              ProposalProcedure
                { pProcReturnAddr = rewardAccount
                , pProcGovAction = TreasuryWithdrawals withdrawals (SJust wrongScriptHash)
                , pProcDeposit = pp ^. ppGovActionDepositL
                , pProcAnchor = def
                }
          res `shouldBeLeft` []

    describe "DRep expiry" $ do
      it "is updated based on to number of dormant epochs" $ do
        modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
        drep <- setupSingleDRep

        expectNumDormantEpochs 0

        -- epoch 0
        _ <- submitConstitution SNothing
        expectCurrentProposals
        expectNoPulserProposals
        expectNumDormantEpochs 0
        expectExtraDRepExpiry drep 0

        passEpoch
        -- epoch 1
        expectCurrentProposals
        expectPulserProposals
        expectNumDormantEpochs 1
        expectExtraDRepExpiry drep 0

        passEpoch
        -- epoch 2
        expectCurrentProposals
        expectPulserProposals
        expectNumDormantEpochs 1
        expectExtraDRepExpiry drep 0

        passEpoch
        -- epoch 3
        expectCurrentProposals
        expectPulserProposals
        expectNumDormantEpochs 1
        expectExtraDRepExpiry drep 0

        passEpoch
        -- epoch 4, proposals expired
        expectNoCurrentProposals
        expectNoPulserProposals
        expectNumDormantEpochs 1
        expectExtraDRepExpiry drep 0

        passEpoch
        -- epoch 5
        expectNoCurrentProposals
        expectNoPulserProposals
        expectNumDormantEpochs 2
        expectExtraDRepExpiry drep 0

        _ <- submitConstitution SNothing
        -- number of dormant epochs is added to the drep expiry and the reset
        expectNumDormantEpochs 0
        expectExtraDRepExpiry drep 2

        passEpoch
        -- epoch 6
        expectCurrentProposals
        expectPulserProposals
        expectNumDormantEpochs 1
        expectExtraDRepExpiry drep 2

    describe "Committee actions validation" $ do
      it "proposals to update the committee get delayed if the expiration exceeds the max term" $ do
        setPParams
        drep <- setupSingleDRep
        maxTermLength <-
          getsNES $
            nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . curPParamsGovStateL . ppCommitteeMaxTermLengthL

        (initialMembers, govIdCom1) <-
          impAnn "Initial committee is enacted" $ do
            c1 <- freshKeyHash
            c2 <- freshKeyHash
            currentEpoch <- getsNES nesELL
            let maxExpiry = addEpochInterval currentEpoch maxTermLength
            let initialMembers = [(c1, maxExpiry), (c2, maxExpiry)]
            govIdCom1 <-
              electCommittee
                SNothing
                drep
                Set.empty
                initialMembers
            passEpoch >> passEpoch
            expectMembers $ Map.keysSet initialMembers
            pure (Map.keysSet initialMembers, govIdCom1)

        (membersExceedingExpiry, exceedingExpiry) <-
          impAnn "Committee with members exceeding the maxTerm is not enacted" $ do
            -- submit a proposal for adding two members to the committee,
            -- one of which has a max term exceeding the maximum
            c3 <- freshKeyHash
            c4 <- freshKeyHash
            currentEpoch <- getsNES nesELL
            let exceedingExpiry = addEpochInterval (addEpochInterval currentEpoch maxTermLength) (EpochInterval 7)
            let membersExceedingExpiry = [(c3, exceedingExpiry), (c4, addEpochInterval currentEpoch maxTermLength)]
            _ <-
              electCommittee
                (SJust govIdCom1)
                drep
                Set.empty
                membersExceedingExpiry
            passEpoch >> passEpoch
            -- the new committee has not been enacted
            expectMembers initialMembers
            pure (Map.keysSet membersExceedingExpiry, exceedingExpiry)

        -- other actions get ratified and enacted
        govIdConst1 <- impAnn "Other actions are ratified and enacted" $ do
          (govIdConst1, constitution) <- submitConstitution SNothing
          submitYesVote_ (DRepVoter (KeyHashObj drep)) govIdConst1
          hks <- traverse registerCommitteeHotKey (Set.toList initialMembers)
          traverse_ (\m -> submitYesVote_ (CommitteeVoter (KeyHashObj m)) govIdConst1) hks

          passEpoch >> passEpoch
          curConstitution <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
          curConstitution `shouldBe` constitution
          pure govIdConst1

        -- after enough epochs pass, the expiration of the new members becomes acceptable
        -- and the new committee is enacted
        impAnn "New committee is enacted" $ do
          currentEpoch <- getsNES nesELL
          let delta =
                fromIntegral (unEpochNo exceedingExpiry)
                  - fromIntegral (unEpochNo (addEpochInterval currentEpoch maxTermLength))
          replicateM_ delta passEpoch

          -- pass one more epoch after ratification, in order to be enacted
          passEpoch
          expectMembers $ initialMembers <> membersExceedingExpiry

        impAnn "New committee can vote" $ do
          (govIdConst2, constitution) <- submitConstitution $ SJust (PrevGovActionId govIdConst1)
          submitYesVote_ (DRepVoter (KeyHashObj drep)) govIdConst2
          hks <- traverse registerCommitteeHotKey (Set.toList membersExceedingExpiry)
          traverse_ (\m -> submitYesVote_ (CommitteeVoter (KeyHashObj m)) govIdConst2) hks

          passEpoch >> passEpoch
          curConstitution <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
          curConstitution `shouldBe` constitution
  where
    expectMembers ::
      HasCallStack => Set.Set (KeyHash 'ColdCommitteeRole (EraCrypto era)) -> ImpTestM era ()
    expectMembers expKhs = do
      committee <-
        getsNES $
          nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgEnactStateL . ensCommitteeL
      let members = Map.keysSet $ foldMap' committeeMembers committee
      impAnn "Expecting committee members" $ members `shouldBe` Set.map KeyHashObj expKhs

    expectNumDormantEpochs :: HasCallStack => EpochNo -> ImpTestM era ()
    expectNumDormantEpochs expected = do
      nd <-
        getsNES $
          nesEsL . esLStateL . lsCertStateL . certVStateL . vsNumDormantEpochsL
      nd `shouldBeExpr` expected

    expectExtraDRepExpiry ::
      HasCallStack => KeyHash 'DRepRole (EraCrypto era) -> EpochNo -> ImpTestM era ()
    expectExtraDRepExpiry kh expected = do
      drepActivity <-
        getsNES $
          nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . curPParamsGovStateL . ppDRepActivityL
      dsMap <-
        getsNES $
          nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL @era
      let ds = Map.lookup (KeyHashObj kh) dsMap
      (^. drepExpiryL)
        <$> ds
          `shouldBe` Just (addEpochInterval expected drepActivity)

    expectCurrentProposals :: HasCallStack => ImpTestM era ()
    expectCurrentProposals = do
      props <- currentProposals
      assertBool "Expected proposals in current gov state" (not (SSeq.null props))

    expectNoCurrentProposals :: HasCallStack => ImpTestM era ()
    expectNoCurrentProposals = do
      props <- currentProposals
      assertBool "Expected no proposals in current gov state" (SSeq.null props)

    expectPulserProposals :: HasCallStack => ImpTestM era ()
    expectPulserProposals = do
      props <- lastEpochProposals
      assertBool "Expected proposals in the pulser" (not (SSeq.null props))

    expectNoPulserProposals :: HasCallStack => ImpTestM era ()
    expectNoPulserProposals = do
      props <- lastEpochProposals
      assertBool "Expected no proposals in the pulser" (SSeq.null props)

    currentProposals =
      proposalsIds
        <$> getsNES (nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . proposalsGovStateL @era)
    lastEpochProposals =
      proposalsIds
        <$> getsNES
          ( nesEsL
              . esLStateL
              . lsUTxOStateL
              . utxosGovStateL
              . drepPulsingStateGovStateL @era
              . pulsingStateSnapshotL
              . psProposalsL'
          )

submitConstitution ::
  forall era.
  ConwayEraImp era =>
  StrictMaybe (PrevGovActionId 'ConstitutionPurpose (EraCrypto era)) ->
  ImpTestM era (GovActionId (EraCrypto era), Constitution era)
submitConstitution prevGovId = do
  constitution <- newConstitution
  let constitutionAction =
        NewConstitution
          prevGovId
          constitution
  govActionId <- submitGovAction constitutionAction
  pure (govActionId, constitution)

newConstitution :: Era era => ImpTestM era (Constitution era)
newConstitution = do
  constitutionHash <- freshSafeHash
  pure $
    Constitution
      ( Anchor
          (fromJust $ textToUrl 64 "constitution.0")
          constitutionHash
      )
      SNothing

proposalWithRewardAccount ::
  forall era.
  ConwayEraImp era =>
  GovAction era ->
  ImpTestM era (ProposalProcedure era)
proposalWithRewardAccount action = do
  rewardAccount <- registerRewardAccount
  pure
    ProposalProcedure
      { pProcDeposit = mempty
      , pProcReturnAddr = rewardAccount
      , pProcGovAction = action
      , pProcAnchor = def
      }

pulsingStateSnapshotL :: Lens' (DRepPulsingState era) (PulsingSnapshot era)
pulsingStateSnapshotL = lens getter setter
  where
    getter (DRComplete x _) = x
    getter state = fst (finishDRepPulser state)
    setter (DRComplete _ y) snap = DRComplete snap y
    setter state snap = DRComplete snap $ snd $ finishDRepPulser state

-- | Accesses the same data as psProposalsL, but converts from (StrictSeq (GovActionState era)) to (Proposals era)
psProposalsL' :: Lens' (PulsingSnapshot era) (Proposals era)
psProposalsL' =
  lens
    (fromGovActionStateSeq . psProposals)
    (\x y -> x {psProposals = proposalsActions y})

setPParams ::
  forall era.
  ( HasCallStack
  , ConwayEraImp era
  ) =>
  ImpTestM era ()
setPParams = do
  modifyPParams $ \pp ->
    pp
      & ppDRepVotingThresholdsL
        .~ def
          { dvtCommitteeNormal = 1 %! 1
          , dvtCommitteeNoConfidence = 1 %! 2
          , dvtUpdateToConstitution = 1 %! 2
          }
      & ppCommitteeMaxTermLengthL .~ EpochInterval 10
      & ppGovActionLifetimeL .~ EpochInterval 100
      & ppGovActionDepositL .~ Coin 123

-- =========================================================
-- Proposing a HardFork should always use a new ProtVer that
-- can follow the one installed in the previous HardFork action.

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

-- | Tests the first hardfork in the Conway era where the PrevGovActionID is SNothing
firstHardForkFollows ::
  forall era.
  (ShelleyEraImp era, ConwayEraTxBody era) =>
  (ProtVer -> ProtVer) ->
  ImpTestM era ()
firstHardForkFollows computeNewFromOld = do
  protVer <- getsNES $ nesEsL . curPParamsEpochStateL . ppProtocolVersionL
  submitGovAction_ $ HardForkInitiation SNothing (computeNewFromOld protVer)

-- | Negative (deliberatey failing) first hardfork in the Conway era where the PrevGovActionID is SNothing
firstHardForkCantFollow ::
  forall era.
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  , -- , GovState era ~ ConwayGovState era
    Inject (ConwayGovPredFailure era) (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  ImpTestM era ()
firstHardForkCantFollow = do
  rewardAccount <- registerRewardAccount
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  let protver0 = pp ^. ppProtocolVersionL
      protver1 = minorFollow protver0
      protver2 = cantFollow protver1
  submitFailingProposal
    ( ProposalProcedure
        { pProcDeposit = pp ^. ppGovActionDepositL
        , pProcReturnAddr = rewardAccount
        , pProcGovAction = HardForkInitiation SNothing protver2
        , pProcAnchor = def
        }
    )
    [inject @(ConwayGovPredFailure era) (ProposalCantFollow SNothing protver2 protver0)]

-- | Tests a second hardfork in the Conway era where the PrevGovActionID is SJust
secondHardForkFollows ::
  forall era.
  (ShelleyEraImp era, ConwayEraTxBody era) =>
  (ProtVer -> ProtVer) ->
  ImpTestM era ()
secondHardForkFollows computeNewFromOld = do
  protver0 <- getsNES $ nesEsL . curPParamsEpochStateL . ppProtocolVersionL
  let protver1 = minorFollow protver0
      protver2 = computeNewFromOld protver1
  gaid1 <- submitGovAction $ HardForkInitiation SNothing protver1
  submitGovAction_ $ HardForkInitiation (SJust (PrevGovActionId gaid1)) protver2

-- | Negative (deliberatey failing) first hardfork in the Conway era where the PrevGovActionID is SJust
secondHardForkCantFollow ::
  forall era.
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  , -- , GovState era ~ ConwayGovState era
    Inject (ConwayGovPredFailure era) (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  ImpTestM era ()
secondHardForkCantFollow = do
  rewardAccount <- registerRewardAccount
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  let protver0 = pp ^. ppProtocolVersionL
      protver1 = minorFollow protver0
      protver2 = cantFollow protver1
  gaid1 <-
    submitProposal $
      ProposalProcedure
        { pProcDeposit = pp ^. ppGovActionDepositL
        , pProcReturnAddr = rewardAccount
        , pProcGovAction = HardForkInitiation SNothing protver1
        , pProcAnchor = def
        }
  submitFailingProposal
    ( ProposalProcedure
        { pProcDeposit = pp ^. ppGovActionDepositL
        , pProcReturnAddr = rewardAccount
        , pProcGovAction = HardForkInitiation (SJust (PrevGovActionId gaid1)) protver2
        , pProcAnchor = def
        }
    )
    [inject @(ConwayGovPredFailure era) (ProposalCantFollow (SJust (PrevGovActionId gaid1)) protver2 protver1)]
