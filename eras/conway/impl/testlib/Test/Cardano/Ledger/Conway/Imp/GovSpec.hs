{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conway.Imp.GovSpec where

import Cardano.Ledger.Allegra.Scripts (
  pattern RequireAllOf,
  pattern RequireAnyOf,
  pattern RequireMOf,
  pattern RequireSignature,
 )
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.CertState (vsNumDormantEpochsL)
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams (dvtCommitteeNoConfidenceL, pvtCommitteeNoConfidenceL, pvtPPSecurityGroupL)
import Cardano.Ledger.Conway.Rules (ConwayGovPredFailure (..))
import Cardano.Ledger.Credential (Credential (KeyHashObj))
import Cardano.Ledger.DRep (drepExpiryL)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Shelley.LedgerState
import Control.Monad (replicateM_)
import Control.State.Transition.Extended (PredicateFailure)
import Data.Default.Class (Default (..))
import Data.Foldable (Foldable (..), traverse_)
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro
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
    describe "Hardfork is the first one (doesn't have a GovPurposeId) " $ do
      it "Hardfork minorFollow" (firstHardForkFollows minorFollow)
      it "Hardfork majorFollow" (firstHardForkFollows majorFollow)
      it "Hardfork cantFollow" firstHardForkCantFollow
    describe "Hardfork is the second one (has a GovPurposeId)" $ do
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
      it "Motion of no confidence can be passed" $ do
        modifyPParams $ \pp ->
          pp
            & ppDRepVotingThresholdsL . dvtCommitteeNoConfidenceL .~ 1 %! 2
            & ppPoolVotingThresholdsL . pvtCommitteeNoConfidenceL .~ 1 %! 2
            & ppCommitteeMaxTermLengthL .~ EpochInterval 200
        let
          getCommittee =
            getsNES $
              nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgEnactStateL . ensCommitteeL
          assertNoCommittee :: HasCallStack => ImpTestM era ()
          assertNoCommittee =
            do
              committee <- getCommittee
              impAnn "There should not be a committee" $ committee `shouldBe` SNothing
        assertNoCommittee
        khCC <- freshKeyHash
        drep <- setupSingleDRep
        let committeeMap =
              Map.fromList
                [ (KeyHashObj khCC, EpochNo 50)
                ]
        prevGaidCommittee@(GovPurposeId gaidCommittee) <-
          electCommittee
            SNothing
            drep
            mempty
            committeeMap
        khSPO <- setupPoolWithStake $ Coin 42_000_000
        logStakeDistr
        submitYesVote_ (StakePoolVoter khSPO) gaidCommittee
        replicateM_ 4 passEpoch
        impAnn "Committee should be elected" $ do
          committee <- getCommittee
          committee `shouldBe` SJust (Committee committeeMap $ 1 %! 2)
        pp <- getsNES $ nesEsL . curPParamsEpochStateL
        returnAddr <- registerRewardAccount
        gaidNoConf <-
          submitProposal $
            ProposalProcedure
              { pProcReturnAddr = returnAddr
              , pProcGovAction = NoConfidence (SJust prevGaidCommittee)
              , pProcDeposit = pp ^. ppGovActionDepositL
              , pProcAnchor = def
              }
        submitYesVote_ (StakePoolVoter khSPO) gaidNoConf
        submitYesVote_ (DRepVoter $ KeyHashObj drep) gaidNoConf
        replicateM_ 4 passEpoch
        assertNoCommittee
      it "SPO needs to vote on security-relevant parameter changes" $ do
        (drep, ccCred) <- electBasicCommittee
        khPool <- setupPoolWithStake $ Coin 42_000_000
        initMinFeeA <- getsNES $ nesEsL . curPParamsEpochStateL . ppMinFeeAL
        gaidThreshold <- impAnn "Update StakePool thresholds" $ do
          pp <- getsNES $ nesEsL . curPParamsEpochStateL
          (pp ^. ppPoolVotingThresholdsL . pvtPPSecurityGroupL) `shouldBe` minBound
          rew <- registerRewardAccount
          let ppUpdate =
                emptyPParamsUpdate
                  & ppuPoolVotingThresholdsL
                    .~ SJust
                      PoolVotingThresholds
                        { pvtPPSecurityGroup = 1 %! 2
                        , pvtMotionNoConfidence = 1 %! 2
                        , pvtHardForkInitiation = 1 %! 2
                        , pvtCommitteeNormal = 1 %! 2
                        , pvtCommitteeNoConfidence = 1 %! 2
                        }
                  & ppuGovActionLifetimeL .~ SJust (EpochInterval 100)
          gaidThreshold <-
            submitProposal $
              ProposalProcedure
                { pProcReturnAddr = rew
                , pProcGovAction = ParameterChange SNothing ppUpdate SNothing
                , pProcDeposit = pp ^. ppGovActionDepositL
                , pProcAnchor = def
                }
          submitYesVote_ (DRepVoter drep) gaidThreshold
          submitYesVote_ (CommitteeVoter ccCred) gaidThreshold
          logAcceptedRatio gaidThreshold
          pure gaidThreshold
        passEpoch
        logAcceptedRatio gaidThreshold
        passEpoch
        let newMinFeeA = Coin 12_345
        gaidMinFee <- do
          pp <- getsNES $ nesEsL . curPParamsEpochStateL
          (pp ^. ppPoolVotingThresholdsL . pvtPPSecurityGroupL) `shouldBe` (1 %! 2)
          rew <- registerRewardAccount
          gaidMinFee <-
            submitProposal $
              ProposalProcedure
                { pProcReturnAddr = rew
                , pProcGovAction =
                    ParameterChange
                      (SJust (GovPurposeId gaidThreshold))
                      ( emptyPParamsUpdate
                          & ppuMinFeeAL .~ SJust newMinFeeA
                      )
                      SNothing
                , pProcDeposit = pp ^. ppGovActionDepositL
                , pProcAnchor = def
                }
          submitYesVote_ (DRepVoter drep) gaidMinFee
          submitYesVote_ (CommitteeVoter ccCred) gaidMinFee
          pure gaidMinFee
        passEpoch
        logAcceptedRatio gaidMinFee
        passEpoch
        do
          pp <- getsNES $ nesEsL . curPParamsEpochStateL
          (pp ^. ppMinFeeAL) `shouldBe` initMinFeeA
          submitYesVote_ (StakePoolVoter khPool) gaidMinFee
        passEpoch
        logStakeDistr
        logAcceptedRatio gaidMinFee
        logRatificationChecks gaidMinFee
        passEpoch
        pp <- getsNES $ nesEsL . curPParamsEpochStateL
        (pp ^. ppMinFeeAL) `shouldBe` newMinFeeA

    describe "Constitution proposals" $ do
      context "accepted for" $
        it "empty PrevGovId before the first constitution is enacted" $ do
          --  Initial proposal does not need a GovPurposeId but after it is enacted, the
          --  following ones are not
          _ <- submitConstitution SNothing
          -- Until the first proposal is enacted all proposals with empty GovPurposeIds are valid
          void $ submitConstitution SNothing

      context "rejected for" $ do
        it "empty PrevGovId after the first constitution was enacted" $ do
          (dRep, committeeMember) <- electBasicCommittee
          (govActionId, _constitution) <- submitConstitution SNothing
          submitYesVote_ (DRepVoter dRep) govActionId
          submitYesVote_ (CommitteeVoter committeeMember) govActionId
          passNEpochs 2
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
        it "invalid index in GovPurposeId" $ do
          (_dRep, _committeeMember) <- electBasicCommittee
          (govActionId, _constitution) <- submitConstitution SNothing
          passNEpochs 2
          constitution <- newConstitution
          let invalidPrevGovActionId =
                -- Expected Ix = 0
                GovPurposeId (govActionId {gaidGovActionIx = GovActionIx 1})
              invalidNewConstitutionGovAction =
                NewConstitution
                  (SJust invalidPrevGovActionId)
                  constitution
          invalidNewConstitutionProposal <- proposalWithRewardAccount invalidNewConstitutionGovAction
          submitFailingProposal
            invalidNewConstitutionProposal
            [ inject $ InvalidPrevGovActionId invalidNewConstitutionProposal
            ]
        it "valid GovPurposeId but invalid purpose" $ do
          (_dRep, _committeeMember) <- electBasicCommittee
          (govActionId, _constitution) <- submitConstitution SNothing
          passNEpochs 2
          let invalidNoConfidenceAction =
                NoConfidence $ SJust $ GovPurposeId govActionId
          invalidNoConfidenceProposal <- proposalWithRewardAccount invalidNoConfidenceAction

          submitFailingProposal
            invalidNoConfidenceProposal
            [ inject $ InvalidPrevGovActionId invalidNoConfidenceProposal
            ]

    describe "Constitution" $ do
      it "submitted successfully with valid GovPurposeId" $ do
        modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 1

        curConstitution <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
        initialPulser <- getsNES $ newEpochStateGovStateL . cgDRepPulsingStateL
        initialEnactState <- getsNES $ newEpochStateGovStateL . cgEnactStateL

        (govActionId, _) <- submitConstitution SNothing
        curConstitution' <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
        impAnn "Constitution has not been enacted yet" $
          curConstitution' `shouldBe` curConstitution

        ConwayGovState expectedProposals expectedEnactState expectedPulser <-
          getsNES newEpochStateGovStateL

        impAnn "EnactState reflects the submitted governance action" $ do
          expectedEnactState `shouldBe` initialEnactState

        impAnn "Proposals contain the submitted proposal" $
          expectedProposals `shouldSatisfy` \props -> govActionId `elem` proposalsIds props

        impAnn "Pulser has not changed" $
          expectedPulser `shouldBe` initialPulser

        passEpoch >> passEpoch
        impAnn "Proposal gets removed after expiry" $ do
          ConwayGovState _ _ pulser <- getsNES newEpochStateGovStateL
          let ratifyState = extractDRepPulsingState pulser
          rsExpired ratifyState `shouldBe` Set.singleton govActionId

      it "submitted and enacted when voted on" $ do
        (dRep, committeeMember) <- electBasicCommittee
        (govActionId, constitution) <- submitConstitution SNothing

        ConwayGovState proposalsBeforeVotes enactStateBeforeVotes pulserBeforeVotes <-
          getsNES newEpochStateGovStateL
        submitYesVote_ (DRepVoter dRep) govActionId
        submitYesVote_ (CommitteeVoter committeeMember) govActionId
        ConwayGovState proposalsAfterVotes enactStateAfterVotes pulserAfterVotes <-
          getsNES newEpochStateGovStateL

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
          gasId <$> rsEnacted ratifyState `shouldBe` Seq.singleton govActionId
          rsEnactState ratifyState ^. ensConstitutionL `shouldBe` constitution

        passEpoch

        impAnn "Constitution is enacted after two epochs" $ do
          curConstitution <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
          curConstitution `shouldBe` constitution

        impAnn "Pulser is reset" $ do
          ConwayGovState _ _ pulser <- getsNES newEpochStateGovStateL
          let pulserRatifyState = extractDRepPulsingState pulser
          rsEnacted pulserRatifyState `shouldBe` Seq.empty
          enactState <- getsNES $ newEpochStateGovStateL . cgEnactStateL
          rsEnactState pulserRatifyState `shouldBe` enactState

      it "policy is respected by proposals" $ do
        keyHash <- freshKeyHash
        scriptHash <- impAddNativeScript $ RequireAllOf (SSeq.singleton (RequireSignature keyHash))
        wrongScriptHash <-
          impAddNativeScript $
            RequireMOf 1 $
              SSeq.fromList [RequireAnyOf mempty, RequireAllOf mempty]
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
          submitProposal_
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
          submitProposal_
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
          res
            `shouldBeLeft` [inject $ InvalidPolicyHash @era (SJust wrongScriptHash) (SJust scriptHash)]

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
          res
            `shouldBeLeft` [inject $ InvalidPolicyHash @era (SJust wrongScriptHash) (SJust scriptHash)]

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
            let initialMembers = [(KeyHashObj c1, maxExpiry), (KeyHashObj c2, maxExpiry)]
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
            let membersExceedingExpiry = [(KeyHashObj c3, exceedingExpiry), (KeyHashObj c4, addEpochInterval currentEpoch maxTermLength)]
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
          traverse_ (\m -> submitYesVote_ (CommitteeVoter m) govIdConst1) hks

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
          (govIdConst2, constitution) <- submitConstitution $ SJust (GovPurposeId govIdConst1)
          submitYesVote_ (DRepVoter (KeyHashObj drep)) govIdConst2
          hks <- traverse registerCommitteeHotKey (Set.toList membersExceedingExpiry)
          traverse_ (\m -> submitYesVote_ (CommitteeVoter m) govIdConst2) hks

          passEpoch >> passEpoch
          curConstitution <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
          curConstitution `shouldBe` constitution
  where
    expectMembers ::
      HasCallStack => Set.Set (Credential 'ColdCommitteeRole (EraCrypto era)) -> ImpTestM era ()
    expectMembers expKhs = do
      committee <-
        getsNES $
          nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgEnactStateL . ensCommitteeL
      let members = Map.keysSet $ foldMap' committeeMembers committee
      impAnn "Expecting committee members" $ members `shouldBe` expKhs

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
      fmap gasId . psProposals
        <$> getsNES
          ( nesEsL
              . esLStateL
              . lsUTxOStateL
              . utxosGovStateL
              . drepPulsingStateGovStateL @era
              . pulsingStateSnapshotL
          )

submitConstitution ::
  forall era.
  ConwayEraImp era =>
  StrictMaybe (GovPurposeId 'ConstitutionPurpose era) ->
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
      { pProcDeposit = Coin 123
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
    [inject (ProposalCantFollow @era SNothing protver2 protver0)]

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
  submitGovAction_ $ HardForkInitiation (SJust (GovPurposeId gaid1)) protver2

-- | Negative (deliberatey failing) first hardfork in the Conway era where the PrevGovActionID is SJust
secondHardForkCantFollow ::
  forall era.
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  , Inject (ConwayGovPredFailure era) (PredicateFailure (EraRule "LEDGER" era))
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
        , pProcGovAction = HardForkInitiation (SJust (GovPurposeId gaid1)) protver2
        , pProcAnchor = def
        }
    )
    [inject (ProposalCantFollow @era (SJust (GovPurposeId gaid1)) protver2 protver1)]
