{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Conway.Imp.RatifySpec (spec) where

import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams
import Cardano.Ledger.Credential
import Cardano.Ledger.Keys
import Cardano.Ledger.Shelley.LedgerState
import qualified Cardano.Ledger.UMap as UM
import Data.Default.Class (def)
import Data.Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.KeyPair
import Test.Cardano.Ledger.Core.Rational ((%!))
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
spec =
  describe "RATIFY" $ do
    votingSpec
    delayingActionsSpec
    spoVotesForHardForkInitiation

spoVotesForHardForkInitiation ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
spoVotesForHardForkInitiation =
  describe "Counting of SPO votes" $ do
    it "HardForkInitiation" $ do
      (_dRepC, hotCC, _gpi) <- electBasicCommittee
      (spoK1, _paymentC1, _stakingC1) <- setupPoolWithStake $ Coin 1_000
      (spoK2, _paymentC2, _stakingC2) <- setupPoolWithStake $ Coin 1_000
      (_spoK3, _paymentC3, _stakingC3) <- setupPoolWithStake $ Coin 1_000
      (_spoK4, _paymentC4, _stakingC4) <- setupPoolWithStake $ Coin 1_000
      modifyPParams $ ppPoolVotingThresholdsL . pvtHardForkInitiationL .~ 1 %! 2
      protVer <- getsNES $ nesEsL . curPParamsEpochStateL . ppProtocolVersionL
      gai <- submitGovAction $ HardForkInitiation SNothing (majorFollow protVer)
      submitYesVote_ (CommitteeVoter hotCC) gai
      -- 1 % 4 stake yes; 3 % 4 stake no; yes / stake - abstain < 1 % 2
      submitYesVote_ (StakePoolVoter spoK1) gai
      passNEpochs 2
      logRatificationChecks gai
      isSpoAccepted gai `shouldReturn` False
      getLastEnactedHardForkInitiation `shouldReturn` SNothing
      -- 1 % 2 stake yes; 1 % 2 stake no; yes / stake - abstain = 1 % 2
      submitYesVote_ (StakePoolVoter spoK2) gai
      isSpoAccepted gai `shouldReturn` True
      passNEpochs 2
      getLastEnactedHardForkInitiation `shouldReturn` SJust (GovPurposeId gai)
    describe "All gov action other than HardForkInitiation" $ do
      it "NoConfidence" $ do
        (spoK1, _paymentC1, _stakingC1) <- setupPoolWithStake $ Coin 1_000
        (_spoK2, _paymentC2, _stakingC2) <- setupPoolWithStake $ Coin 1_000
        (_spoK3, _paymentC3, _stakingC3) <- setupPoolWithStake $ Coin 1_000
        (_spoK4, _paymentC4, _stakingC4) <- setupPoolWithStake $ Coin 1_000
        modifyPParams $ ppPoolVotingThresholdsL . pvtMotionNoConfidenceL .~ 1 %! 2
        gai <- submitGovAction $ NoConfidence SNothing
        -- 1 % 4 stake yes; 3 % 4 stake abstain; yes / stake - abstain > 1 % 2
        submitYesVote_ (StakePoolVoter spoK1) gai
        passNEpochs 2
        getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId gai)
      it "CommitteeUpdate" $ do
        (spoK1, _paymentC1, _stakingC1) <- setupPoolWithStake $ Coin 1_000
        (_spoK2, _paymentC2, _stakingC2) <- setupPoolWithStake $ Coin 1_000
        (_spoK3, _paymentC3, _stakingC3) <- setupPoolWithStake $ Coin 1_000
        (_spoK4, _paymentC4, _stakingC4) <- setupPoolWithStake $ Coin 1_000
        modifyPParams $ ppPoolVotingThresholdsL . pvtCommitteeNormalL .~ 1 %! 2
        modifyPParams $ ppCommitteeMaxTermLengthL .~ EpochInterval 10
        committeeC <- KeyHashObj <$> freshKeyHash
        gai <-
          submitGovAction $
            UpdateCommittee SNothing mempty (Map.singleton committeeC $ EpochNo 5) $
              1 %! 2
        -- 1 % 4 stake yes; 3 % 4 stake abstain; yes / stake - abstain > 1 % 2
        submitYesVote_ (StakePoolVoter spoK1) gai
        passNEpochs 2
        getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId gai)

votingSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
votingSpec =
  describe "Voting" $ do
    it "SPO needs to vote on security-relevant parameter changes" $ do
      (drep, ccCred, _) <- electBasicCommittee
      (khPool, _, _) <- setupPoolWithStake $ Coin 42_000_000
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
        impAnn "Security group threshold should be 1/2" $
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
    describe "Active voting stake" $ do
      describe "DRep" $ do
        it "UTxOs contribute to active voting stake" $ do
          -- Only modify the applicable thresholds
          modifyPParams $ \pp ->
            pp
              & ppDRepVotingThresholdsL
                .~ def
                  { dvtCommitteeNormal = 51 %! 100
                  , dvtCommitteeNoConfidence = 51 %! 100
                  }
              & ppCommitteeMaxTermLengthL .~ EpochInterval 20
          -- Setup DRep delegation #1
          (drepKH1, stakingKH1, paymentKP1) <- setupSingleDRep 1_000_000
          -- Setup DRep delegation #2
          (_drepKH2, _stakingKH2, _paymentKP2) <- setupSingleDRep 1_000_000
          -- Submit a committee proposal
          cc <- KeyHashObj <$> freshKeyHash
          let addCCAction = UpdateCommittee SNothing mempty (Map.singleton cc 10) (75 %! 100)
          addCCGaid <- submitGovAction addCCAction
          -- Submit the vote
          submitVote_ VoteYes (DRepVoter $ KeyHashObj drepKH1) addCCGaid
          passNEpochs 2
          -- The vote should not result in a ratification
          isDRepAccepted addCCGaid `shouldReturn` False
          getLastEnactedCommittee `shouldReturn` SNothing
          -- Bump up the UTxO delegated
          -- to barely make the threshold (51 %! 100)
          stakingKP1 <- lookupKeyPair stakingKH1
          _ <- sendCoinTo (mkAddr (paymentKP1, stakingKP1)) (inject $ Coin 200_000)
          passNEpochs 2
          -- The same vote should now successfully ratify the proposal
          getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId addCCGaid)
        it "Rewards contribute to active voting stake" $ do
          -- Only modify the applicable thresholds
          modifyPParams $ \pp ->
            pp
              & ppDRepVotingThresholdsL
                .~ def
                  { dvtCommitteeNormal = 51 %! 100
                  , dvtCommitteeNoConfidence = 51 %! 100
                  }
              & ppCommitteeMaxTermLengthL .~ EpochInterval 20
          -- Setup DRep delegation #1
          (drepKH1, stakingKH1, _paymentKP1) <- setupSingleDRep 1_000_000
          -- Setup DRep delegation #2
          (_drepKH2, _stakingKH2, _paymentKP2) <- setupSingleDRep 1_000_000
          -- Submit a committee proposal
          cc <- KeyHashObj <$> freshKeyHash
          let addCCAction = UpdateCommittee SNothing mempty (Map.singleton cc 10) (75 %! 100)
          addCCGaid <- submitGovAction addCCAction
          -- Submit the vote
          submitVote_ VoteYes (DRepVoter $ KeyHashObj drepKH1) addCCGaid
          passNEpochs 2
          -- The vote should not result in a ratification
          isDRepAccepted addCCGaid `shouldReturn` False
          getLastEnactedCommittee `shouldReturn` SNothing
          -- Add to the rewards of the delegator to this DRep
          -- to barely make the threshold (51 %! 100)
          modifyNES $
            nesEsL . epochStateUMapL
              %~ UM.adjust
                (\(UM.RDPair r d) -> UM.RDPair (r <> UM.CompactCoin 200_000) d)
                (KeyHashObj stakingKH1)
                . UM.RewDepUView
          passNEpochs 2
          -- The same vote should now successfully ratify the proposal
          getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId addCCGaid)
      describe "StakePool" $ do
        it "UTxOs contribute to active voting stake" $ do
          -- Only modify the applicable thresholds
          modifyPParams $ \pp ->
            pp
              & ppPoolVotingThresholdsL
                .~ def
                  { pvtCommitteeNormal = 51 %! 100
                  , pvtCommitteeNoConfidence = 51 %! 100
                  }
              & ppCommitteeMaxTermLengthL .~ EpochInterval 20
          -- Setup Pool delegation #1
          (poolKH1, delegatorCPayment1, delegatorCStaking1) <- setupPoolWithStake $ Coin 1_000_000
          -- Setup Pool delegation #2
          (poolKH2, _delegatorCPayment2, _delegatorCStaking2) <- setupPoolWithStake $ Coin 1_000_000
          passEpoch
          -- Submit a committee proposal
          cc <- KeyHashObj <$> freshKeyHash
          let addCCAction = UpdateCommittee SNothing mempty (Map.singleton cc 10) (75 %! 100)
          addCCGaid <- submitGovAction addCCAction
          -- Submit the vote
          submitVote_ VoteYes (StakePoolVoter poolKH1) addCCGaid
          submitVote_ VoteNo (StakePoolVoter poolKH2) addCCGaid
          passNEpochs 2
          -- The vote should not result in a ratification
          logRatificationChecks addCCGaid
          isSpoAccepted addCCGaid `shouldReturn` False
          getLastEnactedCommittee `shouldReturn` SNothing
          -- Bump up the UTxO delegated
          -- to barely make the threshold (51 %! 100)
          _ <-
            sendCoinTo
              (Addr Testnet delegatorCPayment1 (StakeRefBase delegatorCStaking1))
              (Coin 200_000)
          passNEpochs 2
          -- The same vote should now successfully ratify the proposal
          getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId addCCGaid)
        it "Rewards contribute to active voting stake" $ do
          -- Only modify the applicable thresholds
          modifyPParams $ \pp ->
            pp
              & ppPoolVotingThresholdsL
                .~ def
                  { pvtCommitteeNormal = 51 %! 100
                  , pvtCommitteeNoConfidence = 51 %! 100
                  }
              & ppCommitteeMaxTermLengthL .~ EpochInterval 20
          -- Setup Pool delegation #1
          (poolKH1, _delegatorCPayment1, delegatorCStaking1) <- setupPoolWithStake $ Coin 1_000_000
          -- Setup Pool delegation #2
          (poolKH2, _delegatorCPayment2, _delegatorCStaking2) <- setupPoolWithStake $ Coin 1_000_000
          passEpoch
          -- Submit a committee proposal
          cc <- KeyHashObj <$> freshKeyHash
          let addCCAction = UpdateCommittee SNothing mempty (Map.singleton cc 10) (75 %! 100)
          addCCGaid <- submitGovAction addCCAction
          -- Submit the vote
          submitVote_ VoteYes (StakePoolVoter poolKH1) addCCGaid
          submitVote_ VoteNo (StakePoolVoter poolKH2) addCCGaid
          passNEpochs 2
          -- The vote should not result in a ratification
          isSpoAccepted addCCGaid `shouldReturn` False
          getLastEnactedCommittee `shouldReturn` SNothing
          -- Add to the rewards of the delegator to this SPO
          -- to barely make the threshold (51 %! 100)
          modifyNES $
            nesEsL . epochStateUMapL
              %~ UM.adjust
                (\(UM.RDPair r d) -> UM.RDPair (r <> UM.CompactCoin 200_000) d)
                delegatorCStaking1
                . UM.RewDepUView
          passNEpochs 2
          -- The same vote should now successfully ratify the proposal
          getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId addCCGaid)

delayingActionsSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
delayingActionsSpec =
  describe "Delaying actions" $ do
    it "A delaying action delays its child even when both ere proposed and ratified in the same epoch" $ do
      (dRep, committeeMember, _gpi) <- electBasicCommittee
      modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 5
      gai0 <- submitInitConstitutionGovAction
      gai1 <- submitChildConstitutionGovAction gai0
      gai2 <- submitChildConstitutionGovAction gai1
      gai3 <- submitChildConstitutionGovAction gai2
      submitYesVote_ (DRepVoter dRep) gai0
      submitYesVote_ (CommitteeVoter committeeMember) gai0
      submitYesVote_ (DRepVoter dRep) gai1
      submitYesVote_ (CommitteeVoter committeeMember) gai1
      submitYesVote_ (DRepVoter dRep) gai2
      submitYesVote_ (CommitteeVoter committeeMember) gai2
      submitYesVote_ (DRepVoter dRep) gai3
      submitYesVote_ (CommitteeVoter committeeMember) gai3
      passNEpochs 2
      getLastEnactedConstitution `shouldReturn` SJust (GovPurposeId gai0)
      passEpoch
      getLastEnactedConstitution `shouldReturn` SJust (GovPurposeId gai1)
      passEpoch
      getLastEnactedConstitution `shouldReturn` SJust (GovPurposeId gai2)
      passEpoch
      getLastEnactedConstitution `shouldReturn` SJust (GovPurposeId gai3)
      getConstitutionProposals `shouldReturn` Map.empty
    it "A delaying action delays all other actions even when all of them may be ratified in the same epoch" $ do
      (dRep, committeeMember, _gpi) <- electBasicCommittee
      modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 5
      pGai0 <-
        submitParameterChange
          SNothing
          $ def & ppuDRepDepositL .~ SJust (Coin 1_000_000)
      pGai1 <-
        submitParameterChange
          (SJust $ GovPurposeId pGai0)
          $ def & ppuDRepDepositL .~ SJust (Coin 1_000_001)
      pGai2 <-
        submitParameterChange
          (SJust $ GovPurposeId pGai1)
          $ def & ppuDRepDepositL .~ SJust (Coin 1_000_002)
      cGai0 <- submitInitConstitutionGovAction
      cGai1 <- submitChildConstitutionGovAction cGai0
      submitYesVote_ (DRepVoter dRep) cGai0
      submitYesVote_ (CommitteeVoter committeeMember) cGai0
      submitYesVote_ (DRepVoter dRep) cGai1
      submitYesVote_ (CommitteeVoter committeeMember) cGai1
      submitYesVote_ (DRepVoter dRep) pGai0
      submitYesVote_ (CommitteeVoter committeeMember) pGai0
      submitYesVote_ (DRepVoter dRep) pGai1
      submitYesVote_ (CommitteeVoter committeeMember) pGai1
      submitYesVote_ (DRepVoter dRep) pGai2
      submitYesVote_ (CommitteeVoter committeeMember) pGai2
      passNEpochs 2
      getLastEnactedConstitution `shouldReturn` SJust (GovPurposeId cGai0)
      getLastEnactedParameterChange `shouldReturn` SNothing
      passEpoch
      -- here 'ParameterChange' action does not get enacted even though
      -- it is not related, since its priority is 4 while the priority
      -- for 'NewConstitution' is 2, so it gets delayed a second time
      getLastEnactedConstitution `shouldReturn` SJust (GovPurposeId cGai1)
      getConstitutionProposals `shouldReturn` Map.empty
      getLastEnactedParameterChange `shouldReturn` SNothing
      passEpoch
      -- all three actions, pGai0, pGai1 and pGai2, are enacted one
      -- after the other in the same epoch
      getLastEnactedParameterChange `shouldReturn` SJust (GovPurposeId pGai2)
      getParameterChangeProposals `shouldReturn` Map.empty
    describe "An action expires when delayed enough even after being ratified" $ do
      it "Same lineage" $ do
        (dRep, committeeMember, _gpi) <- electBasicCommittee
        gai0 <- submitInitConstitutionGovAction
        gai1 <- submitChildConstitutionGovAction gai0
        gai2 <- submitChildConstitutionGovAction gai1
        gai3 <- submitChildConstitutionGovAction gai2
        submitYesVote_ (DRepVoter dRep) gai0
        submitYesVote_ (CommitteeVoter committeeMember) gai0
        submitYesVote_ (DRepVoter dRep) gai1
        submitYesVote_ (CommitteeVoter committeeMember) gai1
        submitYesVote_ (DRepVoter dRep) gai2
        submitYesVote_ (CommitteeVoter committeeMember) gai2
        submitYesVote_ (DRepVoter dRep) gai3
        submitYesVote_ (CommitteeVoter committeeMember) gai3
        passNEpochs 2
        getLastEnactedConstitution `shouldReturn` SJust (GovPurposeId gai0)
        passEpoch
        getLastEnactedConstitution `shouldReturn` SJust (GovPurposeId gai1)
        passEpoch
        getLastEnactedConstitution `shouldReturn` SJust (GovPurposeId gai2)
        getConstitutionProposals `shouldReturn` Map.empty
        passEpoch
        getLastEnactedConstitution `shouldReturn` SJust (GovPurposeId gai2)
      it "Other lineage" $ do
        (dRep, committeeMember, _gpi) <- electBasicCommittee
        pGai0 <-
          submitParameterChange
            SNothing
            $ def & ppuDRepDepositL .~ SJust (Coin 1_000_000)
        pGai1 <-
          submitParameterChange
            (SJust $ GovPurposeId pGai0)
            $ def & ppuDRepDepositL .~ SJust (Coin 1_000_001)
        pGai2 <-
          submitParameterChange
            (SJust $ GovPurposeId pGai1)
            $ def & ppuDRepDepositL .~ SJust (Coin 1_000_002)
        cGai0 <- submitInitConstitutionGovAction
        cGai1 <- submitChildConstitutionGovAction cGai0
        cGai2 <- submitChildConstitutionGovAction cGai1
        submitYesVote_ (DRepVoter dRep) cGai0
        submitYesVote_ (CommitteeVoter committeeMember) cGai0
        submitYesVote_ (DRepVoter dRep) cGai1
        submitYesVote_ (CommitteeVoter committeeMember) cGai1
        submitYesVote_ (DRepVoter dRep) cGai2
        submitYesVote_ (CommitteeVoter committeeMember) cGai2
        submitYesVote_ (DRepVoter dRep) pGai0
        submitYesVote_ (CommitteeVoter committeeMember) pGai0
        submitYesVote_ (DRepVoter dRep) pGai1
        submitYesVote_ (CommitteeVoter committeeMember) pGai1
        submitYesVote_ (DRepVoter dRep) pGai2
        submitYesVote_ (CommitteeVoter committeeMember) pGai2
        passNEpochs 2
        getLastEnactedConstitution `shouldReturn` SJust (GovPurposeId cGai0)
        getLastEnactedParameterChange `shouldReturn` SNothing
        passEpoch
        getLastEnactedConstitution `shouldReturn` SJust (GovPurposeId cGai1)
        getLastEnactedParameterChange `shouldReturn` SNothing
        passEpoch
        getLastEnactedConstitution `shouldReturn` SJust (GovPurposeId cGai2)
        getConstitutionProposals `shouldReturn` Map.empty
        getLastEnactedParameterChange `shouldReturn` SNothing
        passEpoch
        -- all three actions, pGai0, pGai1 and pGai2, are expired here
        -- and nothing gets enacted
        getLastEnactedParameterChange `shouldReturn` SNothing
        getParameterChangeProposals `shouldReturn` Map.empty
      it "proposals to update the committee get delayed if the expiration exceeds the max term" $ do
        let expectMembers ::
              HasCallStack => Set.Set (Credential 'ColdCommitteeRole (EraCrypto era)) -> ImpTestM era ()
            expectMembers expKhs = do
              committee <-
                getsNES $
                  nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . committeeGovStateL
              let members = Map.keysSet $ foldMap' committeeMembers committee
              impAnn "Expecting committee members" $ members `shouldBe` expKhs
            setPParams :: HasCallStack => ImpTestM era ()
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

        setPParams
        (drep, _, _) <- setupSingleDRep 1_000_000
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
