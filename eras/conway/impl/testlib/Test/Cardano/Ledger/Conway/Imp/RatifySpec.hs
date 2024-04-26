{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Conway.Imp.RatifySpec (
  spec,
  relevantDuringBootstrapSpec,
) where

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
import Cardano.Ledger.Val ((<->))
import Data.Default.Class (def)
import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..))
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
    relevantDuringBootstrapSpec
    votingSpec
    delayingActionsSpec
    spoVotesCommitteeUpdates
    committeeMinSizeAffectsInFlightProposalsSpec
    paramChangeAffectsProposalsSpec
    committeeExpiryResignationDiscountSpec

relevantDuringBootstrapSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
relevantDuringBootstrapSpec =
  spoVotesForHardForkInitiation

committeeExpiryResignationDiscountSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
committeeExpiryResignationDiscountSpec =
  describe "Expired and resigned committee members are discounted from quorum" $ do
    it "Expired" $ do
      modifyPParams $ ppCommitteeMinSizeL .~ 2
      (drep, _, _) <- setupSingleDRep 1_000_000
      -- Elect a committee of 2 members
      committeeColdC1 <- KeyHashObj <$> freshKeyHash
      committeeColdC2 <- KeyHashObj <$> freshKeyHash
      gaiCC <-
        submitGovAction $
          UpdateCommittee
            SNothing
            mempty
            (Map.fromList [(committeeColdC1, EpochNo 10), (committeeColdC2, EpochNo 2)])
            (1 %! 2)
      submitYesVote_ (DRepVoter drep) gaiCC
      passNEpochs 2
      getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId gaiCC)
      committeeHotC1 <- registerCommitteeHotKey committeeColdC1
      _committeeHotC2 <- registerCommitteeHotKey committeeColdC2
      -- Submit a constitution with a CC vote
      (gaiConstitution, _constitution) <- submitConstitution SNothing
      submitYesVote_ (CommitteeVoter committeeHotC1) gaiConstitution
      -- Check for CC acceptance
      ccShouldNotBeExpired committeeColdC2
      isCommitteeAccepted gaiConstitution `shouldReturn` True
      -- expire the second CC
      passNEpochs 2
      -- Check for CC acceptance should fail
      ccShouldBeExpired committeeColdC2
      isCommitteeAccepted gaiConstitution `shouldReturn` False
    it "Resigned" $ do
      modifyPParams $ ppCommitteeMinSizeL .~ 2
      (drep, _, _) <- setupSingleDRep 1_000_000
      -- Elect a committee of 2 members
      committeeColdC1 <- KeyHashObj <$> freshKeyHash
      committeeColdC2 <- KeyHashObj <$> freshKeyHash
      gaiCC <-
        submitGovAction $
          UpdateCommittee
            SNothing
            mempty
            (Map.fromList [(committeeColdC1, EpochNo 10), (committeeColdC2, EpochNo 10)])
            (1 %! 2)
      submitYesVote_ (DRepVoter drep) gaiCC
      passNEpochs 2
      getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId gaiCC)
      committeeHotC1 <- registerCommitteeHotKey committeeColdC1
      _committeeHotC2 <- registerCommitteeHotKey committeeColdC2
      -- Submit a constitution with a CC vote
      (gaiConstitution, _constitution) <- submitConstitution SNothing
      submitYesVote_ (CommitteeVoter committeeHotC1) gaiConstitution
      -- Check for CC acceptance
      ccShouldNotBeResigned committeeColdC2
      isCommitteeAccepted gaiConstitution `shouldReturn` True
      -- Resign the second CC
      resignCommitteeColdKey committeeColdC2 SNothing
      -- Check for CC acceptance should fail
      ccShouldBeResigned committeeColdC2
      isCommitteeAccepted gaiConstitution `shouldReturn` False

paramChangeAffectsProposalsSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
paramChangeAffectsProposalsSpec =
  describe "ParameterChange affects existing proposals" $ do
    let largerThreshold :: UnitInterval
        largerThreshold = 51 %! 100
        smallerThreshold :: UnitInterval
        smallerThreshold = 1 %! 2
    describe "DRep" $ do
      let setThreshold :: UnitInterval -> ImpTestM era ()
          setThreshold threshold = do
            drepVotingThresholds <- getsPParams ppDRepVotingThresholdsL
            modifyPParams $
              ppDRepVotingThresholdsL
                .~ (drepVotingThresholds & dvtCommitteeNormalL .~ threshold)
          enactThreshold ::
            UnitInterval ->
            Credential 'DRepRole (EraCrypto era) ->
            Credential 'HotCommitteeRole (EraCrypto era) ->
            ImpTestM era ()
          enactThreshold threshold drepC hotCommitteeC = do
            drepVotingThresholds <- getsPParams ppDRepVotingThresholdsL
            let paramChange =
                  ParameterChange
                    SNothing
                    ( emptyPParamsUpdate
                        & ppuDRepVotingThresholdsL
                          .~ SJust (drepVotingThresholds & dvtCommitteeNormalL .~ threshold)
                    )
                    SNothing
            pcGai <- submitGovAction paramChange
            submitYesVote_ (DRepVoter drepC) pcGai
            submitYesVote_ (CommitteeVoter hotCommitteeC) pcGai
            passNEpochs 2
          submitTwoExampleProposalsAndVoteOnTheChild ::
            GovPurposeId 'CommitteePurpose era ->
            Credential 'DRepRole (EraCrypto era) ->
            ImpTestM era (GovActionId (EraCrypto era), GovActionId (EraCrypto era))
          submitTwoExampleProposalsAndVoteOnTheChild gpiCC drep = do
            committeeC <- KeyHashObj <$> freshKeyHash
            let updateCC parent = UpdateCommittee parent mempty (Map.singleton committeeC $ EpochNo 5) $ 1 %! 2
            gaiParent <- submitGovAction $ updateCC $ SJust gpiCC
            -- We submit a descendent proposal so that even though it is sufficiently
            -- voted on, it cannot be ratified before the ParameterChange proposal
            -- is enacted.
            gaiChild <- submitGovAction $ updateCC $ SJust $ GovPurposeId gaiParent
            submitYesVote_ (DRepVoter drep) gaiChild
            passEpoch -- Make the votes count
            pure (gaiParent, gaiChild)
      it "Increasing the threshold prevents a hitherto-ratifiable proposal from being ratified" $ do
        (drepC, hotCommitteeC, gpiCC) <- electBasicCommittee
        setThreshold smallerThreshold
        (drep, _, _) <- setupSingleDRep 1_000_000
        (_gaiParent, gaiChild) <- submitTwoExampleProposalsAndVoteOnTheChild gpiCC drep
        isDRepAccepted gaiChild `shouldReturn` True
        enactThreshold largerThreshold drepC hotCommitteeC
        isDRepAccepted gaiChild `shouldReturn` False
      it "Decreasing the threshold ratifies a hitherto-unratifiable proposal" $ do
        (drepC, hotCommitteeC, gpiCC) <- electBasicCommittee
        setThreshold largerThreshold
        (drep, _, _) <- setupSingleDRep 1_000_000
        (gaiParent, gaiChild) <- submitTwoExampleProposalsAndVoteOnTheChild gpiCC drep
        isDRepAccepted gaiChild `shouldReturn` False
        enactThreshold smallerThreshold drepC hotCommitteeC
        isDRepAccepted gaiChild `shouldReturn` True
        -- Not vote on the parent too to make sure both get enacted
        submitYesVote_ (DRepVoter drep) gaiParent
        passNEpochs 2
        getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId gaiParent)
        passEpoch -- UpdateCommittee is a delaying action
        getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId gaiChild)
    describe "SPO" $ do
      let setThreshold :: UnitInterval -> ImpTestM era ()
          setThreshold threshold = do
            poolVotingThresholds <- getsPParams ppPoolVotingThresholdsL
            modifyPParams $
              ppPoolVotingThresholdsL
                .~ (poolVotingThresholds & pvtCommitteeNormalL .~ threshold)
          enactThreshold ::
            UnitInterval ->
            Credential 'DRepRole (EraCrypto era) ->
            Credential 'HotCommitteeRole (EraCrypto era) ->
            ImpTestM era ()
          enactThreshold threshold drepC hotCommitteeC = do
            poolVotingThresholds <- getsPParams ppPoolVotingThresholdsL
            let paramChange =
                  ParameterChange
                    SNothing
                    ( emptyPParamsUpdate
                        & ppuPoolVotingThresholdsL
                          .~ SJust (poolVotingThresholds & pvtCommitteeNormalL .~ threshold)
                    )
                    SNothing
            pcGai <- submitGovAction paramChange
            submitYesVote_ (DRepVoter drepC) pcGai
            submitYesVote_ (CommitteeVoter hotCommitteeC) pcGai
            passNEpochs 2
          submitTwoExampleProposalsAndVoteOnTheChild ::
            GovPurposeId 'CommitteePurpose era ->
            KeyHash 'StakePool (EraCrypto era) ->
            KeyHash 'StakePool (EraCrypto era) ->
            Credential 'DRepRole (EraCrypto era) ->
            ImpTestM era (GovActionId (EraCrypto era), GovActionId (EraCrypto era))
          submitTwoExampleProposalsAndVoteOnTheChild gpiCC poolKH1 poolKH2 drepC = do
            committeeC <- KeyHashObj <$> freshKeyHash
            let updateCC parent = UpdateCommittee parent mempty (Map.singleton committeeC $ EpochNo 5) $ 1 %! 2
            gaiParent <- submitGovAction $ updateCC $ SJust gpiCC
            -- We submit a descendent proposal so that even though it is sufficiently
            -- voted on, it cannot be ratified before the ParameterChange proposal
            -- is enacted.
            gaiChild <- submitGovAction $ updateCC $ SJust $ GovPurposeId gaiParent
            submitYesVote_ (DRepVoter drepC) gaiChild
            submitYesVote_ (StakePoolVoter poolKH1) gaiChild
            -- Abstained stake is not counted in the total stake in case of SPOs
            submitVote_ VoteNo (StakePoolVoter poolKH2) gaiChild
            passEpoch -- Make the votes count do
            pure (gaiParent, gaiChild)
      it "Increasing the threshold prevents a hitherto-ratifiable proposal from being ratified" $ do
        (drepC, hotCommitteeC, gpiCC) <- electBasicCommittee
        setThreshold smallerThreshold
        (poolKH1, _paymentC1, _stakingC1) <- setupPoolWithStake $ Coin 1_000_000
        (poolKH2, _paymentC2, _stakingC2) <- setupPoolWithStake $ Coin 1_000_000
        passEpoch -- Make the new pool distribution count
        (_gaiParent, gaiChild) <- submitTwoExampleProposalsAndVoteOnTheChild gpiCC poolKH1 poolKH2 drepC
        isSpoAccepted gaiChild `shouldReturn` True
        enactThreshold largerThreshold drepC hotCommitteeC
        isSpoAccepted gaiChild `shouldReturn` False
      it "Decreasing the threshold ratifies a hitherto-unratifiable proposal" $ do
        (drepC, hotCommitteeC, gpiCC) <- electBasicCommittee
        setThreshold largerThreshold
        (poolKH1, _paymentC1, _stakingC1) <- setupPoolWithStake $ Coin 1_000_000
        (poolKH2, _paymentC2, _stakingC2) <- setupPoolWithStake $ Coin 1_000_000
        (gaiParent, gaiChild) <- submitTwoExampleProposalsAndVoteOnTheChild gpiCC poolKH1 poolKH2 drepC
        isSpoAccepted gaiChild `shouldReturn` False
        enactThreshold smallerThreshold drepC hotCommitteeC
        isSpoAccepted gaiChild `shouldReturn` True
        -- Not vote on the parent too to make sure both get enacted
        submitYesVote_ (DRepVoter drepC) gaiParent
        submitYesVote_ (StakePoolVoter poolKH1) gaiParent
        logRatificationChecks gaiParent
        logRatificationChecks gaiChild
        passNEpochs 2
        getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId gaiParent)
        passEpoch -- UpdateCommittee is a delaying action
        getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId gaiChild)
    it "A parent ParameterChange proposal can prevent its child from being enacted" $ do
      (hotCommitteeC :| _) <- registerInitialCommittee
      (drepC, _, _) <- setupSingleDRep 1_000_000
      -- Setup one other DRep with equal stake
      _ <- setupSingleDRep 1_000_000
      -- Set a smaller DRep threshold
      drepVotingThresholds <- getsPParams ppDRepVotingThresholdsL
      modifyPParams $
        ppDRepVotingThresholdsL
          .~ (drepVotingThresholds & dvtPPGovGroupL .~ smallerThreshold)
      -- Submit a parent-child sequence of ParameterChange proposals and vote on
      -- both equally, so that both may be ratified. But, the parent increases
      -- the threshold, and it should prevent the child from being ratified.
      let paramChange parent threshold =
            ParameterChange
              parent
              ( emptyPParamsUpdate
                  & ppuDRepVotingThresholdsL
                    .~ SJust (drepVotingThresholds & dvtPPGovGroupL .~ threshold)
              )
              SNothing
      parentGai <- submitGovAction $ paramChange SNothing largerThreshold
      childGai <- submitGovAction $ paramChange (SJust $ GovPurposeId parentGai) smallerThreshold
      submitYesVote_ (DRepVoter drepC) parentGai
      submitYesVote_ (CommitteeVoter hotCommitteeC) parentGai
      submitYesVote_ (DRepVoter drepC) childGai
      submitYesVote_ (CommitteeVoter hotCommitteeC) childGai
      passEpoch
      logRatificationChecks parentGai
      logRatificationChecks childGai
      isDRepAccepted parentGai `shouldReturn` True
      isDRepAccepted childGai `shouldReturn` True
      passEpoch
      getLastEnactedParameterChange `shouldReturn` SJust (GovPurposeId parentGai)
      Map.member (GovPurposeId childGai) <$> getParameterChangeProposals `shouldReturn` True
      isDRepAccepted childGai `shouldReturn` False

committeeMinSizeAffectsInFlightProposalsSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
committeeMinSizeAffectsInFlightProposalsSpec =
  describe "CommitteeMinSize affects in-flight proposals" $ do
    let setCommitteeMinSize n = modifyPParams $ ppCommitteeMinSizeL .~ n
        submitATreasuryWithdrawal = do
          rewardAccount <- registerRewardAccount
          submitTreasuryWithdrawals [(rewardAccount, Coin 1_000)]
    it "TreasuryWithdrawal fails to ratify due to an increase in CommitteeMinSize" $ do
      (hotCommitteeC :| _) <- registerInitialCommittee
      (drepC, _, _) <- setupSingleDRep 1_000_000
      passEpoch
      setCommitteeMinSize 1
      gaiTW <- submitATreasuryWithdrawal
      submitYesVote_ (CommitteeVoter hotCommitteeC) gaiTW
      submitYesVote_ (DRepVoter drepC) gaiTW
      isCommitteeAccepted gaiTW `shouldReturn` True
      gaiPC <-
        submitParameterChange SNothing $
          emptyPParamsUpdate
            & ppuCommitteeMinSizeL .~ SJust 2
      submitYesVote_ (CommitteeVoter hotCommitteeC) gaiPC
      submitYesVote_ (DRepVoter drepC) gaiPC
      treasury <- getsNES $ nesEsL . esAccountStateL . asTreasuryL
      passNEpochs 2
      -- The ParameterChange prevents the TreasuryWithdrawal from being enacted,
      -- because it has higher priority.
      getLastEnactedParameterChange `shouldReturn` SJust (GovPurposeId gaiPC)
      isCommitteeAccepted gaiTW `shouldReturn` False
      currentProposalsShouldContain gaiTW
      getsNES (nesEsL . esAccountStateL . asTreasuryL) `shouldReturn` treasury
    it "TreasuryWithdrawal ratifies due to a decrease in CommitteeMinSize" $ do
      (drepC, hotCommitteeC, gpiCC) <- electBasicCommittee
      treasury <- getsNES $ nesEsL . esAccountStateL . asTreasuryL
      gaiTW <- submitATreasuryWithdrawal
      submitYesVote_ (CommitteeVoter hotCommitteeC) gaiTW
      submitYesVote_ (DRepVoter drepC) gaiTW
      setCommitteeMinSize 2
      isCommitteeAccepted gaiTW `shouldReturn` False
      passNEpochs 2
      getsNES (nesEsL . esAccountStateL . asTreasuryL) `shouldReturn` treasury
      -- We do not enact the ParameterChange here because that does not pass
      -- ratification as the CC size is smaller than MinSize.
      -- We instead just add another Committee member to reach the CommitteeMinSize.
      coldCommitteeC' <- KeyHashObj <$> freshKeyHash
      gaiCC <-
        submitGovAction $
          UpdateCommittee
            (SJust gpiCC)
            Set.empty
            (Map.singleton coldCommitteeC' $ EpochNo 10)
            (1 %! 2)
      submitYesVote_ (DRepVoter drepC) gaiCC
      passNEpochs 2
      _hotCommitteeC' <- registerCommitteeHotKey coldCommitteeC'
      isCommitteeAccepted gaiTW `shouldReturn` True
      passNEpochs 2
      getsNES (nesEsL . esAccountStateL . asTreasuryL) `shouldReturn` (treasury <-> Coin 1_000)

spoVotesCommitteeUpdates ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
spoVotesCommitteeUpdates =
  describe "Counting of SPO votes" $ do
    describe "All gov action other than HardForkInitiation" $ do
      it "NoConfidence" $ do
        (spoK1, _, _) <- setupPoolWithStake $ Coin 1_000
        _ <- setupPoolWithStake $ Coin 1_000
        _ <- setupPoolWithStake $ Coin 1_000
        _ <- setupPoolWithStake $ Coin 1_000
        modifyPParams $ \pp ->
          pp
            & ppPoolVotingThresholdsL . pvtMotionNoConfidenceL .~ 1 %! 2
            & ppDRepVotingThresholdsL .~ def
        gai <- submitGovAction $ NoConfidence SNothing
        -- 1 % 4 stake yes; 3 % 4 stake abstain; yes / stake - abstain > 1 % 2
        submitYesVote_ (StakePoolVoter spoK1) gai
        passNEpochs 2
        getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId gai)
      it "CommitteeUpdate" $ do
        (spoK1, _, _) <- setupPoolWithStake $ Coin 1_000
        _ <- setupPoolWithStake $ Coin 1_000
        _ <- setupPoolWithStake $ Coin 1_000
        _ <- setupPoolWithStake $ Coin 1_000
        modifyPParams $ \pp ->
          pp
            & ppPoolVotingThresholdsL . pvtCommitteeNormalL .~ 1 %! 2
            & ppDRepVotingThresholdsL .~ def

        committeeC <- KeyHashObj <$> freshKeyHash
        gai <-
          submitGovAction $
            UpdateCommittee SNothing mempty (Map.singleton committeeC $ EpochNo 5) $
              1 %! 2
        -- 1 % 4 stake yes; 3 % 4 stake abstain; yes / stake - abstain > 1 % 2
        submitYesVote_ (StakePoolVoter spoK1) gai
        passNEpochs 2
        getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId gai)

spoVotesForHardForkInitiation ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
spoVotesForHardForkInitiation =
  describe "Counting of SPO votes" $ do
    it "HardForkInitiation" $ do
      (hotCC :| _) <- registerInitialCommittee
      (spoK1, _, _) <- setupPoolWithStake $ Coin 1_000
      (spoK2, _, _) <- setupPoolWithStake $ Coin 1_000
      _ <- setupPoolWithStake $ Coin 1_000
      _ <- setupPoolWithStake $ Coin 1_000
      modifyPParams $ ppPoolVotingThresholdsL . pvtHardForkInitiationL .~ 1 %! 2
      protVer <- getProtVer
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

votingSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
votingSpec =
  describe "Voting" $ do
    it "SPO needs to vote on security-relevant parameter changes" $ do
      (ccCred :| _) <- registerInitialCommittee
      (drep, _, _) <- setupSingleDRep 1_000_000
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
          -- Setup DRep delegation #1
          (drep1, KeyHashObj stakingKH1, paymentKP1) <- setupSingleDRep 1_000_000
          -- Setup DRep delegation #2
          _ <- setupSingleDRep 1_000_000
          -- Submit a committee proposal
          cc <- KeyHashObj <$> freshKeyHash
          let addCCAction = UpdateCommittee SNothing mempty (Map.singleton cc 10) (75 %! 100)
          addCCGaid <- submitGovAction addCCAction
          -- Submit the vote
          submitVote_ VoteYes (DRepVoter drep1) addCCGaid
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
          -- Setup DRep delegation #1
          (drep1, staking1, _) <- setupSingleDRep 1_000_000
          -- Setup DRep delegation #2
          _ <- setupSingleDRep 1_000_000
          -- Submit a committee proposal
          cc <- KeyHashObj <$> freshKeyHash
          let addCCAction = UpdateCommittee SNothing mempty (Map.singleton cc 10) (75 %! 100)
          addCCGaid <- submitGovAction addCCAction
          -- Submit the vote
          submitVote_ VoteYes (DRepVoter drep1) addCCGaid
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
                staking1
                . UM.RewDepUView
          passNEpochs 2
          -- The same vote should now successfully ratify the proposal
          getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId addCCGaid)
        it "Rewards contribute to active voting stake even in the absence of StakeDistr" $ do
          -- Only modify the applicable thresholds
          modifyPParams $ \pp ->
            pp
              & ppDRepVotingThresholdsL
                .~ def
                  { dvtCommitteeNormal = 51 %! 100
                  , dvtCommitteeNoConfidence = 51 %! 100
                  }
              & ppGovActionDepositL .~ Coin 1_000_000
              & ppPoolDepositL .~ Coin 200_000
              & ppEMaxL .~ EpochInterval 5
              & ppGovActionLifetimeL .~ EpochInterval 5
          -- Setup DRep delegation #1
          (drepKH1, stakingKH1) <- setupDRepWithoutStake
          -- Add rewards to delegation #1
          submitAndExpireProposalToMakeReward 1_000_000 $ KeyHashObj stakingKH1
          lookupReward (KeyHashObj stakingKH1) `shouldReturn` Coin 1_000_000
          -- Setup DRep delegation #2
          (_drepKH2, stakingKH2) <- setupDRepWithoutStake
          -- Add rewards to delegation #2
          submitAndExpireProposalToMakeReward 1_000_000 $ KeyHashObj stakingKH2
          lookupReward (KeyHashObj stakingKH2) `shouldReturn` Coin 1_000_000
          -- Submit a committee proposal
          cc <- KeyHashObj <$> freshKeyHash
          let addCCAction =
                UpdateCommittee
                  SNothing
                  mempty
                  (Map.singleton cc $ 10 + 2 * 5) -- some + 2 * GovActionLifetime
                  (75 %! 100)
          addCCGaid <- submitGovAction addCCAction
          -- Submit the vote
          submitVote_ VoteYes (DRepVoter $ KeyHashObj drepKH1) addCCGaid
          passNEpochs 2
          -- The vote should not result in a ratification
          isDRepAccepted addCCGaid `shouldReturn` False
          getLastEnactedCommittee `shouldReturn` SNothing
          -- Increase the rewards of the delegator to this DRep
          -- to barely make the threshold (51 %! 100)
          registerAndRetirePoolToMakeReward $ KeyHashObj stakingKH1
          passEpoch
          lookupReward (KeyHashObj stakingKH1) `shouldReturn` Coin 1_200_000
          isDRepAccepted addCCGaid `shouldReturn` True
          -- The same vote should now successfully ratify the proposal
          passEpoch
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
              & ppDRepVotingThresholdsL .~ def
          -- Setup Pool delegation #1
          (poolKH1, delegatorCPayment1, delegatorCStaking1) <- setupPoolWithStake $ Coin 1_000_000
          -- Setup Pool delegation #2
          (poolKH2, _, _) <- setupPoolWithStake $ Coin 1_000_000
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
              & ppDRepVotingThresholdsL .~ def
          -- Setup Pool delegation #1
          (poolKH1, _, delegatorCStaking1) <- setupPoolWithStake $ Coin 1_000_000
          -- Setup Pool delegation #2
          (poolKH2, _, _) <- setupPoolWithStake $ Coin 1_000_000
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
        it "Rewards contribute to active voting stake even in the absence of StakeDistr" $ do
          -- Only modify the applicable thresholds
          modifyPParams $ \pp ->
            pp
              & ppPoolVotingThresholdsL
                .~ def
                  { pvtCommitteeNormal = 51 %! 100
                  , pvtCommitteeNoConfidence = 51 %! 100
                  }
              & ppGovActionDepositL .~ Coin 1_000_000
              & ppPoolDepositL .~ Coin 200_000
              & ppEMaxL .~ EpochInterval 5
              & ppGovActionLifetimeL .~ EpochInterval 5
              & ppDRepVotingThresholdsL .~ def
          -- Setup Pool delegation #1
          (poolKH1, delegatorCStaking1) <- setupPoolWithoutStake
          -- Add rewards to delegation #1
          submitAndExpireProposalToMakeReward 1_000_000 delegatorCStaking1
          lookupReward delegatorCStaking1 `shouldReturn` Coin 1_000_000
          -- Setup Pool delegation #2
          (poolKH2, delegatorCStaking2) <- setupPoolWithoutStake
          -- Add rewards to delegation #2
          submitAndExpireProposalToMakeReward 1_000_000 delegatorCStaking2
          lookupReward delegatorCStaking2 `shouldReturn` Coin 1_000_000
          -- Submit a committee proposal
          cc <- KeyHashObj <$> freshKeyHash
          let addCCAction =
                UpdateCommittee
                  SNothing
                  mempty
                  (Map.singleton cc $ 10 + 2 * 5) -- some + 2 * GovActionLifetime
                  (75 %! 100)
          addCCGaid <- submitGovAction addCCAction
          -- Submit the vote
          submitVote_ VoteYes (StakePoolVoter poolKH1) addCCGaid
          submitVote_ VoteNo (StakePoolVoter poolKH2) addCCGaid
          passNEpochs 2
          -- The vote should not result in a ratification
          isSpoAccepted addCCGaid `shouldReturn` False
          getLastEnactedCommittee `shouldReturn` SNothing
          logRatificationChecks addCCGaid
          -- Add to the rewards of the delegator to this SPO
          -- to barely make the threshold (51 %! 100)
          registerAndRetirePoolToMakeReward $ delegatorCStaking1
          passEpoch
          lookupReward delegatorCStaking1 `shouldReturn` Coin 1_200_000
          -- The same vote should now successfully ratify the proposal
          -- NOTE: It takes 2 epochs for SPO votes as opposed to 1 epoch
          -- for DRep votes to ratify a proposal.
          passNEpochs 2
          getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId addCCGaid)

delayingActionsSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
delayingActionsSpec =
  describe "Delaying actions" $ do
    it "A delaying action delays its child even when both ere proposed and ratified in the same epoch" $ do
      (committeeMember :| _) <- registerInitialCommittee
      (dRep, _, _) <- setupSingleDRep 1_000_000
      gai0 <- submitConstitutionGovAction SNothing
      gai1 <- submitConstitutionGovAction $ SJust gai0
      gai2 <- submitConstitutionGovAction $ SJust gai1
      gai3 <- submitConstitutionGovAction $ SJust gai2
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
    it
      "A delaying action delays all other actions even when all of them may be ratified in the same epoch"
      $ do
        (committeeMember :| _) <- registerInitialCommittee
        (dRep, _, _) <- setupSingleDRep 1_000_000
        pGai0 <-
          submitParameterChange
            SNothing
            $ def & ppuDRepDepositL .~ SJust (Coin 1_000_000)
        pGai1 <-
          submitParameterChange
            (SJust pGai0)
            $ def & ppuDRepDepositL .~ SJust (Coin 1_000_001)
        pGai2 <-
          submitParameterChange
            (SJust pGai1)
            $ def & ppuDRepDepositL .~ SJust (Coin 1_000_002)
        cGai0 <- submitConstitutionGovAction SNothing
        cGai1 <- submitConstitutionGovAction $ SJust cGai0
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
        (committeeMember :| _) <- registerInitialCommittee
        (dRep, _, _) <- setupSingleDRep 1_000_000
        modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
        gai0 <- submitConstitutionGovAction SNothing
        gai1 <- submitConstitutionGovAction $ SJust gai0
        gai2 <- submitConstitutionGovAction $ SJust gai1
        gai3 <- submitConstitutionGovAction $ SJust gai2
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
        (committeeMember :| _) <- registerInitialCommittee
        (dRep, _, _) <- setupSingleDRep 1_000_000
        modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
        pGai0 <-
          submitParameterChange
            SNothing
            $ def & ppuDRepDepositL .~ SJust (Coin 1_000_000)
        pGai1 <-
          submitParameterChange
            (SJust pGai0)
            $ def & ppuDRepDepositL .~ SJust (Coin 1_000_001)
        pGai2 <-
          submitParameterChange
            (SJust pGai1)
            $ def & ppuDRepDepositL .~ SJust (Coin 1_000_002)
        cGai0 <- submitConstitutionGovAction SNothing
        cGai1 <- submitConstitutionGovAction $ SJust cGai0
        cGai2 <- submitConstitutionGovAction $ SJust cGai1
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
        (drep, _, _) <- setupSingleDRep 1_000_000
        maxTermLength <-
          getsNES $
            nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . curPParamsGovStateL . ppCommitteeMaxTermLengthL

        void registerInitialCommittee
        initialMembers <- getCommitteeMembers

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
                SNothing
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
          submitYesVote_ (DRepVoter drep) govIdConst1
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
          submitYesVote_ (DRepVoter drep) govIdConst2
          hks <- traverse registerCommitteeHotKey (Set.toList membersExceedingExpiry)
          traverse_ (\m -> submitYesVote_ (CommitteeVoter m) govIdConst2) hks

          passEpoch >> passEpoch
          curConstitution <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
          curConstitution `shouldBe` constitution
