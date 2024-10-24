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
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Credential
import Cardano.Ledger.DRep
import Cardano.Ledger.Keys
import Cardano.Ledger.Shelley.HardForks (bootstrapPhase)
import Cardano.Ledger.Shelley.LedgerState
import qualified Cardano.Ledger.UMap as UM
import Cardano.Ledger.Val (zero, (<->))
import Data.Default.Class (def)
import Data.Foldable
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import qualified Data.Sequence.Strict as SSeq
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
spec = do
  relevantDuringBootstrapSpec
  votingSpec
  delayingActionsSpec
  spoVotesCommitteeUpdates
  committeeMinSizeAffectsInFlightProposalsSpec
  paramChangeAffectsProposalsSpec
  committeeExpiryResignationDiscountSpec
  committeeMaxTermLengthSpec

relevantDuringBootstrapSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
relevantDuringBootstrapSpec = do
  spoVotesForHardForkInitiation
  initiateHardForkWithLessThanMinimalCommitteeSize
  spoAndCCVotingSpec
  it "Many CC Cold Credentials map to the same Hot Credential act as many votes" $ do
    hotCred NE.:| _ <- registerInitialCommittee
    (dRep, _, _) <- setupSingleDRep =<< uniformRM (10_000_000, 1_000_000_000)
    Positive deposit <- arbitrary
    gaId <- submitParameterChange SNothing $ def & ppuDRepDepositL .~ SJust (Coin deposit)
    submitYesVote_ (CommitteeVoter hotCred) gaId
    whenPostBootstrap $ submitYesVote_ (DRepVoter dRep) gaId
    passNEpochs 2
    logAcceptedRatio gaId
    getLastEnactedParameterChange `shouldReturn` SNothing
    -- Make sure all committee members authorize the same hot credential that just voted:
    committeeMembers' <- Set.toList <$> getCommitteeMembers
    case committeeMembers' of
      x : xs -> void $ registerCommitteeHotKeys (pure hotCred) $ x NE.:| xs
      _ -> error "Expected an initial committee"
    passNEpochs 2
    getLastEnactedParameterChange `shouldReturn` SJust (GovPurposeId gaId)

initiateHardForkWithLessThanMinimalCommitteeSize ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
initiateHardForkWithLessThanMinimalCommitteeSize =
  it "Hard Fork can still be initiated with less than minimal committee size" $ do
    hotCs <- registerInitialCommittee
    (spoK1, _, _) <- setupPoolWithStake $ Coin 3_000_000_000
    passEpoch
    modifyPParams $ ppCommitteeMinSizeL .~ 2
    committeeMembers' <- Set.toList <$> getCommitteeMembers
    committeeMember <- elements committeeMembers'
    anchor <- arbitrary
    mHotCred <- resignCommitteeColdKey committeeMember anchor
    protVer <- getProtVer
    gai <- submitGovAction $ HardForkInitiation SNothing (majorFollow protVer)
    submitYesVoteCCs_ (maybe NE.toList (\hotCred -> NE.filter (/= hotCred)) mHotCred hotCs) gai
    submitYesVote_ (StakePoolVoter spoK1) gai
    if bootstrapPhase protVer
      then do
        isCommitteeAccepted gai `shouldReturn` True
        passNEpochs 2
        getLastEnactedHardForkInitiation `shouldReturn` SJust (GovPurposeId gai)
      else do
        isCommitteeAccepted gai `shouldReturn` False
        passNEpochs 2
        getLastEnactedHardForkInitiation `shouldReturn` SNothing

spoAndCCVotingSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
spoAndCCVotingSpec = do
  describe "When CC expired" $ do
    let expireCommitteeMembers = do
          hotCs <- registerInitialCommittee
          -- TODO: change the maxtermlength to make the test faster
          EpochInterval committeeMaxTermLength <-
            getsNES $
              nesEsL . curPParamsEpochStateL . ppCommitteeMaxTermLengthL
          passNEpochs $ fromIntegral committeeMaxTermLength
          ms <- getCommitteeMembers
          -- Make sure that committee expired
          forM_ ms ccShouldBeExpired
          pure hotCs
    it "SPOs alone can't enact hard-fork" $ do
      hotCs <- expireCommitteeMembers
      (spoC, _, _) <- setupPoolWithStake $ Coin 1_000_000_000
      protVer <- getProtVer

      gai <- submitGovAction $ HardForkInitiation SNothing (majorFollow protVer)

      submitYesVote_ (StakePoolVoter spoC) gai
      -- CC members expired so their votes don't count - we are stuck!
      submitYesVoteCCs_ hotCs gai

      passNEpochs 2

      getLastEnactedHardForkInitiation `shouldReturn` SNothing
      getProtVer `shouldReturn` protVer
    it "SPOs alone can't enact security group parameter change" $ do
      void expireCommitteeMembers
      (spoC, _, _) <- setupPoolWithStake $ Coin 1_000_000_000
      initialRefScriptBaseFee <- getsPParams ppMinFeeRefScriptCostPerByteL

      gid <-
        submitParameterChange SNothing $
          emptyPParamsUpdate
            & ppuMinFeeRefScriptCostPerByteL .~ SJust (25 %! 2)

      submitYesVote_ (StakePoolVoter spoC) gid

      passNEpochs 2

      getLastEnactedParameterChange `shouldReturn` SNothing
      getsPParams ppMinFeeRefScriptCostPerByteL `shouldReturn` initialRefScriptBaseFee
  describe "When CC threshold is 0" $ do
    -- During the bootstrap phase, proposals that modify the committee are not allowed,
    -- hence we need to directly set the threshold for the initial members
    let
      modifyCommittee f = modifyNES $ \nes ->
        nes
          & newEpochStateGovStateL . committeeGovStateL %~ f
          & newEpochStateDRepPulsingStateL %~ modifyDRepPulser
        where
          modifyDRepPulser pulser =
            case finishDRepPulser pulser of
              (snapshot, rState) -> DRComplete snapshot (rState & rsEnactStateL . ensCommitteeL %~ f)
    it "SPOs alone can enact hard-fork during bootstrap" $ do
      (spoC, _, _) <- setupPoolWithStake $ Coin 1_000_000_000
      protVer <- getProtVer
      nextMajorVersion <- succVersion $ pvMajor protVer
      let nextProtVer = protVer {pvMajor = nextMajorVersion}
      modifyCommittee $ fmap (committeeThresholdL .~ 0 %! 1)

      gai <- submitGovAction $ HardForkInitiation SNothing (majorFollow protVer)

      submitYesVote_ (StakePoolVoter spoC) gai

      passNEpochs 2

      if bootstrapPhase protVer
        then do
          getLastEnactedHardForkInitiation `shouldReturn` SJust (GovPurposeId gai)
          getProtVer `shouldReturn` nextProtVer
        else do
          getLastEnactedHardForkInitiation `shouldReturn` SNothing
          getProtVer `shouldReturn` protVer
    it "SPOs alone can enact security group parameter change during bootstrap" $ do
      (spoC, _, _) <- setupPoolWithStake $ Coin 1_000_000_000
      protVer <- getProtVer
      initialRefScriptBaseFee <- getsPParams ppMinFeeRefScriptCostPerByteL
      modifyCommittee $ fmap (committeeThresholdL .~ 0 %! 1)

      gai <-
        submitParameterChange SNothing $
          emptyPParamsUpdate
            & ppuMinFeeRefScriptCostPerByteL .~ SJust (25 %! 2)

      submitYesVote_ (StakePoolVoter spoC) gai

      passNEpochs 2

      newRefScriptBaseFee <- getsPParams ppMinFeeRefScriptCostPerByteL
      if bootstrapPhase protVer
        then do
          getLastEnactedParameterChange `shouldReturn` SJust (GovPurposeId gai)
          newRefScriptBaseFee `shouldBe` (25 %! 2)
        else do
          getLastEnactedParameterChange `shouldReturn` SNothing
          newRefScriptBaseFee `shouldBe` initialRefScriptBaseFee

committeeExpiryResignationDiscountSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
committeeExpiryResignationDiscountSpec =
  describe "Expired and resigned committee members are discounted from quorum" $ do
    it "Expired" $ do
      modifyPParams $ ppCommitteeMinSizeL .~ 2
      (drep, _, _) <- setupSingleDRep 1_000_000
      (spoC, _, _) <- setupPoolWithStake $ Coin 42_000_000
      -- Elect a committee of 2 members
      committeeColdC1 <- KeyHashObj <$> freshKeyHash
      committeeColdC2 <- KeyHashObj <$> freshKeyHash
      gaiCC <-
        submitUpdateCommittee
          Nothing
          mempty
          [ (committeeColdC1, EpochInterval 10)
          , (committeeColdC2, EpochInterval 2)
          ]
          (1 %! 2)
      submitYesVote_ (DRepVoter drep) gaiCC
      submitYesVote_ (StakePoolVoter spoC) gaiCC
      passNEpochs 2
      getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId gaiCC)
      committeeHotC1 <- registerCommitteeHotKey committeeColdC1
      _committeeHotC2 <- registerCommitteeHotKey committeeColdC2
      -- Submit a constitution with a CC vote
      gaiConstitution <- submitConstitution SNothing
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
      (spoC, _, _) <- setupPoolWithStake $ Coin 42_000_000
      -- Elect a committee of 2 members
      committeeColdC1 <- KeyHashObj <$> freshKeyHash
      committeeColdC2 <- KeyHashObj <$> freshKeyHash
      gaiCC <-
        submitUpdateCommittee
          Nothing
          mempty
          [ (committeeColdC1, EpochInterval 10)
          , (committeeColdC2, EpochInterval 10)
          ]
          (1 %! 2)
      submitYesVote_ (DRepVoter drep) gaiCC
      submitYesVote_ (StakePoolVoter spoC) gaiCC
      passNEpochs 2
      getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId gaiCC)
      committeeHotC1 <- registerCommitteeHotKey committeeColdC1
      _committeeHotC2 <- registerCommitteeHotKey committeeColdC2
      -- Submit a constitution with a CC vote
      gaiConstitution <- submitConstitution SNothing
      submitYesVote_ (CommitteeVoter committeeHotC1) gaiConstitution
      -- Check for CC acceptance
      ccShouldNotBeResigned committeeColdC2
      isCommitteeAccepted gaiConstitution `shouldReturn` True
      -- Resign the second CC
      _ <- resignCommitteeColdKey committeeColdC2 SNothing
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
        submitTwoExampleProposalsAndVoteOnTheChild ::
          [(KeyHash 'StakePool (EraCrypto era), Vote)] ->
          [(Credential 'DRepRole (EraCrypto era), Vote)] ->
          ImpTestM era (GovActionId (EraCrypto era), GovActionId (EraCrypto era))
        submitTwoExampleProposalsAndVoteOnTheChild spos dreps = do
          committeeC <- KeyHashObj <$> freshKeyHash
          gaiParent <- submitUpdateCommittee Nothing mempty [(committeeC, EpochInterval 5)] (75 %! 100)
          -- We submit a descendent proposal so that even though it is sufficiently
          -- voted on, it cannot be ratified before the ParameterChange proposal
          -- is enacted.
          gaiChild <-
            submitUpdateCommittee
              (Just (SJust (GovPurposeId gaiParent)))
              mempty
              [(committeeC, EpochInterval 5)]
              (75 %! 100)
          forM_ spos $ \(spo, vote) -> submitVote_ vote (StakePoolVoter spo) gaiChild
          forM_ dreps $ \(drep, vote) -> submitVote_ vote (DRepVoter drep) gaiChild
          passEpoch -- Make the votes count
          pure (gaiParent, gaiChild)
    describe "DRep" $ do
      let setThreshold threshold =
            modifyPParams $ ppDRepVotingThresholdsL . dvtCommitteeNormalL .~ threshold
          enactThreshold threshold drepC hotCommitteeC = do
            modifyPParams $ ppDRepVotingThresholdsL . dvtPPGovGroupL .~ 1 %! 10
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
      it "Increasing the threshold prevents a hitherto-ratifiable proposal from being ratified" $ do
        (drepC, hotCommitteeC, _) <- electBasicCommittee
        setThreshold smallerThreshold
        (drep, _, _) <- setupSingleDRep 1_000_000
        (_gaiParent, gaiChild) <- submitTwoExampleProposalsAndVoteOnTheChild [] [(drep, VoteYes)]
        isDRepAccepted gaiChild `shouldReturn` True
        enactThreshold largerThreshold drepC hotCommitteeC
        isDRepAccepted gaiChild `shouldReturn` False
      it "Decreasing the threshold ratifies a hitherto-unratifiable proposal" $ do
        -- This sets up a stake pool with 1_000_000 Coin
        (drepC, hotCommitteeC, _) <- electBasicCommittee
        setThreshold largerThreshold
        (drep, _, _) <- setupSingleDRep 1_000_000
        (spoC, _, _) <- setupPoolWithStake $ Coin 3_000_000
        (gaiParent, gaiChild) <-
          submitTwoExampleProposalsAndVoteOnTheChild [(spoC, VoteYes)] [(drep, VoteYes)]
        isDRepAccepted gaiChild `shouldReturn` False
        enactThreshold smallerThreshold drepC hotCommitteeC
        isDRepAccepted gaiChild `shouldReturn` True
        -- Not vote on the parent too to make sure both get enacted
        submitYesVote_ (DRepVoter drep) gaiParent
        -- bootstrap: 3 % 4 stake yes; 1 % 4 stake abstain; yes / stake - abstain > 1 % 2
        -- post-bootstrap: 3 % 4 stake yes; 1 % 4 stake no
        submitYesVote_ (StakePoolVoter spoC) gaiParent
        passNEpochs 2
        getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId gaiParent)
        passEpoch -- UpdateCommittee is a delaying action
        getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId gaiChild)
    describe "SPO" $ do
      let setThreshold :: UnitInterval -> ImpTestM era ()
          setThreshold threshold =
            modifyPParams $ ppPoolVotingThresholdsL . pvtCommitteeNormalL .~ threshold
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
      it "Increasing the threshold prevents a hitherto-ratifiable proposal from being ratified" $ do
        -- This sets up a stake pool with 1_000_000 Coin
        (drepC, hotCommitteeC, _) <- electBasicCommittee
        setThreshold smallerThreshold
        (poolKH1, _paymentC1, _stakingC1) <- setupPoolWithStake $ Coin 2_000_000
        (poolKH2, _paymentC2, _stakingC2) <- setupPoolWithStake $ Coin 1_000_000
        passEpoch -- Make the new pool distribution count
        -- bootstrap: 1 % 2 stake yes (2_000_000); 1 % 2 stake abstain; yes / stake - abstain == 1 % 2
        -- post-bootstrap: 1 % 2 stake yes (2_000_000); 1 % 4 stake didn't vote; 1 % 4 stake no
        (_gaiParent, gaiChild) <-
          submitTwoExampleProposalsAndVoteOnTheChild
            [(poolKH1, VoteYes), (poolKH2, VoteNo)]
            [(drepC, VoteYes)]
        isSpoAccepted gaiChild `shouldReturn` True
        enactThreshold largerThreshold drepC hotCommitteeC
        isSpoAccepted gaiChild `shouldReturn` False
      it "Decreasing the threshold ratifies a hitherto-unratifiable proposal" $ do
        -- This sets up a stake pool with 1_000_000 Coin
        (drepC, hotCommitteeC, _) <- electBasicCommittee
        setThreshold largerThreshold
        (poolKH1, _paymentC1, _stakingC1) <- setupPoolWithStake $ Coin 2_000_000
        (poolKH2, _paymentC2, _stakingC2) <- setupPoolWithStake $ Coin 1_000_000
        -- bootstrap: 1 % 2 stake yes (2_000_000); 1 % 2 stake abstain; yes / stake - abstain == 1 % 2
        -- post-bootstrap: 1 % 2 stake yes (2_000_000); 1 % 4 stake didn't vote; 1 % 4 stake no
        (gaiParent, gaiChild) <-
          submitTwoExampleProposalsAndVoteOnTheChild
            [(poolKH1, VoteYes), (poolKH2, VoteNo)]
            [(drepC, VoteYes)]
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
      hotCommitteeCs <- registerInitialCommittee
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
      submitYesVoteCCs_ hotCommitteeCs parentGai
      submitYesVote_ (DRepVoter drepC) childGai
      submitYesVoteCCs_ hotCommitteeCs childGai
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
        submitTreasuryWithdrawal amount = do
          rewardAccount <- registerRewardAccount
          submitTreasuryWithdrawals [(rewardAccount, amount)]
    it "TreasuryWithdrawal fails to ratify due to an increase in CommitteeMinSize" $ do
      disableTreasuryExpansion
      amount <- uniformRM (Coin 1, Coin 100_000_000)
      -- Ensure sufficient amount in the treasury
      submitTx_ $ mkBasicTx (mkBasicTxBody & treasuryDonationTxBodyL .~ amount)
      hotCommitteeCs <- registerInitialCommittee
      (drepC, _, _) <- setupSingleDRep 1_000_000
      passEpoch
      setCommitteeMinSize 2
      gaiTW <- submitTreasuryWithdrawal amount
      submitYesVoteCCs_ hotCommitteeCs gaiTW
      submitYesVote_ (DRepVoter drepC) gaiTW
      isCommitteeAccepted gaiTW `shouldReturn` True
      gaiPC <-
        submitParameterChange SNothing $
          emptyPParamsUpdate
            & ppuCommitteeMinSizeL .~ SJust 3
      submitYesVoteCCs_ hotCommitteeCs gaiPC
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
      disableTreasuryExpansion
      (drepC, hotCommitteeC, _) <- electBasicCommittee
      (spoC, _, _) <- setupPoolWithStake $ Coin 42_000_000
      amount <- uniformRM (Coin 1, Coin 100_000_000)
      -- Ensure sufficient amount in the treasury
      submitTx_ $ mkBasicTx (mkBasicTxBody & treasuryDonationTxBodyL .~ amount)
      passEpoch
      treasury <- getsNES $ nesEsL . esAccountStateL . asTreasuryL
      gaiTW <- submitTreasuryWithdrawal amount
      submitYesVote_ (CommitteeVoter hotCommitteeC) gaiTW
      submitYesVote_ (DRepVoter drepC) gaiTW
      setCommitteeMinSize 2
      isCommitteeAccepted gaiTW `shouldReturn` False
      passNEpochs 2
      getsNES (nesEsL . esAccountStateL . asTreasuryL) `shouldReturn` treasury
      -- We do not enact the ParameterChange here because that does not pass
      -- ratification as the CC size is smaller than MinSize.
      -- We instead just add another Committee member to reach the CommitteeMinSize.
      coldCommitteeCred <- KeyHashObj <$> freshKeyHash
      gaiCC <- submitUpdateCommittee Nothing mempty [(coldCommitteeCred, EpochInterval 10)] (1 %! 2)
      submitYesVote_ (DRepVoter drepC) gaiCC
      submitYesVote_ (StakePoolVoter spoC) gaiCC
      passNEpochs 2
      _hotCommitteeC' <- registerCommitteeHotKey coldCommitteeCred
      isCommitteeAccepted gaiTW `shouldReturn` True
      passNEpochs 2
      getsNES (nesEsL . esAccountStateL . asTreasuryL) `shouldReturn` (treasury <-> amount)

spoVotesCommitteeUpdates ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
spoVotesCommitteeUpdates =
  describe "Counting of SPO votes" $ do
    describe "All gov action other than HardForkInitiation" $ do
      it "NoConfidence" $ do
        (spoK1, _, _) <- setupPoolWithStake $ Coin 100_000_000
        _ <- setupPoolWithStake $ Coin 100_000_000
        _ <- setupPoolWithStake $ Coin 100_000_000
        _ <- setupPoolWithStake $ Coin 100_000_000
        modifyPParams $ ppPoolVotingThresholdsL . pvtMotionNoConfidenceL .~ 1 %! 2
        whenPostBootstrap (modifyPParams $ ppDRepVotingThresholdsL .~ def)
        gai <- submitGovAction $ NoConfidence SNothing
        -- bootstrap: 1 % 4 stake yes; 3 % 4 stake abstain; yes / stake - abstain > 1 % 2
        -- post-bootstrap: 1 % 4 stake yes; 3 % 4 stake no; yes stake < 1 % 2
        submitYesVote_ (StakePoolVoter spoK1) gai
        passNEpochs 2
        pv <- getProtVer
        if bootstrapPhase pv
          then getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId gai)
          else getLastEnactedCommittee `shouldReturn` SNothing
      it "CommitteeUpdate" $ do
        (spoK1, _, _) <- setupPoolWithStake $ Coin 100_000_000
        _ <- setupPoolWithStake $ Coin 100_000_000
        _ <- setupPoolWithStake $ Coin 100_000_000
        _ <- setupPoolWithStake $ Coin 100_000_000
        modifyPParams $ ppPoolVotingThresholdsL . pvtCommitteeNormalL .~ 1 %! 2
        whenPostBootstrap (modifyPParams $ ppDRepVotingThresholdsL .~ def)
        cc <- KeyHashObj <$> freshKeyHash
        gai <- submitUpdateCommittee Nothing mempty [(cc, EpochInterval 5)] (1 %! 2)
        -- bootstrap: 1 % 4 stake yes; 3 % 4 stake abstain; yes / stake - abstain > 1 % 2
        -- post-bootstrap: 1 % 4 stake yes; 3 % 4 stake no; yes stake < 1 % 2
        submitYesVote_ (StakePoolVoter spoK1) gai
        passNEpochs 2
        pv <- getProtVer
        if bootstrapPhase pv
          then getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId gai)
          else getLastEnactedCommittee `shouldReturn` SNothing
spoVotesForHardForkInitiation ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
spoVotesForHardForkInitiation =
  describe "Counting of SPO votes" $ do
    it "HardForkInitiation" $ do
      whenPostBootstrap (modifyPParams $ ppDRepVotingThresholdsL . dvtHardForkInitiationL .~ def)
      hotCCs <- registerInitialCommittee
      (spoK1, _, _) <- setupPoolWithStake $ Coin 100_000_000
      (spoK2, _, _) <- setupPoolWithStake $ Coin 100_000_000
      _ <- setupPoolWithStake $ Coin 100_000_000
      _ <- setupPoolWithStake $ Coin 100_000_000
      modifyPParams $ ppPoolVotingThresholdsL . pvtHardForkInitiationL .~ 1 %! 2
      protVer <- getProtVer
      gai <- submitGovAction $ HardForkInitiation SNothing (majorFollow protVer)
      submitYesVoteCCs_ hotCCs gai
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
      ccCreds <- registerInitialCommittee
      (drep, _, _) <- setupSingleDRep 1_000_000
      (khPool, _, _) <- setupPoolWithStake $ Coin 42_000_000
      initMinFeeA <- getsNES $ nesEsL . curPParamsEpochStateL . ppMinFeeAL
      gaidThreshold <- impAnn "Update StakePool thresholds" $ do
        pp <- getsNES $ nesEsL . curPParamsEpochStateL
        (pp ^. ppPoolVotingThresholdsL . pvtPPSecurityGroupL) `shouldBe` (51 %! 100)
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
        gaidThreshold <- mkProposal (ParameterChange SNothing ppUpdate SNothing) >>= submitProposal
        submitYesVote_ (DRepVoter drep) gaidThreshold
        submitYesVoteCCs_ ccCreds gaidThreshold
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
        let ga =
              ParameterChange
                (SJust (GovPurposeId gaidThreshold))
                (emptyPParamsUpdate & ppuMinFeeAL .~ SJust newMinFeeA)
                SNothing
        gaidMinFee <- mkProposal ga >>= submitProposal
        submitYesVote_ (DRepVoter drep) gaidMinFee
        submitYesVoteCCs_ ccCreds gaidMinFee
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
          -- Setup DRep delegation #1
          (drep1, KeyHashObj stakingKH1, paymentKP1) <- setupSingleDRep 1_000_000_000
          -- Setup DRep delegation #2
          _ <- setupSingleDRep 1_000_000_000
          (spoC, _, _) <- setupPoolWithStake $ Coin 42_000_000
          -- Submit a committee proposal
          cc <- KeyHashObj <$> freshKeyHash
          addCCGaid <- submitUpdateCommittee Nothing mempty [(cc, EpochInterval 10)] (75 %! 100)
          -- Submit the vote
          submitVote_ VoteYes (DRepVoter drep1) addCCGaid
          submitYesVote_ (StakePoolVoter spoC) addCCGaid
          passNEpochs 2
          -- The vote should not result in a ratification
          isDRepAccepted addCCGaid `shouldReturn` False
          getLastEnactedCommittee `shouldReturn` SNothing
          -- Bump up the UTxO delegated
          -- to barely make the threshold (51 %! 100)
          stakingKP1 <- lookupKeyPair stakingKH1
          _ <- sendCoinTo (mkAddr (paymentKP1, stakingKP1)) (inject $ Coin 200_000_000)
          passNEpochs 2
          -- The same vote should now successfully ratify the proposal
          getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId addCCGaid)
        it "Rewards contribute to active voting stake" $ do
          -- Setup DRep delegation #1
          (drep1, staking1, _) <- setupSingleDRep 1_000_000_000
          -- Setup DRep delegation #2
          _ <- setupSingleDRep 1_000_000_000
          (spoC, _, _) <- setupPoolWithStake $ Coin 42_000_000
          -- Submit a committee proposal
          cc <- KeyHashObj <$> freshKeyHash
          addCCGaid <- submitUpdateCommittee Nothing mempty [(cc, EpochInterval 10)] (75 %! 100)
          -- Submit the vote
          submitVote_ VoteYes (DRepVoter drep1) addCCGaid
          submitYesVote_ (StakePoolVoter spoC) addCCGaid
          passNEpochs 2
          -- The vote should not result in a ratification
          isDRepAccepted addCCGaid `shouldReturn` False
          getLastEnactedCommittee `shouldReturn` SNothing
          -- Add to the rewards of the delegator to this DRep
          -- to barely make the threshold (51 %! 100)
          modifyNES $
            nesEsL . epochStateUMapL
              %~ UM.adjust
                (\(UM.RDPair r d) -> UM.RDPair (r <> UM.CompactCoin 200_000_000) d)
                staking1
                . UM.RewDepUView
          passNEpochs 2
          -- The same vote should now successfully ratify the proposal
          getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId addCCGaid)
        it "Rewards contribute to active voting stake even in the absence of StakeDistr" $ do
          let govActionLifetime = 5
              govActionDeposit = Coin 1_000_000
              poolDeposit = Coin 200_000
          -- Only modify the applicable thresholds
          modifyPParams $ \pp ->
            pp
              & ppGovActionDepositL .~ govActionDeposit
              & ppPoolDepositL .~ poolDeposit
              & ppEMaxL .~ EpochInterval govActionLifetime
              & ppGovActionLifetimeL .~ EpochInterval govActionLifetime
          -- Setup DRep delegation #1
          (drepKH1, stakingKH1) <- setupDRepWithoutStake
          -- Add rewards to delegation #1
          submitAndExpireProposalToMakeReward $ KeyHashObj stakingKH1
          lookupReward (KeyHashObj stakingKH1) `shouldReturn` govActionDeposit
          -- Setup DRep delegation #2
          (_drepKH2, stakingKH2) <- setupDRepWithoutStake
          (spoC, _, _) <- setupPoolWithStake $ Coin 42_000_000
          -- Add rewards to delegation #2
          submitAndExpireProposalToMakeReward $ KeyHashObj stakingKH2
          lookupReward (KeyHashObj stakingKH2) `shouldReturn` govActionDeposit
          -- Submit a committee proposal
          cc <- KeyHashObj <$> freshKeyHash
          Positive extra <- arbitrary
          let lifetime = EpochInterval (extra + 2 * govActionLifetime)
          addCCGaid <- submitUpdateCommittee Nothing mempty [(cc, lifetime)] (75 %! 100)
          -- Submit the vote
          submitVote_ VoteYes (DRepVoter $ KeyHashObj drepKH1) addCCGaid
          submitYesVote_ (StakePoolVoter spoC) addCCGaid
          passNEpochs 2
          -- The vote should not result in a ratification
          isDRepAccepted addCCGaid `shouldReturn` False
          getLastEnactedCommittee `shouldReturn` SNothing
          -- Increase the rewards of the delegator to this DRep
          -- to barely make the threshold (51 %! 100)
          registerAndRetirePoolToMakeReward $ KeyHashObj stakingKH1
          lookupReward (KeyHashObj stakingKH1) `shouldReturn` poolDeposit <> govActionDeposit
          isDRepAccepted addCCGaid `shouldReturn` True
          -- The same vote should now successfully ratify the proposal
          passEpoch
          getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId addCCGaid)
        describe "Proposal deposits contribute to active voting stake" $ do
          it "Directly" $ do
            -- Only modify the applicable thresholds
            modifyPParams $ ppGovActionDepositL .~ Coin 600_000
            -- Setup DRep delegation without stake #1
            (drepKH1, stakingKH1) <- setupDRepWithoutStake
            -- Setup DRep delegation #2
            (_drepKH2, _stakingKH2, _paymentKP2) <- setupSingleDRep 1_000_000
            (spoC, _, _) <- setupPoolWithStake $ Coin 42_000_000
            -- Make a note of the reward account for the delegator to DRep #1
            dRepRewardAccount <- getRewardAccountFor $ KeyHashObj stakingKH1
            -- Submit the first committee proposal, the one we will test active voting stake against.
            -- The proposal deposit comes from the root UTxO
            cc <- KeyHashObj <$> freshKeyHash
            curEpochNo <- getsNES nesELL
            let
              newCommitteMembers = Map.singleton cc $ addEpochInterval curEpochNo (EpochInterval 10)
            addCCGaid <-
              mkProposalWithRewardAccount
                (UpdateCommittee SNothing mempty newCommitteMembers (75 %! 100))
                dRepRewardAccount
                >>= submitProposal
            -- Submit the vote from DRep #1
            submitVote_ VoteYes (DRepVoter $ KeyHashObj drepKH1) addCCGaid
            submitYesVote_ (StakePoolVoter spoC) addCCGaid
            passNEpochs 2
            -- The vote should not result in a ratification
            isDRepAccepted addCCGaid `shouldReturn` False
            getLastEnactedCommittee `shouldReturn` SNothing
            -- Submit another proposal to bump up the active voting stake
            cc' <- KeyHashObj <$> freshKeyHash
            let
              newCommitteMembers' = Map.singleton cc' $ addEpochInterval curEpochNo (EpochInterval 10)
            mkProposalWithRewardAccount
              (UpdateCommittee SNothing mempty newCommitteMembers' (75 %! 100))
              dRepRewardAccount
              >>= submitProposal_
            passNEpochs 2
            -- The same vote should now successfully ratify the proposal
            getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId addCCGaid)
          it "After switching delegations" $ do
            -- Only modify the applicable thresholds
            modifyPParams $ ppGovActionDepositL .~ Coin 1_000_000
            -- Setup DRep delegation without stake #1
            (drepKH1, stakingKH1) <- setupDRepWithoutStake
            -- Setup DRep delegation #2
            (_drepKH2, _stakingKH2, _paymentKP2) <- setupSingleDRep 1_000_000
            -- Setup DRep delegation #3
            (_drepKH3, stakingKH3) <- setupDRepWithoutStake
            (spoC, _, _) <- setupPoolWithStake $ Coin 42_000_000
            -- Make a note of the reward accounts for the delegators to DReps #1 and #3
            dRepRewardAccount1 <- getRewardAccountFor $ KeyHashObj stakingKH1
            dRepRewardAccount3 <- getRewardAccountFor $ KeyHashObj stakingKH3
            -- Submit committee proposals
            -- The proposal deposits comes from the root UTxO
            -- After this both stakingKH1 and stakingKH3 are expected to have 1_000_000 ADA of stake, each
            cc <- KeyHashObj <$> freshKeyHash
            curEpochNo <- getsNES nesELL
            let
              newCommitteMembers = Map.singleton cc $ addEpochInterval curEpochNo (EpochInterval 10)
            addCCGaid <-
              mkProposalWithRewardAccount
                (UpdateCommittee SNothing mempty newCommitteMembers (75 %! 100))
                dRepRewardAccount1
                >>= submitProposal
            cc' <- KeyHashObj <$> freshKeyHash
            let
              newCommitteMembers' = Map.singleton cc' $ addEpochInterval curEpochNo (EpochInterval 10)
            mkProposalWithRewardAccount
              (UpdateCommittee SNothing mempty newCommitteMembers' (75 %! 100))
              dRepRewardAccount3
              >>= submitProposal_
            -- Submit the vote from DRep #1
            submitVote_ VoteYes (DRepVoter $ KeyHashObj drepKH1) addCCGaid
            submitYesVote_ (StakePoolVoter spoC) addCCGaid
            passNEpochs 2
            -- The vote should not result in a ratification
            isDRepAccepted addCCGaid `shouldReturn` False
            getLastEnactedCommittee `shouldReturn` SNothing
            -- Switch the delegation from DRep #3 to DRep #1
            submitTxAnn_ "Switch the delegation from DRep #3 to DRep #1" $
              mkBasicTx mkBasicTxBody
                & bodyTxL . certsTxBodyL
                  .~ SSeq.fromList
                    [ DelegTxCert
                        (KeyHashObj stakingKH3)
                        (DelegVote (DRepCredential $ KeyHashObj drepKH1))
                    ]
            passNEpochs 2
            -- The same vote should now successfully ratify the proposal
            getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId addCCGaid)
      describe "Predefined DReps" $ do
        it "acceptedRatio with default DReps" $ do
          (drep1, _, committeeGovId) <- electBasicCommittee
          (_, drep2Staking, _) <- setupSingleDRep 1_000_000

          paramChangeGovId <- submitParameterChange SNothing $ def & ppuMinFeeAL .~ SJust (Coin 3000)
          submitYesVote_ (DRepVoter drep1) paramChangeGovId

          passEpoch
          calculateDRepAcceptedRatio paramChangeGovId `shouldReturn` 1 % 2

          kh <- freshKeyHash
          _ <- registerStakeCredential (KeyHashObj kh)
          _ <- delegateToDRep (KeyHashObj kh) (Coin 1_000_000) DRepAlwaysNoConfidence
          passEpoch
          -- AlwaysNoConfidence vote acts like a 'No' vote for actions other than NoConfidence
          calculateDRepAcceptedRatio paramChangeGovId `shouldReturn` 1 % 3

          _ <- delegateToDRep drep2Staking zero DRepAlwaysAbstain
          passEpoch
          -- AlwaysAbstain vote acts like 'Abstain' vote
          calculateDRepAcceptedRatio paramChangeGovId `shouldReturn` 1 % 2

          noConfidenceGovId <- submitGovAction $ NoConfidence (SJust committeeGovId)
          submitYesVote_ (DRepVoter drep1) noConfidenceGovId
          passEpoch
          -- AlwaysNoConfidence vote acts like 'Yes' for NoConfidence actions
          calculateDRepAcceptedRatio noConfidenceGovId `shouldReturn` 2 % 2

        it "AlwaysNoConfidence" $ do
          (drep1, _, committeeGovId) <- electBasicCommittee
          initialMembers <- getCommitteeMembers

          -- drep2 won't explicitly vote, but eventually delegate to AlwaysNoConfidence
          (drep2, drep2Staking, _) <- setupSingleDRep 1_000_000

          -- we register another drep with the same stake as drep1, which will vote No -
          -- in order to make it necessary to redelegate to AlwaysNoConfidence,
          -- rather than just unregister
          (drep3, _, _) <- setupSingleDRep 1_000_000
          (spoC, _, _) <- setupPoolWithStake $ Coin 42_000_000

          noConfidenceGovId <- submitGovAction $ NoConfidence (SJust committeeGovId)
          submitYesVote_ (DRepVoter drep1) noConfidenceGovId
          submitVote_ VoteNo (DRepVoter drep3) noConfidenceGovId
          submitYesVote_ (StakePoolVoter spoC) noConfidenceGovId
          passEpoch
          -- drep1 doesn't have enough stake to enact NoConfidence
          isDRepAccepted noConfidenceGovId `shouldReturn` False
          passEpoch
          getCommitteeMembers `shouldReturn` initialMembers

          -- drep2 unregisters, but NoConfidence still doesn't pass, because there's a tie between drep1 and drep3
          unRegisterDRep drep2
          passEpoch
          isDRepAccepted noConfidenceGovId `shouldReturn` False

          _ <- delegateToDRep drep2Staking zero DRepAlwaysNoConfidence
          passEpoch
          isDRepAccepted noConfidenceGovId `shouldReturn` True
          passEpoch
          getCommitteeMembers `shouldReturn` mempty
        it "AlwaysAbstain" $ do
          let getTreasury = getsNES (nesEsL . esAccountStateL . asTreasuryL)

          disableTreasuryExpansion
          donateToTreasury $ Coin 5_000_000

          (drep1, comMember, _) <- electBasicCommittee
          initialTreasury <- getTreasury

          (drep2, drep2Staking, _) <- setupSingleDRep 1_000_000

          rewardAccount <- registerRewardAccount
          govId <- submitTreasuryWithdrawals [(rewardAccount, initialTreasury)]

          submitYesVote_ (CommitteeVoter comMember) govId
          submitYesVote_ (DRepVoter drep1) govId
          submitVote_ VoteNo (DRepVoter drep2) govId
          passEpoch
          -- drep1 doesn't have enough stake to enact the withdrawals
          isDRepAccepted govId `shouldReturn` False
          passEpoch
          getTreasury `shouldReturn` initialTreasury

          _ <- delegateToDRep drep2Staking zero DRepAlwaysAbstain

          passEpoch
          -- the delegation turns the No vote into an Abstain, enough to pass the action
          isDRepAccepted govId `shouldReturn` True
          passEpoch
          getTreasury `shouldReturn` zero

        it "DRepAlwaysNoConfidence is sufficient to pass NoConfidence" $ do
          modifyPParams $ \pp ->
            pp
              & ppPoolVotingThresholdsL . pvtMotionNoConfidenceL .~ 0 %! 1
              & ppDRepVotingThresholdsL . dvtMotionNoConfidenceL .~ 1 %! 1
              & ppCoinsPerUTxOByteL .~ CoinPerByte (Coin 1)
          (drep, _, committeeId) <- electBasicCommittee
          kh <- freshKeyHash
          _ <- registerStakeCredential (KeyHashObj kh)
          _ <- delegateToDRep (KeyHashObj kh) (Coin 300) DRepAlwaysNoConfidence
          noConfidence <- submitGovAction (NoConfidence (SJust committeeId))
          submitYesVote_ (DRepVoter drep) noConfidence
          logAcceptedRatio noConfidence
          passNEpochs 2
          getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId noConfidence)

      describe "StakePool" $ do
        it "UTxOs contribute to active voting stake" $ do
          -- Only modify the applicable thresholds
          modifyPParams $
            ppPoolVotingThresholdsL
              .~ def
                { pvtCommitteeNormal = 51 %! 100
                , pvtCommitteeNoConfidence = 51 %! 100
                }
          whenPostBootstrap (modifyPParams $ ppDRepVotingThresholdsL .~ def)
          -- Setup Pool delegation #1
          (poolKH1, delegatorCPayment1, delegatorCStaking1) <- setupPoolWithStake $ Coin 1_000_000_000
          -- Setup Pool delegation #2
          (poolKH2, _, _) <- setupPoolWithStake $ Coin 1_000_000_000
          passEpoch
          -- Submit a committee proposal
          cc <- KeyHashObj <$> freshKeyHash
          addCCGaid <- submitUpdateCommittee Nothing mempty [(cc, EpochInterval 10)] (75 %! 100)
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
              (Coin 40_900_000)
          passNEpochs 2
          -- The same vote should now successfully ratify the proposal
          getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId addCCGaid)
        it "Rewards contribute to active voting stake" $ do
          -- Only modify the applicable thresholds
          modifyPParams $
            ppPoolVotingThresholdsL
              .~ def
                { pvtCommitteeNormal = 51 %! 100
                , pvtCommitteeNoConfidence = 51 %! 100
                }
          whenPostBootstrap (modifyPParams $ ppDRepVotingThresholdsL .~ def)

          -- Setup Pool delegation #1
          (poolKH1, _, delegatorCStaking1) <- setupPoolWithStake $ Coin 1_000_000_000
          -- Setup Pool delegation #2
          (poolKH2, _, _) <- setupPoolWithStake $ Coin 1_000_000_000
          passEpoch
          -- Submit a committee proposal
          cc <- KeyHashObj <$> freshKeyHash
          addCCGaid <- submitUpdateCommittee Nothing mempty [(cc, EpochInterval 10)] (75 %! 100)
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
                (\(UM.RDPair r d) -> UM.RDPair (r <> UM.CompactCoin 200_000_000) d)
                delegatorCStaking1
                . UM.RewDepUView
          passNEpochs 2
          -- The same vote should now successfully ratify the proposal
          getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId addCCGaid)
        it "Rewards contribute to active voting stake even in the absence of StakeDistr" $ do
          let govActionLifetime = 5
              govActionDeposit = Coin 1_000_000
              poolDeposit = Coin 200_000
          -- Only modify the applicable thresholds
          modifyPParams $ \pp ->
            pp
              & ppPoolVotingThresholdsL
                .~ def
                  { pvtCommitteeNormal = 51 %! 100
                  , pvtCommitteeNoConfidence = 51 %! 100
                  }
              & ppGovActionDepositL .~ govActionDeposit
              & ppPoolDepositL .~ poolDeposit
              & ppEMaxL .~ EpochInterval 5
              & ppGovActionLifetimeL .~ EpochInterval govActionLifetime
          whenPostBootstrap (modifyPParams $ ppDRepVotingThresholdsL .~ def)

          -- Setup Pool delegation #1
          (poolKH1, delegatorCStaking1) <- setupPoolWithoutStake
          -- Add rewards to delegation #1
          submitAndExpireProposalToMakeReward delegatorCStaking1
          lookupReward delegatorCStaking1 `shouldReturn` govActionDeposit
          -- Setup Pool delegation #2
          (poolKH2, delegatorCStaking2) <- setupPoolWithoutStake
          -- Add rewards to delegation #2
          submitAndExpireProposalToMakeReward delegatorCStaking2
          lookupReward delegatorCStaking2 `shouldReturn` govActionDeposit
          -- Submit a committee proposal
          Positive extra <- arbitrary
          cc <- KeyHashObj <$> freshKeyHash
          addCCGaid <-
            submitUpdateCommittee
              Nothing
              mempty
              [(cc, EpochInterval (extra + 2 * govActionLifetime))]
              (75 %! 100)
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
          registerAndRetirePoolToMakeReward delegatorCStaking1
          lookupReward delegatorCStaking1 `shouldReturn` poolDeposit <> govActionDeposit
          -- The same vote should now successfully ratify the proposal
          -- NOTE: It takes 2 epochs for SPO votes as opposed to 1 epoch
          -- for DRep votes to ratify a proposal.
          passNEpochs 2
          getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId addCCGaid)
        describe "Proposal deposits contribute to active voting stake" $ do
          it "Directly" $ do
            -- Only modify the applicable thresholds
            modifyPParams $ \pp ->
              pp
                & ppPoolVotingThresholdsL
                  .~ def
                    { pvtCommitteeNormal = 51 %! 100
                    , pvtCommitteeNoConfidence = 51 %! 100
                    }
                & ppDRepVotingThresholdsL
                  .~ def
                    { dvtCommitteeNormal = 0 %! 1
                    , dvtCommitteeNoConfidence = 0 %! 1
                    }
                & ppGovActionDepositL .~ Coin 600_000
            -- Setup Pool delegation #1
            (poolKH1, stakingC1) <- setupPoolWithoutStake
            -- Setup Pool delegation #2
            (poolKH2, _paymentC2, _stakingC2) <- setupPoolWithStake $ Coin 1_000_000
            -- Make a note of the reward account for the delegator to SPO #1
            spoRewardAccount <- getRewardAccountFor stakingC1
            -- Submit the first committee proposal, the one we will test active voting stake against.
            -- The proposal deposit comes from the root UTxO
            cc <- KeyHashObj <$> freshKeyHash
            curEpochNo <- getsNES nesELL
            let
              newCommitteMembers = Map.singleton cc $ addEpochInterval curEpochNo (EpochInterval 10)
            addCCGaid <-
              mkProposalWithRewardAccount
                (UpdateCommittee SNothing mempty newCommitteMembers (75 %! 100))
                spoRewardAccount
                >>= submitProposal

            -- Submit a yes vote from SPO #1 and a no vote from SPO #2
            submitVote_ VoteYes (StakePoolVoter poolKH1) addCCGaid
            submitVote_ VoteNo (StakePoolVoter poolKH2) addCCGaid
            passNEpochs 2
            -- The vote should not result in a ratification
            getLastEnactedCommittee `shouldReturn` SNothing
            -- Submit another proposal to bump up the active voting stake of SPO #1
            cc' <- KeyHashObj <$> freshKeyHash
            let
              newCommitteMembers' = Map.singleton cc' $ addEpochInterval curEpochNo (EpochInterval 10)
            mkProposalWithRewardAccount
              (UpdateCommittee SNothing mempty newCommitteMembers' (75 %! 100))
              spoRewardAccount
              >>= submitProposal_
            passNEpochs 2
            -- The same vote should now successfully ratify the proposal
            getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId addCCGaid)
          it "After switching delegations" $ do
            -- Only modify the applicable thresholds
            modifyPParams $ \pp ->
              pp
                & ppPoolVotingThresholdsL
                  .~ def
                    { pvtCommitteeNormal = 51 %! 100
                    , pvtCommitteeNoConfidence = 51 %! 100
                    }
                & ppDRepVotingThresholdsL
                  .~ def
                    { dvtCommitteeNormal = 0 %! 1
                    , dvtCommitteeNoConfidence = 0 %! 1
                    }
                & ppGovActionDepositL .~ Coin 1_000_000
            -- Setup Pool delegation #1
            (poolKH1, stakingC1) <- setupPoolWithoutStake
            -- Setup Pool delegation #2
            (poolKH2, _paymentC2, _stakingC2) <- setupPoolWithStake $ Coin 1_000_000
            -- Setup Pool delegation #3
            (_poolKH3, stakingC3) <- setupPoolWithoutStake
            -- Make a note of the reward accounts for the delegators to SPOs #1 and #3
            spoRewardAccount1 <- getRewardAccountFor stakingC1
            spoRewardAccount3 <- getRewardAccountFor stakingC3
            -- Submit committee proposals
            -- The proposal deposits come from the root UTxO
            -- After this both stakingC1 and stakingC3 are expected to have 1_000_000 ADA of stake, each
            cc <- KeyHashObj <$> freshKeyHash
            curEpochNo <- getsNES nesELL
            let
              newCommitteMembers = Map.singleton cc $ addEpochInterval curEpochNo (EpochInterval 10)
            addCCGaid <-
              mkProposalWithRewardAccount
                (UpdateCommittee SNothing mempty newCommitteMembers (75 %! 100))
                spoRewardAccount1
                >>= submitProposal
            cc' <- KeyHashObj <$> freshKeyHash
            let
              newCommitteMembers' = Map.singleton cc' $ addEpochInterval curEpochNo (EpochInterval 10)
            mkProposalWithRewardAccount
              (UpdateCommittee SNothing mempty newCommitteMembers' (75 %! 100))
              spoRewardAccount3
              >>= submitProposal_
            -- Submit a yes vote from SPO #1 and a no vote from SPO #2
            submitVote_ VoteYes (StakePoolVoter poolKH1) addCCGaid
            submitVote_ VoteNo (StakePoolVoter poolKH2) addCCGaid
            passNEpochs 2
            -- The vote should not result in a ratification
            getLastEnactedCommittee `shouldReturn` SNothing
            submitTxAnn_ "Switch the delegation from SPO #3 to SPO #1" $
              mkBasicTx mkBasicTxBody
                & bodyTxL . certsTxBodyL .~ SSeq.fromList [mkDelegTxCert stakingC3 (DelegStake poolKH1)]
            passNEpochs 2
            -- The same vote should now successfully ratify the proposal
            getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId addCCGaid)
    describe "Interaction between governing bodies" $ do
      it "Motion of no-confidence" $ do
        (drep, _, committeeGovId) <- electBasicCommittee
        (spoC, _, _) <- setupPoolWithStake $ Coin 1_000_000
        initialMembers <- getCommitteeMembers
        noConfidenceGovId <- submitGovAction $ NoConfidence (SJust committeeGovId)

        submitYesVote_ (DRepVoter drep) noConfidenceGovId
        submitVote_ VoteNo (StakePoolVoter spoC) noConfidenceGovId
        passNEpochs 2

        -- DReps accepted the proposal
        isDRepAccepted noConfidenceGovId `shouldReturn` True
        -- SPOs voted no, so NoConfidence won't be ratified, thus committee remains the same
        isSpoAccepted noConfidenceGovId `shouldReturn` False
        getCommitteeMembers `shouldReturn` initialMembers
      it "Update committee - normal state" $ do
        (drep, _, committeeGovId) <- electBasicCommittee
        (spoC, _, _) <- setupPoolWithStake $ Coin 1_000_000
        SJust initialCommittee <- getsNES $ newEpochStateGovStateL . committeeGovStateL
        let initialThreshold = initialCommittee ^. committeeThresholdL
        gid <- submitGovAction $ UpdateCommittee (SJust committeeGovId) mempty mempty (1 %! 100)

        submitYesVote_ (DRepVoter drep) gid
        submitVote_ VoteNo (StakePoolVoter spoC) gid
        passNEpochs 2

        -- DReps accepted the proposal
        isDRepAccepted gid `shouldReturn` True
        -- SPOs voted no, so committee won't be updated
        isSpoAccepted gid `shouldReturn` False
        getLastEnactedCommittee `shouldReturn` SJust committeeGovId
        SJust currentCommittee <- getsNES $ newEpochStateGovStateL . committeeGovStateL
        currentCommittee ^. committeeThresholdL `shouldBe` initialThreshold
      it "Hard-fork initiation" $ do
        ccMembers <- registerInitialCommittee
        (drep, _, _) <- setupSingleDRep 1_000_000
        (spoC, _, _) <- setupPoolWithStake $ Coin 1_000_000
        curProtVer <- getProtVer
        nextMajorVersion <- succVersion $ pvMajor curProtVer
        let nextProtVer = curProtVer {pvMajor = nextMajorVersion}
        gid <- submitGovAction $ HardForkInitiation SNothing nextProtVer

        submitYesVoteCCs_ ccMembers gid
        submitYesVote_ (StakePoolVoter spoC) gid
        passNEpochs 2

        -- No changes so far, since DReps haven't voted yet
        getProtVer `shouldReturn` curProtVer
        submitYesVote_ (DRepVoter drep) gid
        passNEpochs 2
        -- DReps voted yes too for the hard-fork, so protocol version is incremented
        getProtVer `shouldReturn` nextProtVer
      it
        "A governance action is automatically ratified if threshold is set to 0 for all related governance bodies"
        $ do
          modifyPParams $ \pp ->
            pp
              & ppPoolVotingThresholdsL . pvtMotionNoConfidenceL .~ 0 %! 1
              & ppDRepVotingThresholdsL . dvtMotionNoConfidenceL .~ 0 %! 1
          (_drep, _, committeeGovId) <- electBasicCommittee
          _ <- setupPoolWithStake $ Coin 1_000_000

          -- There is a committee initially
          getCommitteeMembers `shouldNotReturn` mempty

          noConfidenceGovId <- submitGovAction $ NoConfidence (SJust committeeGovId)

          -- No votes were made but due to the 0 thresholds, every governance body accepted the gov action by default...
          isDRepAccepted noConfidenceGovId `shouldReturn` True
          isSpoAccepted noConfidenceGovId `shouldReturn` True
          -- ...even the committee which is not allowed to vote on `NoConfidence` action
          isCommitteeAccepted noConfidenceGovId `shouldReturn` True
          passNEpochs 2
          -- `NoConfidence` is ratified -> the committee is no more
          getCommitteeMembers `shouldReturn` mempty

delayingActionsSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
delayingActionsSpec =
  describe "Delaying actions" $ do
    it "A delaying action delays its child even when both ere proposed and ratified in the same epoch" $ do
      committeeMembers' <- registerInitialCommittee
      (dRep, _, _) <- setupSingleDRep 1_000_000
      gai0 <- submitConstitution SNothing
      gai1 <- submitConstitution $ SJust (GovPurposeId gai0)
      gai2 <- submitConstitution $ SJust (GovPurposeId gai1)
      gai3 <- submitConstitution $ SJust (GovPurposeId gai2)
      submitYesVote_ (DRepVoter dRep) gai0
      submitYesVoteCCs_ committeeMembers' gai0
      submitYesVote_ (DRepVoter dRep) gai1
      submitYesVoteCCs_ committeeMembers' gai1
      submitYesVote_ (DRepVoter dRep) gai2
      submitYesVoteCCs_ committeeMembers' gai2
      submitYesVote_ (DRepVoter dRep) gai3
      submitYesVoteCCs_ committeeMembers' gai3
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
        committeeMembers' <- registerInitialCommittee
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
        cGai0 <- submitConstitution SNothing
        cGai1 <- submitConstitution $ SJust (GovPurposeId cGai0)
        submitYesVote_ (DRepVoter dRep) cGai0
        submitYesVoteCCs_ committeeMembers' cGai0
        submitYesVote_ (DRepVoter dRep) cGai1
        submitYesVoteCCs_ committeeMembers' cGai1
        submitYesVote_ (DRepVoter dRep) pGai0
        submitYesVoteCCs_ committeeMembers' pGai0
        submitYesVote_ (DRepVoter dRep) pGai1
        submitYesVoteCCs_ committeeMembers' pGai1
        submitYesVote_ (DRepVoter dRep) pGai2
        submitYesVoteCCs_ committeeMembers' pGai2
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
        committeeMembers' <- registerInitialCommittee
        (dRep, _, _) <- setupSingleDRep 1_000_000
        modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
        gai0 <- submitConstitution SNothing
        gai1 <- submitConstitution $ SJust (GovPurposeId gai0)
        gai2 <- submitConstitution $ SJust (GovPurposeId gai1)
        gai3 <- submitConstitution $ SJust (GovPurposeId gai2)
        submitYesVote_ (DRepVoter dRep) gai0
        submitYesVoteCCs_ committeeMembers' gai0
        submitYesVote_ (DRepVoter dRep) gai1
        submitYesVoteCCs_ committeeMembers' gai1
        submitYesVote_ (DRepVoter dRep) gai2
        submitYesVoteCCs_ committeeMembers' gai2
        submitYesVote_ (DRepVoter dRep) gai3
        submitYesVoteCCs_ committeeMembers' gai3
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
        committeeMembers' <- registerInitialCommittee
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
        cGai0 <- submitConstitution SNothing
        cGai1 <- submitConstitution $ SJust (GovPurposeId cGai0)
        cGai2 <- submitConstitution $ SJust (GovPurposeId cGai1)
        submitYesVote_ (DRepVoter dRep) cGai0
        submitYesVoteCCs_ committeeMembers' cGai0
        submitYesVote_ (DRepVoter dRep) cGai1
        submitYesVoteCCs_ committeeMembers' cGai1
        submitYesVote_ (DRepVoter dRep) cGai2
        submitYesVoteCCs_ committeeMembers' cGai2
        submitYesVote_ (DRepVoter dRep) pGai0
        submitYesVoteCCs_ committeeMembers' pGai0
        submitYesVote_ (DRepVoter dRep) pGai1
        submitYesVoteCCs_ committeeMembers' pGai1
        submitYesVote_ (DRepVoter dRep) pGai2
        submitYesVoteCCs_ committeeMembers' pGai2
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
        (drep, _, _) <- setupSingleDRep 1_000_000
        (spoC, _, _) <- setupPoolWithStake $ Coin 42_000_000
        maxTermLength <- getsNES $ nesEsL . curPParamsEpochStateL . ppCommitteeMaxTermLengthL

        hks <- registerInitialCommittee
        initialMembers <- getCommitteeMembers

        (membersExceedingExpiry, exceedingExpiry) <-
          impAnn "Committee with members exceeding the maxTerm is not enacted" $ do
            -- submit a proposal for adding two members to the committee,
            -- one of which has a max term exceeding the maximum
            c3 <- freshKeyHash
            c4 <- freshKeyHash
            currentEpoch <- getsNES nesELL
            let exceedingExpiry = addEpochInterval (addEpochInterval currentEpoch maxTermLength) (EpochInterval 7)
                membersExceedingExpiry = [(KeyHashObj c3, exceedingExpiry), (KeyHashObj c4, addEpochInterval currentEpoch maxTermLength)]
            GovPurposeId gaid <-
              electCommittee
                SNothing
                drep
                Set.empty
                membersExceedingExpiry
            submitYesVote_ (StakePoolVoter spoC) gaid
            passNEpochs 2
            -- the new committee has not been enacted
            expectMembers initialMembers
            pure (Map.keysSet membersExceedingExpiry, exceedingExpiry)

        -- other actions get ratified and enacted
        govIdConst1 <- impAnn "Other actions are ratified and enacted" $ do
          (proposal, constitution) <- mkConstitutionProposal SNothing
          govIdConst1 <- submitProposal proposal
          submitYesVote_ (DRepVoter drep) govIdConst1
          submitYesVoteCCs_ hks govIdConst1

          passNEpochs 2
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
          (proposal, constitution) <- mkConstitutionProposal $ SJust (GovPurposeId govIdConst1)
          govIdConst2 <- submitProposal proposal
          submitYesVote_ (DRepVoter drep) govIdConst2
          hks' <- traverse registerCommitteeHotKey (Set.toList membersExceedingExpiry)
          submitYesVoteCCs_ hks' govIdConst2

          passNEpochs 2
          curConstitution <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
          curConstitution `shouldBe` constitution

committeeMaxTermLengthSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
committeeMaxTermLengthSpec =
  describe "Committee members can serve full `CommitteeMaxTermLength`" $ do
    let
      electMembersWithMaxTermLength ::
        KeyHash 'StakePool (EraCrypto era) ->
        Credential 'DRepRole (EraCrypto era) ->
        ImpTestM era [Credential 'ColdCommitteeRole (EraCrypto era)]
      electMembersWithMaxTermLength spoC drep = do
        m1 <- KeyHashObj <$> freshKeyHash
        m2 <- KeyHashObj <$> freshKeyHash
        currentEpoch <- getsNES nesELL
        maxTermLength <-
          getsNES $
            nesEsL . curPParamsEpochStateL . ppCommitteeMaxTermLengthL
        let expiry = addEpochInterval (addEpochInterval currentEpoch $ EpochInterval 1) maxTermLength
            members = [(m1, expiry), (m2, expiry)]
        GovPurposeId gaid <-
          electCommittee
            SNothing
            drep
            Set.empty
            members
        submitYesVote_ (StakePoolVoter spoC) gaid
        pure [m1, m2]
    it "maxTermLength = 0" $ do
      -- ======== EPOCH e ========

      let termLength = EpochInterval 0
      modifyPParams $ \pp ->
        pp
          & ppCommitteeMaxTermLengthL .~ termLength

      (drep, _, _) <- setupSingleDRep 1_000_000
      (spoC, _, _) <- setupPoolWithStake $ Coin 10_000_000
      initialMembers <- getCommitteeMembers

      -- Setup new committee members with max term length of 0 epoch
      newMembers <- electMembersWithMaxTermLength spoC drep

      curProtVer <- getProtVer
      nextMajorVersion <- succVersion $ pvMajor curProtVer
      let nextProtVer = curProtVer {pvMajor = nextMajorVersion}

      -- Create a proposal for a hard-fork initiation
      gid <- submitGovAction $ HardForkInitiation SNothing nextProtVer

      -- Accept the proposal with all the governing bodies except for the CC
      submitYesVote_ (StakePoolVoter spoC) gid
      submitYesVote_ (DRepVoter drep) gid

      passEpoch
      -- ======== EPOCH e+1 ========

      hotCs <- mapM registerCommitteeHotKey newMembers
      -- CC members can now vote for the hard-fork
      submitYesVoteCCs_ hotCs gid

      -- Although elected, new CC members are not yet active at this point
      -- since it takes two epochs for their election to take effect, hence
      -- the check fails
      isCommitteeAccepted gid `shouldReturn` False
      mapM_ expectCommitteeMemberAbsence newMembers

      passEpoch
      -- ======== EPOCH e+2 ========

      -- Two epochs passed since the proposal and all the governing bodies except the
      -- CC have voted 'Yes' for the hard-fork immediately. However, since the CC could only
      -- vote in the next epoch, the hard-fork is not yet enacted...
      getLastEnactedHardForkInitiation `shouldReturn` SNothing
      -- ...but now we can see that the CC accepted the proposal...
      isCommitteeAccepted gid `shouldReturn` True
      -- ...and that they are elected members now, albeit already expired ones
      expectMembers $ initialMembers <> Set.fromList newMembers
      mapM_ ccShouldBeExpired newMembers

      passEpoch
      -- ======== EPOCH e+3 ========

      -- Two epochs passed since the CCs also accepted the hard-fork, however
      -- it didn't get enacted because the CCs expired by now and thus
      -- their votes don't count
      getLastEnactedHardForkInitiation `shouldReturn` SNothing
      getProtVer `shouldReturn` curProtVer
      isCommitteeAccepted gid `shouldReturn` False
    it "maxTermLength = 1" $ do
      -- ======== EPOCH e ========

      let termLength = EpochInterval 1
      modifyPParams $ \pp ->
        pp
          & ppCommitteeMaxTermLengthL .~ termLength

      (drep, _, _) <- setupSingleDRep 1_000_000
      (spoC, _, _) <- setupPoolWithStake $ Coin 10_000_000
      initialMembers <- getCommitteeMembers

      -- Setup new committee members with max term length of 1 epoch
      newMembers <- electMembersWithMaxTermLength spoC drep

      curProtVer <- getProtVer
      nextMajorVersion <- succVersion $ pvMajor curProtVer
      let nextProtVer = curProtVer {pvMajor = nextMajorVersion}

      -- Create a proposal for a hard-fork initiation
      gid <- submitGovAction $ HardForkInitiation SNothing nextProtVer

      -- Accept the proposal with all the governing bodies except for the CC
      submitYesVote_ (StakePoolVoter spoC) gid
      submitYesVote_ (DRepVoter drep) gid

      passEpoch
      -- ======== EPOCH e+1 ========

      hotCs <- mapM registerCommitteeHotKey newMembers
      -- CC members can now vote for the hard-fork
      submitYesVoteCCs_ hotCs gid

      -- Although elected, new CC members are not yet active at this point
      -- since it takes two epochs for their election to take effect, hence
      -- the check fails
      isCommitteeAccepted gid `shouldReturn` False
      mapM_ expectCommitteeMemberAbsence newMembers

      passEpoch
      -- ======== EPOCH e+2 ========

      -- Two epochs passed since the proposal and all the governing bodies except the
      -- CC have voted 'Yes' for the hard-fork immediately. However, since the CC could only
      -- vote in the next epoch, the hard-fork is not yet enacted...
      getLastEnactedHardForkInitiation `shouldReturn` SNothing
      -- ...but now we can see that the CC accepted the proposal...
      isCommitteeAccepted gid `shouldReturn` True
      -- ...and that they are active elected members now
      expectMembers $ initialMembers <> Set.fromList newMembers
      mapM_ ccShouldNotBeExpired newMembers

      passEpoch
      -- ======== EPOCH e+3 ========

      -- Two epochs passed since the CCs also accepted the hard-fork, which
      -- is now enacted since the CCs were active during the check
      getLastEnactedHardForkInitiation `shouldReturn` SJust (GovPurposeId gid)
      getProtVer `shouldReturn` nextProtVer
