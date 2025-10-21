{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Conway.Imp.RatifySpec (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway (
  hardforkConwayBootstrapPhase,
  hardforkConwayDisallowUnelectedCommitteeFromVoting,
 )
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Credential
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Val (zero, (<->))
import Data.Default (def)
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
  (HasCallStack, ConwayEraImp era) =>
  SpecWith (ImpInit (LedgerSpec era))
spec = do
  votingSpec
  delayingActionsSpec
  committeeMinSizeAffectsInFlightProposalsSpec
  paramChangeAffectsProposalsSpec
  committeeExpiryResignationDiscountSpec
  committeeMaxTermLengthSpec
  spoVotesForHardForkInitiation
  initiateHardForkWithLessThanMinimalCommitteeSize
  spoAndCCVotingSpec
  it "Many CC Cold Credentials map to the same Hot Credential act as many votes" $ do
    hotCred NE.:| _ <- registerInitialCommittee
    (dRep, _, _) <- setupSingleDRep =<< uniformRM (10_000_000, 1_000_000_000)
    deposit <- uniformRM (1_000_000, 100_000_000_000)
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
  (HasCallStack, ConwayEraImp era) =>
  SpecWith (ImpInit (LedgerSpec era))
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
    if hardforkConwayBootstrapPhase protVer
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
  (HasCallStack, ConwayEraImp era) =>
  SpecWith (ImpInit (LedgerSpec era))
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

      if hardforkConwayBootstrapPhase protVer
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
      if hardforkConwayBootstrapPhase protVer
        then do
          getLastEnactedParameterChange `shouldReturn` SJust (GovPurposeId gai)
          newRefScriptBaseFee `shouldBe` (25 %! 2)
        else do
          getLastEnactedParameterChange `shouldReturn` SNothing
          newRefScriptBaseFee `shouldBe` initialRefScriptBaseFee

committeeExpiryResignationDiscountSpec ::
  forall era.
  (HasCallStack, ConwayEraImp era) =>
  SpecWith (ImpInit (LedgerSpec era))
committeeExpiryResignationDiscountSpec =
  -- Committee-update proposals are disallowed during bootstrap, so we can only run these tests post-bootstrap
  describe "Expired and resigned committee members are discounted from quorum" $ do
    it "Expired" $ whenPostBootstrap $ do
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
    it "Resigned" $ whenPostBootstrap $ do
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
  (HasCallStack, ConwayEraImp era) =>
  SpecWith (ImpInit (LedgerSpec era))
paramChangeAffectsProposalsSpec =
  -- These tests rely on submitting committee-update proposals and on drep votes, which are disallowed during bootstrap,
  -- so we can only run them post-bootstrap
  describe "ParameterChange affects existing proposals" $ do
    let submitTwoExampleProposalsAndVoteOnTheChild ::
          [(KeyHash 'StakePool, Vote)] ->
          [(Credential 'DRepRole, Vote)] ->
          ImpTestM era (GovActionId, GovActionId)
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
      let setCommitteeUpdateThreshold threshold =
            modifyPParams $ ppDRepVotingThresholdsL . dvtCommitteeNormalL .~ threshold
          getDrepVotingThresholds = getsPParams ppDRepVotingThresholdsL
          enactCommitteeUpdateThreshold threshold dreps hotCommitteeC = do
            drepVotingThresholds <- getDrepVotingThresholds
            paramChange <-
              mkParameterChangeGovAction
                SNothing
                ( emptyPParamsUpdate
                    & ppuDRepVotingThresholdsL
                      .~ SJust (drepVotingThresholds & dvtCommitteeNormalL .~ threshold)
                )
            pcGai <- submitGovAction paramChange
            forM_ dreps $ \drep -> submitYesVote_ (DRepVoter drep) pcGai
            submitYesVote_ (CommitteeVoter hotCommitteeC) pcGai
            isDRepAccepted pcGai `shouldReturn` True
            passNEpochs 2
            (^. dvtCommitteeNormalL) <$> getDrepVotingThresholds `shouldReturn` threshold

      it "Increasing the threshold prevents a hitherto-ratifiable proposal from being ratified" $ whenPostBootstrap $ do
        (drepC, hotCommitteeC, _) <- electBasicCommittee
        setCommitteeUpdateThreshold $ 1 %! 2 -- small threshold
        (drep, _, _) <- setupSingleDRep 1_000_000
        (_gaiParent, gaiChild) <- submitTwoExampleProposalsAndVoteOnTheChild [] [(drep, VoteYes)]
        isDRepAccepted gaiChild `shouldReturn` True
        enactCommitteeUpdateThreshold
          (65 %! 100)
          ([drepC, drep] :: [Credential 'DRepRole])
          hotCommitteeC
        isDRepAccepted gaiChild `shouldReturn` False
      it "Decreasing the threshold ratifies a hitherto-unratifiable proposal" $ whenPostBootstrap $ do
        -- This sets up a stake pool with 1_000_000 Coin
        (drepC, hotCommitteeC, _) <- electBasicCommittee
        setCommitteeUpdateThreshold $ 1 %! 1 -- too large threshold
        (drep, _, _) <- setupSingleDRep 3_000_000
        (spoC, _, _) <- setupPoolWithStake $ Coin 3_000_000
        (gaiParent, gaiChild) <-
          submitTwoExampleProposalsAndVoteOnTheChild [(spoC, VoteYes)] [(drep, VoteYes)]
        logAcceptedRatio gaiChild
        isDRepAccepted gaiChild `shouldReturn` False
        enactCommitteeUpdateThreshold
          (65 %! 100)
          ([drepC, drep] :: [Credential 'DRepRole])
          hotCommitteeC
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
      let setCommitteeUpdateThreshold :: UnitInterval -> ImpTestM era ()
          setCommitteeUpdateThreshold threshold =
            modifyPParams $ ppPoolVotingThresholdsL . pvtCommitteeNormalL .~ threshold
          enactCommitteeUpdateThreshold threshold drepC hotCommitteeC = do
            poolVotingThresholds <- getsPParams ppPoolVotingThresholdsL
            paramChange <-
              mkParameterChangeGovAction
                SNothing
                ( emptyPParamsUpdate
                    & ppuPoolVotingThresholdsL
                      .~ SJust (poolVotingThresholds & pvtCommitteeNormalL .~ threshold)
                )
            pcGai <- submitGovAction paramChange
            submitYesVote_ (DRepVoter drepC) pcGai
            submitYesVote_ (CommitteeVoter hotCommitteeC) pcGai
            passNEpochs 2
            (^. pvtCommitteeNormalL) <$> getsPParams ppPoolVotingThresholdsL `shouldReturn` threshold

      it "Increasing the threshold prevents a hitherto-ratifiable proposal from being ratified" $ whenPostBootstrap $ do
        -- This sets up a stake pool with 1_000_000 Coin
        (drepC, hotCommitteeC, _) <- electBasicCommittee
        setCommitteeUpdateThreshold $ 1 %! 2
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
        enactCommitteeUpdateThreshold (65 %! 100) drepC hotCommitteeC
        isSpoAccepted gaiChild `shouldReturn` False
      it "Decreasing the threshold ratifies a hitherto-unratifiable proposal" $ whenPostBootstrap $ do
        -- This sets up a stake pool with 1_000_000 Coin
        (drepC, hotCommitteeC, _) <- electBasicCommittee
        setCommitteeUpdateThreshold $ 1 %! 1 -- too large threshold
        (poolKH1, _paymentC1, _stakingC1) <- setupPoolWithStake $ Coin 4_000_000
        (poolKH2, _paymentC2, _stakingC2) <- setupPoolWithStake $ Coin 1_000_000
        -- bootstrap: 1 % 2 stake yes (2_000_000); 1 % 2 stake abstain; yes / stake - abstain == 1 % 2
        -- post-bootstrap: 1 % 2 stake yes (2_000_000); 1 % 4 stake didn't vote; 1 % 4 stake no
        (gaiParent, gaiChild) <-
          submitTwoExampleProposalsAndVoteOnTheChild
            [(poolKH1, VoteYes), (poolKH2, VoteNo)]
            [(drepC, VoteYes)]
        isSpoAccepted gaiChild `shouldReturn` False
        enactCommitteeUpdateThreshold (65 %! 100) drepC hotCommitteeC -- smaller threshold
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
    it "A parent ParameterChange proposal can prevent its child from being enacted" $ whenPostBootstrap $ do
      hotCommitteeCs <- registerInitialCommittee
      (drepC, _, _) <- setupSingleDRep 1_000_000
      -- Setup one other DRep with equal stake
      _ <- setupSingleDRep 1_000_000
      -- Set a smaller DRep threshold
      drepVotingThresholds <- getsPParams ppDRepVotingThresholdsL
      modifyPParams $
        ppDRepVotingThresholdsL
          .~ (drepVotingThresholds & dvtPPGovGroupL .~ 1 %! 2)
      -- Submit a parent-child sequence of ParameterChange proposals and vote on
      -- both equally, so that both may be ratified. But, the parent increases
      -- the threshold, and it should prevent the child from being ratified.
      let paramChange parent threshold =
            mkParameterChangeGovAction
              parent
              ( emptyPParamsUpdate
                  & ppuDRepVotingThresholdsL
                    .~ SJust (drepVotingThresholds & dvtPPGovGroupL .~ threshold)
              )
      parentGai <- paramChange SNothing (90 %! 100) >>= submitGovAction
      childGai <- paramChange (SJust parentGai) (75 %! 100) >>= submitGovAction
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
  (HasCallStack, ConwayEraImp era) =>
  SpecWith (ImpInit (LedgerSpec era))
committeeMinSizeAffectsInFlightProposalsSpec =
  -- Treasury withdrawals are disallowed during bootstrap, so we can only run these tests post-bootstrap
  describe "CommitteeMinSize affects in-flight proposals" $ do
    let setCommitteeMinSize n = modifyPParams $ ppCommitteeMinSizeL .~ n
        submitTreasuryWithdrawal amount = do
          rewardAccount <- registerRewardAccount
          submitTreasuryWithdrawals [(rewardAccount, amount)]
    it "TreasuryWithdrawal fails to ratify due to an increase in CommitteeMinSize" $ whenPostBootstrap $ do
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
      treasury <- getsNES treasuryL
      passNEpochs 2
      -- The ParameterChange prevents the TreasuryWithdrawal from being enacted,
      -- because it has higher priority.
      getLastEnactedParameterChange `shouldReturn` SJust (GovPurposeId gaiPC)
      isCommitteeAccepted gaiTW `shouldReturn` False
      currentProposalsShouldContain gaiTW
      getsNES treasuryL `shouldReturn` treasury
    it "TreasuryWithdrawal ratifies due to a decrease in CommitteeMinSize" $ whenPostBootstrap $ do
      disableTreasuryExpansion
      (drepC, hotCommitteeC, _) <- electBasicCommittee
      (spoC, _, _) <- setupPoolWithStake $ Coin 42_000_000
      amount <- uniformRM (Coin 1, Coin 100_000_000)
      -- Ensure sufficient amount in the treasury
      submitTx_ $ mkBasicTx (mkBasicTxBody & treasuryDonationTxBodyL .~ amount)
      passEpoch
      treasury <- getsNES treasuryL
      gaiTW <- submitTreasuryWithdrawal amount
      submitYesVote_ (CommitteeVoter hotCommitteeC) gaiTW
      submitYesVote_ (DRepVoter drepC) gaiTW
      setCommitteeMinSize 2
      isCommitteeAccepted gaiTW `shouldReturn` False
      passNEpochs 2
      getsNES treasuryL `shouldReturn` treasury
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
      getsNES treasuryL `shouldReturn` (treasury <-> amount)

spoVotesForHardForkInitiation ::
  forall era.
  (HasCallStack, ConwayEraImp era) =>
  SpecWith (ImpInit (LedgerSpec era))
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
      impAnn "Submit CC yes vote" $ submitYesVoteCCs_ hotCCs gai
      logString $ "Committee: " <> showExpr hotCCs
      GovActionState {gasCommitteeVotes} <- getGovActionState gai
      logString $ "CC Votes: " <> showExpr gasCommitteeVotes
      minSize <- getsPParams ppCommitteeMinSizeL
      committee <- getCommittee
      logString $ "Min committee size: " <> show minSize
      logString $ "Committee: " <> showExpr committee
      logString . show =<< getsNES nesELL
      impAnn "Accepted by committee" $ isCommitteeAccepted gai `shouldReturn` True
      -- 1 % 4 stake yes; 3 % 4 stake no; yes / stake - abstain < 1 % 2
      impAnn "Submit SPO1 yes vote" $ submitYesVote_ (StakePoolVoter spoK1) gai
      passNEpochs 2
      logRatificationChecks gai
      isSpoAccepted gai `shouldReturn` False
      getLastEnactedHardForkInitiation `shouldReturn` SNothing
      -- 1 % 2 stake yes; 1 % 2 stake no; yes / stake - abstain = 1 % 2
      impAnn "Submit SPO2 yes vote" $ submitYesVote_ (StakePoolVoter spoK2) gai
      isSpoAccepted gai `shouldReturn` True
      passNEpochs 2
      getLastEnactedHardForkInitiation `shouldReturn` SJust (GovPurposeId gai)

votingSpec ::
  forall era.
  (HasCallStack, ConwayEraImp era) =>
  SpecWith (ImpInit (LedgerSpec era))
votingSpec =
  describe "Voting" $ do
    -- These tests involve DRep voting, which is not possible in bootstrap,
    -- so we have to run them only post-bootstrap
    it "SPO needs to vote on security-relevant parameter changes" $ whenPostBootstrap $ do
      ccCreds <- registerInitialCommittee
      (drep, _, _) <- setupSingleDRep 1_000_000
      (khPool, _, _) <- setupPoolWithStake $ Coin 42_000_000
      initMinFeeA <- getsNES $ nesEsL . curPParamsEpochStateL . ppMinFeeAL
      gaidThreshold <- impAnn "Update StakePool thresholds" $ do
        pp <- getsNES $ nesEsL . curPParamsEpochStateL
        (pp ^. ppPoolVotingThresholdsL . pvtPPSecurityGroupL) `shouldBe` (51 %! 100)
        let ppu =
              emptyPParamsUpdate
                & ppuPoolVotingThresholdsL
                  .~ SJust
                    PoolVotingThresholds
                      { pvtPPSecurityGroup = 1 %! 2
                      , pvtMotionNoConfidence = 51 %! 100
                      , pvtHardForkInitiation = 51 %! 100
                      , pvtCommitteeNormal = 65 %! 100
                      , pvtCommitteeNoConfidence = 65 %! 100
                      }
                & ppuGovActionLifetimeL .~ SJust (EpochInterval 15)
        ppUpdateGa <- mkParameterChangeGovAction SNothing ppu
        gaidThreshold <- mkProposal ppUpdateGa >>= submitProposal
        submitYesVote_ (DRepVoter drep) gaidThreshold
        submitYesVoteCCs_ ccCreds gaidThreshold
        logAcceptedRatio gaidThreshold
        pure gaidThreshold
      passEpoch
      logAcceptedRatio gaidThreshold
      passEpoch
      let newMinFeeA = Coin 1000
      gaidMinFee <- do
        pp <- getsNES $ nesEsL . curPParamsEpochStateL
        impAnn "Security group threshold should be 1/2" $
          (pp ^. ppPoolVotingThresholdsL . pvtPPSecurityGroupL) `shouldBe` (1 %! 2)
        ga <-
          mkParameterChangeGovAction
            (SJust gaidThreshold)
            (emptyPParamsUpdate & ppuMinFeeAL .~ SJust newMinFeeA)
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
      logInstantStake
      logAcceptedRatio gaidMinFee
      logRatificationChecks gaidMinFee
      passEpoch
      pp <- getsNES $ nesEsL . curPParamsEpochStateL
      (pp ^. ppMinFeeAL) `shouldBe` newMinFeeA
    describe "Active voting stake" $ do
      describe "DRep" $ do
        it "UTxOs contribute to active voting stake" $ whenPostBootstrap $ do
          -- Setup DRep delegation #1
          (drep1, KeyHashObj stakingKH1, paymentKP1) <- setupSingleDRep 1_000_000_000
          -- Setup DRep delegation #2
          _ <- setupSingleDRep 1_000_000_000
          (spoC, _, _) <- setupPoolWithStake mempty
          -- Submit a committee proposal
          cc <- KeyHashObj <$> freshKeyHash
          addCCGaid <- submitUpdateCommittee Nothing mempty [(cc, EpochInterval 10)] (75 %! 100)
          -- Submit the vote
          submitYesVote_ (DRepVoter drep1) addCCGaid
          submitYesVote_ (StakePoolVoter spoC) addCCGaid
          passNEpochs 2
          -- The vote should not result in a ratification
          isDRepAccepted addCCGaid `shouldReturn` False
          getLastEnactedCommittee `shouldReturn` SNothing
          -- Bump up the UTxO delegated
          -- to barely make the threshold (65 %! 100)
          stakingKP1 <- getKeyPair stakingKH1
          sendCoinTo_ (mkAddr paymentKP1 stakingKP1) (inject $ Coin 857_142_858)
          passNEpochs 2
          -- The same vote should now successfully ratify the proposal
          getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId addCCGaid)
        -- https://github.com/IntersectMBO/cardano-ledger/5014
        -- TODO: Re-enable after issue is resolved, by removing this override
        disableInConformanceIt "Rewards contribute to active voting stake" $ whenPostBootstrap $ do
          -- Setup DRep delegation #1
          (drep1, staking1, _) <- setupSingleDRep 1_000_000_000
          -- Setup DRep delegation #2
          _ <- setupSingleDRep 1_000_000_000
          (spoC, _, _) <- setupPoolWithStake mempty
          -- Submit a committee proposal
          cc <- KeyHashObj <$> freshKeyHash
          addCCGaid <- submitUpdateCommittee Nothing mempty [(cc, EpochInterval 10)] (75 %! 100)
          -- Submit the vote
          submitYesVote_ (DRepVoter drep1) addCCGaid
          submitYesVote_ (StakePoolVoter spoC) addCCGaid
          passNEpochs 2
          -- The vote should not result in a ratification
          isDRepAccepted addCCGaid `shouldReturn` False
          getLastEnactedCommittee `shouldReturn` SNothing
          -- Add to the rewards of the delegator to this DRep
          -- to barely make the `dvtCommitteeNormal` threshold (65 %! 100)
          modifyNES $
            nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL
              %~ addToBalanceAccounts (Map.singleton staking1 (CompactCoin 857_142_858))
          passNEpochs 2
          -- The same vote should now successfully ratify the proposal
          getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId addCCGaid)
        -- https://github.com/IntersectMBO/formal-ledger-specifications/issues/926
        -- TODO: Re-enable after issue is resolved, by removing this override
        disableInConformanceIt "Rewards contribute to active voting stake even in the absence of StakeDistr" $ whenPostBootstrap $ do
          let govActionLifetime = 5
              govActionDeposit = Coin 1_000_000
              poolDeposit = Coin 858_000
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
          getBalance (KeyHashObj stakingKH1) `shouldReturn` govActionDeposit
          -- Setup DRep delegation #2
          (_drepKH2, stakingKH2) <- setupDRepWithoutStake
          (spoC, _, _) <- setupPoolWithStake $ Coin 42_000_000
          -- Add rewards to delegation #2
          submitAndExpireProposalToMakeReward $ KeyHashObj stakingKH2
          getBalance (KeyHashObj stakingKH2) `shouldReturn` govActionDeposit
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
          -- to barely make the threshold (65 %! 100)
          registerAndRetirePoolToMakeReward $ KeyHashObj stakingKH1
          getBalance (KeyHashObj stakingKH1) `shouldReturn` poolDeposit <> govActionDeposit
          isDRepAccepted addCCGaid `shouldReturn` True
          -- The same vote should now successfully ratify the proposal
          passEpoch
          getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId addCCGaid)
        describe "Proposal deposits contribute to active voting stake" $ do
          -- https://github.com/IntersectMBO/cardano-ledger/5014
          -- TODO: Re-enable after issue is resolved, by removing this override
          disableInConformanceIt "Directly" $ whenPostBootstrap $ do
            -- Only modify the applicable thresholds
            modifyPParams $ ppGovActionDepositL .~ Coin 1_000_000
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
          it "After switching delegations" $ whenPostBootstrap $ do
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
        it "acceptedRatio with default DReps" $ whenPostBootstrap $ do
          (drep1, _, committeeGovId) <- electBasicCommittee
          (_, drep2Staking, _) <- setupSingleDRep 1_000_000

          paramChangeGovId <- submitParameterChange SNothing $ def & ppuMinFeeAL .~ SJust (Coin 1000)
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

        it "AlwaysNoConfidence" $ whenPostBootstrap $ do
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
        it "AlwaysAbstain" $ whenPostBootstrap $ do
          let getTreasury = getsNES treasuryL

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

        it "DRepAlwaysNoConfidence is sufficient to pass NoConfidence" $ whenPostBootstrap $ do
          modifyPParams $ \pp ->
            pp
              & ppPoolVotingThresholdsL . pvtMotionNoConfidenceL .~ 0 %! 1
              & ppDRepVotingThresholdsL . dvtMotionNoConfidenceL .~ 1 %! 1
              & ppCoinsPerUTxOByteL .~ CoinPerByte (Coin 1)
          (drep, _, committeeId) <- electBasicCommittee
          cred <- KeyHashObj <$> freshKeyHash
          _ <- registerStakeCredential cred
          _ <- delegateToDRep cred (Coin 300) DRepAlwaysNoConfidence
          noConfidence <- submitGovAction (NoConfidence (SJust committeeId))
          submitYesVote_ (DRepVoter drep) noConfidence
          logAcceptedRatio noConfidence
          passNEpochs 2
          getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId noConfidence)

      describe "StakePool" $ do
        it "UTxOs contribute to active voting stake" $ whenPostBootstrap $ do
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
          sendCoinTo_ (mkAddr delegatorCPayment1 delegatorCStaking1) (Coin 40_900_000)
          passNEpochs 2
          -- The same vote should now successfully ratify the proposal
          getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId addCCGaid)
        it "Rewards contribute to active voting stake" $ whenPostBootstrap $ do
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
            nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL
              %~ addToBalanceAccounts (Map.singleton delegatorCStaking1 (CompactCoin 200_000_000))
          passNEpochs 2
          -- The same vote should now successfully ratify the proposal
          getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId addCCGaid)
        -- https://github.com/IntersectMBO/formal-ledger-specifications/issues/926
        -- TODO: Re-enable after issue is resolved, by removing this override
        disableInConformanceIt "Rewards contribute to active voting stake even in the absence of StakeDistr" $
          whenPostBootstrap $ do
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
            getBalance delegatorCStaking1 `shouldReturn` govActionDeposit
            -- Setup Pool delegation #2
            (poolKH2, delegatorCStaking2) <- setupPoolWithoutStake
            -- Add rewards to delegation #2
            submitAndExpireProposalToMakeReward delegatorCStaking2
            getBalance delegatorCStaking2 `shouldReturn` govActionDeposit
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
            getBalance delegatorCStaking1 `shouldReturn` poolDeposit <> govActionDeposit
            -- The same vote should now successfully ratify the proposal
            -- NOTE: It takes 2 epochs for SPO votes as opposed to 1 epoch
            -- for DRep votes to ratify a proposal.
            passNEpochs 2
            getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId addCCGaid)
        describe "Proposal deposits contribute to active voting stake" $ do
          it "Directly" $ whenPostBootstrap $ do
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
          it "After switching delegations" $ whenPostBootstrap $ do
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
      it "Motion of no-confidence" $ whenPostBootstrap $ do
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
      it "Update committee - normal state" $ whenPostBootstrap $ do
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
      it "Hard-fork initiation" $ whenPostBootstrap $ do
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
        $ whenPostBootstrap
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

    describe "SPO default votes" $ do
      describe "During bootstrap phase" $ do
        it "Default vote is Abstain in general" $ whenBootstrap $ do
          (spoC1, _payment, _staking) <- setupPoolWithStake $ Coin 1_000_000
          (spoC2, _payment, _staking) <- setupPoolWithStake $ Coin 1_000_000
          (spoC3, _payment, _staking) <- setupPoolWithStake $ Coin 1_000_000

          gid <-
            submitParameterChange SNothing $
              emptyPParamsUpdate
                & ppuMinFeeRefScriptCostPerByteL .~ SJust (25 %! 2)

          impAnn
            ( "No SPO has voted so far and the default vote is abstain:\n"
                <> "YES: 0; ABSTAIN (default): 3; NO: 0 -> YES / total - ABSTAIN == 0 % 0"
            )
            $ calculatePoolAcceptedRatio gid `shouldReturn` 0

          passNEpochs 2

          impAnn
            ( "One SPO voted yes and the default vote is abstain:\n"
                <> "YES: 1; ABSTAIN (default): 2; NO: 0 -> YES / total - ABSTAIN == 1 % 1"
            )
            $ do
              submitYesVote_ (StakePoolVoter spoC1) gid
              calculatePoolAcceptedRatio gid `shouldReturn` 1

          impAnn
            ( "One SPO voted yes, another voted no and the default vote is abstain:\n"
                <> "YES: 1; ABSTAIN (default): 1; NO: 1 -> YES / total - ABSTAIN == 1 % 2"
            )
            $ do
              submitVote_ VoteNo (StakePoolVoter spoC2) gid
              calculatePoolAcceptedRatio gid `shouldReturn` (1 % 2)

          impAnn
            ( "Two SPOs voted yes, another voted no and the default vote is abstain:\n"
                <> "YES: 2; ABSTAIN (default): 0; NO: 1 -> YES / total - ABSTAIN == 2 % 3"
            )
            $ do
              submitYesVote_ (StakePoolVoter spoC3) gid
              calculatePoolAcceptedRatio gid `shouldReturn` (2 % 3)

        it "HardForkInitiation - default vote is No" $ whenBootstrap $ do
          curProtVer <- getProtVer
          nextMajorVersion <- succVersion $ pvMajor curProtVer
          let nextProtVer = curProtVer {pvMajor = nextMajorVersion}

          (spoC1, _payment, _staking) <- setupPoolWithStake $ Coin 1_000_000
          (spoC2, _payment, _staking) <- setupPoolWithStake $ Coin 1_000_000
          (_spoC3, _payment, _staking) <- setupPoolWithStake $ Coin 1_000_000

          gid <- submitGovAction $ HardForkInitiation SNothing nextProtVer

          impAnn
            ( "No SPO has voted so far and the default vote is no:\n"
                <> "YES: 0; ABSTAIN: 0; NO (default): 3 -> YES / total - ABSTAIN == 0 % 3"
            )
            $ calculatePoolAcceptedRatio gid `shouldReturn` 0

          passNEpochs 2

          impAnn
            ( "One SPO voted yes and the default vote is no:\n"
                <> "YES: 1; ABSTAIN: 0; NO (default): 2 -> YES / total - ABSTAIN == 1 % 3"
            )
            $ do
              submitYesVote_ (StakePoolVoter spoC1) gid
              calculatePoolAcceptedRatio gid `shouldReturn` (1 %! 3)

          impAnn
            ( "One SPO voted yes, another abstained and the default vote is no:\n"
                <> "YES: 1; ABSTAIN: 1; NO (default): 2 -> YES / total - ABSTAIN == 1 % 2"
            )
            $ do
              submitVote_ Abstain (StakePoolVoter spoC2) gid
              calculatePoolAcceptedRatio gid `shouldReturn` (1 %! 2)

      describe "After bootstrap phase" $ do
        it "Default vote is No in general" $ whenPostBootstrap $ do
          (spoC1, _payment, _staking) <- setupPoolWithStake $ Coin 1_000_000
          (spoC2, _payment, _staking) <- setupPoolWithStake $ Coin 1_000_000
          (_spoC3, _payment, _staking) <- setupPoolWithStake $ Coin 1_000_000

          cc <- KeyHashObj <$> freshKeyHash
          gid <- submitUpdateCommittee Nothing mempty [(cc, EpochInterval 5)] (1 %! 2)

          impAnn
            ( "No SPO has voted so far and the default vote is no:\n"
                <> "YES: 0; ABSTAIN: 0; NO (default): 3 -> YES / total - ABSTAIN == 0 % 3"
            )
            $ calculatePoolAcceptedRatio gid `shouldReturn` 0

          passNEpochs 2

          impAnn
            ( "One SPO voted yes and the default vote is no:\n"
                <> "YES: 1; ABSTAIN: 0; NO (default): 2 -> YES / total - ABSTAIN == 1 % 3"
            )
            $ do
              submitYesVote_ (StakePoolVoter spoC1) gid
              calculatePoolAcceptedRatio gid `shouldReturn` (1 %! 3)

          impAnn
            ( "One SPO voted yes, another abstained and the default vote is no:\n"
                <> "YES: 1; ABSTAIN: 1; NO (default): 1 -> YES / total - ABSTAIN == 1 % 2"
            )
            $ do
              submitVote_ Abstain (StakePoolVoter spoC2) gid
              calculatePoolAcceptedRatio gid `shouldReturn` (1 %! 2)

          getLastEnactedCommittee `shouldReturn` SNothing

        it "HardForkInitiation - default vote is No" $ whenPostBootstrap $ do
          curProtVer <- getProtVer
          nextMajorVersion <- succVersion $ pvMajor curProtVer
          let nextProtVer = curProtVer {pvMajor = nextMajorVersion}

          (spoC1, _payment, _staking) <- setupPoolWithStake $ Coin 1_000_000
          (spoC2, _payment, _staking) <- setupPoolWithStake $ Coin 1_000_000
          hotCs <- registerInitialCommittee
          (drep, _, _) <- setupSingleDRep 1_000_000

          gid <- submitGovAction $ HardForkInitiation SNothing nextProtVer

          impAnn
            ( "No SPO has voted so far and the default vote is no:\n"
                <> "YES: 0; ABSTAIN: 0; NO (default): 2 -> YES / total - ABSTAIN == 0 % 2"
            )
            $ calculatePoolAcceptedRatio gid `shouldReturn` 0

          submitYesVoteCCs_ hotCs gid
          submitYesVote_ (DRepVoter drep) gid

          passNEpochs 2

          getLastEnactedHardForkInitiation `shouldReturn` SNothing

          impAnn
            ( "One SPO voted yes and the default vote is no:\n"
                <> "YES: 1; ABSTAIN: 0; NO (default): 1 -> YES / total - ABSTAIN == 1 % 2"
            )
            $ do
              submitVote_ VoteYes (StakePoolVoter spoC2) gid
              calculatePoolAcceptedRatio gid `shouldReturn` (1 %! 2)

          impAnn
            ( "Although the other SPO delegated its reward account to an AlwaysAbstain DRep,\n"
                <> "since this is a HardForkInitiation action, their default vote remains no regardless:\n"
                <> "YES: 1; ABSTAIN: 0; NO (default): 1 -> YES / total - ABSTAIN == 1 % 2"
            )
            $ do
              delegateSPORewardAddressToDRep_ spoC1 (Coin 1_000_000) DRepAlwaysAbstain
              calculatePoolAcceptedRatio gid `shouldReturn` (1 %! 2)

          impAnn
            ( "One SPO voted yes, the other explicitly abstained and the default vote is no:\n"
                <> "YES: 1; ABSTAIN: 1; NO (default): 0 -> YES / total - ABSTAIN == 1 % 1"
            )
            $ do
              submitVote_ Abstain (StakePoolVoter spoC1) gid
              calculatePoolAcceptedRatio gid `shouldReturn` 1

          passNEpochs 2

          getLastEnactedHardForkInitiation `shouldReturn` SJust (GovPurposeId gid)

        it "Reward account delegated to AlwaysNoConfidence" $ whenPostBootstrap $ do
          (spoC, _payment, _staking) <- setupPoolWithStake $ Coin 1_000_000
          (drep, _, _) <- setupSingleDRep 1_000_000

          gid <- submitGovAction $ NoConfidence SNothing

          impAnn
            ( "No SPO has voted so far and the default vote is no:\n"
                <> "YES: 0; ABSTAIN: 0; NO (default): 1 -> YES / total - ABSTAIN == 0 % 1"
            )
            $ calculatePoolAcceptedRatio gid `shouldReturn` 0

          submitYesVote_ (DRepVoter drep) gid

          passNEpochs 2

          impAnn "Only the DReps accepted the proposal so far, so it should not be enacted yet" $
            getCommitteeMembers `shouldNotReturn` mempty

          impAnn
            ( "The SPO delegated its reward account to an AlwaysNoConfidence DRep,\n"
                <> "since this is a NoConfidence action, their default vote will now count as a yes:\n"
                <> "YES: 1; ABSTAIN: 0; NO (default): 0 -> YES / total - ABSTAIN == 1 % 1"
            )
            $ do
              delegateSPORewardAddressToDRep_ spoC (Coin 1_000_000) DRepAlwaysNoConfidence
              calculatePoolAcceptedRatio gid `shouldReturn` 1

          passNEpochs 2

          getCommitteeMembers `shouldReturn` mempty

        it "Reward account delegated to AlwaysAbstain" $ whenPostBootstrap $ do
          (spoC1, _payment, _staking) <- setupPoolWithStake $ Coin 1_000_000
          (spoC2, _payment, _staking) <- setupPoolWithStake $ Coin 1_000_000
          (drep, _, _) <- setupSingleDRep 1_000_000

          gid <- submitGovAction $ NoConfidence SNothing

          impAnn
            ( "No SPO has voted so far and the default vote is no:\n"
                <> "YES: 0; ABSTAIN: 0; NO (default): 2 -> YES / total - ABSTAIN == 0 % 2"
            )
            $ calculatePoolAcceptedRatio gid `shouldReturn` 0

          submitYesVote_ (DRepVoter drep) gid

          passNEpochs 2

          impAnn "Only the DReps accepted the proposal so far, so it should not be enacted yet" $
            getCommitteeMembers `shouldNotReturn` mempty

          impAnn
            ( "One SPO voted yes and the default vote is no:\n"
                <> "YES: 1; ABSTAIN: 0; NO (default): 1 -> YES / total - ABSTAIN == 1 % 2"
            )
            $ do
              submitYesVote_ (StakePoolVoter spoC2) gid
              calculatePoolAcceptedRatio gid `shouldReturn` (1 %! 2)

          impAnn
            ( "One SPO voted yes and the other SPO delegated its reward account to an AlwaysAbstain DRep:\n"
                <> "YES: 1; ABSTAIN: 1; NO (default): 0 -> YES / total - ABSTAIN == 1 % 1"
            )
            $ do
              delegateSPORewardAddressToDRep_ spoC1 (Coin 1_000_000) DRepAlwaysAbstain
              calculatePoolAcceptedRatio gid `shouldReturn` 1

          passNEpochs 2

          getCommitteeMembers `shouldReturn` mempty

delayingActionsSpec ::
  forall era.
  (HasCallStack, ConwayEraImp era) =>
  SpecWith (ImpInit (LedgerSpec era))
delayingActionsSpec =
  -- All tests below are relying on submitting constitution of committe-update proposals, which are disallowed during bootstrap,
  -- so we can only run them post-bootstrap.
  describe "Delaying actions" $ do
    it
      "A delaying action delays its child even when both ere proposed and ratified in the same epoch"
      $ whenPostBootstrap
      $ do
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
      $ whenPostBootstrap
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
      -- https://github.com/IntersectMBO/formal-ledger-specifications/issues/923
      -- TODO: Re-enable after issue is resolved, by removing this override
      disableInConformanceIt "Same lineage" $ whenPostBootstrap $ do
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
      -- https://github.com/IntersectMBO/formal-ledger-specifications/issues/923
      -- TODO: Re-enable after issue is resolved, by removing this override
      disableInConformanceIt "Other lineage" $ whenPostBootstrap $ do
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
      it "proposals to update the committee get delayed if the expiration exceeds the max term" $ whenPostBootstrap $ do
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
              submitCommitteeElection
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
  (HasCallStack, ConwayEraImp era) =>
  SpecWith (ImpInit (LedgerSpec era))
committeeMaxTermLengthSpec =
  -- Committee-update proposals are disallowed during bootstrap, so we can only run these tests post-bootstrap
  describe "Committee members can serve full `CommitteeMaxTermLength`" $ do
    let
      electMembersWithMaxTermLength ::
        KeyHash 'StakePool ->
        Credential 'DRepRole ->
        ImpTestM era [Credential 'ColdCommitteeRole]
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
          submitCommitteeElection
            SNothing
            drep
            Set.empty
            members
        submitYesVote_ (StakePoolVoter spoC) gaid
        pure [m1, m2]
    it "maxTermLength = 0" $ whenPostBootstrap $ do
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

      -- Upto protocol version 10, CC members can vote for the hard-fork
      -- although their election hasn't been enacted yet. We need to pass
      -- another epoch to have their enactment.
      when (hardforkConwayDisallowUnelectedCommitteeFromVoting curProtVer) passEpoch
      submitYesVoteCCs_ hotCs gid

      unless (hardforkConwayDisallowUnelectedCommitteeFromVoting curProtVer) $ do
        -- Although elected, new CC members are not yet active at this point
        -- since it takes two epochs for their election to take effect, hence
        -- the check fails
        isCommitteeAccepted gid `shouldReturn` False
        mapM_ expectCommitteeMemberAbsence newMembers

      passEpoch
      -- ======== EPOCH e+(2 or 3) ========

      -- Two (or three) epochs passed since the proposal and all the governing
      -- bodies except the CC have voted 'Yes' for the hard-fork immediately.
      -- However, since the CC could only vote in the next epoch, the hard-fork
      -- is not yet enacted...
      getLastEnactedHardForkInitiation `shouldReturn` SNothing
      -- ...but now, until PV 10, we can see that the CC accepted the proposal...
      unless (hardforkConwayDisallowUnelectedCommitteeFromVoting curProtVer) $
        isCommitteeAccepted gid `shouldReturn` True
      -- ...and that they are elected members now, albeit already expired ones
      expectMembers $ initialMembers <> Set.fromList newMembers
      mapM_ ccShouldBeExpired newMembers

      passEpoch
      -- ======== EPOCH e+(3 or 4) ========

      -- Three (or four) epochs passed since the CCs also accepted the
      -- hard-fork, however it didn't get enacted because the CCs expired by now
      -- and thus their votes don't count
      getLastEnactedHardForkInitiation `shouldReturn` SNothing
      getProtVer `shouldReturn` curProtVer
      isCommitteeAccepted gid `shouldReturn` False
    it "maxTermLength = 1" $ whenPostBootstrap $ do
      -- ======== EPOCH e ========

      let termLength = EpochInterval 1
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

      passNEpochs 2
      -- ======== EPOCH e+2 ========

      hotCs <- mapM registerCommitteeHotKey newMembers
      -- CC members can now vote for the hard-fork
      submitYesVoteCCs_ hotCs gid

      -- Two epochs passed since the proposal and all the governing bodies except the
      -- CC have voted 'Yes' for the hard-fork immediately. However, since the CC could only
      -- vote in the next epoch, the hard-fork is not yet enacted...
      getLastEnactedHardForkInitiation `shouldReturn` SNothing
      -- ...but now we can see that the CC accepted the proposal...
      isCommitteeAccepted gid `shouldReturn` True
      -- ...and that they are elected members now, albeit already expired ones
      expectMembers $ initialMembers <> Set.fromList newMembers
      mapM_ ccShouldNotBeExpired newMembers

      passNEpochs 2
      -- ======== EPOCH e+4 ========
      mapM_ ccShouldBeExpired newMembers

      -- Two epochs passed since the CCs also accepted the hard-fork, however
      -- it didn't get enacted because the CCs expired by now and thus
      -- their votes don't count
      getLastEnactedHardForkInitiation `shouldReturn` SNothing
      getProtVer `shouldReturn` curProtVer
      isCommitteeAccepted gid `shouldReturn` False
    it "maxTermLength = 2" $ whenPostBootstrap $ do
      -- ======== EPOCH e ========

      let termLength = EpochInterval 2
      modifyPParams $ \pp ->
        pp
          & ppCommitteeMaxTermLengthL .~ termLength

      (drep, _, _) <- setupSingleDRep 1_000_000
      (spoC, _, _) <- setupPoolWithStake $ Coin 10_000_000
      initialMembers <- getCommitteeMembers

      -- Setup new committee members with max term length of 2 epoch
      newMembers <- electMembersWithMaxTermLength spoC drep

      curProtVer <- getProtVer
      nextMajorVersion <- succVersion $ pvMajor curProtVer
      let nextProtVer = curProtVer {pvMajor = nextMajorVersion}

      -- Create a proposal for a hard-fork initiation
      gid <- submitGovAction $ HardForkInitiation SNothing nextProtVer

      -- Accept the proposal with all the governing bodies except for the CC
      submitYesVote_ (StakePoolVoter spoC) gid
      submitYesVote_ (DRepVoter drep) gid

      passNEpochs 2
      -- ======== EPOCH e+2 ========

      hotCs <- mapM registerCommitteeHotKey newMembers
      -- CC members can now vote for the hard-fork
      submitYesVoteCCs_ hotCs gid

      -- Two epochs passed since the proposal and all the governing bodies except the
      -- CC have voted 'Yes' for the hard-fork immediately. However, since the CC could only
      -- vote in the next epoch, the hard-fork is not yet enacted...
      getLastEnactedHardForkInitiation `shouldReturn` SNothing
      -- ...but now we can see that the CC accepted the proposal...
      isCommitteeAccepted gid `shouldReturn` True
      -- ...and that they are active elected members now
      expectMembers $ initialMembers <> Set.fromList newMembers
      mapM_ ccShouldNotBeExpired newMembers

      passNEpochs 2
      -- ======== EPOCH e+4 ========
      mapM_ ccShouldBeExpired newMembers

      -- Two epochs passed since the CCs also accepted the hard-fork, which
      -- is now enacted since the CCs were active during the check
      getLastEnactedHardForkInitiation `shouldReturn` SJust (GovPurposeId gid)
      getProtVer `shouldReturn` nextProtVer
