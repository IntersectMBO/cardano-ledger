{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Conway.Imp.EpochSpec (spec) where

import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.BaseTypes (EpochInterval (..), addEpochInterval)
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams
import Cardano.Ledger.Conway.Rules (
  ConwayEpochEvent (GovInfoEvent),
  ConwayNewEpochEvent (..),
 )
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (Event, ShelleyTickEvent (..))
import Cardano.Ledger.Val
import Data.Default (Default (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Data.Tree
import Data.Typeable (cast)
import Lens.Micro
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational (IsRatio (..), (%!))
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ( ConwayEraImp era
  , Event (EraRule "NEWEPOCH" era) ~ ConwayNewEpochEvent era
  , Event (EraRule "EPOCH" era) ~ ConwayEpochEvent era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec = do
  dRepVotingSpec
  treasurySpec
  proposalsSpec
  dRepSpec
  eventsSpec

proposalsSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
proposalsSpec =
  describe "Proposals" $ do
    it "Proposals survive multiple epochs without any activity" $ do
      -- + 2 epochs to pass to get the desired effect
      modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 4
      _tree <-
        submitParameterChangeTree SNothing $
          Node
            ()
            [ Node
                ()
                [ Node () []
                , Node () []
                ]
            , Node () []
            ]

      forest <- getProposals
      passNEpochs 5
      forest' <- getProposals
      forest' `shouldBe` forest
      passEpoch
      forest'' <- getProposals
      forest'' `shouldBe` def
    it "Expired proposal deposit refunded" $ do
      let deposit = Coin 999
      modifyPParams $ \pp ->
        pp
          & ppGovActionLifetimeL .~ EpochInterval 1
          & ppGovActionDepositL .~ deposit
      rewardAccount <- registerRewardAccount

      initialValue <- getsNES (nesEsL . curPParamsEpochStateL . ppMinFeeAL)

      parameterChangeAction <- mkMinFeeUpdateGovAction SNothing
      govActionId <-
        mkProposalWithRewardAccount
          parameterChangeAction
          rewardAccount
          >>= submitProposal
      expectPresentGovActionId govActionId
      passNEpochs 3
      expectMissingGovActionId govActionId

      getsNES (nesEsL . curPParamsEpochStateL . ppMinFeeAL) `shouldReturn` initialValue
      getAccountBalance rewardAccount `shouldReturn` deposit

    it "Proposals are expired and removed as expected" $ whenPostBootstrap $ do
      modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 1

      curConstitution <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
      initialPulser <- getsNES $ newEpochStateGovStateL . drepPulsingStateGovStateL
      initialEnactState <- getEnactState

      govActionId <- submitConstitution SNothing
      curConstitution' <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
      impAnn "Constitution has not been enacted yet" $
        curConstitution' `shouldBe` curConstitution

      govState <- getsNES newEpochStateGovStateL
      let expectedProposals = govState ^. proposalsGovStateL
          expectedPulser = govState ^. drepPulsingStateGovStateL
      expectedEnactState <- getEnactState

      impAnn "EnactState reflects the submitted governance action" $ do
        expectedEnactState `shouldBe` initialEnactState

      impAnn "Proposals contain the submitted proposal" $
        expectedProposals `shouldSatisfy` \props -> govActionId `elem` proposalsIds props

      impAnn "Pulser has not changed" $
        expectedPulser `shouldBe` initialPulser

      passNEpochs 2
      impAnn "Proposal gets removed after expiry" $ do
        govStateFinal <- getsNES newEpochStateGovStateL
        let ratifyState = extractDRepPulsingState (govStateFinal ^. drepPulsingStateGovStateL)
        rsExpired ratifyState `shouldBe` Set.singleton govActionId
  where
    submitParameterChangeTree = submitGovActionTree $ mkMinFeeUpdateGovAction >=> submitGovAction

dRepSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
dRepSpec =
  describe "DRep" $ do
    let submitParamChangeProposal = mkMinFeeUpdateGovAction SNothing >>= submitGovAction_
    -- https://github.com/IntersectMBO/formal-ledger-specifications/issues/923
    -- TODO: Re-enable after issue is resolved, by removing this override
    disableInConformanceIt "expiry is updated based on the number of dormant epochs" $ do
      modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
      (drep, _, _) <- setupSingleDRep 1_000_000

      startEpochNo <- getsNES nesELL
      let
        -- compute the epoch number that is an offset from starting epoch number
        offDRepActivity = addEpochInterval startEpochNo . EpochInterval
      expectNumDormantEpochs 0

      -- epoch 0: we submit a proposal
      submitParamChangeProposal
      passNEpochsChecking 2 $ do
        expectNumDormantEpochs 0
        expectDRepExpiry drep $ offDRepActivity 100

      passEpoch -- entering epoch 3
      -- proposal has expired
      expectNumDormantEpochs 1
      expectDRepExpiry drep $ offDRepActivity 100

      passEpoch -- entering epoch 4
      expectNumDormantEpochs 2
      expectDRepExpiry drep $ offDRepActivity 100

      passEpoch -- entering epoch 5
      expectNumDormantEpochs 3
      expectDRepExpiry drep $ offDRepActivity 100

      submitParamChangeProposal
      -- number of dormant epochs is added to the drep expiry and reset to 0
      expectNumDormantEpochs 0
      expectDRepExpiry drep $ offDRepActivity 103

      passEpoch -- entering epoch 6
      expectNumDormantEpochs 0
      expectDRepExpiry drep $ offDRepActivity 103
    -- https://github.com/IntersectMBO/formal-ledger-specifications/issues/923
    -- TODO: Re-enable after issue is resolved, by removing this override
    disableInConformanceIt "expiry is not updated for inactive DReps" $ do
      let
        drepActivity = 2
      modifyPParams $ \pp ->
        pp
          & ppGovActionLifetimeL .~ EpochInterval 2
          & ppDRepActivityL .~ EpochInterval drepActivity
      (drep, _, _) <- setupSingleDRep 1_000_000
      startEpochNo <- getsNES nesELL
      let
        -- compute the epoch number that is an offset from starting epoch number plus
        -- the ppDRepActivity parameter
        offDRepActivity offset =
          addEpochInterval startEpochNo $ EpochInterval (drepActivity + offset)

      expectNumDormantEpochs 0

      -- epoch 0: we submit a proposal
      submitParamChangeProposal
      passNEpochsChecking 2 $ do
        expectNumDormantEpochs 0
        expectDRepExpiry drep $ offDRepActivity 0

      passEpoch -- entering epoch 3
      -- proposal has expired
      -- drep has expired
      expectNumDormantEpochs 1
      expectDRepExpiry drep $ offDRepActivity 0
      expectActualDRepExpiry drep $ offDRepActivity 1
      isDRepExpired drep `shouldReturn` False -- numDormantEpochs is added to the drep exiry calculation
      passEpoch -- entering epoch 4
      expectNumDormantEpochs 2
      expectDRepExpiry drep $ offDRepActivity 0
      expectActualDRepExpiry drep $ offDRepActivity 2

      passEpoch -- entering epoch 5
      expectNumDormantEpochs 3
      expectDRepExpiry drep $ offDRepActivity 0
      expectActualDRepExpiry drep $ offDRepActivity 3

      submitParamChangeProposal
      -- number of dormant epochs is added to the drep, considering they are not actually expired,
      -- and is reset to 0
      expectNumDormantEpochs 0
      expectDRepExpiry drep $ offDRepActivity 3

      passEpoch -- entering epoch 6
      expectNumDormantEpochs 0
      expectDRepExpiry drep $ offDRepActivity 3
    -- https://github.com/IntersectMBO/formal-ledger-specifications/issues/926
    -- TODO: Re-enable after issue is resolved, by removing this override
    disableInConformanceIt "expiry updates are correct for a mixture of cases" $ do
      let
        drepActivity = 4
      modifyPParams $ \pp ->
        pp
          & ppGovActionLifetimeL .~ EpochInterval 2
          & ppDRepActivityL .~ EpochInterval drepActivity
      startEpochNo <- getsNES nesELL
      let
        -- compute the epoch number that is an offset from starting epoch number plus
        -- the ppDRepActivity parameter
        offDRepActivity offset =
          addEpochInterval startEpochNo $ EpochInterval (drepActivity + offset)
      (drep1, _, _) <- setupSingleDRep 1_000_000 -- Receives an expiry update transaction certificate
      (drep2, _, _) <- setupSingleDRep 1_000_000 -- Turns inactive due to natural expiry
      (drep3, _, _) <- setupSingleDRep 1_000_000 -- Unregisters and gets deleted
      expectNumDormantEpochs 0

      -- epoch 0: we submit a proposal
      _ <- submitGovAction InfoAction
      passNEpochsChecking 2 $ do
        expectNumDormantEpochs 0
        expectDRepExpiry drep1 $ offDRepActivity 0
        expectDRepExpiry drep2 $ offDRepActivity 0
        expectDRepExpiry drep3 $ offDRepActivity 0

      passEpoch -- entering epoch 3
      -- proposal has expired
      expectCurrentProposals
      expectPulserProposals
      expectNumDormantEpochs 1
      expectDRepExpiry drep1 $ offDRepActivity 0
      expectDRepExpiry drep2 $ offDRepActivity 0
      expectDRepExpiry drep3 $ offDRepActivity 0

      passEpoch -- entering epoch 4
      expectNumDormantEpochs 2
      expectDRepExpiry drep1 $ offDRepActivity 0
      expectDRepExpiry drep2 $ offDRepActivity 0
      expectDRepExpiry drep3 $ offDRepActivity 0

      updateDRep drep1 -- DRep expiry becomes (current epoch (4) + drep activity (4) - dormant epochs (2))
      expectDRepExpiry drep1 $ offDRepActivity 2
      unRegisterDRep drep3
      passEpoch -- entering epoch 5
      -- Updated drep1 shows their new expiry
      -- numDormantEpochs bumps up further
      -- drep3 has unregistered
      -- drep2 has not expired since we now have dormant epochs
      expectNumDormantEpochs 3
      expectDRepExpiry drep1 $ offDRepActivity 2
      expectActualDRepExpiry drep1 $ offDRepActivity 5
      expectDRepExpiry drep2 $ offDRepActivity 0
      expectActualDRepExpiry drep2 $ offDRepActivity 3
      expectDRepNotRegistered drep3

      _ <- submitGovAction InfoAction
      -- number of dormant epochs is added to the dreps expiry, and reset to 0
      expectNumDormantEpochs 0
      expectDRepExpiry drep1 $ offDRepActivity 5 -- 6 + 3
      expectDRepExpiry drep2 $ offDRepActivity 3 -- 4 + 3
      passNEpochsChecking 2 $ do
        expectNumDormantEpochs 0
        expectDRepExpiry drep1 $ offDRepActivity 5
        expectDRepExpiry drep2 $ offDRepActivity 3

      passEpoch
      expectNumDormantEpochs 1
      expectDRepExpiry drep1 $ offDRepActivity 5
      expectActualDRepExpiry drep1 $ offDRepActivity 6
      expectDRepExpiry drep2 $ offDRepActivity 3
      expectActualDRepExpiry drep2 $ offDRepActivity 4

      gai <- submitGovAction InfoAction

      passNEpochsChecking 2 $ do
        expectNumDormantEpochs 0
        expectDRepExpiry drep1 $ offDRepActivity 6
        expectActualDRepExpiry drep1 $ offDRepActivity 6
        expectDRepExpiry drep2 $ offDRepActivity 4
        expectActualDRepExpiry drep2 $ offDRepActivity 4

      submitYesVote_ (DRepVoter drep2) gai

      passEpoch
      expectNumDormantEpochs 1
      expectDRepExpiry drep1 $ offDRepActivity 6
      expectActualDRepExpiry drep1 $ offDRepActivity 7
      expectDRepExpiry drep2 $ offDRepActivity 10
      expectActualDRepExpiry drep2 $ offDRepActivity 11

      passEpoch
      expectNumDormantEpochs 2
      expectDRepExpiry drep1 $ offDRepActivity 6
      expectActualDRepExpiry drep1 $ offDRepActivity 8
      expectDRepExpiry drep2 $ offDRepActivity 10
      expectActualDRepExpiry drep2 $ offDRepActivity 12

    it "DRep registration should succeed" $ do
      logString "Stake distribution before DRep registration:"
      logInstantStake
      _ <- registerDRep
      logString "Stake distribution after DRep registration:"
      logInstantStake
      passEpoch

dRepVotingSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
dRepVotingSpec =
  describe "DRep" $ do
    -- DRep voting for anything other than Info is disallowed during bootstrap,
    -- so we can only run this test post-bootstrap
    it "proposal is accepted after two epochs" $ whenPostBootstrap $ do
      modifyPParams $ ppDRepVotingThresholdsL . dvtPPEconomicGroupL .~ 1 %! 1
      let getParamValue = getsNES (nesEsL . curPParamsEpochStateL . ppMinFeeAL)
      initialParamValue <- getParamValue

      let proposedValue = initialParamValue <+> Coin 300
      let proposedUpdate = def & ppuMinFeeAL .~ SJust proposedValue

      -- Submit NewConstitution proposal two epoch too early to check that the action
      -- doesn't expire prematurely (ppGovActionLifetimeL is set to two epochs)
      logString "Submitting new minFee proposal"
      gid <- submitParameterChange SNothing proposedUpdate

      committeeHotCreds <- registerInitialCommittee
      (dRepCred, _, _) <- setupSingleDRep 1_000_000
      (spoC, _, _) <- setupPoolWithStake $ Coin 42_000_000
      passEpoch
      logRatificationChecks gid
      do
        isAccepted <- isDRepAccepted gid
        assertBool "Gov action should not be accepted" $ not isAccepted
      submitYesVote_ (DRepVoter dRepCred) gid
      submitYesVote_ (StakePoolVoter spoC) gid
      submitYesVoteCCs_ committeeHotCreds gid
      logAcceptedRatio gid
      do
        isAccepted <- isDRepAccepted gid
        assertBool "Gov action should be accepted" isAccepted

      passEpoch
      do
        isAccepted <- isDRepAccepted gid
        assertBool "Gov action should be accepted" isAccepted
      logAcceptedRatio gid
      logRatificationChecks gid
      getParamValue `shouldReturn` initialParamValue
      passEpoch
      getParamValue `shouldReturn` proposedValue

treasurySpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
treasurySpec =
  -- Treasury withdrawal are disallowed during bootstrap,
  -- so we can run tests that submit such proposal only post-bootstrap.
  describe "Treasury" $ do
    it "TreasuryWithdrawal" $ whenPostBootstrap $ do
      treasuryWithdrawalExpectation []

    it "TreasuryWithdrawalExtra" $ whenPostBootstrap $ do
      disableTreasuryExpansion
      rewardAccount <- registerRewardAccount
      rewardAccountOther <- registerRewardAccount
      govPolicy <- getGovPolicy
      treasuryWithdrawalExpectation
        [ TreasuryWithdrawals (Map.singleton rewardAccount (Coin 667)) govPolicy
        , TreasuryWithdrawals (Map.singleton rewardAccountOther (Coin 668)) govPolicy
        ]

    it
      "deposit is moved to treasury when the reward address is not registered"
      depositMovesToTreasuryWhenStakingAddressUnregisters

treasuryWithdrawalExpectation ::
  forall era.
  (HasCallStack, ConwayEraImp era) =>
  [GovAction era] ->
  ImpTestM era ()
treasuryWithdrawalExpectation extraWithdrawals = do
  disableTreasuryExpansion
  withdrawalAmount <- uniformRM (Coin 1, Coin 1_000_000_000)
  -- Before making a withdrawal, we need to make sure there is enough money in the treasury:
  donateToTreasury withdrawalAmount
  committeeHotCreds <- registerInitialCommittee
  (dRepCred, _, _) <- setupSingleDRep 1_000_000
  treasuryStart <- getsNES treasuryL
  treasuryStart `shouldBe` withdrawalAmount
  rewardAccount <- registerRewardAccount
  govPolicy <- getGovPolicy
  (govActionId NE.:| _) <-
    submitGovActions $
      TreasuryWithdrawals (Map.singleton rewardAccount withdrawalAmount) govPolicy
        NE.:| extraWithdrawals
  submitYesVote_ (DRepVoter dRepCred) govActionId
  submitYesVoteCCs_ committeeHotCreds govActionId
  passEpoch -- 1st epoch crossing starts DRep pulser
  impAnn "Withdrawal should not be received yet" $
    getBalance (raCredential rewardAccount) `shouldReturn` mempty
  passEpoch -- 2nd epoch crossing enacts all the ratified actions
  expectMissingGovActionId govActionId
  treasuryEnd <- getsNES treasuryL
  impAnn "Withdrawal deducted from treasury" $
    treasuryStart <-> treasuryEnd `shouldBe` withdrawalAmount
  impAnn "Withdrawal received by reward account" $
    getBalance (raCredential rewardAccount) `shouldReturn` withdrawalAmount

depositMovesToTreasuryWhenStakingAddressUnregisters ::
  ConwayEraImp era => ImpTestM era ()
depositMovesToTreasuryWhenStakingAddressUnregisters = do
  disableTreasuryExpansion
  initialTreasury <- getsNES treasuryL
  modifyPParams $ \pp ->
    pp
      & ppGovActionLifetimeL .~ EpochInterval 8
      & ppGovActionDepositL .~ Coin 100
      & ppCommitteeMaxTermLengthL .~ EpochInterval 0
  returnAddr <- registerRewardAccount
  govActionDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionDepositL
  keyDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
  govPolicy <- getGovPolicy
  gaid <-
    mkProposalWithRewardAccount
      ( ParameterChange
          SNothing
          (emptyPParamsUpdate & ppuGovActionDepositL .~ SJust (Coin 1000000))
          govPolicy
      )
      returnAddr
      >>= submitProposal
  expectPresentGovActionId gaid
  replicateM_ 5 passEpoch
  expectTreasury initialTreasury
  expectRegisteredRewardAddress returnAddr
  submitTx_ $
    mkBasicTx mkBasicTxBody
      & bodyTxL . certsTxBodyL
        .~ SSeq.singleton
          (UnRegDepositTxCert (raCredential returnAddr) keyDeposit)
  expectNotRegisteredRewardAddress returnAddr
  replicateM_ 5 passEpoch
  expectMissingGovActionId gaid
  expectTreasury $ initialTreasury <> govActionDeposit

eventsSpec ::
  forall era.
  ( ConwayEraImp era
  , Event (EraRule "NEWEPOCH" era) ~ ConwayNewEpochEvent era
  , Event (EraRule "EPOCH" era) ~ ConwayEpochEvent era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
eventsSpec = describe "Events" $ do
  describe "emits event" $ do
    it "GovInfoEvent" $ do
      ccCreds <- registerInitialCommittee
      (spoCred, _, _) <- setupPoolWithStake $ Coin 42_000_000

      let actionLifetime = 10
      modifyPParams $ \pp ->
        pp
          & ppGovActionLifetimeL .~ EpochInterval actionLifetime
          & ppPoolVotingThresholdsL . pvtPPSecurityGroupL .~ 1 %! 1
      whenPostBootstrap (modifyPParams $ ppDRepVotingThresholdsL . dvtPPEconomicGroupL .~ def)
      propDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionDepositL
      keyDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
      let
        proposeParameterChange = do
          newVal <- CoinPerByte . Coin <$> choose (3000, 6500)
          proposal <- submitParameterChange SNothing $ def & ppuCoinsPerUTxOByteL .~ SJust newVal
          pure
            (proposal, getsNES (nesEsL . curPParamsEpochStateL . ppCoinsPerUTxOByteL) `shouldReturn` newVal)
      (proposalA, checkProposedParameterA) <- proposeParameterChange
      (proposalB, _) <- proposeParameterChange
      rewardAccount@(RewardAccount _ rewardCred) <- registerRewardAccount
      passEpoch -- prevent proposalC expiry and force it's deletion due to conflit.
      proposalC <- impAnn "proposalC" $ do
        newVal <- CoinPerByte . Coin <$> choose (3000, 6500)
        paramChange <- mkParameterChangeGovAction SNothing $ (def & ppuCoinsPerUTxOByteL .~ SJust newVal)
        mkProposalWithRewardAccount
          paramChange
          rewardAccount
          >>= submitProposal
      let
        isGovInfoEvent (SomeSTSEvent ev)
          | Just (TickNewEpochEvent (EpochEvent (GovInfoEvent {})) :: ShelleyTickEvent era) <- cast ev = True
        isGovInfoEvent _ = False
        passEpochWithNoDroppedActions = do
          evs <- impEventsFrom passEpoch
          filter isGovInfoEvent evs
            `shouldBeExpr` [ SomeSTSEvent @era @"TICK" . injectEvent $
                               GovInfoEvent mempty mempty mempty mempty
                           ]
      replicateM_ (fromIntegral actionLifetime - 1) passEpochWithNoDroppedActions
      logAcceptedRatio proposalA
      submitYesVote_ (StakePoolVoter spoCred) proposalA
      submitYesVoteCCs_ ccCreds proposalA
      gasA <- getGovActionState proposalA
      gasB <- getGovActionState proposalB
      gasC <- getGovActionState proposalC
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ SSeq.singleton (UnRegDepositTxCert rewardCred keyDeposit)
      passEpochWithNoDroppedActions
      evs <- impEventsFrom passEpoch
      checkProposedParameterA
      let
        filteredEvs = filter isGovInfoEvent evs
      filteredEvs
        `shouldBeExpr` [ SomeSTSEvent @era @"TICK" . injectEvent $
                           GovInfoEvent
                             (Set.singleton gasA)
                             (Set.singleton gasC)
                             (Set.singleton gasB)
                             (Map.singleton proposalC propDeposit)
                       ]
