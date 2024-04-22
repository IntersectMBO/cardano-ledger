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
import Cardano.Ledger.BaseTypes (EpochInterval (..), EpochNo (..), textToUrl)
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules (ConwayEpochEvent (GovInfoEvent), ConwayNewEpochEvent (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Shelley.LedgerState (
  asTreasuryL,
  curPParamsEpochStateL,
  epochStateGovStateL,
  esAccountStateL,
  nesEpochStateL,
  nesEsL,
 )
import Cardano.Ledger.Shelley.Rules (Event, ShelleyTickEvent (..))
import Cardano.Ledger.Val
import Control.Monad.Writer (listen)
import Data.Data (cast)
import Data.Default.Class (Default (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Data.Tree
import Lens.Micro ((%~), (&), (.~))
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleEvent "TICK" ConwayEpochEvent era
  , Event (EraRule "EPOCH" era) ~ ConwayEpochEvent era
  , Event (EraRule "NEWEPOCH" era) ~ ConwayNewEpochEvent era
  ) =>
  SpecWith (ImpTestState era)
spec =
  describe "EPOCH" $ do
    proposalsSpec
    dRepSpec
    treasurySpec
    eventsSpec

proposalsSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
proposalsSpec =
  describe "Proposals" $ do
    it "Proposals survive multiple epochs without any activity" $ do
      -- + 2 epochs to pass to get the desired effect
      modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 4
      _tree <-
        submitConstitutionGovActionTree SNothing $
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

      getRewardAccountAmount rewardAccount `shouldReturn` Coin 0

      policy <-
        getsNES $
          nesEpochStateL . epochStateGovStateL . constitutionGovStateL . constitutionScriptL
      govActionId <-
        submitProposal $
          ProposalProcedure
            { pProcDeposit = deposit
            , pProcReturnAddr = rewardAccount
            , pProcGovAction = TreasuryWithdrawals [(rewardAccount, Coin 123_456_789)] policy
            , pProcAnchor = def
            }
      expectPresentGovActionId govActionId
      passEpoch
      passEpoch
      passEpoch
      expectMissingGovActionId govActionId

      getRewardAccountAmount rewardAccount `shouldReturn` deposit

dRepSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
dRepSpec =
  describe "DRep" $ do
    it "expiry is updated based on the number of dormant epochs" $ do
      modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
      (drep, _, _) <- setupSingleDRep 1_000_000

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
    it "DRep registration should succeed" $ do
      logEntry "Stake distribution before DRep registration:"
      logStakeDistr
      _ <- registerDRep
      logEntry "Stake distribution after DRep registration:"
      logStakeDistr
      passEpoch
    it "constitution is accepted after two epochs" $ do
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

      constitutionHash <- freshSafeHash
      let
        constitutionAction =
          NewConstitution
            SNothing
            ( Constitution
                ( Anchor
                    (fromJust $ textToUrl 64 "constitution.0")
                    constitutionHash
                )
                SNothing
            )
      -- Submit NewConstitution proposal two epoch too early to check that the action
      -- doesn't expire prematurely (ppGovActionLifetimeL is set to two epochs)
      logEntry "Submitting new constitution"
      gaidConstitutionProp <- submitGovAction constitutionAction

      (committeeHotCred :| _) <- registerInitialCommittee
      (dRepCred, _, _) <- setupSingleDRep 1_000_000
      passEpoch
      logRatificationChecks gaidConstitutionProp
      do
        isAccepted <- isDRepAccepted gaidConstitutionProp
        assertBool "Gov action should not be accepted" $ not isAccepted
      submitYesVote_ (DRepVoter dRepCred) gaidConstitutionProp
      submitYesVote_ (CommitteeVoter committeeHotCred) gaidConstitutionProp
      logAcceptedRatio gaidConstitutionProp
      do
        isAccepted <- isDRepAccepted gaidConstitutionProp
        assertBool "Gov action should be accepted" isAccepted

      passEpoch
      do
        isAccepted <- isDRepAccepted gaidConstitutionProp
        assertBool "Gov action should be accepted" isAccepted
      logAcceptedRatio gaidConstitutionProp
      logRatificationChecks gaidConstitutionProp
      constitutionShouldBe ""

      passEpoch
      constitutionShouldBe "constitution.0"

treasurySpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
treasurySpec =
  describe "Treasury" $ do
    it "TreasuryWithdrawal" $ do
      treasuryWithdrawalExpectation []

    it "TreasuryWithdrawalExtra" $ do
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
  ConwayEraImp era =>
  [GovAction era] ->
  ImpTestM era ()
treasuryWithdrawalExpectation extraWithdrawals = do
  (committeeHotCred :| _) <- registerInitialCommittee
  (dRepCred, _, _) <- setupSingleDRep 1_000_000
  treasuryStart <- getsNES $ nesEsL . esAccountStateL . asTreasuryL
  rewardAccount <- registerRewardAccount
  govPolicy <- getGovPolicy
  let withdrawalAmount = Coin 666
  (govActionId NE.:| _) <-
    submitGovActions $
      TreasuryWithdrawals (Map.singleton rewardAccount withdrawalAmount) govPolicy
        NE.:| extraWithdrawals
  submitYesVote_ (DRepVoter dRepCred) govActionId
  submitYesVote_ (CommitteeVoter committeeHotCred) govActionId
  passEpoch -- 1st epoch crossing starts DRep pulser
  impAnn "Withdrawal should not be received yet" $
    lookupReward (raCredential rewardAccount) `shouldReturn` mempty
  passEpoch -- 2nd epoch crossing enacts all the ratified actions
  treasuryEnd <- getsNES $ nesEsL . esAccountStateL . asTreasuryL
  impAnn "Withdrawal deducted from treasury" $
    treasuryStart <-> treasuryEnd `shouldBe` withdrawalAmount
  impAnn "Withdrawal received by reward account" $
    lookupReward (raCredential rewardAccount) `shouldReturn` withdrawalAmount
  expectMissingGovActionId govActionId

depositMovesToTreasuryWhenStakingAddressUnregisters :: ConwayEraImp era => ImpTestM era ()
depositMovesToTreasuryWhenStakingAddressUnregisters = do
  initialTreasury <- getsNES $ nesEsL . esAccountStateL . asTreasuryL
  modifyPParams $ \pp ->
    pp
      & ppGovActionLifetimeL .~ EpochInterval 8
      & ppGovActionDepositL .~ Coin 100
  returnAddr <- registerRewardAccount
  govActionDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionDepositL
  khCC <- KeyHashObj <$> freshKeyHash
  committeeActionId <-
    submitProposal
      ProposalProcedure
        { pProcReturnAddr = returnAddr
        , pProcGovAction =
            UpdateCommittee
              SNothing
              mempty
              (Map.singleton khCC $ EpochNo 10)
              (1 %! 2)
        , pProcDeposit = govActionDeposit
        , pProcAnchor = def
        }
  expectPresentGovActionId committeeActionId
  replicateM_ 5 passEpoch
  expectTreasury initialTreasury
  expectRegisteredRewardAddress returnAddr
  submitTx_ $
    mkBasicTx mkBasicTxBody
      & bodyTxL . certsTxBodyL
        .~ SSeq.singleton
          (UnRegTxCert $ raCredential returnAddr)
  expectNotRegisteredRewardAddress returnAddr
  replicateM_ 5 passEpoch
  expectMissingGovActionId committeeActionId
  expectTreasury $ initialTreasury <> govActionDeposit

eventsSpec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleEvent "TICK" ConwayEpochEvent era
  , Event (EraRule "NEWEPOCH" era) ~ ConwayNewEpochEvent era
  , Event (EraRule "EPOCH" era) ~ ConwayEpochEvent era
  ) =>
  SpecWith (ImpTestState era)
eventsSpec = describe "Events" $ do
  describe "emits event" $ do
    it "GovInfoEvent" $ do
      (ccCred :| _) <- registerInitialCommittee
      (dRepCred, _, _) <- setupSingleDRep 1_000_000
      let actionLifetime = 10
      modifyPParams $ \pp ->
        pp
          & ppGovActionLifetimeL .~ EpochInterval actionLifetime
          & ppDRepVotingThresholdsL .~ def {dvtPPEconomicGroup = 1 %! 2}
      propDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionDepositL
      let
        proposeCostModel = do
          newTau <- arbitrary
          submitParameterChange SNothing $ def & ppuTauL .~ SJust newTau
      proposalA <- impAnn "proposalA" proposeCostModel
      proposalB <- impAnn "proposalB" proposeCostModel
      rewardAccount@(RewardAccount _ rewardCred) <- registerRewardAccount
      proposalC <- impAnn "proposalC" $ do
        newTau <- arbitrary
        submitProposal
          ProposalProcedure
            { pProcReturnAddr = rewardAccount
            , pProcGovAction =
                ParameterChange
                  SNothing
                  (def & ppuTauL .~ SJust newTau)
                  SNothing
            , pProcDeposit = propDeposit
            , pProcAnchor = def
            }
      let
        isGovInfoEvent (SomeSTSEvent ev)
          | Just (TickNewEpochEvent (EpochEvent (GovInfoEvent {})) :: ShelleyTickEvent era) <- cast ev = True
        isGovInfoEvent _ = False
        passEpochWithNoDroppedActions = do
          (_, evs) <- listen passEpoch
          filter isGovInfoEvent evs
            `shouldBeExpr` [ SomeSTSEvent @era @"TICK" . injectEvent $
                              GovInfoEvent mempty mempty mempty
                           ]
      replicateM_ (fromIntegral actionLifetime) passEpochWithNoDroppedActions
      logAcceptedRatio proposalA
      submitYesVote_ (DRepVoter dRepCred) proposalA
      submitYesVote_ (CommitteeVoter ccCred) proposalA
      gasA <- getGovActionState proposalA
      gasB <- getGovActionState proposalB
      gasC <- getGovActionState proposalC
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL . certsTxBodyL
            .~ SSeq.singleton (UnRegTxCert rewardCred)
      passEpochWithNoDroppedActions
      (_, evs) <- listen passEpoch
      let
        filteredEvs = filter isGovInfoEvent evs
      filteredEvs
        `shouldBeExpr` [ SomeSTSEvent @era @"TICK" . injectEvent $
                          GovInfoEvent
                            (Set.singleton gasA)
                            (Set.fromList [gasB, gasC])
                            (Set.singleton proposalC)
                       ]
