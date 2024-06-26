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

module Test.Cardano.Ledger.Babel.Imp.EpochSpec (
  spec,
  relevantDuringBootstrapSpec,
) where

import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.Babel.Core
import Cardano.Ledger.Babel.Rules ()
import Cardano.Ledger.BaseTypes (EpochInterval (..), EpochNo (..))
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules (Event, ShelleyTickEvent (..))
import Cardano.Ledger.Val
import Control.Monad.Writer (listen)
import Data.Data (cast)
import Data.Default.Class (Default (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Data.Tree
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Babel.ImpTest
import Test.Cardano.Ledger.Core.Rational (IsRatio (..), (%!))
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ( BabelEraImp era
  , InjectRuleEvent "TICK" ConwayEpochEvent era
  , Event (EraRule "EPOCH" era) ~ ConwayEpochEvent era
  , Event (EraRule "NEWEPOCH" era) ~ ConwayNewEpochEvent era
  ) =>
  SpecWith (ImpTestState era)
spec =
  describe "EPOCH" $ do
    relevantDuringBootstrapSpec
    dRepVotingSpec
    treasurySpec

relevantDuringBootstrapSpec ::
  forall era.
  ( BabelEraImp era
  , InjectRuleEvent "TICK" ConwayEpochEvent era
  , Event (EraRule "EPOCH" era) ~ ConwayEpochEvent era
  , Event (EraRule "NEWEPOCH" era) ~ ConwayNewEpochEvent era
  ) =>
  SpecWith (ImpTestState era)
relevantDuringBootstrapSpec = do
  proposalsSpec
  dRepSpec
  eventsSpec

proposalsSpec ::
  forall era.
  BabelEraImp era =>
  SpecWith (ImpTestState era)
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
          & ppGovActionLifetimeL
          .~ EpochInterval 1
          & ppGovActionDepositL
          .~ deposit
      rewardAccount <- registerRewardAccount

      initialValue <- getsNES (nesEsL . curPParamsEpochStateL . ppMinFeeAL)

      policy <-
        getsNES $
          nesEpochStateL . epochStateGovStateL . constitutionGovStateL . constitutionScriptL
      govActionId <-
        submitProposal $
          ProposalProcedure
            { pProcDeposit = deposit
            , pProcReturnAddr = rewardAccount
            , pProcGovAction =
                ParameterChange
                  SNothing
                  (def & ppuMinFeeAL .~ SJust (Coin 3000))
                  policy
            , pProcAnchor = def
            }
      expectPresentGovActionId govActionId
      passEpoch
      passEpoch
      passEpoch
      expectMissingGovActionId govActionId

      getsNES (nesEsL . curPParamsEpochStateL . ppMinFeeAL) `shouldReturn` initialValue
      getRewardAccountAmount rewardAccount `shouldReturn` deposit
  where
    submitParameterChangeTree = submitGovActionTree $ submitGovAction . paramAction
    paramAction p =
      ParameterChange (GovPurposeId <$> p) (def & ppuMinFeeAL .~ SJust (Coin 10)) SNothing

dRepSpec ::
  forall era.
  BabelEraImp era =>
  SpecWith (ImpTestState era)
dRepSpec =
  describe "DRep" $ do
    it "expiry is updated based on the number of dormant epochs" $ do
      modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
      (drep, _, _) <- setupSingleDRep 1_000_000

      let submitParamChangeProposal =
            submitParameterChange SNothing $ def & ppuMinFeeAL .~ SJust (Coin 3000)
      expectNumDormantEpochs 0

      -- epoch 0
      _ <- submitParamChangeProposal
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

      _ <- submitParamChangeProposal
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

dRepVotingSpec ::
  forall era.
  BabelEraImp era =>
  SpecWith (ImpTestState era)
dRepVotingSpec =
  describe "DRep" $ do
    it "proposal is accepted after two epochs" $ do
      modifyPParams $ \pp ->
        pp
          & ppDRepVotingThresholdsL
          .~ def
            { dvtPPEconomicGroup = 1 %! 1
            }
      let getParamValue = getsNES (nesEsL . curPParamsEpochStateL . ppMinFeeAL)
      initialParamValue <- getParamValue

      let proposedValue = initialParamValue <+> Coin 300
      let proposedUpdate = def & ppuMinFeeAL .~ SJust proposedValue

      -- Submit NewConstitution proposal two epoch too early to check that the action
      -- doesn't expire prematurely (ppGovActionLifetimeL is set to two epochs)
      logEntry "Submitting new minFee proposal"
      gid <- submitParameterChange SNothing proposedUpdate

      (committeeHotCred :| _) <- registerInitialCommittee
      (dRepCred, _, _) <- setupSingleDRep 1_000_000
      passEpoch
      logRatificationChecks gid
      do
        isAccepted <- isDRepAccepted gid
        assertBool "Gov action should not be accepted" $ not isAccepted
      submitYesVote_ (DRepVoter dRepCred) gid
      submitYesVote_ (CommitteeVoter committeeHotCred) gid
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
  BabelEraImp era =>
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
  BabelEraImp era =>
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

depositMovesToTreasuryWhenStakingAddressUnregisters :: BabelEraImp era => ImpTestM era ()
depositMovesToTreasuryWhenStakingAddressUnregisters = do
  initialTreasury <- getsNES $ nesEsL . esAccountStateL . asTreasuryL
  modifyPParams $ \pp ->
    pp
      & ppGovActionLifetimeL
      .~ EpochInterval 8
      & ppGovActionDepositL
      .~ Coin 100
      & ppCommitteeMaxTermLengthL
      .~ EpochInterval 0
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
      & bodyTxL
      . certsTxBodyL
      .~ SSeq.singleton
        (UnRegTxCert $ raCredential returnAddr)
  expectNotRegisteredRewardAddress returnAddr
  replicateM_ 5 passEpoch
  expectMissingGovActionId committeeActionId
  expectTreasury $ initialTreasury <> govActionDeposit

eventsSpec ::
  forall era.
  ( BabelEraImp era
  , InjectRuleEvent "TICK" ConwayEpochEvent era
  , Event (EraRule "NEWEPOCH" era) ~ ConwayNewEpochEvent era
  , Event (EraRule "EPOCH" era) ~ ConwayEpochEvent era
  ) =>
  SpecWith (ImpTestState era)
eventsSpec = describe "Events" $ do
  describe "emits event" $ do
    it "GovInfoEvent" $ do
      (ccCred :| _) <- registerInitialCommittee
      (spoCred, _, _) <- setupPoolWithStake $ Coin 42_000_000

      let actionLifetime = 10
      modifyPParams $ \pp ->
        pp
          & ppGovActionLifetimeL
          .~ EpochInterval actionLifetime
          & ppDRepVotingThresholdsL
          . dvtPPEconomicGroupL
          .~ def
          & ppPoolVotingThresholdsL
          . pvtPPSecurityGroupL
          .~ 1
          %! 1
      propDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionDepositL
      let
        proposeCostModel = do
          newVal <- arbitrary
          submitParameterChange SNothing $ def & ppuCoinsPerUTxOByteL .~ SJust newVal
      proposalA <- impAnn "proposalA" proposeCostModel
      proposalB <- impAnn "proposalB" proposeCostModel
      rewardAccount@(RewardAccount _ rewardCred) <- registerRewardAccount
      proposalC <- impAnn "proposalC" $ do
        newVal <- arbitrary
        submitProposal
          ProposalProcedure
            { pProcReturnAddr = rewardAccount
            , pProcGovAction =
                ParameterChange
                  SNothing
                  (def & ppuCoinsPerUTxOByteL .~ SJust newVal)
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
      submitYesVote_ (StakePoolVoter spoCred) proposalA
      submitYesVote_ (CommitteeVoter ccCred) proposalA
      gasA <- getGovActionState proposalA
      gasB <- getGovActionState proposalB
      gasC <- getGovActionState proposalC
      submitTx_ $
        mkBasicTx mkBasicTxBody
          & bodyTxL
          . certsTxBodyL
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
