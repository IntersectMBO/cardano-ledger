{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Conway.Imp.EpochSpec (spec, electBasicCommittee) where

import Cardano.Ledger.Address (RewardAcnt (..))
import Cardano.Ledger.BaseTypes (textToUrl)
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (
  Anchor (..),
  ConwayGovState,
  GovAction (..),
  ProposalProcedure (..),
  Voter (..),
  cgEnactStateL,
  ensCommitteeL,
 )
import Cardano.Ledger.Conway.PParams (
  ppCommitteeMaxTermLengthL,
  ppDRepVotingThresholdsL,
  ppGovActionDepositL,
  ppGovActionLifetimeL,
 )
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Keys
import Cardano.Ledger.Shelley.LedgerState (
  asTreasuryL,
  esAccountStateL,
  esLStateL,
  lsUTxOStateL,
  nesEsL,
  utxosGovStateL,
 )
import Cardano.Ledger.Val
import Data.Default.Class (Default (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Maybe.Strict (StrictMaybe (..), isSJust)
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ( ConwayEraImp era
  , GovState era ~ ConwayGovState era
  ) =>
  SpecWith (ImpTestState era)
spec =
  describe "EPOCH" $ do
    it "DRep registration should succeed" $ do
      logEntry "Stake distribution before DRep registration:"
      logStakeDistr
      _ <- registerDRep
      logEntry "Stake distribution after DRep registration:"
      logStakeDistr
      passEpoch

    it "constitution is accepted after two epochs" $ do
      constitutionHash <- freshSafeHash
      let
        constitutionAction =
          NewConstitution
            SNothing
            ( Constitution
                ( Anchor
                    (fromJust $ textToUrl "constitution.0")
                    constitutionHash
                )
                SNothing
            )
      -- Submit NewConstitution proposal two epoch too early to check that the action
      -- doesn't expire prematurely (ppGovActionLifetimeL is set to two epochs)
      logEntry "Submitting new constitution"
      gaidConstitutionProp <- submitGovAction constitutionAction

      (dRepCred, committeeHotCred) <- electBasicCommittee

      logRatificationChecks gaidConstitutionProp
      do
        isAccepted <- canGovActionBeDRepAccepted gaidConstitutionProp
        assertBool "Gov action should not be accepted" $ not isAccepted
      submitYesVote_ (DRepVoter dRepCred) gaidConstitutionProp
      submitYesVote_ (CommitteeVoter committeeHotCred) gaidConstitutionProp
      logAcceptedRatio gaidConstitutionProp
      do
        isAccepted <- canGovActionBeDRepAccepted gaidConstitutionProp
        assertBool "Gov action should be accepted" isAccepted

      passEpoch
      do
        isAccepted <- canGovActionBeDRepAccepted gaidConstitutionProp
        assertBool "Gov action should be accepted" isAccepted
      logAcceptedRatio gaidConstitutionProp
      logRatificationChecks gaidConstitutionProp
      constitutionShouldBe ""

      passEpoch
      constitutionShouldBe "constitution.0"

    it "TreasuryWithdrawal" $ do
      (dRepCred, committeeHotCred) <- electBasicCommittee

      treasuryStart <- getsNES $ nesEsL . esAccountStateL . asTreasuryL

      rewardAcount <- registerRewardAccount
      let withdrawalAmount = Coin 666
      (govActionId NE.:| _) <-
        submitGovActions
          [ TreasuryWithdrawals (Map.singleton rewardAcount withdrawalAmount)
          , TreasuryWithdrawals (Map.singleton rewardAcount withdrawalAmount)
          ]

      submitYesVote_ (DRepVoter dRepCred) govActionId
      submitYesVote_ (CommitteeVoter committeeHotCred) govActionId

      passEpoch -- 1st epoch crossing starts DRep pulser
      lookupReward (getRwdCred rewardAcount) `shouldReturn` mempty

      passEpoch -- 2nd epoch crossing enacts all the ratified actions
      treasuryEnd <- getsNES $ nesEsL . esAccountStateL . asTreasuryL

      treasuryStart <-> treasuryEnd `shouldBe` withdrawalAmount

      lookupReward (getRwdCred rewardAcount) `shouldReturn` withdrawalAmount

      expectMissingGovActionId govActionId

    it "Expired proposal deposit refunded" $ do
      let deposit = Coin 999
      modifyPParams $ \pp ->
        pp
          & ppGovActionLifetimeL .~ 1
          & ppGovActionDepositL .~ deposit
      rewardAcount <- registerRewardAccount

      getRewardAccountAmount rewardAcount `shouldReturn` Coin 0

      govActionId <-
        submitProposal $
          ProposalProcedure
            { pProcDeposit = deposit
            , pProcReturnAddr = rewardAcount
            , pProcGovAction = TreasuryWithdrawals [(rewardAcount, Coin 123456789)]
            , pProcAnchor = def
            }
      expectPresentGovActionId govActionId
      passEpoch
      passEpoch
      passEpoch
      expectMissingGovActionId govActionId

      getRewardAccountAmount rewardAcount `shouldReturn` deposit

electBasicCommittee ::
  forall era.
  ( HasCallStack
  , ConwayEraImp era
  , GovState era ~ ConwayGovState era
  ) =>
  ImpTestM era (Credential 'DRepRole (EraCrypto era), Credential 'HotCommitteeRole (EraCrypto era))
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
      & ppCommitteeMaxTermLengthL .~ 10
      & ppGovActionLifetimeL .~ 2
      & ppGovActionDepositL .~ Coin 123
  khDRep <- setupSingleDRep

  logEntry "Registering committee member"
  khCommitteeMember <- freshKeyHash
  let
    committeeAction =
      UpdateCommittee
        SNothing
        mempty
        (Map.singleton (KeyHashObj khCommitteeMember) 10)
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
          nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgEnactStateL . ensCommitteeL
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
        nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgEnactStateL . ensCommitteeL
    impAnn "There should be a committee" $ committee `shouldSatisfy` isSJust

  khCommitteeMemberHot <- registerCommitteeHotKey khCommitteeMember
  pure (KeyHashObj khDRep, KeyHashObj khCommitteeMemberHot)
