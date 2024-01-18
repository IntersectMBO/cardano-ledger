{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Conway.Imp.EpochSpec (spec) where

import Cardano.Ledger.BaseTypes (EpochInterval (..), textToUrl)
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (
  Anchor (..),
  ConwayGovState,
  GovAction (..),
  ProposalProcedure (..),
  Voter (..),
  cgEnactStateL,
  ensConstitutionL,
 )
import Cardano.Ledger.Shelley.LedgerState (
  asTreasuryL,
  epochStateGovStateL,
  esAccountStateL,
  nesEpochStateL,
  nesEsL,
 )
import Cardano.Ledger.Val
import Data.Default.Class (Default (..))
import Data.Maybe (fromJust)
import Data.Maybe.Strict (StrictMaybe (..))
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Conway.ImpTest
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
                    (fromJust $ textToUrl 64 "constitution.0")
                    constitutionHash
                )
                SNothing
            )
      -- Submit NewConstitution proposal two epoch too early to check that the action
      -- doesn't expire prematurely (ppGovActionLifetimeL is set to two epochs)
      logEntry "Submitting new constitution"
      gaidConstitutionProp <- submitGovAction constitutionAction

      (dRepCred, committeeHotCred, _) <- electBasicCommittee

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
      (dRepCred, committeeHotCred, _) <- electBasicCommittee

      treasuryStart <- getsNES $ nesEsL . esAccountStateL . asTreasuryL

      rewardAcount <- registerRewardAccount
      let withdrawalAmount = Coin 666
      govActionId <- submitTreasuryWithdrawals [(rewardAcount, withdrawalAmount)]

      submitYesVote_ (DRepVoter dRepCred) govActionId
      submitYesVote_ (CommitteeVoter committeeHotCred) govActionId

      passEpoch
      passEpoch

      treasuryEnd <- getsNES $ nesEsL . esAccountStateL . asTreasuryL

      treasuryStart <-> treasuryEnd `shouldBe` withdrawalAmount

    it "Expired proposl deposit refunded" $ do
      let deposit = Coin 999
      modifyPParams $ \pp ->
        pp
          & ppGovActionLifetimeL .~ EpochInterval 1
          & ppGovActionDepositL .~ deposit
      rewardAcount <- registerRewardAccount

      getRewardAccountAmount rewardAcount `shouldReturn` Coin 0

      policy <-
        getsNES $
          nesEpochStateL . epochStateGovStateL . cgEnactStateL . ensConstitutionL . constitutionScriptL
      govActionId <-
        submitProposal $
          ProposalProcedure
            { pProcDeposit = deposit
            , pProcReturnAddr = rewardAcount
            , pProcGovAction = TreasuryWithdrawals [(rewardAcount, Coin 123456789)] policy
            , pProcAnchor = def
            }
      expectPresentGovActionId govActionId
      passEpoch
      passEpoch
      passEpoch
      expectMissingGovActionId govActionId

      getRewardAccountAmount rewardAcount `shouldReturn` deposit
