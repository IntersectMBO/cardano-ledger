{-# LANGUAGE AllowAmbiguousTypes #-}
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

module Test.Cardano.Ledger.Conway.Imp.EpochSpec (spec) where

import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes (textToUrl)
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (
  Anchor (..),
  ConwayGovState,
  GovAction (..),
  Voter (..),
  cgEnactStateL,
  ensCommitteeL,
 )
import Cardano.Ledger.Conway.PParams (
  ppCommitteeMaxTermLengthL,
  ppDRepVotingThresholdsL,
  ppGovActionLifetimeL,
 )
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Keys
import Cardano.Ledger.Shelley.LedgerState (
  asTreasuryL,
  epochStateUMapL,
  esAccountStateL,
  esLStateL,
  lsUTxOStateL,
  nesEsL,
  utxosGovStateL,
 )
import Cardano.Ledger.UMap as UMap
import Cardano.Ledger.Val
import Data.Default.Class (Default (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Maybe.Strict (StrictMaybe (..), isSJust)
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))

spec ::
  forall era.
  ( ConwayEraImp era
  , GovState era ~ ConwayGovState era
  ) =>
  Spec
spec =
  describe "EPOCH" $ do
    itM @era "DRep registration should succeed" $ do
      logEntry "Stake distribution before DRep registration:"
      logStakeDistr
      _ <- registerDRep
      logEntry "Stake distribution after DRep registration:"
      logStakeDistr
      passEpoch

    itM @era "constitution is accepted after two epochs" $ do
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
      gaidConstitutionProp <- submitProposal constitutionAction

      (dRepCred, committeeHotCred) <- electBasicCommittee

      logRatificationChecks gaidConstitutionProp
      do
        isAccepted <- canGovActionBeDRepAccepted gaidConstitutionProp
        impIOMsg "Gov action should not be accepted" $ isAccepted `shouldBe` False
      _ <- voteForProposal (DRepVoter dRepCred) gaidConstitutionProp
      _ <- voteForProposal (CommitteeVoter committeeHotCred) gaidConstitutionProp
      logAcceptedRatio gaidConstitutionProp
      do
        isAccepted <- canGovActionBeDRepAccepted gaidConstitutionProp
        impIOMsg "Gov action should be accepted" $ isAccepted `shouldBe` True

      passEpoch
      do
        isAccepted <- canGovActionBeDRepAccepted gaidConstitutionProp
        impIOMsg "Gov action should be accepted" $ isAccepted `shouldBe` True
      logAcceptedRatio gaidConstitutionProp
      logRatificationChecks gaidConstitutionProp
      constitutionShouldBe ""

      passEpoch
      constitutionShouldBe "constitution.0"

    itM @era "TreasuryWithdrawal" $ do
      (dRepCred, committeeHotCred) <- electBasicCommittee

      treasuryStart <- getsNES $ nesEsL . esAccountStateL . asTreasuryL

      rewardAcount <- registerRewardAccount
      let withdrawalAmount = Coin 666
          govAction = TreasuryWithdrawals [(rewardAcount, withdrawalAmount)]
      govActionId <- submitProposal govAction

      _ <- voteForProposal (DRepVoter dRepCred) govActionId
      _ <- voteForProposal (CommitteeVoter committeeHotCred) govActionId

      passEpoch
      passEpoch

      treasuryEnd <- getsNES $ nesEsL . esAccountStateL . asTreasuryL
      umap <- getsNES $ nesEsL . epochStateUMapL
      impIO $ do
        let cred = getRwdCred rewardAcount
        case UMap.lookup cred (RewDepUView umap) of
          Nothing -> error $ "Expected a reward account: " ++ show cred
          Just RDPair {rdReward} -> fromCompact rdReward `shouldBe` withdrawalAmount
        treasuryStart <-> treasuryEnd `shouldBe` withdrawalAmount

electBasicCommittee ::
  forall era.
  ( ConwayEraImp era
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
  gaidCommitteeProp <- submitProposal committeeAction

  _ <- voteForProposal (DRepVoter $ KeyHashObj khDRep) gaidCommitteeProp

  let
    assertNoCommittee = do
      committee <-
        getsNES $
          nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgEnactStateL . ensCommitteeL
      impIOMsg "There should not be a committee" $ committee `shouldBe` SNothing
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
    impIOMsg "There should be a committee" $ committee `shouldSatisfy` isSJust

  khCommitteeMemberHot <- registerCommitteeHotKey khCommitteeMember
  pure (KeyHashObj khDRep, KeyHashObj khCommitteeMemberHot)
