{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Conway.EpochSpec (spec) where

import Cardano.Ledger.BaseTypes (textToUrl)
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Core (
  Constitution (..),
  DRepVotingThresholds (..),
 )
import Cardano.Ledger.Conway.Governance (
  Anchor (..),
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
import Cardano.Ledger.Shelley.LedgerState (
  esLStateL,
  lsUTxOStateL,
  nesEsL,
  utxosGovStateL,
 )
import Data.Default.Class (Default (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Maybe.Strict (StrictMaybe (..), isSJust)
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Common (
  HasCallStack,
  Spec,
  describe,
  shouldBe,
  shouldSatisfy,
 )
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))

spec :: HasCallStack => Spec
spec =
  describe "Ratify traces" $ do
    itM @Conway "DRep registration should succeed" $ do
      logEntry "Stake distribution before DRep registration:"
      logStakeDistr
      _ <- registerDRep
      logEntry "Stake distribution after DRep registration:"
      logStakeDistr
      passEpoch

    itM @Conway "constitution is accepted after two epochs" $ do
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
      logEntry "Submitting new constitution"
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
      gaidConstitutionProp <-
        submitProposal constitutionAction
      logRatificationChecks gaidConstitutionProp
      passEpoch
      passEpoch
      do
        isAccepted <- canGovActionBeDRepAccepted gaidConstitutionProp
        impIOMsg "Gov action should not be accepted" $ isAccepted `shouldBe` False
      khCommitteeMemberHot <- registerCommitteeHotKey khCommitteeMember
      _ <- voteForProposal (DRepVoter $ KeyHashObj khDRep) gaidConstitutionProp
      _ <- voteForProposal (CommitteeVoter $ KeyHashObj khCommitteeMemberHot) gaidConstitutionProp
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
