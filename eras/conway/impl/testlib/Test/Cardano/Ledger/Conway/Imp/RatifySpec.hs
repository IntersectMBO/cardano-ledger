{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Conway.Imp.RatifySpec (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map
import Lens.Micro
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
spec =
  describe "RATIFY" $ do
    delayingActionsSpec

delayingActionsSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
delayingActionsSpec =
  describe "Delaying actions" $ do
    it "A delaying action delays its child even when both ere proposed and ratified in the same epoch" $ do
      (dRep, committeeMember, _gpi) <- electBasicCommittee
      modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 5
      gai0 <- submitInitConstitutionGovAction
      gai1 <- submitChildConstitutionGovAction gai0
      gai2 <- submitChildConstitutionGovAction gai1
      gai3 <- submitChildConstitutionGovAction gai2
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
    it "A delaying action delays all other actions even when all of them may be ratified in the same epoch" $ do
      (dRep, committeeMember, _gpi) <- electBasicCommittee
      modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 5
      pGai0 <-
        submitParameterChange
          SNothing
          $ def & ppuDRepDepositL .~ SJust (Coin 1_000_000)
      pGai1 <-
        submitParameterChange
          (SJust $ GovPurposeId pGai0)
          $ def & ppuDRepDepositL .~ SJust (Coin 1_000_001)
      pGai2 <-
        submitParameterChange
          (SJust $ GovPurposeId pGai1)
          $ def & ppuDRepDepositL .~ SJust (Coin 1_000_002)
      cGai0 <- submitInitConstitutionGovAction
      cGai1 <- submitChildConstitutionGovAction cGai0
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
        (dRep, committeeMember, _gpi) <- electBasicCommittee
        gai0 <- submitInitConstitutionGovAction
        gai1 <- submitChildConstitutionGovAction gai0
        gai2 <- submitChildConstitutionGovAction gai1
        gai3 <- submitChildConstitutionGovAction gai2
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
        (dRep, committeeMember, _gpi) <- electBasicCommittee
        pGai0 <-
          submitParameterChange
            SNothing
            $ def & ppuDRepDepositL .~ SJust (Coin 1_000_000)
        pGai1 <-
          submitParameterChange
            (SJust $ GovPurposeId pGai0)
            $ def & ppuDRepDepositL .~ SJust (Coin 1_000_001)
        pGai2 <-
          submitParameterChange
            (SJust $ GovPurposeId pGai1)
            $ def & ppuDRepDepositL .~ SJust (Coin 1_000_002)
        cGai0 <- submitInitConstitutionGovAction
        cGai1 <- submitChildConstitutionGovAction cGai0
        cGai2 <- submitChildConstitutionGovAction cGai1
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
