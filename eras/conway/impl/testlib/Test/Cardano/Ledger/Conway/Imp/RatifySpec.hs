{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Conway.Imp.RatifySpec (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Val
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map
import qualified Data.OMap.Strict as OMap
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
    describe "Ratified actions are delayed as expected" $ do
      it "Same lineage" $ do
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
      it "Other lineage" $ do
        (dRep, committeeMember, _gpi) <- electBasicCommittee
        modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 5
        treasuryValue <- getCurrentTreasury
        rewardAccount <- registerRewardAccount
        twGai <-
          submitTreasuryWithdrawals
            [(rewardAccount, Coin 1_000)]
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
        submitYesVote_ (DRepVoter dRep) twGai
        submitYesVote_ (CommitteeVoter committeeMember) twGai
        passNEpochs 2
        getLastEnactedConstitution `shouldReturn` SJust (GovPurposeId cGai0)
        getLastEnactedParameterChange `shouldReturn` SNothing
        passEpoch
        -- here 'ParameterChange' and `TreasuryWithdrawals` do not
        -- get enacted even though the are not related, since their
        -- `actionPriority` is 4 and 5, while that for 'NewConstitution'
        -- is 2, so they get delayed a second time
        getLastEnactedConstitution `shouldReturn` SJust (GovPurposeId cGai1)
        getConstitutionProposals `shouldReturn` Map.empty
        getLastEnactedParameterChange `shouldReturn` SNothing
        passEpoch
        -- all three actions, pGai0, pGai1 and pGai2, are enacted one
        -- after the other in the same epoch, and so is twGai
        getLastEnactedParameterChange `shouldReturn` SJust (GovPurposeId pGai2)
        getCurrentTreasury `shouldReturn` (treasuryValue <-> Coin 1_000)
        getParameterChangeProposals `shouldReturn` Map.empty
        fmap (^. pPropsL) getProposals `shouldReturn` OMap.empty
    describe "Ratified actions do not expire when delayed beyond their expiry" $ do
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
        passEpoch
        getLastEnactedConstitution `shouldReturn` SJust (GovPurposeId gai3)
        getConstitutionProposals `shouldReturn` Map.empty
      it "Other lineage" $ do
        (dRep, committeeMember, _gpi) <- electBasicCommittee
        treasuryValue <- getCurrentTreasury
        rewardAccount <- registerRewardAccount
        twGai0 <-
          submitTreasuryWithdrawals
            [(rewardAccount, Coin 1_000)]
        twGai1 <-
          submitTreasuryWithdrawals
            [(rewardAccount, Coin 1_000)]
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
        pGai3 <-
          submitParameterChange
            (SJust $ GovPurposeId pGai2)
            $ def & ppuDRepDepositL .~ SJust (Coin 1_000_003)
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
        submitYesVote_ (DRepVoter dRep) pGai3
        submitYesVote_ (CommitteeVoter committeeMember) pGai3
        submitYesVote_ (DRepVoter dRep) twGai0
        submitYesVote_ (CommitteeVoter committeeMember) twGai0
        submitYesVote_ (DRepVoter dRep) twGai1
        submitYesVote_ (CommitteeVoter committeeMember) twGai1
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
        getLastEnactedParameterChange `shouldReturn` SJust (GovPurposeId pGai3)
        getCurrentTreasury `shouldReturn` (treasuryValue <-> Coin (1_000 * 2))
        getParameterChangeProposals `shouldReturn` Map.empty
        fmap (^. pPropsL) getProposals `shouldReturn` OMap.empty
