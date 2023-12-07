{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conway.Imp.GovSpec where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams (ppGovActionLifetimeL)
import Cardano.Ledger.Conway.Rules (ConwayGovPredFailure (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (KeyHashObj))
import Cardano.Ledger.Shelley.LedgerState
import Control.State.Transition.Extended (PredicateFailure)
import Data.Default.Class (Default (..))
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Lens.Micro
import Test.Cardano.Ledger.Conway.Imp.EpochSpec (electBasicCommittee)
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ( ConwayEraImp era
  , GovState era ~ ConwayGovState era
  , Inject (ConwayGovPredFailure era) (PredicateFailure (EraRule "LEDGER" era))
  ) =>
  SpecWith (ImpTestState era)
spec =
  describe "GOV" $ do
    let submitInitConstitutionGovAction = do
          constitutionHash <- freshSafeHash
          let constitutionAction =
                NewConstitution
                  SNothing
                  ( Constitution
                      ( Anchor
                          (fromJust $ textToUrl "constitution.0")
                          constitutionHash
                      )
                      SNothing
                  )
          submitGovAction constitutionAction
    describe "Votes fail as expected" $ do
      it "on expired gov-actions" $ do
        modifyPParams $ ppGovActionLifetimeL .~ 2 -- Voting after the 3rd epoch should fail
        khDRep <- setupSingleDRep
        gaidConstitutionProp <- submitInitConstitutionGovAction
        passEpoch
        passEpoch
        passEpoch
        let voter = DRepVoter $ KeyHashObj khDRep
        submitFailingVote voter gaidConstitutionProp $
          [ inject $
              VotingOnExpiredGovAction @era $
                Map.singleton gaidConstitutionProp voter
          ]
      it "on non-existant gov-actions" $ do
        khDRep <- setupSingleDRep
        gaidConstitutionProp <- submitInitConstitutionGovAction
        let voter = DRepVoter $ KeyHashObj khDRep
            dummyGaid = gaidConstitutionProp {gaidGovActionIx = GovActionIx 99} -- non-existant `GovActionId`
        submitFailingVote voter dummyGaid $
          [inject $ GovActionsDoNotExist @era $ Set.singleton dummyGaid]

    describe "Proposals always have valid previous actions" $ do
      it "Another proposal after the first ever proposal is accepted" $ do
        --  Initial proposal does not need a PrevGovActionId but after it is enacted, the
        --  following ones are not
        gaidConstitutionProp <- submitInitConstitutionGovAction
        constitutionHash' <- freshSafeHash
        let constitutionAction' =
              NewConstitution
                SNothing
                ( Constitution
                    ( Anchor
                        (fromJust $ textToUrl "constitution.0")
                        constitutionHash'
                    )
                    SNothing
                )
        -- Until the first proposal is enacted all proposals with empty PrevGovActionIds are valid
        submitGovAction_ constitutionAction'
        modifyNES $ \nes ->
          nes
            & nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgEnactStateL . ensPrevConstitutionL
              .~ SJust (PrevGovActionId gaidConstitutionProp) -- Add first proposal to PrevGovActionIds in enacted state
            & nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgProposalsL
              .~ def -- Remove all proposals, so that the lookup only succeeds for ecacted state.
        constitutionHash'' <- freshSafeHash
        -- Once a proposal with a purpose has been enacted, following proposals can no
        -- longer have empty PrevGovActionIds
        rewardAccount <- registerRewardAccount
        let invalidNewConstitutionGovAction =
              NewConstitution
                SNothing
                ( Constitution
                    ( Anchor
                        (fromJust $ textToUrl "constitution.0")
                        constitutionHash''
                    )
                    SNothing
                )
            invalidNewConstitutionProposal =
              ProposalProcedure
                { pProcDeposit = mempty
                , pProcReturnAddr = rewardAccount
                , pProcGovAction = invalidNewConstitutionGovAction
                , pProcAnchor = def
                }
        submitFailingProposal
          invalidNewConstitutionProposal
          [ inject $ InvalidPrevGovActionId invalidNewConstitutionProposal
          ]
      context "Invalid proposals are rejected" $ do
        it "Invalid Index in PrevGovActionId" $ do
          gaidConstitutionProp <- submitInitConstitutionGovAction
          constitutionHash <- freshSafeHash
          rewardAccount <- registerRewardAccount
          let invalidPrevGovActionId =
                -- Expected Ix = 0
                PrevGovActionId (gaidConstitutionProp {gaidGovActionIx = GovActionIx 1})
              invalidNewConstitutionGovAction =
                NewConstitution
                  (SJust invalidPrevGovActionId)
                  ( Constitution
                      ( Anchor
                          (fromJust $ textToUrl "constitution.1")
                          constitutionHash
                      )
                      SNothing
                  )
              invalidNewConstitutionProposal =
                ProposalProcedure
                  { pProcDeposit = mempty
                  , pProcReturnAddr = rewardAccount
                  , pProcGovAction = invalidNewConstitutionGovAction
                  , pProcAnchor = def
                  }
          submitFailingProposal
            invalidNewConstitutionProposal
            [ inject $ InvalidPrevGovActionId invalidNewConstitutionProposal
            ]
        it "Valid PrevGovActionId but invalid purpose" $ do
          gaidConstitutionProp <- submitInitConstitutionGovAction
          rewardAccount <- registerRewardAccount
          let invalidNoConfidenceAction =
                NoConfidence $ SJust $ PrevGovActionId gaidConstitutionProp
              invalidNoConfidenceProposal =
                ProposalProcedure
                  { pProcDeposit = mempty
                  , pProcReturnAddr = rewardAccount
                  , pProcGovAction = invalidNoConfidenceAction
                  , pProcAnchor = def
                  }
          submitFailingProposal
            invalidNoConfidenceProposal
            [ inject $ InvalidPrevGovActionId invalidNoConfidenceProposal
            ]
      context "Valid proposals are accepted" $ do
        it "Submit Constitution and use valid PrevGovActionId" $ do
          curConstitution <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
          gaidConstitutionProp <- submitInitConstitutionGovAction
          constitutionHash <- freshSafeHash
          let constitutionActionNext =
                NewConstitution
                  (SJust $ PrevGovActionId gaidConstitutionProp)
                  ( Constitution
                      ( Anchor
                          (fromJust $ textToUrl "constitution.1")
                          constitutionHash
                      )
                      SNothing
                  )
          curConstitution' <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
          impAnn "Constitution has not been enacted yet" $
            curConstitution' `shouldBe` curConstitution
          submitGovAction_ constitutionActionNext
        it "Enact Constitution and use valid PrevGovActionId" $ do
          (dRep, committeeMember) <- electBasicCommittee
          constitutionHash <- freshSafeHash
          let constitution =
                Constitution
                  ( Anchor
                      (fromJust $ textToUrl "constitution.0")
                      constitutionHash
                  )
                  SNothing
              constitutionAction =
                NewConstitution SNothing constitution
          gaidConstitutionProp <- submitGovAction constitutionAction
          submitYesVote_ (DRepVoter dRep) gaidConstitutionProp
          submitYesVote_ (CommitteeVoter committeeMember) gaidConstitutionProp
          passEpoch
          passEpoch
          curConstitution <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
          impAnn "Constitution has been enacted" $
            curConstitution `shouldBe` constitution
          constitutionHash1 <- freshSafeHash
          let constitutionAction1 =
                NewConstitution
                  (SJust $ PrevGovActionId gaidConstitutionProp)
                  ( Constitution
                      ( Anchor
                          (fromJust $ textToUrl "constitution.1")
                          constitutionHash1
                      )
                      SNothing
                  )
          submitGovAction_ constitutionAction1
