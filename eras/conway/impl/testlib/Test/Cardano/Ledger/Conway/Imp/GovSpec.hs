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
    describe "Voting" $ do
      context "fails for" $ do
        it "expired gov-actions" $ do
          modifyPParams $ ppGovActionLifetimeL .~ 2 -- Voting after the 3rd epoch should fail
          khDRep <- setupSingleDRep
          (govActionId, _) <- submitConstitution SNothing
          passEpoch
          passEpoch
          passEpoch
          let voter = DRepVoter $ KeyHashObj khDRep
          submitFailingVote
            voter
            govActionId
            [ inject $
                VotingOnExpiredGovAction @era $
                  Map.singleton govActionId voter
            ]
        it "non-existent gov-actions" $ do
          khDRep <- setupSingleDRep
          (govActionId, _) <- submitConstitution SNothing
          let voter = DRepVoter $ KeyHashObj khDRep
              dummyGaid = govActionId {gaidGovActionIx = GovActionIx 99} -- non-existent `GovActionId`
          submitFailingVote
            voter
            dummyGaid
            [inject $ GovActionsDoNotExist @era $ Set.singleton dummyGaid]

    describe "Constitution proposals" $ do
      context "accepted for" $
        it "empty PrevGovId before the first constitution is enacted" $ do
          --  Initial proposal does not need a PrevGovActionId but after it is enacted, the
          --  following ones are not
          _ <- submitConstitution SNothing
          -- Until the first proposal is enacted all proposals with empty PrevGovActionIds are valid
          void $ submitConstitution SNothing

      context "rejected for" $ do
        it "empty PrevGovId after the first constitution was enacted" $ do
          (govActionId, _) <- submitConstitution SNothing
          modifyNES $ \nes ->
            nes
              & nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgEnactStateL . ensPrevConstitutionL
                .~ SJust (PrevGovActionId govActionId) -- Add first proposal to PrevGovActionIds in enacted state
              & nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgProposalsL
                .~ def -- Remove all proposals, so that the lookup only succeeds for enacted state.
                -- Once a proposal with a purpose has been enacted, following proposals can no
                -- longer have empty PrevGovActionIds
          constitution <- newConstitution
          let invalidNewConstitutionGovAction =
                NewConstitution
                  SNothing
                  constitution
          invalidNewConstitutionProposal <- proposalWithRewardAccount invalidNewConstitutionGovAction
          submitFailingProposal
            invalidNewConstitutionProposal
            [ inject $ InvalidPrevGovActionId invalidNewConstitutionProposal
            ]
        it "invalid index in PrevGovActionId" $ do
          (govActionId, _) <- submitConstitution SNothing
          constitution <- newConstitution
          let invalidPrevGovActionId =
                -- Expected Ix = 0
                PrevGovActionId (govActionId {gaidGovActionIx = GovActionIx 1})
              invalidNewConstitutionGovAction =
                NewConstitution
                  (SJust invalidPrevGovActionId)
                  constitution
          invalidNewConstitutionProposal <- proposalWithRewardAccount invalidNewConstitutionGovAction
          submitFailingProposal
            invalidNewConstitutionProposal
            [ inject $ InvalidPrevGovActionId invalidNewConstitutionProposal
            ]
        it "valid PrevGovActionId but invalid purpose" $ do
          (govActionId, _) <- submitConstitution SNothing
          let invalidNoConfidenceAction =
                NoConfidence $ SJust $ PrevGovActionId govActionId
          invalidNoConfidenceProposal <- proposalWithRewardAccount invalidNoConfidenceAction

          submitFailingProposal
            invalidNoConfidenceProposal
            [ inject $ InvalidPrevGovActionId invalidNoConfidenceProposal
            ]

    describe "Constitution" $ do
      it "submitted successfully with valid PrevGovActionId" $ do
        curConstitution <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
        (govActionId, _) <- submitConstitution SNothing
        curConstitution' <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
        impAnn "Constitution has not been enacted yet" $
          curConstitution' `shouldBe` curConstitution
        void $ submitConstitution (SJust $ PrevGovActionId govActionId)

      it "submitted and enacted when voted on" $ do
        (dRep, committeeMember) <- electBasicCommittee
        (govActionId, constitution) <- submitConstitution SNothing
        submitYesVote_ (DRepVoter dRep) govActionId
        submitYesVote_ (CommitteeVoter committeeMember) govActionId
        passEpoch
        passEpoch
        curConstitution <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
        impAnn "Constitution has been enacted" $
          curConstitution `shouldBe` constitution

submitConstitution ::
  forall era.
  ConwayEraImp era =>
  StrictMaybe (PrevGovActionId 'ConstitutionPurpose (EraCrypto era)) ->
  ImpTestM era (GovActionId (EraCrypto era), Constitution era)
submitConstitution prevGovId = do
  constitution <- newConstitution
  let constitutionAction =
        NewConstitution
          prevGovId
          constitution
  govActionId <- submitGovAction constitutionAction
  pure (govActionId, constitution)

newConstitution :: Era era => ImpTestM era (Constitution era)
newConstitution = do
  constitutionHash <- freshSafeHash
  pure $
    Constitution
      ( Anchor
          (fromJust $ textToUrl "constitution.0")
          constitutionHash
      )
      SNothing

proposalWithRewardAccount ::
  forall era.
  ConwayEraImp era =>
  GovAction era ->
  ImpTestM era (ProposalProcedure era)
proposalWithRewardAccount action = do
  rewardAccount <- registerRewardAccount
  pure
    ProposalProcedure
      { pProcDeposit = mempty
      , pProcReturnAddr = rewardAccount
      , pProcGovAction = action
      , pProcAnchor = def
      }
