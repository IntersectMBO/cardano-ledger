{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conway.Imp.GovSpec where

import Cardano.Ledger.Address (Addr (..), RewardAccount (..))
import Cardano.Ledger.Allegra.Scripts (
  pattern RequireAllOf,
  pattern RequireAnyOf,
  pattern RequireMOf,
  pattern RequireSignature,
 )
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.CertState (vsNumDormantEpochsL)
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams (
  dvtCommitteeNoConfidenceL,
  pvtCommitteeNoConfidenceL,
  pvtPPSecurityGroupL,
 )
import Cardano.Ledger.Conway.Rules (ConwayGovPredFailure (..))
import Cardano.Ledger.Credential (Credential (KeyHashObj), StakeReference (..))
import Cardano.Ledger.DRep (drepExpiryL)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Shelley.LedgerState
import qualified Cardano.Ledger.UMap as UM
import Cardano.Ledger.Val (zero)
import Data.Default.Class (Default (..))
import Data.Foldable (Foldable (..), traverse_)
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Data.Tree
import Lens.Micro
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
import Test.Cardano.Ledger.Imp.Common hiding (Success)

spec ::
  forall era.
  ( ConwayEraImp era
  , GovState era ~ ConwayGovState era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  SpecWith (ImpTestState era)
spec =
  describe "GOV" $ do
    describe "Hardfork is the first one (doesn't have a GovPurposeId) " $ do
      it "Hardfork minorFollow" (firstHardForkFollows minorFollow)
      it "Hardfork majorFollow" (firstHardForkFollows majorFollow)
      it "Hardfork cantFollow" firstHardForkCantFollow
    describe "Hardfork is the second one (has a GovPurposeId)" $ do
      it "Hardfork minorFollow" (secondHardForkFollows minorFollow)
      it "Hardfork majorFollow" (secondHardForkFollows majorFollow)
      it "Hardfork cantFollow" secondHardForkCantFollow
    describe "PPU needs to be wellformed" $ do
      let testMalformedProposal lbl lenz val = it lbl $ do
            pp <- getsNES $ nesEsL . curPParamsEpochStateL
            rew <- registerRewardAccount
            let ppUpdate =
                  emptyPParamsUpdate
                    & lenz .~ SJust val
                ga = ParameterChange SNothing ppUpdate SNothing
            submitFailingProposal
              ( ProposalProcedure
                  { pProcReturnAddr = rew
                  , pProcGovAction = ga
                  , pProcDeposit = pp ^. ppGovActionDepositL
                  , pProcAnchor = def
                  }
              )
              [injectFailure $ MalformedProposal ga]
      testMalformedProposal
        "ppuMaxBBSizeL cannot be 0"
        ppuMaxBBSizeL
        0
      testMalformedProposal
        "ppuMaxTxSizeL cannot be 0"
        ppuMaxTxSizeL
        0
      testMalformedProposal
        "ppuMaxBHSizeL cannot be 0"
        ppuMaxBHSizeL
        0
      testMalformedProposal
        "ppuMaxValSizeL cannot be 0"
        ppuMaxValSizeL
        0
      testMalformedProposal
        "ppuCollateralPercentageL cannot be 0"
        ppuCollateralPercentageL
        0
      testMalformedProposal
        "ppuCommitteeMaxTermLengthL cannot be 0"
        ppuCommitteeMaxTermLengthL
        $ EpochInterval 0
      testMalformedProposal
        "ppuGovActionLifetimeL cannot be 0"
        ppuGovActionLifetimeL
        $ EpochInterval 0
      testMalformedProposal
        "ppuPoolDepositL cannot be 0"
        ppuPoolDepositL
        zero
      testMalformedProposal
        "ppuGovActionDepositL cannot be 0"
        ppuGovActionDepositL
        zero
      testMalformedProposal
        "ppuDRepDepositL cannot be 0"
        ppuDRepDepositL
        zero
      it "PPU cannot be empty" $ do
        pp <- getsNES $ nesEsL . curPParamsEpochStateL
        rew <- registerRewardAccount
        let ga = ParameterChange SNothing emptyPParamsUpdate SNothing
        submitFailingProposal
          ( ProposalProcedure
              { pProcReturnAddr = rew
              , pProcGovAction = ga
              , pProcDeposit = pp ^. ppGovActionDepositL
              , pProcAnchor = def
              }
          )
          [injectFailure $ MalformedProposal ga]
    context "Proposal-forests" $ do
      context "GOV, EPOCH, RATIFY and ENACT" $ do
        it "Proposals submitted without proper parent fail" $ do
          let mkCorruptGovActionId :: GovActionId c -> GovActionId c
              mkCorruptGovActionId (GovActionId txi (GovActionIx gaix)) =
                GovActionId txi $ GovActionIx $ gaix + 999
          modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 4
          Node p1 [Node _p11 []] <-
            submitConstitutionGovActionTree
              SNothing
              $ Node
                ()
                [ Node () []
                ]
          constitutionHash <- freshSafeHash
          pp <- getsNES $ nesEsL . curPParamsEpochStateL
          khPropRwd <- freshKeyHash
          let constitutionAction =
                NewConstitution
                  (SJust $ GovPurposeId $ mkCorruptGovActionId p1)
                  ( Constitution
                      ( Anchor
                          (fromJust $ textToUrl 64 "constitution.after.0")
                          constitutionHash
                      )
                      SNothing
                  )
              constitutionProposal =
                ProposalProcedure
                  { pProcDeposit = pp ^. ppGovActionDepositL
                  , pProcReturnAddr = RewardAccount Testnet (KeyHashObj khPropRwd)
                  , pProcGovAction = constitutionAction
                  , pProcAnchor = def
                  }
          submitFailingProposal
            constitutionProposal
            [ injectFailure $ InvalidPrevGovActionId constitutionProposal
            ]
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
        it "Subtrees are pruned when proposals expire" $ do
          modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 4
          p1 <- submitInitConstitutionGovAction
          passNEpochs 3
          a <-
            submitConstitutionGovActionTree
              (SJust p1)
              $ Node
                ()
                [ Node () []
                , Node () []
                ]
          b <-
            submitConstitutionGovActionTree
              SNothing
              $ Node
                ()
                [ Node () []
                ]
          getProposalsForest
            `shouldReturn` [ Node SNothing []
                           , Node SNothing []
                           , Node SNothing []
                           , Node
                              SNothing
                              [ Node (SJust p1) [SJust <$> a]
                              , SJust <$> b
                              ]
                           ]
          passNEpochs 3
          getProposalsForest
            `shouldReturn` [ Node SNothing []
                           , Node SNothing []
                           , Node SNothing []
                           , Node SNothing [SJust <$> b]
                           ]
        it "Subtrees are pruned when competing proposals are enacted" $ do
          (dRep, committeeMember, GovPurposeId committeeGovActionId) <- electBasicCommittee
          a@[ _
              , b@(Node p2 _)
              ] <-
            submitConstitutionGovActionForest
              SNothing
              [ Node
                  ()
                  [ Node
                      ()
                      [ Node () []
                      , Node () []
                      ]
                  ]
              , Node
                  ()
                  [ Node () []
                  ]
              ]

          getProposalsForest
            `shouldReturn` [ Node SNothing []
                           , Node SNothing []
                           , Node (SJust committeeGovActionId) []
                           , Node SNothing (fmap SJust <$> a)
                           ]
          passEpoch
          submitYesVote_ (DRepVoter dRep) p2
          submitYesVote_ (CommitteeVoter committeeMember) p2
          passNEpochs 2
          getProposalsForest
            `shouldReturn` [ Node SNothing []
                           , Node SNothing []
                           , Node (SJust committeeGovActionId) []
                           , SJust <$> b
                           ]
        it "Subtrees are pruned when proposals expire over multiple rounds" $ do
          modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 4
          p1 <- submitInitConstitutionGovAction
          getProposalsForest
            `shouldReturn` [ Node SNothing []
                           , Node SNothing []
                           , Node SNothing []
                           , Node
                              SNothing
                              [ Node (SJust p1) []
                              ]
                           ]
          passEpoch
          p2 <- submitInitConstitutionGovAction
          p11 <- submitChildConstitutionGovAction p1
          getProposalsForest
            `shouldReturn` [ Node SNothing []
                           , Node SNothing []
                           , Node SNothing []
                           , Node
                              SNothing
                              [ Node (SJust p1) [Node (SJust p11) []]
                              , Node (SJust p2) []
                              ]
                           ]

          passEpoch
          p3 <- submitInitConstitutionGovAction
          p21 <- submitChildConstitutionGovAction p2
          a <-
            submitConstitutionGovActionForest
              (SJust p11)
              [ Node () []
              , Node () []
              ]
          getProposalsForest
            `shouldReturn` [ Node SNothing []
                           , Node SNothing []
                           , Node SNothing []
                           , Node
                              SNothing
                              [ Node
                                  (SJust p1)
                                  [ Node
                                      (SJust p11)
                                      (fmap SJust <$> a)
                                  ]
                              , Node (SJust p2) [Node (SJust p21) []]
                              , Node (SJust p3) []
                              ]
                           ]
          passEpoch
          p4 <- submitInitConstitutionGovAction
          p31 <- submitChildConstitutionGovAction p3
          p211 <- submitChildConstitutionGovAction p21
          getProposalsForest
            `shouldReturn` [ Node SNothing []
                           , Node SNothing []
                           , Node SNothing []
                           , Node
                              SNothing
                              [ Node
                                  (SJust p1)
                                  [ Node
                                      (SJust p11)
                                      (fmap SJust <$> a)
                                  ]
                              , Node (SJust p2) [Node (SJust p21) [Node (SJust p211) []]]
                              , Node (SJust p3) [Node (SJust p31) []]
                              , Node (SJust p4) []
                              ]
                           ]
          passNEpochs 3
          getProposalsForest
            `shouldReturn` [ Node SNothing []
                           , Node SNothing []
                           , Node SNothing []
                           , Node
                              SNothing
                              [ Node (SJust p2) [Node (SJust p21) [Node (SJust p211) []]]
                              , Node (SJust p3) [Node (SJust p31) []]
                              , Node (SJust p4) []
                              ]
                           ]
          p5 <- submitInitConstitutionGovAction
          p41 <- submitChildConstitutionGovAction p4
          p311 <- submitChildConstitutionGovAction p31
          p212 <- submitChildConstitutionGovAction p21
          getProposalsForest
            `shouldReturn` [ Node SNothing []
                           , Node SNothing []
                           , Node SNothing []
                           , Node
                              SNothing
                              [ Node
                                  (SJust p2)
                                  [ Node
                                      (SJust p21)
                                      [ Node (SJust p211) []
                                      , Node (SJust p212) []
                                      ]
                                  ]
                              , Node (SJust p3) [Node (SJust p31) [Node (SJust p311) []]]
                              , Node (SJust p4) [Node (SJust p41) []]
                              , Node (SJust p5) []
                              ]
                           ]
          passEpoch
          p6 <- submitInitConstitutionGovAction
          p51 <- submitChildConstitutionGovAction p5
          p411 <- submitChildConstitutionGovAction p41
          p312 <- submitChildConstitutionGovAction p31
          getProposalsForest
            `shouldReturn` [ Node SNothing []
                           , Node SNothing []
                           , Node SNothing []
                           , Node
                              SNothing
                              [ Node
                                  (SJust p3)
                                  [ Node
                                      (SJust p31)
                                      [ Node (SJust p311) []
                                      , Node (SJust p312) []
                                      ]
                                  ]
                              , Node (SJust p4) [Node (SJust p41) [Node (SJust p411) []]]
                              , Node (SJust p5) [Node (SJust p51) []]
                              , Node (SJust p6) []
                              ]
                           ]
          passEpoch
          getProposalsForest
            `shouldReturn` [ Node SNothing []
                           , Node SNothing []
                           , Node SNothing []
                           , Node
                              SNothing
                              [ Node (SJust p4) [Node (SJust p41) [Node (SJust p411) []]]
                              , Node (SJust p5) [Node (SJust p51) []]
                              , Node (SJust p6) []
                              ]
                           ]
          passEpoch
          getProposalsForest
            `shouldReturn` [ Node SNothing []
                           , Node SNothing []
                           , Node SNothing []
                           , Node
                              SNothing
                              [ Node (SJust p5) [Node (SJust p51) []]
                              , Node (SJust p6) []
                              ]
                           ]
          passNEpochs 3
          getProposalsForest
            `shouldReturn` [ Node SNothing []
                           , Node SNothing []
                           , Node SNothing []
                           , Node
                              SNothing
                              [ Node (SJust p6) []
                              ]
                           ]
          passEpoch
          getProposalsForest
            `shouldReturn` [ Node SNothing []
                           , Node SNothing []
                           , Node SNothing []
                           , Node SNothing []
                           ]
        it "Subtrees are pruned when competing proposals are enacted over multiple rounds" $ do
          (dRep, committeeMember, _) <- electBasicCommittee
          a@[ c
              , Node
                  p2
                  [ Node p21 []
                    , Node p22 []
                    ]
              , Node p3 []
              ] <-
            submitConstitutionGovActionForest
              SNothing
              [ Node
                  ()
                  [ Node
                      ()
                      [ Node () []
                      , Node () []
                      ]
                  ]
              , Node
                  ()
                  [ Node () []
                  , Node () []
                  ]
              , Node () []
              ]
          submitYesVote_ (DRepVoter dRep) p2
          submitYesVote_ (CommitteeVoter committeeMember) p2
          submitYesVote_ (DRepVoter dRep) p21
          submitYesVote_ (CommitteeVoter committeeMember) p21
          submitYesVote_ (DRepVoter dRep) p3
          submitYesVote_ (CommitteeVoter committeeMember) p3 -- Two competing proposals break the tie based on proposal order
          fmap (!! 3) getProposalsForest
            `shouldReturn` Node SNothing (fmap SJust <$> a)
          passEpoch
          p4 <- submitInitConstitutionGovAction
          p31 <- submitChildConstitutionGovAction p3
          p211 <- submitChildConstitutionGovAction p21
          fmap (!! 3) getProposalsForest
            `shouldReturn` Node
              SNothing
              [ SJust <$> c
              , Node
                  (SJust p2)
                  [ Node (SJust p21) [Node (SJust p211) []]
                  , Node (SJust p22) []
                  ]
              , Node (SJust p3) [Node (SJust p31) []]
              , Node (SJust p4) []
              ]
          passEpoch
          fmap (!! 3) getProposalsForest
            `shouldReturn` Node
              (SJust p2)
              [ Node (SJust p21) [Node (SJust p211) []]
              , Node (SJust p22) []
              ]
          [ Node p212 []
            , Node p213 []
            , Node p214 []
            ] <-
            submitConstitutionGovActionForest
              (SJust p21)
              [ Node () []
              , Node () []
              , Node () []
              ]
          p2131 <- submitChildConstitutionGovAction p213
          p2141 <- submitChildConstitutionGovAction p214
          submitYesVote_ (DRepVoter dRep) p212
          submitYesVote_ (CommitteeVoter committeeMember) p212
          fmap (!! 3) getProposalsForest
            `shouldReturn` Node
              (SJust p2)
              [ Node
                  (SJust p21)
                  [ Node (SJust p211) []
                  , Node (SJust p212) []
                  , Node (SJust p213) [Node (SJust p2131) []]
                  , Node (SJust p214) [Node (SJust p2141) []]
                  ]
              , Node (SJust p22) []
              ]
          passNEpochs 2
          fmap (!! 3) getProposalsForest
            `shouldReturn` Node (SJust p212) []
          props <- getProposals
          proposalsSize props `shouldBe` 0
        it "Votes from subsequent epochs are considered for ratification" $ do
          modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 4
          (dRep, committeeMember, _) <- electBasicCommittee
          [Node p1 []] <-
            submitConstitutionGovActionForest
              SNothing
              [Node () []]
          fmap (!! 3) getProposalsForest
            `shouldReturn` Node SNothing [Node (SJust p1) []]
          passNEpochs 2
          submitYesVote_ (DRepVoter dRep) p1
          submitYesVote_ (CommitteeVoter committeeMember) p1
          passNEpochs 2
          fmap (!! 3) getProposalsForest
            `shouldReturn` Node (SJust p1) []
        it "Subtrees are pruned for both enactment and expiry over multiple rounds" $ do
          (dRep, committeeMember, _) <- electBasicCommittee
          modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 4
          [ a@( Node
                  p1
                  [ b@( Node
                          p11
                          [ Node _p111 []
                            , Node _p112 []
                            ]
                        )
                    ]
                )
            , Node
                _p2
                [ Node _p21 []
                  , Node _p22 []
                  ]
            , Node p3 []
            ] <-
            submitConstitutionGovActionForest
              SNothing
              [ Node
                  ()
                  [ Node
                      ()
                      [ Node () []
                      , Node () []
                      ]
                  ]
              , Node
                  ()
                  [ Node () []
                  , Node () []
                  ]
              , Node () []
              ]
          passNEpochs 2
          submitYesVote_ (DRepVoter dRep) p1
          submitYesVote_ (CommitteeVoter committeeMember) p1
          submitYesVote_ (DRepVoter dRep) p11
          submitYesVote_ (CommitteeVoter committeeMember) p11
          submitYesVote_ (DRepVoter dRep) p3
          submitYesVote_ (CommitteeVoter committeeMember) p3 -- Two competing proposals break the tie based on proposal order
          passNEpochs 2
          fmap (!! 3) getProposalsForest
            `shouldReturn` SJust
            <$> a
          passEpoch -- ConstitutionPurpose is a delayed action
          fmap (!! 3) getProposalsForest
            `shouldReturn` SJust
            <$> b
          passNEpochs 2
          fmap (!! 3) getProposalsForest
            `shouldReturn` Node (SJust p11) []
          c@[ Node _p113 []
              , Node _p114 []
              ] <-
            submitConstitutionGovActionForest
              (SJust p11)
              [ Node () []
              , Node () []
              ]
          fmap (!! 3) getProposalsForest
            `shouldReturn` Node (SJust p11) (fmap SJust <$> c)
          passNEpochs 4
          d@[ Node _p115 []
              , Node p116 []
              ] <-
            submitConstitutionGovActionForest
              (SJust p11)
              [ Node () []
              , Node () []
              ]
          fmap (!! 3) getProposalsForest
            `shouldReturn` Node (SJust p11) (fmap SJust <$> (c <> d))
          passNEpochs 2
          fmap (!! 3) getProposalsForest
            `shouldReturn` Node (SJust p11) (fmap SJust <$> d)
          submitYesVote_ (DRepVoter dRep) p116
          submitYesVote_ (CommitteeVoter committeeMember) p116
          passNEpochs 3
          fmap (!! 3) getProposalsForest
            `shouldReturn` Node (SJust p116) []
    describe "Proposals always have valid previous actions" $ do
      context "Invalid proposals are rejected" $ do
        it "Invalid Index in GovPurposeId" $ do
          gaidConstitutionProp <- submitInitConstitutionGovAction
          curConstitution <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
          constitutionHash <- freshSafeHash
          let constitutionActionNext =
                NewConstitution
                  (SJust $ GovPurposeId gaidConstitutionProp)
                  ( Constitution
                      ( Anchor
                          (fromJust $ textToUrl 64 "constitution.1")
                          constitutionHash
                      )
                      SNothing
                  )
          curConstitution' <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
          impAnn "Constitution has not been enacted yet" $
            curConstitution' `shouldBe` curConstitution
          submitGovAction_ constitutionActionNext
        it "Enact Constitution and use valid GovPurposeId" $ do
          (dRep, committeeMember, _) <- electBasicCommittee
          constitutionHash <- freshSafeHash
          let constitution =
                Constitution
                  ( Anchor
                      (fromJust $ textToUrl 64 "constitution.0")
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
                  (SJust $ GovPurposeId gaidConstitutionProp)
                  ( Constitution
                      ( Anchor
                          (fromJust $ textToUrl 64 "constitution.1")
                          constitutionHash1
                      )
                      SNothing
                  )
          submitGovAction_ constitutionAction1

    describe "Voting" $ do
      context "fails for" $ do
        it "expired gov-actions" $ do
          -- Voting after the 3rd epoch should fail
          modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
          (khDRep, _, _) <- setupSingleDRep 1_000_000
          (govActionId, _) <- submitConstitution SNothing
          passEpoch
          passEpoch
          passEpoch
          let voter = DRepVoter $ KeyHashObj khDRep
          submitFailingVote
            voter
            govActionId
            [ injectFailure $ VotingOnExpiredGovAction [(voter, govActionId)]
            ]
        it "non-existent gov-actions" $ do
          (khDRep, _, _) <- setupSingleDRep 1_000_000
          (govActionId, _) <- submitConstitution SNothing
          let voter = DRepVoter $ KeyHashObj khDRep
              dummyGaid = govActionId {gaidGovActionIx = GovActionIx 99} -- non-existent `GovActionId`
          submitFailingVote
            voter
            dummyGaid
            [injectFailure $ GovActionsDoNotExist $ pure dummyGaid]
        it
          "committee member voting on committee change"
          committeeMemberVotingOnCommitteeChange
        it "non-committee-member voting on committee change as a committee member" $ do
          (_, _, prevGovActionId) <- electBasicCommittee
          credCandidate <- KeyHashObj <$> freshKeyHash
          credVoter <- KeyHashObj <$> freshKeyHash
          committeeUpdateId <-
            submitGovAction $
              UpdateCommittee
                (SJust prevGovActionId)
                mempty
                (Map.singleton credCandidate $ EpochNo 28)
                (3 %! 5)
          let voter = CommitteeVoter credVoter
          trySubmitVote VoteNo voter committeeUpdateId
            `shouldReturn` Left
              [ injectFailure $ DisallowedVoters [(voter, committeeUpdateId)]
              ]
        it
          "committee member can't vote on committee update when sandwiched between other votes"
          ccVoteOnConstitutionFailsWithMultipleVotes
      it "Motion of no confidence can be passed" $ do
        modifyPParams $ \pp ->
          pp
            & ppDRepVotingThresholdsL . dvtCommitteeNoConfidenceL .~ 1 %! 2
            & ppPoolVotingThresholdsL . pvtCommitteeNoConfidenceL .~ 1 %! 2
            & ppCommitteeMaxTermLengthL .~ EpochInterval 200
        let
          getCommittee =
            getsNES $
              nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . committeeGovStateL
          assertNoCommittee :: HasCallStack => ImpTestM era ()
          assertNoCommittee =
            do
              committee <- getCommittee
              impAnn "There should not be a committee" $ committee `shouldBe` SNothing
        assertNoCommittee
        khCC <- freshKeyHash
        (drep, _, _) <- setupSingleDRep 1_000_000
        let committeeMap =
              Map.fromList
                [ (KeyHashObj khCC, EpochNo 50)
                ]
        prevGaidCommittee@(GovPurposeId gaidCommittee) <-
          electCommittee
            SNothing
            drep
            mempty
            committeeMap
        (khSPO, _, _) <- setupPoolWithStake $ Coin 42_000_000
        logStakeDistr
        submitYesVote_ (StakePoolVoter khSPO) gaidCommittee
        replicateM_ 4 passEpoch
        impAnn "Committee should be elected" $ do
          committee <- getCommittee
          committee `shouldBe` SJust (Committee committeeMap $ 1 %! 2)
        pp <- getsNES $ nesEsL . curPParamsEpochStateL
        returnAddr <- registerRewardAccount
        gaidNoConf <-
          submitProposal $
            ProposalProcedure
              { pProcReturnAddr = returnAddr
              , pProcGovAction = NoConfidence (SJust prevGaidCommittee)
              , pProcDeposit = pp ^. ppGovActionDepositL
              , pProcAnchor = def
              }
        submitYesVote_ (StakePoolVoter khSPO) gaidNoConf
        submitYesVote_ (DRepVoter $ KeyHashObj drep) gaidNoConf
        replicateM_ 4 passEpoch
        assertNoCommittee
      it "SPO needs to vote on security-relevant parameter changes" $ do
        (drep, ccCred, _) <- electBasicCommittee
        (khPool, _, _) <- setupPoolWithStake $ Coin 42_000_000
        initMinFeeA <- getsNES $ nesEsL . curPParamsEpochStateL . ppMinFeeAL
        gaidThreshold <- impAnn "Update StakePool thresholds" $ do
          pp <- getsNES $ nesEsL . curPParamsEpochStateL
          (pp ^. ppPoolVotingThresholdsL . pvtPPSecurityGroupL) `shouldBe` minBound
          rew <- registerRewardAccount
          let ppUpdate =
                emptyPParamsUpdate
                  & ppuPoolVotingThresholdsL
                    .~ SJust
                      PoolVotingThresholds
                        { pvtPPSecurityGroup = 1 %! 2
                        , pvtMotionNoConfidence = 1 %! 2
                        , pvtHardForkInitiation = 1 %! 2
                        , pvtCommitteeNormal = 1 %! 2
                        , pvtCommitteeNoConfidence = 1 %! 2
                        }
                  & ppuGovActionLifetimeL .~ SJust (EpochInterval 100)
          gaidThreshold <-
            submitProposal $
              ProposalProcedure
                { pProcReturnAddr = rew
                , pProcGovAction = ParameterChange SNothing ppUpdate SNothing
                , pProcDeposit = pp ^. ppGovActionDepositL
                , pProcAnchor = def
                }
          submitYesVote_ (DRepVoter drep) gaidThreshold
          submitYesVote_ (CommitteeVoter ccCred) gaidThreshold
          logAcceptedRatio gaidThreshold
          pure gaidThreshold
        passEpoch
        logAcceptedRatio gaidThreshold
        passEpoch
        let newMinFeeA = Coin 12_345
        gaidMinFee <- do
          pp <- getsNES $ nesEsL . curPParamsEpochStateL
          impAnn "Security group threshold should be 1/2" $
            (pp ^. ppPoolVotingThresholdsL . pvtPPSecurityGroupL) `shouldBe` (1 %! 2)
          rew <- registerRewardAccount
          gaidMinFee <-
            submitProposal $
              ProposalProcedure
                { pProcReturnAddr = rew
                , pProcGovAction =
                    ParameterChange
                      (SJust (GovPurposeId gaidThreshold))
                      ( emptyPParamsUpdate
                          & ppuMinFeeAL .~ SJust newMinFeeA
                      )
                      SNothing
                , pProcDeposit = pp ^. ppGovActionDepositL
                , pProcAnchor = def
                }
          submitYesVote_ (DRepVoter drep) gaidMinFee
          submitYesVote_ (CommitteeVoter ccCred) gaidMinFee
          pure gaidMinFee
        passEpoch
        logAcceptedRatio gaidMinFee
        passEpoch
        do
          pp <- getsNES $ nesEsL . curPParamsEpochStateL
          (pp ^. ppMinFeeAL) `shouldBe` initMinFeeA
          submitYesVote_ (StakePoolVoter khPool) gaidMinFee
        passEpoch
        logStakeDistr
        logAcceptedRatio gaidMinFee
        logRatificationChecks gaidMinFee
        passEpoch
        pp <- getsNES $ nesEsL . curPParamsEpochStateL
        (pp ^. ppMinFeeAL) `shouldBe` newMinFeeA

    describe "Constitution proposals" $ do
      context "accepted for" $
        it "empty PrevGovId before the first constitution is enacted" $ do
          --  Initial proposal does not need a GovPurposeId but after it is enacted, the
          --  following ones are not
          _ <- submitConstitution SNothing
          -- Until the first proposal is enacted all proposals with empty GovPurposeIds are valid
          void $ submitConstitution SNothing

      context "rejected for" $ do
        it "empty PrevGovId after the first constitution was enacted" $ do
          (dRep, committeeMember, _) <- electBasicCommittee
          (govActionId, _constitution) <- submitConstitution SNothing
          submitYesVote_ (DRepVoter dRep) govActionId
          submitYesVote_ (CommitteeVoter committeeMember) govActionId
          passNEpochs 2
          constitution <- newConstitution
          let invalidNewConstitutionGovAction =
                NewConstitution
                  SNothing
                  constitution
          invalidNewConstitutionProposal <- proposalWithRewardAccount invalidNewConstitutionGovAction
          submitFailingProposal
            invalidNewConstitutionProposal
            [ injectFailure $ InvalidPrevGovActionId invalidNewConstitutionProposal
            ]
        it "invalid index in GovPurposeId" $ do
          (_dRep, _committeeMember, _) <- electBasicCommittee
          (govActionId, _constitution) <- submitConstitution SNothing
          passNEpochs 2
          constitution <- newConstitution
          let invalidPrevGovActionId =
                -- Expected Ix = 0
                GovPurposeId (govActionId {gaidGovActionIx = GovActionIx 1})
              invalidNewConstitutionGovAction =
                NewConstitution
                  (SJust invalidPrevGovActionId)
                  constitution
          invalidNewConstitutionProposal <- proposalWithRewardAccount invalidNewConstitutionGovAction
          submitFailingProposal
            invalidNewConstitutionProposal
            [ injectFailure $ InvalidPrevGovActionId invalidNewConstitutionProposal
            ]
        it "valid GovPurposeId but invalid purpose" $ do
          (_dRep, _committeeMember, _) <- electBasicCommittee
          (govActionId, _constitution) <- submitConstitution SNothing
          passNEpochs 2
          let invalidNoConfidenceAction =
                NoConfidence $ SJust $ GovPurposeId govActionId
          invalidNoConfidenceProposal <- proposalWithRewardAccount invalidNoConfidenceAction

          submitFailingProposal
            invalidNoConfidenceProposal
            [ injectFailure $ InvalidPrevGovActionId invalidNoConfidenceProposal
            ]

    describe "Constitution" $ do
      it "submitted successfully with valid GovPurposeId" $ do
        modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 1

        curConstitution <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
        initialPulser <- getsNES $ newEpochStateGovStateL . drepPulsingStateGovStateL
        initialEnactState <- getEnactState

        (govActionId, _) <- submitConstitution SNothing
        curConstitution' <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
        impAnn "Constitution has not been enacted yet" $
          curConstitution' `shouldBe` curConstitution

        ConwayGovState expectedProposals _ _ _ _ expectedPulser <-
          getsNES newEpochStateGovStateL
        expectedEnactState <- getEnactState

        impAnn "EnactState reflects the submitted governance action" $ do
          expectedEnactState `shouldBe` initialEnactState

        impAnn "Proposals contain the submitted proposal" $
          expectedProposals `shouldSatisfy` \props -> govActionId `elem` proposalsIds props

        impAnn "Pulser has not changed" $
          expectedPulser `shouldBe` initialPulser

        passEpoch >> passEpoch
        impAnn "Proposal gets removed after expiry" $ do
          ConwayGovState _ _ _ _ _ pulser <- getsNES newEpochStateGovStateL
          let ratifyState = extractDRepPulsingState pulser
          rsExpired ratifyState `shouldBe` Set.singleton govActionId

      it "submitted and enacted when voted on" $ do
        (dRep, committeeMember, _) <- electBasicCommittee
        (govActionId, constitution) <- submitConstitution SNothing

        ConwayGovState proposalsBeforeVotes enactStateBeforeVotes pulserBeforeVotes _ _ _ <-
          getsNES newEpochStateGovStateL
        submitYesVote_ (DRepVoter dRep) govActionId
        submitYesVote_ (CommitteeVoter committeeMember) govActionId
        ConwayGovState proposalsAfterVotes enactStateAfterVotes pulserAfterVotes _ _ _ <-
          getsNES newEpochStateGovStateL

        impAnn "Votes are recorded in the proposals" $ do
          let proposalsWithVotes =
                proposalsAddVote
                  (CommitteeVoter committeeMember)
                  VoteYes
                  govActionId
                  ( proposalsAddVote
                      (DRepVoter dRep)
                      VoteYes
                      govActionId
                      proposalsBeforeVotes
                  )
          proposalsAfterVotes `shouldBe` proposalsWithVotes

        impAnn "EnactState has not changed" $
          enactStateAfterVotes `shouldBe` enactStateBeforeVotes

        impAnn "Pulser has not changed" $
          pulserAfterVotes `shouldBe` pulserBeforeVotes

        passEpoch

        impAnn "New constitution is not enacted after one epoch" $ do
          constitutionAfterOneEpoch <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
          constitutionAfterOneEpoch `shouldBe` def

        impAnn "Pulser should reflect the constitution to be enacted" $ do
          ConwayGovState _ _ _ _ _ pulser <- getsNES newEpochStateGovStateL
          let ratifyState = extractDRepPulsingState pulser
          gasId <$> rsEnacted ratifyState `shouldBe` govActionId Seq.:<| Seq.Empty
          rsEnactState ratifyState ^. ensConstitutionL `shouldBe` constitution

        passEpoch

        impAnn "Constitution is enacted after two epochs" $ do
          curConstitution <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
          curConstitution `shouldBe` constitution

        impAnn "Pulser is reset" $ do
          ConwayGovState _ _ _ _ _ pulser <- getsNES newEpochStateGovStateL
          let pulserRatifyState = extractDRepPulsingState pulser
          rsEnacted pulserRatifyState `shouldBe` Seq.empty
          enactState <- getEnactState
          rsEnactState pulserRatifyState `shouldBe` enactState

      it "policy is respected by proposals" $ do
        keyHash <- freshKeyHash
        scriptHash <- impAddNativeScript $ RequireAllOf (SSeq.singleton (RequireSignature keyHash))
        wrongScriptHash <-
          impAddNativeScript $
            RequireMOf 1 $
              SSeq.fromList [RequireAnyOf mempty, RequireAllOf mempty]
        modifyNES $
          nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . constitutionGovStateL
            .~ Constitution def (SJust scriptHash)
        pp <- getsNES $ nesEsL . curPParamsEpochStateL
        impAnn "ParameterChange with correct policy succeeds" $ do
          let
            pparamsUpdate =
              def
                & ppuCommitteeMinSizeL .~ SJust 1
          rewardAccount <- registerRewardAccount
          submitProposal_
            ProposalProcedure
              { pProcReturnAddr = rewardAccount
              , pProcGovAction = ParameterChange SNothing pparamsUpdate (SJust scriptHash)
              , pProcDeposit = pp ^. ppGovActionDepositL
              , pProcAnchor = def
              }

        impAnn "TreasuryWithdrawals with correct policy succeeds" $ do
          rewardAccount <- registerRewardAccount
          let
            withdrawals =
              Map.fromList
                [ (rewardAccount, Coin 1000)
                ]
          submitProposal_
            ProposalProcedure
              { pProcReturnAddr = rewardAccount
              , pProcGovAction = TreasuryWithdrawals withdrawals (SJust scriptHash)
              , pProcDeposit = pp ^. ppGovActionDepositL
              , pProcAnchor = def
              }

        impAnn "ParameterChange with invalid policy succeeds" $ do
          rewardAccount <- registerRewardAccount
          let
            pparamsUpdate =
              def
                & ppuCommitteeMinSizeL .~ SJust 2
          res <-
            trySubmitProposal
              ProposalProcedure
                { pProcReturnAddr = rewardAccount
                , pProcGovAction = ParameterChange SNothing pparamsUpdate (SJust wrongScriptHash)
                , pProcDeposit = pp ^. ppGovActionDepositL
                , pProcAnchor = def
                }
          res
            `shouldBeLeft` [ injectFailure $
                              InvalidPolicyHash (SJust wrongScriptHash) (SJust scriptHash)
                           ]

        impAnn "TreasuryWithdrawals with invalid policy succeeds" $ do
          rewardAccount <- registerRewardAccount
          let
            withdrawals =
              Map.fromList
                [ (rewardAccount, Coin 1000)
                ]
          res <-
            trySubmitProposal
              ProposalProcedure
                { pProcReturnAddr = rewardAccount
                , pProcGovAction = TreasuryWithdrawals withdrawals (SJust wrongScriptHash)
                , pProcDeposit = pp ^. ppGovActionDepositL
                , pProcAnchor = def
                }
          res
            `shouldBeLeft` [ injectFailure $
                              InvalidPolicyHash (SJust wrongScriptHash) (SJust scriptHash)
                           ]

    describe "DRep expiry" $ do
      it "is updated based on to number of dormant epochs" $ do
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
    describe "Active voting stake" $ do
      context "DRep" $ do
        it "UTxOs contribute to active voting stake" $ do
          -- Only modify the applicable thresholds
          modifyPParams $ \pp ->
            pp
              & ppDRepVotingThresholdsL
                .~ def
                  { dvtCommitteeNormal = 51 %! 100
                  , dvtCommitteeNoConfidence = 51 %! 100
                  }
              & ppCommitteeMaxTermLengthL .~ EpochInterval 20
          -- Setup DRep delegation #1
          (drepKH1, stakingKH1, paymentKP1) <- setupSingleDRep 1_000_000
          -- Setup DRep delegation #2
          (_drepKH2, _stakingKH2, _paymentKP2) <- setupSingleDRep 1_000_000
          -- Submit a committee proposal
          cc <- KeyHashObj <$> freshKeyHash
          let addCCAction = UpdateCommittee SNothing mempty (Map.singleton cc 10) (75 %! 100)
          addCCGaid <- submitGovAction addCCAction
          -- Submit the vote
          submitVote_ VoteYes (DRepVoter $ KeyHashObj drepKH1) addCCGaid
          passNEpochs 2
          -- The vote should not result in a ratification
          isDRepAccepted addCCGaid `shouldReturn` False
          getLastEnactedCommittee `shouldReturn` SNothing
          -- Bump up the UTxO delegated
          -- to barely make the threshold (51 %! 100)
          stakingKP1 <- lookupKeyPair stakingKH1
          sendCoinTo (mkAddr (paymentKP1, stakingKP1)) (inject $ Coin 200_000)
          passNEpochs 2
          -- The same vote should now successfully ratify the proposal
          getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId addCCGaid)
        it "Rewards contribute to active voting stake" $ do
          -- Only modify the applicable thresholds
          modifyPParams $ \pp ->
            pp
              & ppDRepVotingThresholdsL
                .~ def
                  { dvtCommitteeNormal = 51 %! 100
                  , dvtCommitteeNoConfidence = 51 %! 100
                  }
              & ppCommitteeMaxTermLengthL .~ EpochInterval 20
          -- Setup DRep delegation #1
          (drepKH1, stakingKH1, _paymentKP1) <- setupSingleDRep 1_000_000
          -- Setup DRep delegation #2
          (_drepKH2, _stakingKH2, _paymentKP2) <- setupSingleDRep 1_000_000
          -- Submit a committee proposal
          cc <- KeyHashObj <$> freshKeyHash
          let addCCAction = UpdateCommittee SNothing mempty (Map.singleton cc 10) (75 %! 100)
          addCCGaid <- submitGovAction addCCAction
          -- Submit the vote
          submitVote_ VoteYes (DRepVoter $ KeyHashObj drepKH1) addCCGaid
          passNEpochs 2
          -- The vote should not result in a ratification
          isDRepAccepted addCCGaid `shouldReturn` False
          getLastEnactedCommittee `shouldReturn` SNothing
          -- Add to the rewards of the delegator to this DRep
          -- to barely make the threshold (51 %! 100)
          modifyNES $
            nesEsL . epochStateUMapL
              %~ UM.adjust
                (\(UM.RDPair r d) -> UM.RDPair (r <> UM.CompactCoin 200_000) d)
                (KeyHashObj stakingKH1)
                . UM.RewDepUView
          passNEpochs 2
          -- The same vote should now successfully ratify the proposal
          getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId addCCGaid)
      context "StakePool" $ do
        it "UTxOs contribute to active voting stake" $ do
          -- Only modify the applicable thresholds
          modifyPParams $ \pp ->
            pp
              & ppPoolVotingThresholdsL
                .~ def
                  { pvtCommitteeNormal = 51 %! 100
                  , pvtCommitteeNoConfidence = 51 %! 100
                  }
              & ppCommitteeMaxTermLengthL .~ EpochInterval 20
          -- Setup Pool delegation #1
          (poolKH1, delegatorCPayment1, delegatorCStaking1) <- setupPoolWithStake $ Coin 1_000_000
          -- Setup Pool delegation #2
          (_poolKH2, _delegatorCPayment2, _delegatorCStaking2) <- setupPoolWithStake $ Coin 1_000_000
          passEpoch
          -- Submit a committee proposal
          cc <- KeyHashObj <$> freshKeyHash
          let addCCAction = UpdateCommittee SNothing mempty (Map.singleton cc 10) (75 %! 100)
          addCCGaid <- submitGovAction addCCAction
          -- Submit the vote
          submitVote_ VoteYes (StakePoolVoter poolKH1) addCCGaid
          passNEpochs 2
          -- The vote should not result in a ratification
          logRatificationChecks addCCGaid
          isSpoAccepted addCCGaid `shouldReturn` False
          getLastEnactedCommittee `shouldReturn` SNothing
          -- Bump up the UTxO delegated
          -- to barely make the threshold (51 %! 100)
          sendCoinTo
            (Addr Testnet delegatorCPayment1 (StakeRefBase delegatorCStaking1))
            (Coin 200_000)
          passNEpochs 2
          -- The same vote should now successfully ratify the proposal
          getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId addCCGaid)
        it "Rewards contribute to active voting stake" $ do
          -- Only modify the applicable thresholds
          modifyPParams $ \pp ->
            pp
              & ppPoolVotingThresholdsL
                .~ def
                  { pvtCommitteeNormal = 51 %! 100
                  , pvtCommitteeNoConfidence = 51 %! 100
                  }
              & ppCommitteeMaxTermLengthL .~ EpochInterval 20
          -- Setup Pool delegation #1
          (poolKH1, _delegatorCPayment1, delegatorCStaking1) <- setupPoolWithStake $ Coin 1_000_000
          -- Setup Pool delegation #2
          (_poolKH2, _delegatorCPayment2, _delegatorCStaking2) <- setupPoolWithStake $ Coin 1_000_000
          passEpoch
          -- Submit a committee proposal
          cc <- KeyHashObj <$> freshKeyHash
          let addCCAction = UpdateCommittee SNothing mempty (Map.singleton cc 10) (75 %! 100)
          addCCGaid <- submitGovAction addCCAction
          -- Submit the vote
          submitVote_ VoteYes (StakePoolVoter poolKH1) addCCGaid
          passNEpochs 2
          -- The vote should not result in a ratification
          isSpoAccepted addCCGaid `shouldReturn` False
          getLastEnactedCommittee `shouldReturn` SNothing
          -- Add to the rewards of the delegator to this SPO
          -- to barely make the threshold (51 %! 100)
          modifyNES $
            nesEsL . epochStateUMapL
              %~ UM.adjust
                (\(UM.RDPair r d) -> UM.RDPair (r <> UM.CompactCoin 200_000) d)
                delegatorCStaking1
                . UM.RewDepUView
          passNEpochs 2
          -- The same vote should now successfully ratify the proposal
          getLastEnactedCommittee `shouldReturn` SJust (GovPurposeId addCCGaid)
    describe "Committee actions validation" $ do
      it "A CC that has resigned will need to be first voted out and then voted in to be considered active" $ do
        (dRep, _, gaidCC) <- electBasicCommittee
        passNEpochs 2
        -- Add a fresh CC
        cc <- KeyHashObj <$> freshKeyHash
        let addCCAction = UpdateCommittee (SJust gaidCC) mempty (Map.singleton cc 20) (1 %! 2)
        addCCGaid <- submitGovAction addCCAction
        submitYesVote_ (DRepVoter dRep) addCCGaid
        passNEpochs 2
        -- Confirm that they are added
        SJust committee <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . committeeGovStateL
        let assertCCMembership comm =
              assertBool "Expected CC to be present in the committee" $
                Map.member cc (comm ^. committeeMembersL)
            assertCCMissing comm =
              assertBool "Expected CC to be absent in the committee" $
                Map.notMember cc (comm ^. committeeMembersL)
        assertCCMembership committee
        -- Confirm their hot key registration
        _hotKey <- registerCommitteeHotKey cc
        ccShouldNotBeResigned cc
        -- Have them resign
        resignCommitteeColdKey cc
        ccShouldBeResigned cc
        -- Re-add the same CC
        let reAddCCAction = UpdateCommittee (SJust $ GovPurposeId addCCGaid) mempty (Map.singleton cc 20) (1 %! 2)
        reAddCCGaid <- submitGovAction reAddCCAction
        submitYesVote_ (DRepVoter dRep) reAddCCGaid
        passNEpochs 2
        -- Confirm that they are still resigned
        ccShouldBeResigned cc
        -- Remove them
        let removeCCAction = UpdateCommittee (SJust $ GovPurposeId reAddCCGaid) (Set.singleton cc) mempty (1 %! 2)
        removeCCGaid <- submitGovAction removeCCAction
        submitYesVote_ (DRepVoter dRep) removeCCGaid
        passNEpochs 2
        -- Confirm that they have been removed
        SJust committeeAfterRemove <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . committeeGovStateL
        assertCCMissing committeeAfterRemove
        -- Add the same CC back a second time
        let secondAddCCAction = UpdateCommittee (SJust $ GovPurposeId removeCCGaid) mempty (Map.singleton cc 20) (1 %! 2)
        secondAddCCGaid <- submitGovAction secondAddCCAction
        submitYesVote_ (DRepVoter dRep) secondAddCCGaid
        passNEpochs 2
        -- Confirm that they have been added
        SJust committeeAfterRemoveAndAdd <- getsNES $ nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . committeeGovStateL
        assertCCMembership committeeAfterRemoveAndAdd
        -- Confirm that after registering a hot key, they are active
        _hotKey <- registerCommitteeHotKey cc
        ccShouldNotBeResigned cc
      it "proposals to update the committee get delayed if the expiration exceeds the max term" $ do
        setPParams
        (drep, _, _) <- setupSingleDRep 1_000_000
        maxTermLength <-
          getsNES $
            nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . curPParamsGovStateL . ppCommitteeMaxTermLengthL

        (initialMembers, govIdCom1) <-
          impAnn "Initial committee is enacted" $ do
            c1 <- freshKeyHash
            c2 <- freshKeyHash
            currentEpoch <- getsNES nesELL
            let maxExpiry = addEpochInterval currentEpoch maxTermLength
            let initialMembers = [(KeyHashObj c1, maxExpiry), (KeyHashObj c2, maxExpiry)]
            govIdCom1 <-
              electCommittee
                SNothing
                drep
                Set.empty
                initialMembers
            passEpoch >> passEpoch
            expectMembers $ Map.keysSet initialMembers
            pure (Map.keysSet initialMembers, govIdCom1)

        (membersExceedingExpiry, exceedingExpiry) <-
          impAnn "Committee with members exceeding the maxTerm is not enacted" $ do
            -- submit a proposal for adding two members to the committee,
            -- one of which has a max term exceeding the maximum
            c3 <- freshKeyHash
            c4 <- freshKeyHash
            currentEpoch <- getsNES nesELL
            let exceedingExpiry = addEpochInterval (addEpochInterval currentEpoch maxTermLength) (EpochInterval 7)
            let membersExceedingExpiry = [(KeyHashObj c3, exceedingExpiry), (KeyHashObj c4, addEpochInterval currentEpoch maxTermLength)]
            _ <-
              electCommittee
                (SJust govIdCom1)
                drep
                Set.empty
                membersExceedingExpiry
            passEpoch >> passEpoch
            -- the new committee has not been enacted
            expectMembers initialMembers
            pure (Map.keysSet membersExceedingExpiry, exceedingExpiry)

        -- other actions get ratified and enacted
        govIdConst1 <- impAnn "Other actions are ratified and enacted" $ do
          (govIdConst1, constitution) <- submitConstitution SNothing
          submitYesVote_ (DRepVoter (KeyHashObj drep)) govIdConst1
          hks <- traverse registerCommitteeHotKey (Set.toList initialMembers)
          traverse_ (\m -> submitYesVote_ (CommitteeVoter m) govIdConst1) hks

          passEpoch >> passEpoch
          curConstitution <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
          curConstitution `shouldBe` constitution
          pure govIdConst1

        -- after enough epochs pass, the expiration of the new members becomes acceptable
        -- and the new committee is enacted
        impAnn "New committee is enacted" $ do
          currentEpoch <- getsNES nesELL
          let delta =
                fromIntegral (unEpochNo exceedingExpiry)
                  - fromIntegral (unEpochNo (addEpochInterval currentEpoch maxTermLength))
          replicateM_ delta passEpoch

          -- pass one more epoch after ratification, in order to be enacted
          passEpoch
          expectMembers $ initialMembers <> membersExceedingExpiry

        impAnn "New committee can vote" $ do
          (govIdConst2, constitution) <- submitConstitution $ SJust (GovPurposeId govIdConst1)
          submitYesVote_ (DRepVoter (KeyHashObj drep)) govIdConst2
          hks <- traverse registerCommitteeHotKey (Set.toList membersExceedingExpiry)
          traverse_ (\m -> submitYesVote_ (CommitteeVoter m) govIdConst2) hks

          passEpoch >> passEpoch
          curConstitution <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
          curConstitution `shouldBe` constitution
    describe "Network ID" $ do
      it "Fails with invalid network ID in proposal return address" $ do
        rewardCredential <- KeyHashObj <$> freshKeyHash
        let badRewardAccount =
              RewardAccount
                { raNetwork = Mainnet -- Our network is Testnet
                , raCredential = rewardCredential
                }
        propDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionDepositL
        submitFailingProposal
          ProposalProcedure
            { pProcReturnAddr = badRewardAccount
            , pProcGovAction = InfoAction
            , pProcDeposit = propDeposit
            , pProcAnchor = def
            }
          [ injectFailure $
              ProposalProcedureNetworkIdMismatch
                badRewardAccount
                Testnet
          ]
      it "Fails with invalid network ID in withdrawal addresses" $ do
        rewardAccount <- registerRewardAccount
        rewardCredential <- KeyHashObj <$> freshKeyHash
        let badRewardAccount =
              RewardAccount
                { raNetwork = Mainnet -- Our network is Testnet
                , raCredential = rewardCredential
                }
        propDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionDepositL
        submitFailingProposal
          ProposalProcedure
            { pProcReturnAddr = rewardAccount
            , pProcGovAction =
                TreasuryWithdrawals
                  (Map.singleton badRewardAccount $ Coin 100_000_000)
                  SNothing
            , pProcDeposit = propDeposit
            , pProcAnchor = def
            }
          [ injectFailure $
              TreasuryWithdrawalsNetworkIdMismatch
                (Set.singleton badRewardAccount)
                Testnet
          ]
  where
    expectMembers ::
      HasCallStack => Set.Set (Credential 'ColdCommitteeRole (EraCrypto era)) -> ImpTestM era ()
    expectMembers expKhs = do
      committee <-
        getsNES $
          nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . committeeGovStateL
      let members = Map.keysSet $ foldMap' committeeMembers committee
      impAnn "Expecting committee members" $ members `shouldBe` expKhs

    expectNumDormantEpochs :: HasCallStack => EpochNo -> ImpTestM era ()
    expectNumDormantEpochs expected = do
      nd <-
        getsNES $
          nesEsL . esLStateL . lsCertStateL . certVStateL . vsNumDormantEpochsL
      nd `shouldBeExpr` expected

    expectExtraDRepExpiry ::
      HasCallStack => KeyHash 'DRepRole (EraCrypto era) -> EpochNo -> ImpTestM era ()
    expectExtraDRepExpiry kh expected = do
      drepActivity <-
        getsNES $
          nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . curPParamsGovStateL . ppDRepActivityL
      dsMap <-
        getsNES $
          nesEsL . esLStateL . lsCertStateL . certVStateL . vsDRepsL @era
      let ds = Map.lookup (KeyHashObj kh) dsMap
      (^. drepExpiryL)
        <$> ds
          `shouldBe` Just (addEpochInterval expected drepActivity)

    expectCurrentProposals :: HasCallStack => ImpTestM era ()
    expectCurrentProposals = do
      props <- currentProposals
      assertBool "Expected proposals in current gov state" (not (SSeq.null props))

    expectNoCurrentProposals :: HasCallStack => ImpTestM era ()
    expectNoCurrentProposals = do
      props <- currentProposals
      assertBool "Expected no proposals in current gov state" (SSeq.null props)

    expectPulserProposals :: HasCallStack => ImpTestM era ()
    expectPulserProposals = do
      props <- lastEpochProposals
      assertBool "Expected proposals in the pulser" (not (SSeq.null props))

    expectNoPulserProposals :: HasCallStack => ImpTestM era ()
    expectNoPulserProposals = do
      props <- lastEpochProposals
      assertBool "Expected no proposals in the pulser" (SSeq.null props)

    currentProposals =
      proposalsIds
        <$> getsNES (nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . proposalsGovStateL @era)
    lastEpochProposals =
      fmap gasId . psProposals
        <$> getsNES
          ( nesEsL
              . esLStateL
              . lsUTxOStateL
              . utxosGovStateL
              . drepPulsingStateGovStateL @era
              . pulsingStateSnapshotL
          )

submitConstitution ::
  forall era.
  ConwayEraImp era =>
  StrictMaybe (GovPurposeId 'ConstitutionPurpose era) ->
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
          (fromJust $ textToUrl 64 "constitution.0")
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
  govActionDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionDepositL
  pure
    ProposalProcedure
      { pProcDeposit = govActionDeposit
      , pProcReturnAddr = rewardAccount
      , pProcGovAction = action
      , pProcAnchor = def
      }

pulsingStateSnapshotL :: Lens' (DRepPulsingState era) (PulsingSnapshot era)
pulsingStateSnapshotL = lens getter setter
  where
    getter (DRComplete x _) = x
    getter state = fst (finishDRepPulser state)
    setter (DRComplete _ y) snap = DRComplete snap y
    setter state snap = DRComplete snap $ snd $ finishDRepPulser state

setPParams ::
  forall era.
  ( HasCallStack
  , ConwayEraImp era
  ) =>
  ImpTestM era ()
setPParams = do
  modifyPParams $ \pp ->
    pp
      & ppDRepVotingThresholdsL
        .~ def
          { dvtCommitteeNormal = 1 %! 1
          , dvtCommitteeNoConfidence = 1 %! 2
          , dvtUpdateToConstitution = 1 %! 2
          }
      & ppCommitteeMaxTermLengthL .~ EpochInterval 10
      & ppGovActionLifetimeL .~ EpochInterval 100
      & ppGovActionDepositL .~ Coin 123

-- =========================================================
-- Proposing a HardFork should always use a new ProtVer that
-- can follow the one installed in the previous HardFork action.

-- | A legal ProtVer that differs in the minor Version
minorFollow :: ProtVer -> ProtVer
minorFollow (ProtVer x y) = ProtVer x (y + 1)

-- | A legal ProtVer that moves to the next major Version
majorFollow :: ProtVer -> ProtVer
majorFollow pv@(ProtVer x _) = case succVersion x of
  Just x' -> ProtVer x' 0
  Nothing -> error ("The last major version can't be incremented. " ++ show pv)

-- | An illegal ProtVer that skips 3 minor versions
cantFollow :: ProtVer -> ProtVer
cantFollow (ProtVer x y) = ProtVer x (y + 3)

-- | Tests the first hardfork in the Conway era where the PrevGovActionID is SNothing
firstHardForkFollows ::
  forall era.
  (ShelleyEraImp era, ConwayEraTxBody era) =>
  (ProtVer -> ProtVer) ->
  ImpTestM era ()
firstHardForkFollows computeNewFromOld = do
  protVer <- getsNES $ nesEsL . curPParamsEpochStateL . ppProtocolVersionL
  submitGovAction_ $ HardForkInitiation SNothing (computeNewFromOld protVer)

-- | Negative (deliberatey failing) first hardfork in the Conway era where the PrevGovActionID is SNothing
firstHardForkCantFollow ::
  forall era.
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  ImpTestM era ()
firstHardForkCantFollow = do
  rewardAccount <- registerRewardAccount
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  let protver0 = pp ^. ppProtocolVersionL
      protver1 = minorFollow protver0
      protver2 = cantFollow protver1
  submitFailingProposal
    ( ProposalProcedure
        { pProcDeposit = pp ^. ppGovActionDepositL
        , pProcReturnAddr = rewardAccount
        , pProcGovAction = HardForkInitiation SNothing protver2
        , pProcAnchor = def
        }
    )
    [injectFailure $ ProposalCantFollow SNothing protver2 protver0]

-- | Tests a second hardfork in the Conway era where the PrevGovActionID is SJust
secondHardForkFollows ::
  forall era.
  (ShelleyEraImp era, ConwayEraTxBody era) =>
  (ProtVer -> ProtVer) ->
  ImpTestM era ()
secondHardForkFollows computeNewFromOld = do
  protver0 <- getsNES $ nesEsL . curPParamsEpochStateL . ppProtocolVersionL
  let protver1 = minorFollow protver0
      protver2 = computeNewFromOld protver1
  gaid1 <- submitGovAction $ HardForkInitiation SNothing protver1
  submitGovAction_ $ HardForkInitiation (SJust (GovPurposeId gaid1)) protver2

-- | Negative (deliberatey failing) first hardfork in the Conway era where the PrevGovActionID is SJust
secondHardForkCantFollow ::
  forall era.
  ( ShelleyEraImp era
  , ConwayEraTxBody era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  ImpTestM era ()
secondHardForkCantFollow = do
  rewardAccount <- registerRewardAccount
  pp <- getsNES $ nesEsL . curPParamsEpochStateL
  let protver0 = pp ^. ppProtocolVersionL
      protver1 = minorFollow protver0
      protver2 = cantFollow protver1
  gaid1 <-
    submitProposal $
      ProposalProcedure
        { pProcDeposit = pp ^. ppGovActionDepositL
        , pProcReturnAddr = rewardAccount
        , pProcGovAction = HardForkInitiation SNothing protver1
        , pProcAnchor = def
        }
  submitFailingProposal
    ( ProposalProcedure
        { pProcDeposit = pp ^. ppGovActionDepositL
        , pProcReturnAddr = rewardAccount
        , pProcGovAction = HardForkInitiation (SJust (GovPurposeId gaid1)) protver2
        , pProcAnchor = def
        }
    )
    [injectFailure $ ProposalCantFollow (SJust (GovPurposeId gaid1)) protver2 protver1]

committeeMemberVotingOnCommitteeChange ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  ImpTestM era ()
committeeMemberVotingOnCommitteeChange = do
  (_, ccHot, prevGovActionId) <- electBasicCommittee
  khCommittee <- KeyHashObj <$> freshKeyHash
  committeeUpdateId <-
    submitGovAction $
      UpdateCommittee
        (SJust prevGovActionId)
        mempty
        (Map.singleton khCommittee $ EpochNo 28)
        (3 %! 5)
  let voter = CommitteeVoter ccHot
  submitFailingVote
    voter
    committeeUpdateId
    [ injectFailure $ DisallowedVoters [(voter, committeeUpdateId)]
    ]

ccVoteOnConstitutionFailsWithMultipleVotes ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  ImpTestM era ()
ccVoteOnConstitutionFailsWithMultipleVotes = do
  (drepCred, ccCred, prevCommitteeId) <- electBasicCommittee
  drepCred2 <- KeyHashObj <$> registerDRep
  newCommitteeMember <- KeyHashObj <$> freshKeyHash
  committeeProposal <-
    submitGovAction $
      UpdateCommittee
        (SJust prevCommitteeId)
        mempty
        (Map.singleton newCommitteeMember $ EpochNo 10)
        (1 %! 2)
  let
    voteTx =
      mkBasicTx $
        mkBasicTxBody
          & votingProceduresTxBodyL
            .~ VotingProcedures
              ( Map.fromList
                  [
                    ( DRepVoter drepCred2
                    , Map.singleton committeeProposal $ VotingProcedure VoteYes SNothing
                    )
                  ,
                    ( CommitteeVoter ccCred
                    , Map.singleton committeeProposal $ VotingProcedure VoteNo SNothing
                    )
                  ,
                    ( DRepVoter drepCred
                    , Map.singleton committeeProposal $ VotingProcedure VoteYes SNothing
                    )
                  ]
              )
  impAnn "Try to vote as a committee member" $
    submitFailingTx
      voteTx
      [ injectFailure $
          DisallowedVoters [(CommitteeVoter ccCred, committeeProposal)]
      ]
