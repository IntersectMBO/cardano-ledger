{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conway.Imp.GovSpec (spec) where

import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.Allegra.Scripts (
  pattern RequireAllOf,
  pattern RequireAnyOf,
  pattern RequireMOf,
  pattern RequireSignature,
 )
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules (ConwayGovPredFailure (..))
import Cardano.Ledger.Credential (Credential (KeyHashObj))
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Val (zero, (<->))
import Data.Default.Class (Default (..))
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Data.Tree
import Lens.Micro
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.ImpTest
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
    hardForkSpec
    pparamUpdateSpec
    proposalsSpec
    votingSpec
    constitutionSpec
    policySpec
    networkIdSpec
    predicateFailuresSpec

predicateFailuresSpec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  SpecWith (ImpTestState era)
predicateFailuresSpec =
  describe "Predicate failures" $ do
    it "ExpirationEpochTooSmall" $ do
      pp <- getsNES $ nesEsL . curPParamsEpochStateL
      committeeC <- KeyHashObj <$> freshKeyHash
      rewardAccount <- registerRewardAccount
      let expiration = EpochNo 1
          action =
            UpdateCommittee
              SNothing
              mempty
              (Map.singleton committeeC expiration)
              (0 %! 1)
      passEpoch
      submitFailingProposal
        ( ProposalProcedure
            { pProcReturnAddr = rewardAccount
            , pProcGovAction = action
            , pProcDeposit = pp ^. ppGovActionDepositL
            , pProcAnchor = def
            }
        )
        [injectFailure $ ExpirationEpochTooSmall $ Map.singleton committeeC expiration]
    it "ProposalDepositIncorrect" $ do
      committeeC <- KeyHashObj <$> freshKeyHash
      rewardAccount <- registerRewardAccount
      let expiration = EpochNo 1
          actionDeposit = Coin 2
          action =
            UpdateCommittee
              SNothing
              mempty
              (Map.singleton committeeC expiration)
              (0 %! 1)
      modifyPParams $ ppGovActionDepositL .~ actionDeposit
      submitFailingProposal
        ( ProposalProcedure
            { pProcReturnAddr = rewardAccount
            , pProcGovAction = action
            , pProcDeposit = actionDeposit <-> Coin 1
            , pProcAnchor = def
            }
        )
        [injectFailure $ ProposalDepositIncorrect (actionDeposit <-> Coin 1) actionDeposit]
    it "ConflictingCommitteeUpdate" $ do
      pp <- getsNES $ nesEsL . curPParamsEpochStateL
      committeeC <- KeyHashObj <$> freshKeyHash
      rewardAccount <- registerRewardAccount
      let expiration = EpochNo 1
          action =
            UpdateCommittee
              SNothing
              (Set.singleton committeeC)
              (Map.singleton committeeC expiration)
              (0 %! 1)
      submitFailingProposal
        ( ProposalProcedure
            { pProcReturnAddr = rewardAccount
            , pProcGovAction = action
            , pProcDeposit = pp ^. ppGovActionDepositL
            , pProcAnchor = def
            }
        )
        [injectFailure $ ConflictingCommitteeUpdate $ Set.singleton committeeC]

hardForkSpec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  SpecWith (ImpTestState era)
hardForkSpec =
  describe "HardFork" $ do
    describe "Hardfork is the first one (doesn't have a GovPurposeId) " $ do
      it "Hardfork minorFollow" (firstHardForkFollows minorFollow)
      it "Hardfork majorFollow" (firstHardForkFollows majorFollow)
      it "Hardfork cantFollow" firstHardForkCantFollow
    describe "Hardfork is the second one (has a GovPurposeId)" $ do
      it "Hardfork minorFollow" (secondHardForkFollows minorFollow)
      it "Hardfork majorFollow" (secondHardForkFollows majorFollow)
      it "Hardfork cantFollow" secondHardForkCantFollow

pparamUpdateSpec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  SpecWith (ImpTestState era)
pparamUpdateSpec =
  describe "PParamUpdate" $ do
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

proposalsSpec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  SpecWith (ImpTestState era)
proposalsSpec =
  describe "Proposals" $ do
    describe "Consistency" $ do
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

votingSpec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  SpecWith (ImpTestState era)
votingSpec =
  describe "Voting" $ do
    describe "fails for" $ do
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

constitutionSpec ::
  forall era.
  ( ConwayEraImp era
  , GovState era ~ ConwayGovState era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  SpecWith (ImpTestState era)
constitutionSpec =
  describe "Constitution proposals" $ do
    describe "accepted for" $ do
      it "empty PrevGovId before the first constitution is enacted" $ do
        --  Initial proposal does not need a GovPurposeId but after it is enacted, the
        --  following ones are not
        _ <- submitConstitution SNothing
        -- Until the first proposal is enacted all proposals with empty GovPurposeIds are valid
        void $ submitConstitution SNothing
      it "valid GovPurposeId" $ do
        (dRep, committeeMember, _) <- electBasicCommittee
        constitution <- arbitrary
        gaidConstitutionProp <- enactConstitution SNothing constitution dRep committeeMember
        constitution1 <- arbitrary
        void $
          enactConstitution (SJust $ GovPurposeId gaidConstitutionProp) constitution1 dRep committeeMember

    describe "rejected for" $ do
      it "empty PrevGovId after the first constitution was enacted" $ do
        (dRep, committeeMember, _) <- electBasicCommittee
        (govActionId, _constitution) <- submitConstitution SNothing
        submitYesVote_ (DRepVoter dRep) govActionId
        submitYesVote_ (CommitteeVoter committeeMember) govActionId
        passNEpochs 2
        constitution <- arbitrary
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
        constitution <- arbitrary
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

policySpec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  SpecWith (ImpTestState era)
policySpec =
  describe "Policy" $ do
    it "policy is respected by proposals" $ do
      (dRep, committeeMember, _) <- electBasicCommittee
      keyHash <- freshKeyHash
      scriptHash <- impAddNativeScript $ RequireAllOf (SSeq.singleton (RequireSignature keyHash))
      anchor <- arbitrary
      _ <- enactConstitution SNothing (Constitution anchor (SJust scriptHash)) dRep committeeMember
      wrongScriptHash <-
        impAddNativeScript $
          RequireMOf 1 $
            SSeq.fromList [RequireAnyOf mempty, RequireAllOf mempty]
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

      impAnn "ParameterChange with invalid policy fails" $ do
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

      impAnn "TreasuryWithdrawals with invalid policy fails" $ do
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

networkIdSpec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  SpecWith (ImpTestState era)
networkIdSpec =
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

-- =========================================================
-- Proposing a HardFork should always use a new ProtVer that
-- can follow the one installed in the previous HardFork action.

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
