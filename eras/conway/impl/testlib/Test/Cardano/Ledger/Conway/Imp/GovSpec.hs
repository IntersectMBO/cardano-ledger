{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conway.Imp.GovSpec where

import Cardano.Ledger.Address (RewardAcnt (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.CertState (vsNumDormantEpochsL)
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules (
  ConwayGovPredFailure (..),
  ConwayLedgerPredFailure (ConwayGovFailure),
 )
import Cardano.Ledger.Conway.TxBody (ConwayEraTxBody)
import Cardano.Ledger.Credential (Credential (KeyHashObj))
import Cardano.Ledger.DRep (drepExpiryL)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Shelley.LedgerState
import Control.Monad (replicateM_)
import Control.State.Transition.Extended (PredicateFailure)
import Data.Default.Class (Default (..))
import Data.Foldable (Foldable (..), traverse_)
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ( ConwayEraImp era
  , GovState era ~ ConwayGovState era
  , Inject (ConwayGovPredFailure era) (PredicateFailure (EraRule "LEDGER" era))
  , era ~ Conway
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
    let _submitInvalidChildConstitutionGovAction govActionId = do
          constitutionHash <- freshSafeHash
          pp <- getsNES $ nesEsL . curPParamsEpochStateL
          khPropRwd <- freshKeyHash
          let constitutionAction =
                NewConstitution
                  (SJust $ GovPurposeId $ mkCorruptGovActionId govActionId)
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
                  , pProcReturnAddr = RewardAcnt Testnet (KeyHashObj khPropRwd)
                  , pProcGovAction = constitutionAction
                  , pProcAnchor = def
                  }
          -- submitGovAction_ constitutionAction
          submitFailingProposal
            constitutionProposal
            [ ConwayGovFailure $ InvalidPrevGovActionId constitutionProposal
            ]
    context "Proposal-forests" $ do
      context "GOV, EPOCH, RATIFY and ENACT" $ do
        it "Proposals survive multiple epochs without any activity" $ do
          modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 4 -- + 2 epochs to pass to get the desired effect
          _tree <-
            submitConstitutionGovActionTree SNothing $
              E
                [ E
                    [ E []
                    , E []
                    ]
                , E []
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
          N
            p11
            [ N p111 []
              , N p112 []
              ] <-
            submitConstitutionGovActionTree
              (SJust p1)
              $ E
                [ E []
                , E []
                ]
          N p2 [N p21 []] <-
            submitConstitutionGovActionTree
              SNothing
              $ E
                [ E []
                ]
          [ I []
            , I []
            , I []
            , I
                [ J
                    p1'
                    [ J
                        p11'
                        [ J p111' []
                          , J p112' []
                          ]
                      ]
                  , J p2' [J p21' []]
                  ]
            ] <-
            getProposalsForest
          SJust p1 `shouldBe` p1'
          SJust p2 `shouldBe` p2'
          SJust p11 `shouldBe` p11'
          SJust p21 `shouldBe` p21'
          SJust p111 `shouldBe` p111'
          SJust p112 `shouldBe` p112'
          passNEpochs 3
          [ I []
            , I []
            , I []
            , I
                [ J p2'' [J p21'' []]
                  ]
            ] <-
            getProposalsForest
          SJust p2 `shouldBe` p2''
          SJust p21 `shouldBe` p21''
        it "Subtrees are pruned when competing proposals are enacted" $ do
          (dRep, committeeMember) <- electBasicCommittee
          [ N
              p1
              [ N
                  p11
                  [ N p111 []
                    , N p112 []
                    ]
                ]
            , N p2 [N p21 []]
            ] <-
            submitConstitutionGovActionForest
              SNothing
              [ E
                  [ E
                      [ E []
                      , E []
                      ]
                  ]
              , E
                  [ E []
                  ]
              ]
          [ I []
            , I []
            , J _ [] -- Due to the committee enactment
            , I
                [ J
                    p1'
                    [ J
                        p11'
                        [ J p111' []
                          , J p112' []
                          ]
                      ]
                  , J p2' [J p21' []]
                  ]
            ] <-
            getProposalsForest
          SJust p1 `shouldBe` p1'
          SJust p2 `shouldBe` p2'
          SJust p11 `shouldBe` p11'
          SJust p21 `shouldBe` p21'
          SJust p111 `shouldBe` p111'
          SJust p112 `shouldBe` p112'
          passEpoch
          submitYesVote_ (DRepVoter dRep) p2
          submitYesVote_ (CommitteeVoter committeeMember) p2
          passNEpochs 2
          [ I []
            , I []
            , J _ []
            , J p2'' [J p21'' []]
            ] <-
            getProposalsForest
          SJust p2 `shouldBe` p2''
          SJust p21 `shouldBe` p21''
        it "Subtrees are pruned when proposals expire over multiple rounds" $ do
          modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 4
          p1 <- submitInitConstitutionGovAction
          [ I []
            , I []
            , I []
            , I
                [ J p1' []
                  ]
            ] <-
            getProposalsForest
          SJust p1 `shouldBe` p1'
          passEpoch
          p2 <- submitInitConstitutionGovAction
          p11 <- submitChildConstitutionGovAction p1
          [ I []
            , I []
            , I []
            , I
                [ J p1'' [J p11' []]
                  , J p2' []
                  ]
            ] <-
            getProposalsForest
          SJust p1 `shouldBe` p1''
          SJust p11 `shouldBe` p11'
          SJust p2 `shouldBe` p2'
          passEpoch
          p3 <- submitInitConstitutionGovAction
          p21 <- submitChildConstitutionGovAction p2
          p111 <- submitChildConstitutionGovAction p11
          p112 <- submitChildConstitutionGovAction p11
          [ I []
            , I []
            , I []
            , I
                [ J
                    p1'''
                    [ J
                        p11''
                        [ J p111' []
                          , J p112' []
                          ]
                      ]
                  , J p2'' [J p21' []]
                  , J p3' []
                  ]
            ] <-
            getProposalsForest
          SJust p1 `shouldBe` p1'''
          SJust p11 `shouldBe` p11''
          SJust p111 `shouldBe` p111'
          SJust p112 `shouldBe` p112'
          SJust p2 `shouldBe` p2''
          SJust p21 `shouldBe` p21'
          SJust p3 `shouldBe` p3'
          passEpoch
          p4 <- submitInitConstitutionGovAction
          p31 <- submitChildConstitutionGovAction p3
          p211 <- submitChildConstitutionGovAction p21
          [ I []
            , I []
            , I []
            , I
                [ J
                    p1''''
                    [ J
                        p11'''
                        [ J p111'' []
                          , J p112'' []
                          ]
                      ]
                  , J p2''' [J p21'' [J p211' []]]
                  , J p3'' [J p31' []]
                  , J p4' []
                  ]
            ] <-
            getProposalsForest
          SJust p1 `shouldBe` p1''''
          SJust p11 `shouldBe` p11'''
          SJust p111 `shouldBe` p111''
          SJust p112 `shouldBe` p112''
          SJust p2 `shouldBe` p2'''
          SJust p21 `shouldBe` p21''
          SJust p211 `shouldBe` p211'
          SJust p3 `shouldBe` p3''
          SJust p31 `shouldBe` p31'
          SJust p4 `shouldBe` p4'
          passNEpochs 3
          [ I []
            , I []
            , I []
            , I
                [ J p2'''' [J p21''' [J p211'' []]]
                  , J p3''' [J p31'' []]
                  , J p4'' []
                  ]
            ] <-
            getProposalsForest
          SJust p2 `shouldBe` p2''''
          SJust p21 `shouldBe` p21'''
          SJust p211 `shouldBe` p211''
          SJust p3 `shouldBe` p3'''
          SJust p31 `shouldBe` p31''
          SJust p4 `shouldBe` p4''
          p5 <- submitInitConstitutionGovAction
          p41 <- submitChildConstitutionGovAction p4
          p311 <- submitChildConstitutionGovAction p31
          p212 <- submitChildConstitutionGovAction p21
          [ I []
            , I []
            , I []
            , I
                [ J
                    p2'''''
                    [ J
                        p21''''
                        [ J p211''' []
                          , J p212' []
                          ]
                      ]
                  , J p3'''' [J p31''' [J p311' []]]
                  , J p4''' [J p41' []]
                  , J p5' []
                  ]
            ] <-
            getProposalsForest
          SJust p2 `shouldBe` p2'''''
          SJust p21 `shouldBe` p21''''
          SJust p211 `shouldBe` p211'''
          SJust p212 `shouldBe` p212'
          SJust p3 `shouldBe` p3''''
          SJust p31 `shouldBe` p31'''
          SJust p311 `shouldBe` p311'
          SJust p4 `shouldBe` p4'''
          SJust p41 `shouldBe` p41'
          SJust p5 `shouldBe` p5'
          passEpoch
          p6 <- submitInitConstitutionGovAction
          p51 <- submitChildConstitutionGovAction p5
          p411 <- submitChildConstitutionGovAction p41
          p312 <- submitChildConstitutionGovAction p31
          [ I []
            , I []
            , I []
            , I
                [ J
                    p3'''''
                    [ J
                        p31''''
                        [ J p311'' []
                          , J p312' []
                          ]
                      ]
                  , J p4'''' [J p41'' [J p411' []]]
                  , J p5'' [J p51' []]
                  , J p6' []
                  ]
            ] <-
            getProposalsForest
          SJust p3 `shouldBe` p3'''''
          SJust p31 `shouldBe` p31''''
          SJust p311 `shouldBe` p311''
          SJust p312 `shouldBe` p312'
          SJust p4 `shouldBe` p4''''
          SJust p41 `shouldBe` p41''
          SJust p411 `shouldBe` p411'
          SJust p5 `shouldBe` p5''
          SJust p51 `shouldBe` p51'
          SJust p6 `shouldBe` p6'
          passEpoch
          [ I []
            , I []
            , I []
            , I
                [ J p4''''' [J p41''' [J p411'' []]]
                  , J p5''' [J p51'' []]
                  , J p6'' []
                  ]
            ] <-
            getProposalsForest
          SJust p4 `shouldBe` p4'''''
          SJust p41 `shouldBe` p41'''
          SJust p411 `shouldBe` p411''
          SJust p5 `shouldBe` p5'''
          SJust p51 `shouldBe` p51''
          SJust p6 `shouldBe` p6''
          passEpoch
          [ I []
            , I []
            , I []
            , I
                [ J p5'''' [J p51''' []]
                  , J p6''' []
                  ]
            ] <-
            getProposalsForest
          SJust p5 `shouldBe` p5''''
          SJust p51 `shouldBe` p51'''
          SJust p6 `shouldBe` p6'''
          passNEpochs 3
          [ I []
            , I []
            , I []
            , I
                [ J p6'''' []
                  ]
            ] <-
            getProposalsForest
          SJust p6 `shouldBe` p6''''
          passEpoch
          [ I []
            , I []
            , I []
            , I []
            ] <-
            getProposalsForest
          props <- getProposals
          props ^. pRootsL . pfrConstitutionL . prChildrenL `shouldBe` Set.empty
          proposalsSize props `shouldBe` 0
          props `shouldBe` def

        it "Subtrees are pruned when competing proposals are enacted over multiple rounds" $ do
          -- TODO: @aniketd Write a test that combines expiry and enactment pruning together
          (dRep, committeeMember) <- electBasicCommittee
          [ N
              p1
              [ N
                  p11
                  [ N p111 []
                    , N p112 []
                    ]
                ]
            , N
                p2
                [ N p21 []
                  , N p22 []
                  ]
            , N p3 []
            ] <-
            submitConstitutionGovActionForest
              SNothing
              [ E
                  [ E
                      [ E []
                      , E []
                      ]
                  ]
              , E
                  [ E []
                  , E []
                  ]
              , E []
              ]
          submitYesVote_ (DRepVoter dRep) p2
          submitYesVote_ (CommitteeVoter committeeMember) p2
          submitYesVote_ (DRepVoter dRep) p21
          submitYesVote_ (CommitteeVoter committeeMember) p21
          submitYesVote_ (DRepVoter dRep) p3
          submitYesVote_ (CommitteeVoter committeeMember) p3 -- Two competing proposals break the tie based on proposal order
          [ I []
            , I []
            , J _ []
            , I
                [ J
                    p1'
                    [ J
                        p11'
                        [ J p111' []
                          , J p112' []
                          ]
                      ]
                  , J
                      p2'
                      [ J p21' []
                        , J p22' []
                        ]
                  , J p3' []
                  ]
            ] <-
            getProposalsForest
          SJust p1 `shouldBe` p1'
          SJust p11 `shouldBe` p11'
          SJust p111 `shouldBe` p111'
          SJust p112 `shouldBe` p112'
          SJust p2 `shouldBe` p2'
          SJust p21 `shouldBe` p21'
          SJust p22 `shouldBe` p22'
          SJust p3 `shouldBe` p3'
          passEpoch
          p4 <- submitInitConstitutionGovAction
          p31 <- submitChildConstitutionGovAction p3
          p211 <- submitChildConstitutionGovAction p21
          [ I []
            , I []
            , J _ []
            , I
                [ J
                    p1''
                    [ J
                        p11''
                        [ J p111'' []
                          , J p112'' []
                          ]
                      ]
                  , J
                      p2''
                      [ J p21'' [J p211' []]
                        , J p22'' []
                        ]
                  , J p3'' [J p31' []]
                  , J p4' []
                  ]
            ] <-
            getProposalsForest
          SJust p1 `shouldBe` p1''
          SJust p11 `shouldBe` p11''
          SJust p111 `shouldBe` p111''
          SJust p112 `shouldBe` p112''
          SJust p2 `shouldBe` p2''
          SJust p21 `shouldBe` p21''
          SJust p211 `shouldBe` p211'
          SJust p22 `shouldBe` p22''
          SJust p3 `shouldBe` p3''
          SJust p31 `shouldBe` p31'
          SJust p4 `shouldBe` p4'
          passEpoch
          [ I []
            , I []
            , J _ []
            , J
                p2'''
                [ J p21''' [J p211'' []]
                  , J p22''' []
                  ]
            ] <-
            getProposalsForest
          SJust p2 `shouldBe` p2'''
          SJust p21 `shouldBe` p21'''
          SJust p211 `shouldBe` p211''
          SJust p22 `shouldBe` p22'''
          [ N p212 []
            , N p213 []
            , N p214 []
            ] <-
            submitConstitutionGovActionForest
              (SJust p21)
              [ E []
              , E []
              , E []
              ]
          p2131 <- submitChildConstitutionGovAction p213
          p2141 <- submitChildConstitutionGovAction p214
          submitYesVote_ (DRepVoter dRep) p212
          submitYesVote_ (CommitteeVoter committeeMember) p212
          [ I []
            , I []
            , J _ []
            , J
                p2''''
                [ J
                    p21''''
                    [ J p211''' []
                      , J p212' []
                      , J p213' [J p2131' []]
                      , J p214' [J p2141' []]
                      ]
                  , J p22'''' []
                  ]
            ] <-
            getProposalsForest
          SJust p2 `shouldBe` p2''''
          SJust p21 `shouldBe` p21''''
          SJust p211 `shouldBe` p211'''
          SJust p212 `shouldBe` p212'
          SJust p213 `shouldBe` p213'
          SJust p2131 `shouldBe` p2131'
          SJust p214 `shouldBe` p214'
          SJust p2141 `shouldBe` p2141'
          SJust p22 `shouldBe` p22''''
          passNEpochs 2
          [ I []
            , I []
            , J _ []
            , J p212'' []
            ] <-
            getProposalsForest
          SJust p212 `shouldBe` p212''
          props <- getProposals
          proposalsSize props `shouldBe` 0
        it "Subtrees are pruned for both enactment and expiry over multiple rounds RRRR" $ do
          modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 4
          (dRep, committeeMember) <- electBasicCommittee
          [ N
              p1
              [ N
                  p11
                  [ N p111 []
                    , N p112 []
                    ]
                ]
            , N
                _p2
                [ N _p21 []
                  , N _p22 []
                  ]
            , N p3 []
            ] <-
            submitConstitutionGovActionForest
              SNothing
              [ E
                  [ E
                      [ E []
                      , E []
                      ]
                  ]
              , E
                  [ E []
                  , E []
                  ]
              , E []
              ]
          submitYesVote_ (DRepVoter dRep) p1
          submitYesVote_ (CommitteeVoter committeeMember) p1
          submitYesVote_ (DRepVoter dRep) p11
          submitYesVote_ (CommitteeVoter committeeMember) p11
          submitYesVote_ (DRepVoter dRep) p3
          submitYesVote_ (CommitteeVoter committeeMember) p3 -- Two competing proposals break the tie based on proposal order
          passNEpochs 2
          [ I []
            , I []
            , J _ []
            , J
                p1''
                [ J
                    p11''
                    [ J p111'' []
                      , J p112'' []
                      ]
                  ]
            ] <-
            getProposalsForest
          SJust p1 `shouldBe` p1''
          SJust p11 `shouldBe` p11''
          SJust p111 `shouldBe` p111''
          SJust p112 `shouldBe` p112''
          passEpoch -- ConstitutionPurpose is a delayed action
          [ I []
            , I []
            , J _ []
            , J
                p11'''
                [ J p111''' []
                  , J p112''' []
                  ]
            ] <-
            getProposalsForest
          SJust p11 `shouldBe` p11'''
          SJust p111 `shouldBe` p111'''
          SJust p112 `shouldBe` p112'''
          passNEpochs 3
          [ I []
            , I []
            , J _ []
            , J p11'''' []
            ] <-
            getProposalsForest
          SJust p11 `shouldBe` p11''''
          props <- getProposals
          proposalsSize props `shouldBe` 0 -- TODO: @aniketd Add more rounds
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
          (dRep, committeeMember) <- electBasicCommittee
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
    describe "Votes fail as expected" $ do
      it "on expired gov-actions" $ do
        modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2 -- Voting after the 3rd epoch should fail
        khDRep <- setupSingleDRep
        gaidConstitutionProp <- submitInitConstitutionGovAction
        passEpoch
        passEpoch
        passEpoch
        let voter = DRepVoter $ KeyHashObj khDRep
        submitFailingVote
          voter
          gaidConstitutionProp
          [ ConwayGovFailure $ VotingOnExpiredGovAction $ Map.singleton gaidConstitutionProp voter
          ]
      it "on non-existant gov-actions" $ do
        khDRep <- setupSingleDRep
        gaidConstitutionProp <- submitInitConstitutionGovAction
        let voter = DRepVoter $ KeyHashObj khDRep
            dummyGaid = gaidConstitutionProp {gaidGovActionIx = GovActionIx 99} -- non-existant `GovActionId`
        submitFailingVote
          voter
          dummyGaid
          [ConwayGovFailure $ GovActionsDoNotExist $ Set.singleton dummyGaid]

    describe "Voting" $ do
      context "fails for" $ do
        it "expired gov-actions" $ do
          -- Voting after the 3rd epoch should fail
          modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
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
          --  Initial proposal does not need a GovPurposeId but after it is enacted, the
          --  following ones are not
          _ <- submitConstitution SNothing
          -- Until the first proposal is enacted all proposals with empty GovPurposeIds are valid
          void $ submitConstitution SNothing

      context "rejected for" $ do
        -- it "empty PrevGovId after the first constitution was enacted" $ do
        --   (govActionId, _) <- submitConstitution SNothing
        --   modifyNES $ \nes ->
        --     nes
        --       & nesEsL
        --       . esLStateL
        --       . lsUTxOStateL
        --       . utxosGovStateL
        --       . cgEnactStateL
        --       . ensPrevConstitutionL
        --       .~ SJust (GovPurposeId govActionId) -- Add first proposal to GovPurposeIds in enacted state
        --       & nesEsL
        --       . esLStateL
        --       . lsUTxOStateL
        --       . utxosGovStateL
        --       . cgProposalsL
        --       .~ def -- Remove all proposals, so that the lookup only succeeds for enacted state.
        --       -- Once a proposal with a purpose has been enacted, following proposals can no
        --       -- longer have empty GovPurposeIds
        --   constitution <- newConstitution
        --   let invalidNewConstitutionGovAction =
        --         NewConstitution
        --           SNothing
        --           constitution
        --   invalidNewConstitutionProposal <- proposalWithRewardAccount invalidNewConstitutionGovAction
        --   submitFailingProposal
        --     invalidNewConstitutionProposal
        --     [ inject $ InvalidPrevGovActionId invalidNewConstitutionProposal
        --     ]
        it "invalid index in GovPurposeId" $ do
          (govActionId, _) <- submitConstitution SNothing
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
            [ inject $ InvalidPrevGovActionId invalidNewConstitutionProposal
            ]
        it "valid GovPurposeId but invalid purpose" $ do
          (govActionId, _) <- submitConstitution SNothing
          let invalidNoConfidenceAction =
                NoConfidence $ SJust $ GovPurposeId govActionId
          invalidNoConfidenceProposal <- proposalWithRewardAccount invalidNoConfidenceAction

          submitFailingProposal
            invalidNoConfidenceProposal
            [ inject $ InvalidPrevGovActionId invalidNoConfidenceProposal
            ]

    describe "Constitution" $ do
      it "submitted successfully with valid GovPurposeId" $ do
        modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 1

        curConstitution <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
        initialPulser <- getsNES $ newEpochStateGovStateL . cgDRepPulsingStateL

        (govActionId, _) <- submitConstitution SNothing
        curConstitution' <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
        impAnn "Constitution has not been enacted yet" $
          curConstitution' `shouldBe` curConstitution

        ConwayGovState expectedProposals _expectedEnactState expectedPulser <- getsNES newEpochStateGovStateL

        impAnn "Proposals contain the submitted proposal" $
          expectedProposals `shouldSatisfy` \props -> govActionId `elem` proposalsIds props

        impAnn "Pulser has not changed" $
          expectedPulser `shouldBe` initialPulser

        passEpoch >> passEpoch
        impAnn "Proposal gets removed after expiry" $ do
          ConwayGovState _ _ pulser <- getsNES newEpochStateGovStateL
          let ratifyState = extractDRepPulsingState pulser
          rsExpired ratifyState `shouldBe` Set.singleton govActionId

      it "submitted and enacted when voted on" $ do
        (dRep, committeeMember) <- electBasicCommittee
        (govActionId, constitution) <- submitConstitution SNothing

        ConwayGovState proposalsBeforeVotes enactStateBeforeVotes pulserBeforeVotes <- getsNES newEpochStateGovStateL
        submitYesVote_ (DRepVoter dRep) govActionId
        submitYesVote_ (CommitteeVoter committeeMember) govActionId
        ConwayGovState proposalsAfterVotes enactStateAfterVotes pulserAfterVotes <- getsNES newEpochStateGovStateL

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
          ConwayGovState _ _ pulser <- getsNES newEpochStateGovStateL
          let ratifyState = extractDRepPulsingState pulser
          rsEnacted ratifyState `shouldBe` govActionId Seq.:<| Seq.Empty
          rsEnactState ratifyState ^. ensConstitutionL `shouldBe` constitution

        passEpoch

        impAnn "Constitution is enacted after two epochs" $ do
          curConstitution <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
          curConstitution `shouldBe` constitution

        impAnn "Pulser is reset" $ do
          ConwayGovState _ _ pulser <- getsNES newEpochStateGovStateL
          let pulserRatifyState = extractDRepPulsingState pulser
          rsEnacted pulserRatifyState `shouldBe` Seq.Empty
          enactState <- getsNES $ newEpochStateGovStateL . cgEnactStateL
          rsEnactState pulserRatifyState `shouldBe` enactState

    describe "DRep expiry" $ do
      it "is updated based on to number of dormant epochs" $ do
        modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
        drep <- setupSingleDRep

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

    describe "Committee actions validation" $ do
      it "proposals to update the committee get delayed if the expiration exceeds the max term" $ do
        setPParams
        drep <- setupSingleDRep
        maxTermLength <-
          getsNES $
            nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . curPParamsGovStateL . ppCommitteeMaxTermLengthL

        (initialMembers, govIdCom1) <-
          impAnn "Initial committee is enacted" $ do
            c1 <- freshKeyHash
            c2 <- freshKeyHash
            currentEpoch <- getsNES nesELL
            let maxExpiry = addEpochInterval currentEpoch maxTermLength
            let initialMembers = [(c1, maxExpiry), (c2, maxExpiry)]
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
            let membersExceedingExpiry = [(c3, exceedingExpiry), (c4, addEpochInterval currentEpoch maxTermLength)]
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
          traverse_ (\m -> submitYesVote_ (CommitteeVoter (KeyHashObj m)) govIdConst1) hks

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
          traverse_ (\m -> submitYesVote_ (CommitteeVoter (KeyHashObj m)) govIdConst2) hks

          passEpoch >> passEpoch
          curConstitution <- getsNES $ newEpochStateGovStateL . constitutionGovStateL
          curConstitution `shouldBe` constitution
  where
    expectMembers ::
      HasCallStack => Set.Set (KeyHash 'ColdCommitteeRole (EraCrypto era)) -> ImpTestM era ()
    expectMembers expKhs = do
      committee <-
        getsNES $
          nesEsL . esLStateL . lsUTxOStateL . utxosGovStateL . cgEnactStateL . ensCommitteeL
      let members = Map.keysSet $ foldMap' committeeMembers committee
      impAnn "Expecting committee members" $ members `shouldBe` Set.map KeyHashObj expKhs

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
      fmap gasId
        <$> getsNES
          ( nesEsL
              . esLStateL
              . lsUTxOStateL
              . utxosGovStateL
              . drepPulsingStateGovStateL @era
              . pulsingStateSnapshotL
              . psProposalsL
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
  pure
    ProposalProcedure
      { pProcDeposit = mempty
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

-- -- | Accesses the same data as psProposalsL, but converts from (StrictSeq (GovActionState era)) to (Proposals era)
-- psProposalsL' :: Lens' (PulsingSnapshot era) (Proposals era)
-- psProposalsL' =
--   lens
--     (fromGovActionStateSeq . psProposals)
--     (\x y -> x {psProposals = proposalsActions y})

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
  , -- , GovState era ~ ConwayGovState era
    Inject (ConwayGovPredFailure era) (PredicateFailure (EraRule "LEDGER" era))
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
    [inject @(ConwayGovPredFailure era) (ProposalCantFollow SNothing protver2 protver0)]

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
  , -- , GovState era ~ ConwayGovState era
    Inject (ConwayGovPredFailure era) (PredicateFailure (EraRule "LEDGER" era))
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
    [inject @(ConwayGovPredFailure era) (ProposalCantFollow (SJust (GovPurposeId gaid1)) protver2 protver1)]
