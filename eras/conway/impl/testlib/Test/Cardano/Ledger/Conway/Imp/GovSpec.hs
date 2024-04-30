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

module Test.Cardano.Ledger.Conway.Imp.GovSpec (
  spec,
  relevantDuringBootstrapSpec,
) where

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
import Cardano.Ledger.Plutus.CostModels (updateCostModels)
import qualified Cardano.Ledger.Shelley.HardForks as HF
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Val (zero, (<->))
import Data.Default.Class (Default (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.OMap.Strict as OMap
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Data.Tree
import Lens.Micro
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Arbitrary (FlexibleCostModels (..))
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
    relevantDuringBootstrapSpec
    constitutionSpec
    proposalsWithVotingSpec
    votingSpec
    policySpec
    networkIdWithdrawalsSpec
    predicateFailuresSpec
    unknownCostModelsSpec

relevantDuringBootstrapSpec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  SpecWith (ImpTestState era)
relevantDuringBootstrapSpec = do
  hardForkSpec
  pparamUpdateSpec
  proposalsSpec
  networkIdSpec
  bootstrapPhaseSpec

unknownCostModelsSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpTestState era)
unknownCostModelsSpec =
  describe "Unknown CostModels" $ do
    it "Are accepted" $ do
      costModels <- getsPParams ppCostModelsL
      FlexibleCostModels newCostModels <- arbitrary
      (hotCommitteeC :| _) <- registerInitialCommittee
      (drepC, _, _) <- setupSingleDRep 1_000_000
      gai <-
        submitParameterChange SNothing $
          emptyPParamsUpdate
            & ppuCostModelsL .~ SJust newCostModels
      submitYesVote_ (DRepVoter drepC) gai
      submitYesVote_ (CommitteeVoter hotCommitteeC) gai
      passNEpochs 2
      getLastEnactedParameterChange `shouldReturn` SJust (GovPurposeId gai)
      getsPParams ppCostModelsL `shouldReturn` updateCostModels costModels newCostModels

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

proposalsWithVotingSpec ::
  forall era.
  ( ConwayEraImp era
  , GovState era ~ ConwayGovState era
  ) =>
  SpecWith (ImpTestState era)
proposalsWithVotingSpec =
  describe "Proposals" $ do
    describe "Consistency" $ do
      it "Subtrees are pruned when competing proposals are enacted" $ do
        (dRep, committeeMember, GovPurposeId committeeGovActionId) <- electBasicCommittee
        a@[ _
            , b@(Node p2 _)
            ] <-
          submitConstitutionForest
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
      it "Subtrees are pruned when competing proposals are enacted over multiple rounds" $ do
        (committeeMember :| _) <- registerInitialCommittee
        (drepC, _, _) <- setupSingleDRep 1_000_000
        a@[ c
            , Node
                p2
                [ Node p21 []
                  , Node p22 []
                  ]
            , Node p3 []
            ] <-
          submitConstitutionForest
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
        submitYesVote_ (DRepVoter drepC) p2
        submitYesVote_ (CommitteeVoter committeeMember) p2
        submitYesVote_ (DRepVoter drepC) p21
        submitYesVote_ (CommitteeVoter committeeMember) p21
        submitYesVote_ (DRepVoter drepC) p3
        submitYesVote_ (CommitteeVoter committeeMember) p3 -- Two competing proposals break the tie based on proposal order
        fmap (!! 3) getProposalsForest
          `shouldReturn` Node SNothing (fmap SJust <$> a)
        passEpoch
        p4 <- submitConstitutionGovAction SNothing
        p31 <- submitConstitutionGovAction $ SJust p3
        p211 <- submitConstitutionGovAction $ SJust p21
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
          submitConstitutionForest
            (SJust p21)
            [ Node () []
            , Node () []
            , Node () []
            ]
        p2131 <- submitConstitutionGovAction $ SJust p213
        p2141 <- submitConstitutionGovAction $ SJust p214
        submitYesVote_ (DRepVoter drepC) p212
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
        (committeeMember :| _) <- registerInitialCommittee
        (dRep, _, _) <- setupSingleDRep 1_000_000
        [Node p1 []] <-
          submitConstitutionForest
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
        (committeeMember :| _) <- registerInitialCommittee
        (dRep, _, _) <- setupSingleDRep 1_000_000
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
          submitConstitutionForest
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
          submitConstitutionForest
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
          submitConstitutionForest
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
    it "Proposals are stored in the expected order" $ do
      modifyPParams $
        ppMaxValSizeL .~ 1_000_000_000
      returnAddr <- registerRewardAccount
      deposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionDepositL
      ens <- getEnactState
      withdrawals <- arbitrary
      let
        mkProp name action = do
          ProposalProcedure
            { pProcReturnAddr = returnAddr
            , pProcGovAction = action
            , pProcDeposit = deposit
            , pProcAnchor = Anchor (fromJust $ textToUrl 16 name) def
            }
        prop0 = mkProp "prop0" InfoAction
        prop1 = mkProp "prop1" $ NoConfidence (ens ^. ensPrevCommitteeL)
        prop2 = mkProp "prop2" InfoAction
        prop3 = mkProp "prop3" $ TreasuryWithdrawals withdrawals SNothing
      submitProposal_ prop0
      submitProposal_ prop1
      let
        checkProps l = do
          props <-
            getsNES $
              nesEsL . epochStateGovStateL @era . cgsProposalsL . pPropsL
          fmap (pProcAnchor . gasProposalProcedure . snd) (OMap.assocList props)
            `shouldBe` fmap pProcAnchor l
      checkProps [prop0, prop1]
      submitProposal_ prop2
      submitProposal_ prop3
      checkProps [prop0, prop1, prop2, prop3]
  where
    submitConstitutionForest = submitGovActionForest submitConstitutionGovAction

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
        Node p1 [Node _p11 []] <-
          submitParameterChangeTree
            SNothing
            $ Node
              ()
              [ Node () []
              ]
        pp <- getsNES $ nesEsL . curPParamsEpochStateL
        khPropRwd <- freshKeyHash
        let parameterChangeAction =
              ParameterChange
                (SJust $ GovPurposeId $ mkCorruptGovActionId p1)
                (def & ppuMinFeeAL .~ SJust (Coin 3000))
                SNothing
            parameterChangeProposal =
              ProposalProcedure
                { pProcDeposit = pp ^. ppGovActionDepositL
                , pProcReturnAddr = RewardAccount Testnet (KeyHashObj khPropRwd)
                , pProcGovAction = parameterChangeAction
                , pProcAnchor = def
                }
        submitFailingProposal
          parameterChangeProposal
          [ injectFailure $ InvalidPrevGovActionId parameterChangeProposal
          ]
      it "Subtrees are pruned when proposals expire" $ do
        modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 4
        p1 <- submitParameterChange SNothing (def & ppuMinFeeAL .~ SJust (Coin 3000))
        passNEpochs 3
        a <-
          submitParameterChangeTree
            (SJust p1)
            $ Node
              ()
              [ Node () []
              , Node () []
              ]
        b <-
          submitParameterChangeTree
            SNothing
            $ Node
              ()
              [ Node () []
              ]
        getProposalsForest
          `shouldReturn` [ Node
                            SNothing
                            [ Node (SJust p1) [SJust <$> a]
                            , SJust <$> b
                            ]
                         , Node SNothing []
                         , Node SNothing []
                         , Node SNothing []
                         ]
        passNEpochs 3
        getProposalsForest
          `shouldReturn` [ Node SNothing [SJust <$> b]
                         , Node SNothing []
                         , Node SNothing []
                         , Node SNothing []
                         ]
      it "Subtrees are pruned when proposals expire over multiple rounds" $ do
        let ppupdate = def & ppuMinFeeAL .~ SJust (Coin 3000)
        let submitInitialProposal = submitParameterChange SNothing ppupdate
        let submitChildProposal parent = submitParameterChange (SJust parent) ppupdate
        modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 4
        p1 <- submitInitialProposal
        getProposalsForest
          `shouldReturn` [ Node
                            SNothing
                            [ Node (SJust p1) []
                            ]
                         , Node SNothing []
                         , Node SNothing []
                         , Node SNothing []
                         ]

        passEpoch
        p2 <- submitInitialProposal
        p11 <- submitChildProposal p1
        getProposalsForest
          `shouldReturn` [ Node
                            SNothing
                            [ Node (SJust p1) [Node (SJust p11) []]
                            , Node (SJust p2) []
                            ]
                         , Node SNothing []
                         , Node SNothing []
                         , Node SNothing []
                         ]

        passEpoch
        p3 <- submitInitialProposal
        p21 <- submitChildProposal p2
        a <-
          submitParameterChangeForest
            (SJust p11)
            [ Node () []
            , Node () []
            ]
        getProposalsForest
          `shouldReturn` [ Node
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
                         , Node SNothing []
                         , Node SNothing []
                         , Node SNothing []
                         ]

        passEpoch
        p4 <- submitInitialProposal
        p31 <- submitChildProposal p3
        p211 <- submitChildProposal p21
        getProposalsForest
          `shouldReturn` [ Node
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
                         , Node SNothing []
                         , Node SNothing []
                         , Node SNothing []
                         ]
        passNEpochs 3
        getProposalsForest
          `shouldReturn` [ Node
                            SNothing
                            [ Node (SJust p2) [Node (SJust p21) [Node (SJust p211) []]]
                            , Node (SJust p3) [Node (SJust p31) []]
                            , Node (SJust p4) []
                            ]
                         , Node SNothing []
                         , Node SNothing []
                         , Node SNothing []
                         ]
        p5 <- submitInitialProposal
        p41 <- submitChildProposal p4
        p311 <- submitChildProposal p31
        p212 <- submitChildProposal p21
        getProposalsForest
          `shouldReturn` [ Node
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
                         , Node SNothing []
                         , Node SNothing []
                         , Node SNothing []
                         ]
        passEpoch
        p6 <- submitInitialProposal
        p51 <- submitChildProposal p5
        p411 <- submitChildProposal p41
        p312 <- submitChildProposal p31
        getProposalsForest
          `shouldReturn` [ Node
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
                         , Node SNothing []
                         , Node SNothing []
                         , Node SNothing []
                         ]
        passEpoch
        getProposalsForest
          `shouldReturn` [ Node
                            SNothing
                            [ Node (SJust p4) [Node (SJust p41) [Node (SJust p411) []]]
                            , Node (SJust p5) [Node (SJust p51) []]
                            , Node (SJust p6) []
                            ]
                         , Node SNothing []
                         , Node SNothing []
                         , Node SNothing []
                         ]
        passEpoch
        getProposalsForest
          `shouldReturn` [ Node
                            SNothing
                            [ Node (SJust p5) [Node (SJust p51) []]
                            , Node (SJust p6) []
                            ]
                         , Node SNothing []
                         , Node SNothing []
                         , Node SNothing []
                         ]
        passNEpochs 3
        getProposalsForest
          `shouldReturn` [ Node
                            SNothing
                            [ Node (SJust p6) []
                            ]
                         , Node SNothing []
                         , Node SNothing []
                         , Node SNothing []
                         ]
        passEpoch
        getProposalsForest
          `shouldReturn` [ Node SNothing []
                         , Node SNothing []
                         , Node SNothing []
                         , Node SNothing []
                         ]
  where
    submitParameterChangeForest = submitGovActionForest $ submitGovAction . paramAction
    submitParameterChangeTree = submitGovActionTree $ submitGovAction . paramAction
    paramAction p =
      ParameterChange (GovPurposeId <$> p) (def & ppuMinFeeAL .~ SJust (Coin 10)) SNothing

votingSpec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  , GovState era ~ ConwayGovState era
  ) =>
  SpecWith (ImpTestState era)
votingSpec =
  describe "Voting" $ do
    describe "fails for" $ do
      it "expired gov-actions" $ do
        -- Voting after the 3rd epoch should fail
        modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
        (drep, _, _) <- setupSingleDRep 1_000_000
        (govActionId, _) <- submitConstitution SNothing
        passEpoch
        passEpoch
        passEpoch
        submitFailingVote
          (DRepVoter drep)
          govActionId
          [ injectFailure $ VotingOnExpiredGovAction [(DRepVoter drep, govActionId)]
          ]
      it "non-existent gov-actions" $ do
        (drep, _, _) <- setupSingleDRep 1_000_000
        (govActionId, _) <- submitConstitution SNothing
        let dummyGaid = govActionId {gaidGovActionIx = GovActionIx 99} -- non-existent `GovActionId`
        submitFailingVote
          (DRepVoter drep)
          dummyGaid
          [injectFailure $ GovActionsDoNotExist $ pure dummyGaid]
      it
        "committee member voting on committee change"
        committeeMemberVotingOnCommitteeChange
      it "non-committee-member voting on committee change as a committee member" $ do
        credCandidate <- KeyHashObj <$> freshKeyHash
        credVoter <- KeyHashObj <$> freshKeyHash
        committeeUpdateId <-
          submitGovAction $
            UpdateCommittee
              SNothing
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
      it "CC cannot ratify if below threshold" $ do
        modifyPParams $ \pp ->
          pp
            & ppGovActionLifetimeL .~ EpochInterval 3
            & ppDRepVotingThresholdsL
              .~ def
                { dvtUpdateToConstitution = 1 %! 2
                }
            & ppCommitteeMinSizeL .~ 2
        (dRepCred, _, _) <- setupSingleDRep 1_000_000
        ccColdCred0 <- KeyHashObj <$> freshKeyHash
        ccColdCred1 <- KeyHashObj <$> freshKeyHash
        electionGovAction <-
          submitGovAction $
            UpdateCommittee
              SNothing
              mempty
              ( Map.fromList
                  [ (ccColdCred0, EpochNo 10)
                  , (ccColdCred1, EpochNo 10)
                  ]
              )
              (3 %! 5)
        submitYesVote_ (DRepVoter dRepCred) electionGovAction
        logAcceptedRatio electionGovAction
        passNEpochs 3
        expectNoCurrentProposals
        ccHotKey0 <- registerCommitteeHotKey ccColdCred0
        ccHotKey1 <- registerCommitteeHotKey ccColdCred1
        anchor <- arbitrary
        constitutionChangeId <-
          submitGovAction $
            NewConstitution
              SNothing
              Constitution
                { constitutionScript = SNothing
                , constitutionAnchor = anchor
                }
        submitYesVote_ (DRepVoter dRepCred) constitutionChangeId
        resignCommitteeColdKey ccColdCred0 SNothing
        submitYesVote_ (CommitteeVoter ccHotKey0) constitutionChangeId
        submitYesVote_ (CommitteeVoter ccHotKey1) constitutionChangeId
        passEpoch
        logAcceptedRatio constitutionChangeId
        logToExpr =<< lookupGovActionState constitutionChangeId
        passNEpochs 4
        conAnchor <-
          getsNES $
            nesEsL
              . esLStateL
              . lsUTxOStateL
              . utxosGovStateL
              . cgsConstitutionL
              . constitutionAnchorL
        expectNoCurrentProposals
        conAnchor `shouldNotBe` anchor

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
        (committeeMember :| _) <- registerInitialCommittee
        (dRep, _, _) <- setupSingleDRep 1_000_000
        constitution <- arbitrary
        gaidConstitutionProp <- enactConstitution SNothing constitution dRep committeeMember
        constitution1 <- arbitrary
        void $
          enactConstitution
            (SJust $ GovPurposeId gaidConstitutionProp)
            constitution1
            dRep
            committeeMember

    describe "rejected for" $ do
      it "empty PrevGovId after the first constitution was enacted" $ do
        (committeeMember :| _) <- registerInitialCommittee
        (dRep, _, _) <- setupSingleDRep 1_000_000
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
      (committeeMember :| _) <- registerInitialCommittee
      (dRep, _, _) <- setupSingleDRep 1_000_000
      keyHash <- freshKeyHash
      scriptHash <- impAddNativeScript $ RequireAllOf (SSeq.singleton (RequireSignature keyHash))
      anchor <- arbitrary
      _ <-
        enactConstitution
          SNothing
          (Constitution anchor (SJust scriptHash))
          dRep
          committeeMember
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

networkIdWithdrawalsSpec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  SpecWith (ImpTestState era)
networkIdWithdrawalsSpec =
  describe "Network ID" $ do
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
  protVer <- getProtVer
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
  protver0 <- getProtVer
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
  (ccHot :| _) <- registerInitialCommittee
  khCommittee <- KeyHashObj <$> freshKeyHash
  committeeUpdateId <-
    submitGovAction $
      UpdateCommittee
        SNothing
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
  (ccCred :| _) <- registerInitialCommittee
  (drepCred, _, _) <- setupSingleDRep 1_000_000
  drepCred2 <- KeyHashObj <$> registerDRep
  newCommitteeMember <- KeyHashObj <$> freshKeyHash
  committeeProposal <-
    submitGovAction $
      UpdateCommittee
        SNothing
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

bootstrapPhaseSpec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  ) =>
  SpecWith (ImpTestState era)
bootstrapPhaseSpec =
  describe "Proposing and voting during bootstrap phase" $ do
    it "Parameter change" $ do
      gid <- submitParameterChange SNothing (def & ppuMinFeeAL .~ SJust (Coin 3000))
      (committee :| _) <- registerInitialCommittee
      (drep, _, _) <- setupSingleDRep 1_000_000
      (spo, _, _) <- setupPoolWithStake $ Coin 42_000_000
      checkVotingFailure (DRepVoter drep) gid
      submitYesVote_ (StakePoolVoter spo) gid
      submitYesVote_ (CommitteeVoter committee) gid
    it "Hardfork initiation" $ do
      curProtVer <- getProtVer
      nextMajorVersion <- succVersion $ pvMajor curProtVer
      gid <-
        submitGovAction $
          HardForkInitiation SNothing (curProtVer {pvMajor = nextMajorVersion})
      (committee :| _) <- registerInitialCommittee
      (drep, _, _) <- setupSingleDRep 1_000_000
      (spo, _, _) <- setupPoolWithStake $ Coin 42_000_000
      checkVotingFailure (DRepVoter drep) gid
      submitYesVote_ (StakePoolVoter spo) gid
      submitYesVote_ (CommitteeVoter committee) gid
    it "Info action" $ do
      gid <- submitGovAction InfoAction
      (committee :| _) <- registerInitialCommittee
      (drep, _, _) <- setupSingleDRep 1_000_000
      (spo, _, _) <- setupPoolWithStake $ Coin 42_000_000
      submitYesVote_ (DRepVoter drep) gid
      submitYesVote_ (StakePoolVoter spo) gid
      submitYesVote_ (CommitteeVoter committee) gid
    it "Treasury withdrawal" $ do
      rewardAccount <- registerRewardAccount
      govActionDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionDepositL
      let action = TreasuryWithdrawals [(rewardAccount, Coin 1000)] SNothing
      let proposal =
            ProposalProcedure
              { pProcDeposit = govActionDeposit
              , pProcReturnAddr = rewardAccount
              , pProcGovAction = action
              , pProcAnchor = def
              }
      checkProposalFailure proposal
    it "NoConfidence" $ do
      proposal <- proposalWithRewardAccount $ NoConfidence SNothing
      checkProposalFailure proposal
    it "UpdateCommittee" $ do
      cCred <- KeyHashObj <$> freshKeyHash
      let action = UpdateCommittee SNothing mempty [(cCred, EpochNo 30)] (1 %! 1)
      proposal <- proposalWithRewardAccount action
      checkProposalFailure proposal
    it "NewConstitution" $ do
      constitution <- arbitrary
      proposal <- proposalWithRewardAccount $ NewConstitution SNothing constitution
      checkProposalFailure proposal
  where
    checkProposalFailure proposal = do
      curProtVer <- getProtVer
      when (HF.bootstrapPhase curProtVer) $
        submitFailingProposal proposal [injectFailure $ DisallowedProposalDuringBootstrap proposal]
    checkVotingFailure voter gid = do
      curProtVer <- getProtVer
      when (HF.bootstrapPhase curProtVer) $
        submitFailingVote voter gid [injectFailure $ DisallowedVotesDuringBootstrap [(voter, gid)]]
