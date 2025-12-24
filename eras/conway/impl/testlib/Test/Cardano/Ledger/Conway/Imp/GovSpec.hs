{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Conway.Imp.GovSpec (spec) where

import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Conway (hardforkConwayDisallowUnelectedCommitteeFromVoting)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules (ConwayGovPredFailure (..))
import Cardano.Ledger.Credential (Credential (KeyHashObj))
import Cardano.Ledger.Plutus.CostModels (updateCostModels)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Scripts (
  pattern RequireAllOf,
  pattern RequireAnyOf,
  pattern RequireMOf,
  pattern RequireSignature,
 )
import Cardano.Ledger.Val (zero, (<->))
import Data.Default (Default (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.OMap.Strict as OMap
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NES
import Data.Tree
import Lens.Micro
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
import Test.Cardano.Ledger.Imp.Common hiding (Success)

spec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
spec = do
  constitutionSpec
  proposalsSpec
  votingSpec
  policySpec
  predicateFailuresSpec
  unknownCostModelsSpec
  withdrawalsSpec
  hardForkSpec
  pparamUpdateSpec
  networkIdSpec
  bootstrapPhaseSpec

unknownCostModelsSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
unknownCostModelsSpec =
  describe "Unknown CostModels" $ do
    it "Are accepted" $ do
      costModels <- getsPParams ppCostModelsL
      newCostModels <- arbitrary
      hotCommitteeCs <- registerInitialCommittee
      (drepC, _, _) <- setupSingleDRep 1_000_000
      gai <-
        submitParameterChange SNothing $
          emptyPParamsUpdate
            & ppuCostModelsL .~ SJust newCostModels
      whenPostBootstrap $ submitYesVote_ (DRepVoter drepC) gai
      submitYesVoteCCs_ hotCommitteeCs gai
      passNEpochs 2
      getLastEnactedParameterChange `shouldReturn` SJust (GovPurposeId gai)
      getsPParams ppCostModelsL `shouldReturn` updateCostModels costModels newCostModels

predicateFailuresSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
predicateFailuresSpec =
  describe "Predicate failures" $ do
    it "ProposalReturnAccountDoesNotExist" $ do
      mkProposal InfoAction >>= submitProposal_
      unregisteredRewardAccount <- freshKeyHash >>= getRewardAccountFor . KeyHashObj

      proposal <- mkProposalWithRewardAccount InfoAction unregisteredRewardAccount
      submitBootstrapAwareFailingProposal_ proposal $
        FailPostBootstrap
          [injectFailure $ ProposalReturnAccountDoesNotExist unregisteredRewardAccount]

    it "ExpirationEpochTooSmall" $ do
      committeeC <- KeyHashObj <$> freshKeyHash
      let expiration = EpochNo 1
          action =
            UpdateCommittee
              SNothing
              mempty
              (Map.singleton committeeC expiration)
              (0 %! 1)
      passEpoch
      let expectedFailure =
            injectFailure $ ExpirationEpochTooSmall $ Map.singleton committeeC expiration
      proposal <- mkProposal action
      submitBootstrapAwareFailingProposal_ proposal $
        FailBootstrapAndPostBootstrap
          FailBoth
            { bootstrapFailures = [disallowedProposalFailure proposal, expectedFailure]
            , postBootstrapFailures = [expectedFailure]
            }

    it "ProposalDepositIncorrect" $ do
      rewardAccount <- registerRewardAccount
      actionDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppGovActionDepositL
      anchor <- arbitrary
      submitFailingProposal
        ( ProposalProcedure
            { pProcReturnAddr = rewardAccount
            , pProcGovAction = InfoAction
            , pProcDeposit = actionDeposit <-> Coin 1
            , pProcAnchor = anchor
            }
        )
        [ injectFailure $
            ProposalDepositIncorrect $
              Mismatch
                { mismatchSupplied = actionDeposit <-> Coin 1
                , mismatchExpected = actionDeposit
                }
        ]
    it "ConflictingCommitteeUpdate" $ do
      committeeC <- KeyHashObj <$> freshKeyHash
      curEpochNo <- getsNES nesELL
      let action =
            UpdateCommittee
              SNothing
              (Set.singleton committeeC)
              (Map.singleton committeeC (addEpochInterval curEpochNo (EpochInterval 1)))
              (1 %! 1)
      let expectedFailure = injectFailure $ ConflictingCommitteeUpdate $ NES.singleton committeeC
      proposal <- mkProposal action
      submitBootstrapAwareFailingProposal_ proposal $
        FailBootstrapAndPostBootstrap $
          FailBoth
            { bootstrapFailures = [disallowedProposalFailure proposal, expectedFailure]
            , postBootstrapFailures = [expectedFailure]
            }
  where
    disallowedProposalFailure = injectFailure . DisallowedProposalDuringBootstrap

hardForkSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
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
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
pparamUpdateSpec =
  describe "PParamUpdate" $ do
    describe "PPU needs to be wellformed" $ do
      let testMalformedProposal lbl lenz val = it lbl $ do
            let ppu =
                  emptyPParamsUpdate
                    & lenz .~ SJust val
            ga <- mkParameterChangeGovAction SNothing ppu
            mkProposal ga
              >>= flip
                submitFailingProposal
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
        $ Coin 0
      testMalformedProposal
        "ppuGovActionDepositL cannot be 0"
        ppuGovActionDepositL
        zero
      testMalformedProposal
        "ppuDRepDepositL cannot be 0"
        ppuDRepDepositL
        zero
      it "PPU cannot be empty" $ do
        ga <- mkParameterChangeGovAction SNothing emptyPParamsUpdate
        mkProposal ga
          >>= flip
            submitFailingProposal
            [injectFailure $ MalformedProposal ga]

proposalsSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
proposalsSpec = do
  describe "Proposals" $ do
    describe "Consistency" $ do
      it "Proposals submitted without proper parent fail" $ do
        let mkCorruptGovActionId :: GovActionId -> GovActionId
            mkCorruptGovActionId (GovActionId txi (GovActionIx gaix)) =
              GovActionId txi $ GovActionIx $ gaix + 999
        Node p1 [Node _p11 []] <-
          submitParameterChangeTree
            SNothing
            $ Node
              ()
              [ Node () []
              ]
        parameterChangeAction <- mkMinFeeUpdateGovAction (SJust $ mkCorruptGovActionId p1)
        parameterChangeProposal <- mkProposal parameterChangeAction
        submitFailingProposal
          parameterChangeProposal
          [ injectFailure $ InvalidPrevGovActionId parameterChangeProposal
          ]
      it "Subtrees are pruned when proposals expire" $ do
        modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 4
        p1 <- mkMinFeeUpdateGovAction SNothing >>= submitGovAction
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
        let ppupdate = def & ppuTxFeePerByteL .~ SJust (CoinPerByte $ CompactCoin 1000)
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
      it "Subtrees are pruned when competing proposals are enacted" $ whenPostBootstrap $ do
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
      it "Subtrees are pruned when competing proposals are enacted over multiple rounds" $ whenPostBootstrap $ do
        committeeMembers' <- registerInitialCommittee
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
        submitYesVoteCCs_ committeeMembers' p2
        submitYesVote_ (DRepVoter drepC) p21
        submitYesVoteCCs_ committeeMembers' p21
        submitYesVote_ (DRepVoter drepC) p3
        submitYesVoteCCs_ committeeMembers' p3 -- Two competing proposals break the tie based on proposal order
        fmap (!! 3) getProposalsForest
          `shouldReturn` Node SNothing (fmap SJust <$> a)
        passEpoch
        p4 <- submitConstitution SNothing
        p31 <- submitConstitution $ SJust (GovPurposeId p3)
        p211 <- submitConstitution $ SJust (GovPurposeId p21)
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
        p2131 <- submitConstitution $ SJust (GovPurposeId p213)
        p2141 <- submitConstitution $ SJust (GovPurposeId p214)
        submitYesVote_ (DRepVoter drepC) p212
        submitYesVoteCCs_ committeeMembers' p212
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
      -- https://github.com/IntersectMBO/formal-ledger-specifications/issues/923
      -- TODO: Re-enable after issues are resolved, by removing this override
      disableInConformanceIt "Subtrees are pruned for both enactment and expiry over multiple rounds" $ whenPostBootstrap $ do
        committeeMembers' <- registerInitialCommittee
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
        submitYesVoteCCs_ committeeMembers' p1
        submitYesVote_ (DRepVoter dRep) p11
        submitYesVoteCCs_ committeeMembers' p11
        submitYesVote_ (DRepVoter dRep) p3
        submitYesVoteCCs_ committeeMembers' p3 -- Two competing proposals break the tie based on proposal order
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
        submitYesVoteCCs_ committeeMembers' p116
        passNEpochs 3
        fmap (!! 3) getProposalsForest
          `shouldReturn` Node (SJust p116) []
      it "Votes from subsequent epochs are considered for ratification" $ whenPostBootstrap $ do
        modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 4

        committeeMembers' <- registerInitialCommittee
        (dRep, _, _) <- setupSingleDRep 1_000_000
        [Node p1 []] <-
          submitConstitutionForest
            SNothing
            [Node () []]
        fmap (!! 3) getProposalsForest
          `shouldReturn` Node SNothing [Node (SJust p1) []]
        passNEpochs 2
        submitYesVote_ (DRepVoter dRep) p1
        submitYesVoteCCs_ committeeMembers' p1
        passNEpochs 2
        fmap (!! 3) getProposalsForest
          `shouldReturn` Node (SJust p1) []
      it "Proposals are stored in the expected order" $ whenPostBootstrap $ do
        modifyPParams $ ppMaxValSizeL .~ 1_000_000_000
        ens <- getEnactState
        returnAddr <- registerRewardAccount
        withdrawal <-
          (: []) . (returnAddr,) . Coin . getPositive
            <$> (arbitrary :: ImpTestM era (Positive Integer))
        wdrl <- mkTreasuryWithdrawalsGovAction withdrawal
        [prop0, prop1, prop2, prop3] <-
          traverse
            mkProposal
            ( [ InfoAction
              , NoConfidence (ens ^. ensPrevCommitteeL)
              , InfoAction
              , wdrl
              ] ::
                [GovAction era]
            )
        submitProposal_ prop0
        submitProposal_ prop1
        let
          checkProps l = do
            props <-
              getsNES $
                nesEsL . epochStateGovStateL @era . proposalsGovStateL . pPropsL
            fmap (pProcAnchor . gasProposalProcedure . snd) (OMap.assocList props)
              `shouldBe` fmap pProcAnchor l
        checkProps [prop0, prop1]
        submitProposal_ prop2
        submitProposal_ prop3
        checkProps [prop0, prop1, prop2, prop3]
  where
    submitParameterChangeForest = submitGovActionForest $ paramAction >=> submitGovAction
    submitParameterChangeTree = submitGovActionTree (paramAction >=> submitGovAction)
    submitConstitutionForest = submitGovActionForest $ submitConstitution . fmap GovPurposeId
    paramAction p = mkParameterChangeGovAction p (def & ppuTxFeePerByteL .~ SJust (CoinPerByte $ CompactCoin 500))

votingSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
votingSpec =
  describe "Voting" $ do
    it "VotersDoNotExist" $ do
      pp <- getsNES $ nesEsL . curPParamsEpochStateL
      let pv@(ProtVer major minor) = pp ^. ppProtocolVersionL
      gaId <- submitGovAction $ HardForkInitiation SNothing $ ProtVer major (succ minor)
      hotCred <- KeyHashObj <$> freshKeyHash
      if hardforkConwayDisallowUnelectedCommitteeFromVoting pv
        then
          submitFailingVote
            (CommitteeVoter hotCred)
            gaId
            [ injectFailure $ UnelectedCommitteeVoters [hotCred]
            , injectFailure $ VotersDoNotExist [CommitteeVoter hotCred]
            ]
        else
          submitFailingVote
            (CommitteeVoter hotCred)
            gaId
            [injectFailure $ VotersDoNotExist [CommitteeVoter hotCred]]
      poolId <- freshKeyHash
      submitFailingVote
        (StakePoolVoter poolId)
        gaId
        [injectFailure $ VotersDoNotExist [StakePoolVoter poolId]]
      dRepCred <- KeyHashObj <$> freshKeyHash
      submitFailingVote
        (DRepVoter dRepCred)
        gaId
        [injectFailure $ VotersDoNotExist [DRepVoter dRepCred]]
    it "DRep votes are removed" $ do
      pp <- getsNES $ nesEsL . curPParamsEpochStateL
      gaId <- submitGovAction InfoAction
      dRepCred <- KeyHashObj <$> registerDRep
      submitVote_ VoteNo (DRepVoter dRepCred) gaId
      gas <- getGovActionState gaId
      gasDRepVotes gas `shouldBe` [(dRepCred, VoteNo)]
      let deposit = pp ^. ppDRepDepositL
      submitTx_ $ mkBasicTx (mkBasicTxBody & certsTxBodyL .~ [UnRegDRepTxCert dRepCred deposit])
      gasAfterRemoval <- getGovActionState gaId
      gasDRepVotes gasAfterRemoval `shouldBe` []

    -- https://github.com/IntersectMBO/formal-ledger-specifications/issues/923
    -- TODO: Re-enable after issues are resolved, by removing this override
    disableInConformanceIt "expired gov-actions" $ do
      -- Voting for expired actions should fail
      modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
      (drep, _, _) <- setupSingleDRep 1_000_000
      govActionId <- mkProposal InfoAction >>= submitProposal
      passNEpochs 3
      submitFailingVote
        (DRepVoter drep)
        govActionId
        [ injectFailure $ VotingOnExpiredGovAction [(DRepVoter drep, govActionId)]
        ]
    it "non-existent gov-actions" $ do
      (drep, _, _) <- setupSingleDRep 1_000_000
      govActionId <- mkProposal InfoAction >>= submitProposal
      let dummyGaid = govActionId {gaidGovActionIx = GovActionIx 99} -- non-existent `GovActionId`
      submitFailingVote
        (DRepVoter drep)
        dummyGaid
        [injectFailure $ GovActionsDoNotExist $ pure dummyGaid]
    it "committee member can not vote on UpdateCommittee action" $ whenPostBootstrap $ do
      (ccHot :| _) <- registerInitialCommittee
      newMembers <- listOf $ do
        newCommitteeMember <- KeyHashObj <$> freshKeyHash
        Positive lifetime <- arbitrary
        pure (newCommitteeMember, EpochInterval lifetime)
      threshold <- arbitrary
      committeeUpdateId <- submitUpdateCommittee Nothing mempty newMembers threshold
      let voter = CommitteeVoter ccHot
      submitFailingVote
        voter
        committeeUpdateId
        [ injectFailure $ DisallowedVoters [(voter, committeeUpdateId)]
        ]
    it "committee member can not vote on NoConfidence action" $ whenPostBootstrap $ do
      hotCred :| _ <- registerInitialCommittee
      gaid <- submitGovAction $ NoConfidence SNothing
      let voter = CommitteeVoter hotCred
      trySubmitVote VoteNo voter gaid
        `shouldReturn` Left
          [ injectFailure $ DisallowedVoters [(voter, gaid)]
          ]
    it "committee member mixed with other voters can not vote on UpdateCommittee action" $
      whenPostBootstrap ccVoteOnConstitutionFailsWithMultipleVotes
    -- https://github.com/IntersectMBO/formal-ledger-specifications/issues/923
    -- TODO: Re-enable after issues are resolved, by removing this override
    disableInConformanceIt "CC cannot ratify if below threshold" $ whenPostBootstrap $ do
      modifyPParams $ \pp ->
        pp
          & ppGovActionLifetimeL .~ EpochInterval 3
          & ppCommitteeMinSizeL .~ 2
      (dRepCred, _, _) <- setupSingleDRep 1_000_000
      (spoC, _, _) <- setupPoolWithStake $ Coin 42_000_000
      ccColdCred0 <- KeyHashObj <$> freshKeyHash
      ccColdCred1 <- KeyHashObj <$> freshKeyHash
      electionGovAction <-
        submitUpdateCommittee
          Nothing
          mempty
          [ (ccColdCred0, EpochInterval 10)
          , (ccColdCred1, EpochInterval 10)
          ]
          (3 %! 5)
      submitYesVote_ (DRepVoter dRepCred) electionGovAction
      submitYesVote_ (StakePoolVoter spoC) electionGovAction
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
              { constitutionGuardrailsScriptHash = SNothing
              , constitutionAnchor = anchor
              }
      submitYesVote_ (DRepVoter dRepCred) constitutionChangeId
      submitYesVote_ (CommitteeVoter ccHotKey0) constitutionChangeId
      _ <- resignCommitteeColdKey ccColdCred0 SNothing
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
            . constitutionGovStateL
            . constitutionAnchorL
      expectNoCurrentProposals
      conAnchor `shouldNotBe` anchor
    it "can submit SPO votes" $ do
      spoHash <- freshKeyHash
      registerPool spoHash
      passNEpochs 3
      gaId <-
        submitParameterChange SNothing $
          def
            & ppuTxFeePerByteL .~ SJust (CoinPerByte $ CompactCoin 100)
      submitVote_ @era VoteYes (StakePoolVoter spoHash) gaId

constitutionSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
constitutionSpec =
  describe "Constitution proposals" $ do
    describe "accepted for" $ do
      it "empty PrevGovId before the first constitution is enacted" $ do
        --  Initial proposal does not need a GovPurposeId but after it is enacted, the
        --  following ones are not
        _ <- submitConstitutionFailingBootstrap SNothing
        -- Until the first proposal is enacted all proposals with empty GovPurposeIds are valid
        void $ submitConstitutionFailingBootstrap SNothing
      it "valid GovPurposeId" $ whenPostBootstrap $ do
        committeeMembers' <- registerInitialCommittee
        (dRep, _, _) <- setupSingleDRep 1_000_000
        constitution <- arbitrary
        gaidConstitutionProp <- enactConstitution SNothing constitution dRep committeeMembers'
        constitution1 <- arbitrary
        void $
          enactConstitution
            (SJust $ GovPurposeId gaidConstitutionProp)
            constitution1
            dRep
            committeeMembers'

    describe "rejected for" $ do
      it "empty PrevGovId after the first constitution was enacted" $ do
        committeeMembers' <- registerInitialCommittee
        (dRep, _, _) <- setupSingleDRep 1_000_000
        mbGovActionId <- submitConstitutionFailingBootstrap SNothing
        forM_ mbGovActionId $ \govActionId -> do
          submitYesVote_ (DRepVoter dRep) govActionId
          submitYesVoteCCs_ committeeMembers' govActionId
          passNEpochs 2
          constitution <- arbitrary
          let invalidNewConstitutionGovAction =
                NewConstitution
                  SNothing
                  constitution
          invalidNewConstitutionProposal <- mkProposal invalidNewConstitutionGovAction
          submitFailingProposal
            invalidNewConstitutionProposal
            [ injectFailure $ InvalidPrevGovActionId invalidNewConstitutionProposal
            ]
      it "invalid index in GovPurposeId" $ do
        mbGovActionId <- submitConstitutionFailingBootstrap SNothing
        forM_ mbGovActionId $ \govActionId -> do
          passNEpochs 2
          constitution <- arbitrary
          let invalidPrevGovActionId =
                -- Expected Ix = 0
                GovPurposeId (govActionId {gaidGovActionIx = GovActionIx 1})
              invalidNewConstitutionGovAction =
                NewConstitution
                  (SJust invalidPrevGovActionId)
                  constitution
          invalidNewConstitutionProposal <- mkProposal invalidNewConstitutionGovAction
          submitFailingProposal
            invalidNewConstitutionProposal
            [ injectFailure $ InvalidPrevGovActionId invalidNewConstitutionProposal
            ]
      it "valid GovPurposeId but invalid purpose" $ do
        mbGovActionId <- submitConstitutionFailingBootstrap SNothing
        forM_ mbGovActionId $ \govActionId -> do
          passNEpochs 2
          let invalidNoConfidenceAction =
                NoConfidence $ SJust $ GovPurposeId govActionId
          invalidNoConfidenceProposal <- mkProposal invalidNoConfidenceAction

          submitFailingProposal
            invalidNoConfidenceProposal
            [ injectFailure $ InvalidPrevGovActionId invalidNoConfidenceProposal
            ]
  where
    submitConstitutionFailingBootstrap prevGovId = do
      proposal <- fst <$> mkConstitutionProposal prevGovId
      submitBootstrapAwareFailingProposal
        proposal
        (FailBootstrap [injectFailure (DisallowedProposalDuringBootstrap proposal)])

policySpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
policySpec =
  describe "Policy" $ do
    it "policy is respected by proposals" $ whenPostBootstrap $ do
      committeeMembers' <- registerInitialCommittee
      (dRep, _, _) <- setupSingleDRep 1_000_000
      keyHash <- freshKeyHash
      scriptHash <- impAddNativeScript $ RequireAllOf (SSeq.singleton (RequireSignature keyHash))
      anchor <- arbitrary
      _ <-
        enactConstitution
          SNothing
          (Constitution anchor (SJust scriptHash))
          dRep
          committeeMembers'
      wrongScriptHash <-
        impAddNativeScript $
          RequireMOf 1 $
            SSeq.fromList [RequireAnyOf mempty, RequireAllOf mempty]
      impAnn "ParameterChange with correct policy succeeds" $ do
        let pparamsUpdate = def & ppuCommitteeMinSizeL .~ SJust 1
        mkProposal (ParameterChange SNothing pparamsUpdate (SJust scriptHash)) >>= submitProposal_

      impAnn "TreasuryWithdrawals with correct policy succeeds" $ do
        rewardAccount <- registerRewardAccount
        let withdrawals = Map.fromList [(rewardAccount, Coin 1000)]
        mkProposal (TreasuryWithdrawals withdrawals (SJust scriptHash)) >>= submitProposal_

      impAnn "ParameterChange with invalid policy fails" $ do
        let pparamsUpdate = def & ppuCommitteeMinSizeL .~ SJust 2
        mkProposal (ParameterChange SNothing pparamsUpdate (SJust wrongScriptHash))
          >>= flip
            submitFailingProposal
            [injectFailure $ InvalidGuardrailsScriptHash (SJust wrongScriptHash) (SJust scriptHash)]

      impAnn "TreasuryWithdrawals with invalid policy fails" $ do
        rewardAccount <- registerRewardAccount
        let withdrawals = Map.fromList [(rewardAccount, Coin 1000)]
        mkProposal (TreasuryWithdrawals withdrawals (SJust wrongScriptHash))
          >>= flip
            submitFailingProposal
            [injectFailure $ InvalidGuardrailsScriptHash (SJust wrongScriptHash) (SJust scriptHash)]

networkIdSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
networkIdSpec =
  describe "Network ID" $ do
    it "Fails with invalid network ID in proposal return address" $ do
      rewardCredential <- KeyHashObj <$> freshKeyHash
      let badRewardAccount =
            RewardAccount
              { raNetwork = Mainnet -- Our network is Testnet
              , raCredential = rewardCredential
              }
      proposal <- mkProposalWithRewardAccount InfoAction badRewardAccount
      submitBootstrapAwareFailingProposal_ proposal $
        FailBootstrapAndPostBootstrap $
          FailBoth
            { bootstrapFailures =
                [ injectFailure $
                    ProposalProcedureNetworkIdMismatch
                      badRewardAccount
                      Testnet
                ]
            , postBootstrapFailures =
                [ injectFailure $
                    ProposalReturnAccountDoesNotExist
                      badRewardAccount
                , injectFailure $
                    ProposalProcedureNetworkIdMismatch
                      badRewardAccount
                      Testnet
                ]
            }

withdrawalsSpec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
withdrawalsSpec =
  describe "Withdrawals" $ do
    it "Fails predicate when treasury withdrawal has nonexistent return address" $ do
      policy <- getGovPolicy
      unregisteredRewardAccount <- freshKeyHash >>= getRewardAccountFor . KeyHashObj
      registeredRewardAccount <- registerRewardAccount
      let genPositiveCoin = Coin . getPositive <$> arbitrary
      withdrawals <-
        sequence
          [ (unregisteredRewardAccount,) <$> genPositiveCoin
          , (registeredRewardAccount,) <$> genPositiveCoin
          ]
      proposal <- mkProposal $ TreasuryWithdrawals (Map.fromList withdrawals) policy
      void $
        submitBootstrapAwareFailingProposal proposal $
          FailBootstrapAndPostBootstrap $
            FailBoth
              { bootstrapFailures = [injectFailure $ DisallowedProposalDuringBootstrap proposal]
              , postBootstrapFailures =
                  [ injectFailure $
                      TreasuryWithdrawalReturnAccountsDoNotExist [unregisteredRewardAccount]
                  ]
              }

    it "Fails with invalid network ID in withdrawal addresses" $ do
      rewardCredential <- KeyHashObj <$> freshKeyHash
      let badRewardAccount =
            RewardAccount
              { raNetwork = Mainnet -- Our network is Testnet
              , raCredential = rewardCredential
              }
      proposal <-
        mkTreasuryWithdrawalsGovAction [(badRewardAccount, Coin 100_000_000)] >>= mkProposal
      let idMismatch =
            injectFailure $
              TreasuryWithdrawalsNetworkIdMismatch (NES.singleton badRewardAccount) Testnet
          returnAddress =
            injectFailure $
              TreasuryWithdrawalReturnAccountsDoNotExist [badRewardAccount]
      void $
        submitBootstrapAwareFailingProposal proposal $
          FailBootstrapAndPostBootstrap $
            FailBoth
              { bootstrapFailures = [disallowedProposalFailure proposal, idMismatch]
              , postBootstrapFailures = [returnAddress, idMismatch]
              }

    it "Fails for empty withdrawals" $ do
      mkTreasuryWithdrawalsGovAction [] >>= expectZeroTreasuryFailurePostBootstrap

      rwdAccount1 <- registerRewardAccount
      mkTreasuryWithdrawalsGovAction [(rwdAccount1, zero)] >>= expectZeroTreasuryFailurePostBootstrap

      rwdAccount2 <- registerRewardAccount
      let withdrawals = [(rwdAccount1, zero), (rwdAccount2, zero)]

      mkTreasuryWithdrawalsGovAction withdrawals >>= expectZeroTreasuryFailurePostBootstrap

      wdrls <- mkTreasuryWithdrawalsGovAction $ withdrawals ++ [(rwdAccount2, Coin 100_000)]
      proposal <- mkProposal wdrls
      submitBootstrapAwareFailingProposal_ proposal $
        FailBootstrap [disallowedProposalFailure proposal]
  where
    expectZeroTreasuryFailurePostBootstrap wdrls = do
      proposal <- mkProposal wdrls
      void $
        submitBootstrapAwareFailingProposal proposal $
          FailBootstrapAndPostBootstrap $
            FailBoth
              { bootstrapFailures = [disallowedProposalFailure proposal]
              , postBootstrapFailures = [injectFailure $ ZeroTreasuryWithdrawals wdrls]
              }

    disallowedProposalFailure = injectFailure . DisallowedProposalDuringBootstrap

-- =========================================================
-- Proposing a HardFork should always use a new ProtVer that
-- can follow the one installed in the previous HardFork action.

-- | Tests the first hardfork in the Conway era where the PrevGovActionID is SNothing
firstHardForkFollows ::
  forall era.
  ConwayEraImp era =>
  (ProtVer -> ProtVer) ->
  ImpTestM era ()
firstHardForkFollows computeNewFromOld = do
  protVer <- getProtVer
  submitGovAction_ $ HardForkInitiation SNothing (computeNewFromOld protVer)

-- | Negative (deliberatey failing) first hardfork in the Conway era where the PrevGovActionID is SNothing
firstHardForkCantFollow ::
  forall era.
  ConwayEraImp era =>
  ImpTestM era ()
firstHardForkCantFollow = do
  protver0 <- getProtVer
  let protver1 = minorFollow protver0
      protver2 = cantFollow protver1
  proposal <- mkProposal $ HardForkInitiation SNothing protver2
  submitFailingProposal
    proposal
    [ injectFailure $
        ProposalCantFollow SNothing $
          Mismatch
            { mismatchSupplied = protver2
            , mismatchExpected = protver0
            }
    ]

-- | Tests a second hardfork in the Conway era where the PrevGovActionID is SJust
secondHardForkFollows ::
  forall era.
  ConwayEraImp era =>
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
  ConwayEraImp era =>
  ImpTestM era ()
secondHardForkCantFollow = do
  protver0 <- getProtVer
  let protver1 = minorFollow protver0
      protver2 = cantFollow protver1
  gaid1 <- mkProposal (HardForkInitiation SNothing protver1) >>= submitProposal
  mkProposal (HardForkInitiation (SJust (GovPurposeId gaid1)) protver2)
    >>= flip
      submitFailingProposal
      [ injectFailure $
          ProposalCantFollow (SJust (GovPurposeId gaid1)) $
            Mismatch
              { mismatchSupplied = protver2
              , mismatchExpected = protver1
              }
      ]

ccVoteOnConstitutionFailsWithMultipleVotes ::
  forall era.
  ConwayEraImp era =>
  ImpTestM era ()
ccVoteOnConstitutionFailsWithMultipleVotes = do
  (ccCred :| _) <- registerInitialCommittee
  (drepCred, _, _) <- setupSingleDRep 1_000_000
  drepCred2 <- KeyHashObj <$> registerDRep
  newCommitteeMember <- KeyHashObj <$> freshKeyHash
  committeeProposal <-
    submitUpdateCommittee Nothing mempty [(newCommitteeMember, EpochInterval 10)] (1 %! 2)
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
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
bootstrapPhaseSpec =
  describe "Proposing and voting" $ do
    it "Parameter change" $ do
      gid <- mkMinFeeUpdateGovAction SNothing >>= submitGovAction
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
      action <- mkTreasuryWithdrawalsGovAction [(rewardAccount, Coin 1000)]
      proposal <- mkProposalWithRewardAccount action rewardAccount
      checkProposalFailure proposal
    it "NoConfidence" $ do
      proposal <- mkProposal $ NoConfidence SNothing
      checkProposalFailure proposal
    it "UpdateCommittee" $ do
      cCred <- KeyHashObj <$> freshKeyHash
      curEpochNo <- getsNES nesELL
      let newMembers = [(cCred, addEpochInterval curEpochNo (EpochInterval 30))]
      proposal <- mkProposal $ UpdateCommittee SNothing mempty newMembers (1 %! 1)
      checkProposalFailure proposal
    it "NewConstitution" $ do
      constitution <- arbitrary
      proposal <- mkProposal $ NewConstitution SNothing constitution
      checkProposalFailure proposal
  where
    checkProposalFailure proposal =
      submitBootstrapAwareFailingProposal_ proposal $
        FailBootstrap [injectFailure $ DisallowedProposalDuringBootstrap proposal]
    checkVotingFailure voter gid = do
      vote <- arbitrary
      submitBootstrapAwareFailingVote vote voter gid $
        FailBootstrap [injectFailure $ DisallowedVotesDuringBootstrap [(voter, gid)]]
