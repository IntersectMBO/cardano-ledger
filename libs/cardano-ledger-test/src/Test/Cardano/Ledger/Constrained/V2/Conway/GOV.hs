{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the GOV rule
module Test.Cardano.Ledger.Constrained.V2.Conway.GOV where

import Data.Coerce

import Cardano.Ledger.Api
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams
import Cardano.Ledger.Conway.Rules
import Data.Map qualified as Map
import Data.Set qualified as Set
import Lens.Micro

import Constrained

import Test.Cardano.Ledger.Constrained.V2.Conway
import Test.Cardano.Ledger.Constrained.V2.Conway.PParams

govEnvSpec ::
  IsConwayUniv fn =>
  Spec fn (GovEnv (ConwayEra StandardCrypto))
govEnvSpec = constrained $ \ge ->
  match ge $ \_ _ pp _ _ _ ->
    satisfies pp pparamsSpec

-- TODO:
--  - check `toGovRelationTree` invariants here
--  - check `checkInvariantOnAddition` invariants here
--  - check `checkInvariantOnDeletion` invariants here
--  - check `toPTree` invariants here

-- NOTE: it is probably OK not to check uniqueness of ids here, because a clash
-- is never going to be generated, and the real representation of `Proposals` doesn't
-- allow the id to appear twice.
govProposalsSpec ::
  IsConwayUniv fn =>
  GovEnv (ConwayEra StandardCrypto) ->
  Spec fn (Proposals (ConwayEra StandardCrypto))
govProposalsSpec GovEnv {..} =
  constrained $ \props ->
    match props $ \ppupTree hardForkTree committeeTree constitutionTree unorderedProposals ->
      [ -- Protocol parameter updates
        wellFormedChildren (lit $ coerce grPParamUpdate) ppupTree
      , allGASInTree ppupTree $ \gas ->
          [ isCon @"ParameterChange" (pProcGovAction_ . gasProposalProcedure_ $ gas)
          , onCon @"ParameterChange" (pProcGovAction_ . gasProposalProcedure_ $ gas) $
              \_ ppup policy ->
                [ wfPParamsUpdate ppup
                , assert $ policy ==. lit gePPolicy
                ]
          ]
      , forAll (snd_ ppupTree) (genHint treeGenHint)
      , genHint listSizeHint (snd_ ppupTree)
      , -- Hard forks
        wellFormedChildren (lit $ coerce grHardFork) hardForkTree
      , allGASInTree hardForkTree $ \gas ->
          isCon @"HardForkInitiation" (pProcGovAction_ . gasProposalProcedure_ $ gas)
      , allGASAndChildInTree hardForkTree $ \gas gas' ->
          [ onHardFork gas $ \_ pv ->
              onHardFork gas' $ \_ pv' ->
                match pv $ \majV minV ->
                  match pv' $ \majV' minV' ->
                    [ assert $ majV <=. majV'
                    , ifElse
                        (majV ==. lit maxBound)
                        (majV' ==. majV)
                        (majV' <=. succV_ majV)
                    , ifElse
                        (majV ==. majV')
                        (minV' ==. minV + 1)
                        (minV' ==. 0)
                    , minV' `dependsOn` majV'
                    ]
          , -- TODO: get rid of this when we've fixed the performance problem around deep forall
            -- fusion
            isCon @"HardForkInitiation" (pProcGovAction_ . gasProposalProcedure_ $ gas)
          ]
      , forAll (snd_ hardForkTree) (genHint treeGenHint)
      , genHint listSizeHint (snd_ hardForkTree)
      , -- Committee
        wellFormedChildren (lit $ coerce grCommittee) committeeTree
      , -- TODO: it would be nice to have a trick like `isCon` that can
        -- do disjunction without having to write down all the cases.
        allGASInTree committeeTree $ \gas ->
          caseOn
            (pProcGovAction_ . gasProposalProcedure_ $ gas)
            (branch $ \_ _ _ -> False)
            (branch $ \_ _ -> False)
            (branch $ \_ _ -> False)
            (branch $ \_ -> True)
            -- UpdateCommittee
            ( branch $ \_ _ added _ ->
                forAll (rng_ added) $ \epoch ->
                  lit geEpoch <. epoch
            )
            (branch $ \_ _ -> False)
            (branch $ \_ -> False)
      , forAll (snd_ committeeTree) (genHint treeGenHint)
      , genHint listSizeHint (snd_ committeeTree)
      , -- Constitution
        wellFormedChildren (lit $ coerce grConstitution) constitutionTree
      , allGASInTree constitutionTree $ \gas ->
          isCon @"NewConstitution" (pProcGovAction_ . gasProposalProcedure_ $ gas)
      , forAll (snd_ constitutionTree) (genHint treeGenHint)
      , genHint listSizeHint (snd_ constitutionTree)
      , -- Withdrawals and info
        forAll unorderedProposals $ \gas ->
          caseOn
            (pProcGovAction_ . gasProposalProcedure_ $ gas)
            (branch $ \_ _ _ -> False)
            (branch $ \_ _ -> False)
            -- Treasury Withdrawal
            ( branch $ \withdrawMap policy ->
                [ forAll (dom_ withdrawMap) $ \rewAcnt ->
                    match rewAcnt $ \net _ -> net ==. lit Testnet
                , assert $ policy ==. lit gePPolicy
                ]
            )
            (branch $ \_ -> False)
            (branch $ \_ _ _ _ -> False)
            (branch $ \_ _ -> False)
            -- Info
            (branch $ \_ -> True)
      , genHint listSizeHint unorderedProposals
      ]
  where
    GovRelation {..} = gePrevGovActionIds
    treeGenHint = (Just 2, 10)
    listSizeHint = 5

allGASInTree ::
  (IsConwayUniv fn, IsPred p fn) =>
  Term fn ProposalTree ->
  (Term fn (GovActionState (ConwayEra StandardCrypto)) -> p) ->
  Pred fn
allGASInTree t k =
  forAll (snd_ t) $ \t' ->
    forAll' t' $ \gas _ ->
      k gas

allGASAndChildInTree ::
  (IsConwayUniv fn, IsPred p fn) =>
  Term fn ProposalTree ->
  ( Term fn (GovActionState (ConwayEra StandardCrypto)) ->
    Term fn (GovActionState (ConwayEra StandardCrypto)) ->
    p
  ) ->
  Pred fn
allGASAndChildInTree t k =
  forAll (snd_ t) $ \t' ->
    forAll' t' $ \gas cs ->
      forAll cs $ \t'' ->
        k gas (roseRoot_ t'')

wellFormedChildren ::
  IsConwayUniv fn =>
  Term fn (StrictMaybe (GovActionId StandardCrypto)) ->
  Term fn ProposalTree ->
  Pred fn
wellFormedChildren root rootAndTrees =
  match rootAndTrees $ \root' trees ->
    [ assert $ root ==. root' -- The root matches the root given in the environment
    , forAll trees $ \t ->
        [ -- Every node just below the root has the root as its parent
          withPrevActId (roseRoot_ t) (assert . (==. root))
        , -- Every node's children have the id of the node as its parent
          forAll' t $ \gas children ->
            [ forAll children $ \t' ->
                [ withPrevActId (roseRoot_ t') (assert . (==. cSJust_ (gasId_ gas)))
                -- TODO: figure out why this causes a crash!
                -- , t' `dependsOn` gas
                ]
            , children `dependsOn` gas
            ]
        ]
    ]

withPrevActId ::
  IsConwayUniv fn =>
  Term fn (GovActionState (ConwayEra StandardCrypto)) ->
  (Term fn (StrictMaybe (GovActionId StandardCrypto)) -> Pred fn) ->
  Pred fn
withPrevActId gas k =
  caseOn
    (pProcGovAction_ . gasProposalProcedure_ $ gas)
    ( branch $ \mPrevActionId _ _ ->
        caseOn
          mPrevActionId
          (branch $ \_ -> k cSNothing_)
          (branch $ \i -> k (cSJust_ $ toGeneric_ i))
    )
    ( branch $ \mPrevActionId _ ->
        caseOn
          mPrevActionId
          (branch $ \_ -> k cSNothing_)
          (branch $ \i -> k (cSJust_ $ toGeneric_ i))
    )
    (branch $ \_ _ -> True)
    -- NoConfidence
    --   !(StrictMaybe (PrevGovActionId 'CommitteePurpose (EraCrypto era)))
    ( branch $ \mPrevActionId ->
        caseOn
          mPrevActionId
          (branch $ \_ -> k cSNothing_)
          (branch $ \i -> k (cSJust_ $ toGeneric_ i))
    )
    -- UpdateCommittee
    --   !(StrictMaybe (PrevGovActionId 'CommitteePurpose (EraCrypto era)))
    --   !(Set (Credential 'ColdCommitteeRole (EraCrypto era)))
    --   !(Map (Credential 'ColdCommitteeRole (EraCrypto era)) EpochNo)
    --   !UnitInterval
    ( branch $ \mPrevActionId _ _ _ ->
        caseOn
          mPrevActionId
          (branch $ \_ -> k cSNothing_)
          (branch $ \i -> k (cSJust_ $ toGeneric_ i))
    )
    -- NewConstitution
    --  !(StrictMaybe (PrevGovActionId 'ConstitutionPurpose (EraCrypto era)))
    --  !(Constitution era)
    ( branch $ \mPrevActionId _ ->
        caseOn
          mPrevActionId
          (branch $ \_ -> k cSNothing_)
          (branch $ \i -> k (cSJust_ $ toGeneric_ i))
    )
    -- InfoAction
    (branch $ \_ -> True)

onHardFork ::
  (IsConwayUniv fn, IsPred p fn) =>
  Term fn (GovActionState (ConwayEra StandardCrypto)) ->
  ( Term fn (StrictMaybe (GovPurposeId 'HardForkPurpose (ConwayEra StandardCrypto))) ->
    Term fn ProtVer ->
    p
  ) ->
  Pred fn
onHardFork gas k = onCon @"HardForkInitiation" (pProcGovAction_ . gasProposalProcedure_ $ gas) k

govProceduresSpec ::
  IsConwayUniv fn =>
  GovEnv (ConwayEra StandardCrypto) ->
  Proposals (ConwayEra StandardCrypto) ->
  Spec fn (GovProcedures (ConwayEra StandardCrypto))
govProceduresSpec ge@GovEnv {..} ps =
  let actions f =
        [ gid
        | (gid, act) <- Map.toList $ proposalsActionsMap ps
        , geEpoch <= act ^. gasExpiresAfterL
        , f (gasAction act)
        ]
      committeeVotableActionIds =
        actions (isCommitteeVotingAllowed geCommitteeState)
      drepVotableActionIds =
        actions isDRepVotingAllowed
      stakepoolVotableActionIds =
        actions isStakePoolVotingAllowed
   in constrained $ \govProcs ->
        match govProcs $ \votingProcs proposalProcs ->
          [ match votingProcs $ \votingProcsMap ->
              forAll votingProcsMap $ \kvp ->
                match kvp $ \voter mapActVote ->
                  (caseOn voter)
                    ( branch $ \_c ->
                        subset_ (dom_ mapActVote) (lit $ Set.fromList committeeVotableActionIds)
                    )
                    ( branch $ \_c ->
                        subset_ (dom_ mapActVote) (lit $ Set.fromList drepVotableActionIds)
                    )
                    ( branch $ \_c ->
                        subset_ (dom_ mapActVote) (lit $ Set.fromList stakepoolVotableActionIds)
                    )
          , forAll proposalProcs $ \proc ->
              match proc $ \deposit returnAddr govAction _ ->
                [ assert $ deposit ==. lit (gePParams ^. ppGovActionDepositL)
                , match returnAddr $ \net _cred ->
                    net ==. lit Testnet
                , wfGovAction ge govAction
                ]
          ]

-- TODO: The pointer to the previous action could also point to a
-- key in the `Proposals`, need a more flexible way to say what previous
-- action id is correct
wfGovAction ::
  IsConwayUniv fn =>
  GovEnv (ConwayEra StandardCrypto) ->
  Term fn (GovAction (ConwayEra StandardCrypto)) ->
  Pred fn
wfGovAction GovEnv {..} govAction =
  caseOn
    govAction
    -- ParameterChange
    ( branch $ \mPrevActionId ppUpdate policy ->
        -- TODO: this should be any ppup proposal in the tree
        [ assert $ mPrevActionId ==. lit (gePrevGovActionIds ^. grPParamUpdateL)
        , wfPParamsUpdate ppUpdate
        , assert $ policy ==. lit gePPolicy
        ]
    )
    -- HardForkInitiation
    ( branch $ \mPrevActionId protVer ->
        [ -- TODO: this needs to be loosened to be any hard for proposal in the tree
          assert $ mPrevActionId ==. lit (gePrevGovActionIds ^. grHardForkL)
        , -- TODO: here we need to say that the `protVer` can follow the last prot ver
          --        - the tricky thing about this is that it's not enough to say that you
          --        follow what is in the Env, you actually have to follow what is in the
          --        node above you in the tree
          let ProtVer currentMajorVersion currentMinorVersion = gePParams ^. ppProtocolVersionL
           in match protVer $ \majorVersion minorVersion ->
                case succVersion currentMajorVersion of
                  Nothing ->
                    [ assert $ majorVersion ==. lit currentMajorVersion
                    , assert $ minorVersion ==. lit currentMinorVersion + 1
                    ]
                  Just majorVersion' ->
                    [ assert $ majorVersion `elem_` lit [currentMajorVersion, majorVersion']
                    , ifElse
                        (lit currentMajorVersion <. majorVersion)
                        (minorVersion ==. 0)
                        (minorVersion ==. lit currentMinorVersion + 1)
                    ]
        ]
    )
    -- TreasuryWithdrawals
    ( branch $ \withdrawMap policy ->
        [ forAll (dom_ withdrawMap) $ \rewAcnt ->
            match rewAcnt $ \net _ -> net ==. lit Testnet
        , assert $ policy ==. lit gePPolicy
        ]
    )
    -- NoConfidence
    ( branch $ \mPrevActionId ->
        -- TODO: this should be any committee purpose in the tree
        [ assert $ mPrevActionId ==. lit (gePrevGovActionIds ^. grCommitteeL)
        ]
    )
    -- UpdateCommittee
    ( branch $ \mPrevActionId _removed added _quorum ->
        -- TODO: this should be any committee purpose in the tree
        [ assert $ mPrevActionId ==. lit (gePrevGovActionIds ^. grCommitteeL)
        , forAll (rng_ added) $ \epoch ->
            lit geEpoch <. epoch
        ]
    )
    -- NewConstitution
    ( branch $ \mPrevActionId _c ->
        -- TODO: this should be any constitution purpose in the tree
        [ assert $ mPrevActionId ==. lit (gePrevGovActionIds ^. grConstitutionL)
        ]
    )
    -- InfoAction
    (branch $ \_ -> True)

wfPParamsUpdate ::
  IsConwayUniv fn =>
  Term fn (PParamsUpdate (ConwayEra StandardCrypto)) ->
  Pred fn
wfPParamsUpdate ppu =
  toPred
    [ assert $ ppu /=. lit emptyPParamsUpdate
    , match ppu $
        \_cppMinFeeA
         _cppMinFeeB
         cppMaxBBSize
         cppMaxTxSize
         cppMaxBHSize
         _cppKeyDeposit
         cppPoolDeposit
         _cppEMax
         _cppNOpt
         _cppA0
         _cppRho
         _cppTau
         _cppProtocolVersion
         _cppMinPoolCost
         _cppCoinsPerUTxOByte
         cppCostModels
         _cppPrices
         _cppMaxTxExUnits
         _cppMaxBlockExUnits
         cppMaxValSize
         cppCollateralPercentage
         _cppMaxCollateralInputs
         _cppPoolVotingThresholds
         _cppDRepVotingThresholds
         _cppCommitteeMinSize
         cppCommitteeMaxTermLength
         cppGovActionLifetime
         cppGovActionDeposit
         cppDRepDeposit
         _cppDRepActivity
         _cppMinFeeRefScriptCoinsPerByte ->
            [ cppMaxBBSize /=. lit (THKD $ SJust 0)
            , cppMaxTxSize /=. lit (THKD $ SJust 0)
            , cppMaxBHSize /=. lit (THKD $ SJust 0)
            , cppMaxValSize /=. lit (THKD $ SJust 0)
            , cppCollateralPercentage /=. lit (THKD $ SJust 0)
            , cppCommitteeMaxTermLength /=. lit (THKD $ SJust $ EpochInterval 0)
            , cppGovActionLifetime /=. lit (THKD $ SJust $ EpochInterval 0)
            , cppPoolDeposit /=. lit (THKD $ SJust mempty)
            , cppGovActionDeposit /=. lit (THKD $ SJust mempty)
            , cppDRepDeposit /=. lit (THKD $ SJust mempty)
            , cppCostModels ==. lit (THKD SNothing) -- NOTE: this is because the cost
            -- model generator is way too slow
            ]
    ]
