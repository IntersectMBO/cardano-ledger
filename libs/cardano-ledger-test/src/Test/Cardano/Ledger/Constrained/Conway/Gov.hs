{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the GOV rule
module Test.Cardano.Ledger.Constrained.Conway.Gov where

import Cardano.Ledger.Shelley.HardForks qualified as HardForks
import Data.Foldable

import Data.Coerce

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.CertState
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams
import Cardano.Ledger.Conway.Rules
import Data.Map qualified as Map
import Data.Set qualified as Set
import Lens.Micro

import Constrained

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Crypto (StandardCrypto)
import Lens.Micro qualified as L
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.PParams

govEnvSpec ::
  IsConwayUniv fn =>
  Specification fn (GovEnv (ConwayEra StandardCrypto))
govEnvSpec = constrained $ \ge ->
  match ge $ \_ _ pp _ _ ->
    satisfies pp pparamsSpec

-- NOTE: it is probably OK not to check uniqueness of ids here, because a clash
-- is never going to be generated, and the real representation of `Proposals` doesn't
-- allow the id to appear twice.
govProposalsSpec ::
  IsConwayUniv fn =>
  GovEnv (ConwayEra StandardCrypto) ->
  Specification fn (Proposals (ConwayEra StandardCrypto))
govProposalsSpec GovEnv {geEpoch, gePPolicy} =
  constrained $ \props ->
    match props $ \ppupTree hardForkTree committeeTree constitutionTree unorderedProposals ->
      [ -- Protocol parameter updates
        wellFormedChildren ppupTree
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
        wellFormedChildren hardForkTree
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
                    ]
          ]
      , forAll (snd_ hardForkTree) (genHint treeGenHint)
      , genHint listSizeHint (snd_ hardForkTree)
      , -- Committee
        wellFormedChildren committeeTree
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
        wellFormedChildren constitutionTree
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
        k gas (rootLabel_ t'')

wellFormedChildren ::
  IsConwayUniv fn =>
  Term fn ProposalTree ->
  Pred fn
wellFormedChildren rootAndTrees =
  match rootAndTrees $ \root trees ->
    [ forAll trees $ \t ->
        [ -- Every node just below the root has the root as its parent
          withPrevActId (rootLabel_ t) (assert . (==. root))
        , -- Every node's children have the id of the node as its parent
          forAll' t $ \gas children ->
            [ forAll children $ \t' ->
                [ withPrevActId (rootLabel_ t') (assert . (==. cSJust_ (gasId_ gas)))
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
  Specification fn (GovProcedures (ConwayEra StandardCrypto))
govProceduresSpec ge@GovEnv {..} ps =
  let actions f =
        [ gid
        | (gid, act) <- Map.toList $ proposalsActionsMap ps
        , geEpoch <= act ^. gasExpiresAfterL
        , f (gasAction act)
        ]
      committeeState = vsCommitteeState (certVState geCertState)
      knownDReps = Map.keysSet $ vsDReps (certVState geCertState)
      knownStakePools = Map.keysSet $ psStakePoolParams (certPState geCertState)
      knownCommitteeAuthorizations = authorizedHotCommitteeCredentials committeeState
      committeeVotableActionIds =
        actions (isCommitteeVotingAllowed geEpoch committeeState)
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
                    ( branch $ \committeeHotCred ->
                        [subset_ (dom_ mapActVote) (lit $ Set.fromList committeeVotableActionIds)
                        , member_ committeeHotCred $ lit knownCommitteeAuthorizations
                        ]
                    )
                    ( branch $ \drepCred ->
                        [ subset_ (dom_ mapActVote) (lit $ Set.fromList drepVotableActionIds)
                        , member_ drepCred $ lit knownDReps
                        ]
                    )
                    ( branch $ \poolKeyHash ->
                        [ subset_ (dom_ mapActVote) (lit $ Set.fromList stakepoolVotableActionIds)
                        , member_ poolKeyHash $ lit knownStakePools
                        ]
                    )
          , forAll proposalProcs $ \proc ->
              match proc $ \deposit returnAddr govAction _ ->
                [ assert $ deposit ==. lit (gePParams ^. ppGovActionDepositL)
                , match returnAddr $ \net _cred ->
                    net ==. lit Testnet
                , wfGovAction ge ps govAction
                ]
          ]

wfGovAction ::
  IsConwayUniv fn =>
  GovEnv (ConwayEra StandardCrypto) ->
  Proposals (ConwayEra StandardCrypto) ->
  Term fn (GovAction (ConwayEra StandardCrypto)) ->
  Pred fn
wfGovAction GovEnv {gePPolicy, geEpoch, gePParams} ps govAction =
  caseOn
    govAction
    -- ParameterChange
    ( branch $ \mPrevActionId ppUpdate policy ->
        [ assert $ mPrevActionId `elem_` lit ppupIds
        , wfPParamsUpdate ppUpdate
        , assert $ policy ==. lit gePPolicy
        ]
    )
    -- HardForkInitiation
    ( branch $ \mPrevActionId protVer ->
        [ assert $ mPrevActionId `elem_` lit hardForkIds
        , match protVer $ \majorVersion minorVersion ->
            reify' mPrevActionId hfIdMajorVer $ \currentMajorVersion mSuccMajorVersion ->
              reify mPrevActionId hfIdMinorVer $ \currentMinorVersion ->
                caseOn
                  mSuccMajorVersion
                  ( branch $ \_ ->
                      [ majorVersion ==. currentMajorVersion
                      , minorVersion ==. currentMinorVersion + 1
                      ]
                  )
                  ( branch $ \majorVersion' ->
                      [ assert $ majorVersion `member_` (singleton_ currentMajorVersion <> singleton_ majorVersion')
                      , ifElse
                          (currentMajorVersion <. majorVersion)
                          (minorVersion ==. 0)
                          (minorVersion ==. currentMinorVersion + 1)
                      ]
                  )
        ]
    )
    -- TreasuryWithdrawals
    ( branch $ \withdrawMap policy ->
        [ forAll (dom_ withdrawMap) $ \rewAcnt ->
            match rewAcnt $ \net _ -> net ==. lit Testnet
        , assert $ policy ==. lit gePPolicy
        , assert $ not $ HardForks.bootstrapPhase (gePParams ^. ppProtocolVersionL)
        ]
    )
    -- NoConfidence
    ( branch $ \mPrevActionId ->
        [ assert $ mPrevActionId `elem_` lit committeeIds
        , assert $ not $ HardForks.bootstrapPhase (gePParams ^. ppProtocolVersionL)
        ]
    )
    -- UpdateCommittee
    ( branch $ \mPrevActionId _removed added _quorum ->
        [ assert $ mPrevActionId `elem_` lit committeeIds
        , forAll (rng_ added) $ \epoch ->
            lit geEpoch <. epoch
        , assert $ not $ HardForks.bootstrapPhase (gePParams ^. ppProtocolVersionL)
        ]
    )
    -- NewConstitution
    ( branch $ \mPrevActionId _c ->
        [ assert $ mPrevActionId `elem_` lit constitutionIds
        , assert $ not $ HardForks.bootstrapPhase (gePParams ^. ppProtocolVersionL)
        ]
    )
    -- InfoAction
    (branch $ \_ -> True)
  where
    prevGovActionIds = ps ^. pRootsL . L.to toPrevGovActionIds
    constitutionIds =
      (prevGovActionIds ^. grConstitutionL)
        : [ SJust $ coerce $ gasId gas
          | gas <- actions
          , NewConstitution {} <- [pProcGovAction $ gasProposalProcedure gas]
          ]
    committeeIds =
      (prevGovActionIds ^. grCommitteeL)
        : [ SJust $ coerce $ gasId gas
          | gas <- actions
          , isCommitteeAction (pProcGovAction $ gasProposalProcedure gas)
          ]
    ppupIds =
      (prevGovActionIds ^. grPParamUpdateL)
        : [ SJust $ coerce $ gasId gas
          | gas <- actions
          , ParameterChange {} <- [pProcGovAction $ gasProposalProcedure gas]
          ]
    hardForkIds =
      (prevGovActionIds ^. grHardForkL)
        : [ SJust $ coerce $ gasId gas
          | gas <- actions
          , HardForkInitiation {} <- [pProcGovAction $ gasProposalProcedure gas]
          ]
    isCommitteeAction UpdateCommittee {} = True
    isCommitteeAction NoConfidence {} = True
    isCommitteeAction _ = False

    findProtVer SNothing = gePParams ^. ppProtocolVersionL
    findProtVer (SJust hid) =
      case proposalsLookupId hid ps of
        Just gas
          | HardForkInitiation _ protVer <- pProcGovAction $ gasProposalProcedure gas -> protVer
        _ -> gePParams ^. ppProtocolVersionL

    hfIdMajorVer mId =
      let ProtVer currentMajorVersion _ = findProtVer (coerce mId)
       in (currentMajorVersion, succVersion @Maybe currentMajorVersion)

    hfIdMinorVer mId =
      let ProtVer _ currentMinorVersion = findProtVer (coerce mId)
       in currentMinorVersion

    actions = toList $ proposalsActions ps

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
