{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the GOV rule
module Test.Cardano.Ledger.Constrained.Conway.Gov where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra, hardforkConwayBootstrapPhase)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.State
import Constrained.API
import Data.Coerce
import Data.Foldable
import Data.Map qualified as Map
import Data.Set qualified as Set
import Lens.Micro
import Lens.Micro qualified as L
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.PParams

govEnvSpec ::
  Specification (GovEnv ConwayEra)
govEnvSpec = constrained $ \ge ->
  match ge $ \_ _ pp _ _ _ ->
    satisfies pp pparamsSpec

-- NOTE: it is probably OK not to check uniqueness of ids here, because a clash
-- is never going to be generated, and the real representation of `Proposals` doesn't
-- allow the id to appear twice.
govProposalsSpec ::
  GovEnv ConwayEra ->
  Specification (Proposals ConwayEra)
govProposalsSpec GovEnv {geEpoch, gePPolicy, geCertState} =
  proposalsSpec (lit geEpoch) (lit gePPolicy) (lit geCertState)

proposalsSpec ::
  Term EpochNo ->
  Term (StrictMaybe ScriptHash) ->
  Term (CertState ConwayEra) ->
  Specification (Proposals ConwayEra)
proposalsSpec geEpoch gePPolicy geCertState =
  constrained $ \ [var|props|] ->
    -- Note each of ppupTree, hardForkTree, committeeTree, constitutionTree
    -- have the pair type ProposalTree = (StrictMaybe (GovActionId StandardCrypto), [Tree GAS])
    match props $ \ [var|ppupTree|] [var|hardForkTree|] [var|committeeTree|] [var|constitutionTree|] [var|unorderedProposals|] ->
      [ -- Protocol parameter updates
        wellFormedChildren ppupTree
      , satisfies
          ppupTree
          ( allGASInTree
              ( \ [var|gasPpup|] ->
                  -- Extract the GovAction from the GovActionID and match against the constructor ParameterChange
                  [ isCon @"ParameterChange" (pProcGovAction_ . gasProposalProcedure_ $ gasPpup)
                  , onCon @"ParameterChange" (pProcGovAction_ . gasProposalProcedure_ $ gasPpup) $
                      \_ ppup policy ->
                        [ assert $ ppup /=. lit emptyPParamsUpdate
                        , satisfies ppup wfPParamsUpdateSpec
                        , assert $ policy ==. gePPolicy
                        ]
                  ]
              )
          )
      , forAll (snd_ ppupTree) (genHint treeGenHint)
      , genHint listSizeHint (snd_ ppupTree)
      , -- Hard forks
        wellFormedChildren hardForkTree
      , satisfies
          hardForkTree
          ( allGASInTree
              ( \ [var|gasHfork|] ->
                  -- Extract the GovAction from the GovActionID and match against the constructor HardForkInitiation
                  isCon @"HardForkInitiation" (pProcGovAction_ . gasProposalProcedure_ $ gasHfork)
              )
          )
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
        satisfies
          committeeTree
          ( allGASInTree
              ( \ [var|gasComm|] ->
                  -- Extract the GovAction from the GovActionID and case on it
                  caseOn
                    (pProcGovAction_ . gasProposalProcedure_ $ gasComm)
                    (branch $ \_ _ _ -> False)
                    (branch $ \_ _ -> False)
                    (branch $ \_ _ -> False)
                    (branch $ \_ -> True)
                    -- UpdateCommittee - We are only interested in this case
                    ( branch $ \_ _ added _ ->
                        forAll (rng_ added) $ \epoch ->
                          geEpoch <. epoch
                    )
                    (branch $ \_ _ -> False)
                    (branch $ \_ -> False)
              )
          )
      , forAll (snd_ committeeTree) (genHint treeGenHint)
      , genHint listSizeHint (snd_ committeeTree)
      , -- Constitution
        wellFormedChildren constitutionTree
      , satisfies
          constitutionTree
          ( allGASInTree -- Extract the GovAction from the GovActionID and match against the constructor NewConstitution
              ( \ [var|gasNewConst|] -> isCon @"NewConstitution" (pProcGovAction_ . gasProposalProcedure_ $ gasNewConst)
              )
          )
      , forAll (snd_ constitutionTree) (genHint treeGenHint)
      , genHint listSizeHint (snd_ constitutionTree)
      , -- TreasuryWithdrawals and InfoAction, Recall,  unorderedProposals :: [GAS]
        -- for each GAS in the list, match against the constructor
        forAll unorderedProposals $ \ [var|gasOther|] ->
          caseOn
            (pProcGovAction_ . gasProposalProcedure_ $ gasOther)
            (branch $ \_ _ _ -> False) -- Parameter Change
            (branch $ \_ _ -> False) -- HardForkInitiation
            -- Treasury Withdrawal
            ( branch $ \ [var|withdrawMap|] [var|policy|] ->
                explanation (pure "TreasuryWithdrawal fails") $
                  fold $
                    [ dependsOn gasOther withdrawMap
                    , match geCertState $ \_vState _pState [var|dState|] ->
                        match dState $ \ [var|accounts|] _ _ _ ->
                          reify accounts (Map.keysSet . (^. accountsMapL)) $ \ [var|registeredCredentials|] ->
                            forAll (dom_ withdrawMap) $ \ [var|rewAcnt|] ->
                              match rewAcnt $ \ [var|network|] [var|credential|] ->
                                [ network ==. lit Testnet
                                , credential `member_` registeredCredentials
                                ]
                    , assert $ policy ==. gePPolicy
                    ]
            )
            (branch $ \_ -> False) -- NoConfidence
            (branch $ \_ _ _ _ -> False) -- Update Committee
            (branch $ \_ _ -> False) -- NewConstitution
            -- Info
            (branch $ \_ -> True) -- InfoAction
      , genHint listSizeHint unorderedProposals
      ]
  where
    treeGenHint = (Just 2, 6)
    listSizeHint = 5

allGASInTree ::
  IsPred p =>
  (Term (GovActionState ConwayEra) -> p) ->
  Specification (ProposalTree ConwayEra)
allGASInTree k = constrained $ \ [var|proposalTree|] ->
  forAll (snd_ proposalTree) $ \ [var|subtree|] ->
    forAll' subtree $ \ [var|gas|] _ ->
      k gas

allGASAndChildInTree ::
  IsPred p =>
  Term (ProposalTree ConwayEra) ->
  ( Term (GovActionState ConwayEra) ->
    Term (GovActionState ConwayEra) ->
    p
  ) ->
  Pred
allGASAndChildInTree t k =
  forAll (snd_ t) $ \ [var|subtree|] ->
    forAll' subtree $ \ [var|gas|] [var|cs|] ->
      forAll cs $ \ [var|t''|] ->
        k gas (rootLabel_ t'')

wellFormedChildren ::
  Term (ProposalTree ConwayEra) ->
  Pred
wellFormedChildren rootAndTrees =
  match rootAndTrees $ \root trees ->
    [ dependsOn rootAndTrees root
    , dependsOn rootAndTrees trees
    , forAll trees $ \t ->
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
  Term (GovActionState ConwayEra) ->
  (Term (StrictMaybe GovActionId) -> Pred) ->
  Pred
withPrevActId gas k =
  fold
    [ match (gasProposalProcedure_ gas) $ \_deposit [var|retAddr|] _action _anchor ->
        match retAddr $ \ [var|net|] _ -> [dependsOn gas net, assert $ net ==. lit Testnet]
    , caseOn
        (pProcGovAction_ . gasProposalProcedure_ $ gas)
        -- ParameterChange
        ( branch $ \mPrevActionId _ _ ->
            caseOn
              mPrevActionId
              (branch $ \_ -> k cSNothing_)
              (branch $ \i -> k (cSJust_ $ toGeneric_ i))
        )
        -- HardForkInitiation
        ( branch $ \mPrevActionId _ ->
            caseOn
              mPrevActionId
              (branch $ \_ -> k cSNothing_)
              (branch $ \i -> k (cSJust_ $ toGeneric_ i))
        )
        -- TreasuryWithdrawals
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
    ]

onHardFork ::
  IsPred p =>
  Term (GovActionState ConwayEra) ->
  ( Term (StrictMaybe (GovPurposeId 'HardForkPurpose)) ->
    Term ProtVer ->
    p
  ) ->
  Pred
onHardFork gas k = onCon @"HardForkInitiation" (pProcGovAction_ . gasProposalProcedure_ $ gas) k

govProceduresSpec ::
  GovEnv ConwayEra ->
  Proposals ConwayEra ->
  Specification (GovSignal ConwayEra)
govProceduresSpec ge@GovEnv {..} ps =
  let actions f =
        [ gid
        | (gid, act) <- Map.toList $ proposalsActionsMap ps
        , geEpoch <= act ^. gasExpiresAfterL
        , f (gasAction act)
        ]
      committeeState = geCertState ^. certVStateL . vsCommitteeStateL
      knownDReps = Map.keysSet $ geCertState ^. certVStateL . vsDRepsL
      knownStakePools = Map.keysSet $ geCertState ^. certPStateL . psStakePoolsL
      knownCommitteeAuthorizations = authorizedHotCommitteeCredentials committeeState
      maxVoters = sum [length knownCommitteeAuthorizations, length knownDReps, length knownStakePools]
      committeeVotableActionIds =
        actions (isCommitteeVotingAllowed geEpoch committeeState)
      drepVotableActionIds =
        actions isDRepVotingAllowed
      stakepoolVotableActionIds =
        actions isStakePoolVotingAllowed
      registeredCredentials = Map.keysSet $ geCertState ^. certDStateL . accountsL . accountsMapL
   in constrained $ \govSignal ->
        match govSignal $ \votingProcs proposalProcs _certificates ->
          [ match votingProcs $ \votingProcsMap ->
              [ assert $ sizeOf_ votingProcsMap <=. lit (toInteger maxVoters)
              , forAll votingProcsMap $ \kvp ->
                  match kvp $ \voter mapActVote ->
                    (caseOn voter)
                      ( branch $ \committeeHotCred ->
                          [ subset_ (dom_ mapActVote) (lit $ Set.fromList committeeVotableActionIds)
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
              ]
          , forAll proposalProcs $ \proc ->
              match proc $ \deposit returnAddr govAction _ ->
                [ assert $ deposit ==. lit (gePParams ^. ppGovActionDepositL)
                , match returnAddr $ \net cred ->
                    [ dependsOn proc net
                    , assert $ net ==. lit Testnet
                    , assert $ cred `member_` lit registeredCredentials
                    ]
                , wfGovAction ge ps govAction
                ]
          ]

wfGovAction ::
  GovEnv ConwayEra ->
  Proposals ConwayEra ->
  Term (GovAction ConwayEra) ->
  Pred
wfGovAction GovEnv {gePPolicy, geEpoch, gePParams, geCertState} ps govAction =
  caseOn
    govAction
    -- ParameterChange
    ( branch $ \mPrevActionId ppUpdate policy ->
        [ assert $ mPrevActionId `elem_` lit ppupIds
        , assert $ ppUpdate /=. lit emptyPParamsUpdate
        , satisfies ppUpdate wfPParamsUpdateSpec
        , assert $ policy ==. lit gePPolicy
        ]
    )
    -- HardForkInitiation
    ( branch $ \ [var|mPrevActionId|] [var|protVer|] ->
        [ assert $ mPrevActionId `elem_` lit hardForkIds
        , match protVer $ \ [var|majorVersion|] [var|minorVersion|] ->
            reify mPrevActionId hfIdMajorVer $ \ [var|pvpair|] -> match pvpair $ \ [var|currentMajorVersion|] [var|mSuccMajorVersion|] ->
              reify mPrevActionId hfIdMinorVer $ \ [var|currentMinorVersion|] ->
                caseOn
                  mSuccMajorVersion
                  ( branch $ \_ ->
                      [ majorVersion ==. currentMajorVersion
                      , minorVersion ==. currentMinorVersion + 1
                      ]
                  )
                  ( branch $ \ [var|majorVersionPrime|] ->
                      [ assert $ majorVersion `member_` (singleton_ currentMajorVersion <> singleton_ majorVersionPrime)
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
            match rewAcnt $ \net cred ->
              [ net ==. lit Testnet
              , cred `member_` lit registeredCredentials
              ]
        , assert $ sum_ (rng_ withdrawMap) >. lit (Coin 0)
        , assert $ policy ==. lit gePPolicy
        , assert $ not $ hardforkConwayBootstrapPhase (gePParams ^. ppProtocolVersionL)
        ]
    )
    -- NoConfidence
    ( branch $ \mPrevActionId ->
        [ assert $ mPrevActionId `elem_` lit committeeIds
        , assert $ not $ hardforkConwayBootstrapPhase (gePParams ^. ppProtocolVersionL)
        ]
    )
    -- UpdateCommittee
    ( branch $ \mPrevActionId _removed added _quorum ->
        [ assert $ mPrevActionId `elem_` lit committeeIds
        , forAll (rng_ added) $ \epoch ->
            lit geEpoch <. epoch
        , assert $ not $ hardforkConwayBootstrapPhase (gePParams ^. ppProtocolVersionL)
        ]
    )
    -- NewConstitution
    ( branch $ \mPrevActionId _c ->
        [ assert $ mPrevActionId `elem_` lit constitutionIds
        , assert $ not $ hardforkConwayBootstrapPhase (gePParams ^. ppProtocolVersionL)
        ]
    )
    -- InfoAction
    (branch $ \_ -> True)
  where
    registeredCredentials = Map.keysSet $ geCertState ^. certDStateL . accountsL . accountsMapL
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

wfPParamsUpdateSpec :: Specification (PParamsUpdate ConwayEra)
wfPParamsUpdateSpec =
  constrained' $ \ppupdate ->
    -- Note that ppupdate :: SimplePPUpdate
    match ppupdate $
      \_minFeeFactor
       _minFeeConstant
       maxBBSize
       maxTxSize
       maxBHSize
       _keyDeposit
       poolDeposit
       _eMax
       _nOpt
       _a0
       _rho
       _tau
       _decentral
       _protocolVersion
       _minUTxOValue
       _minPoolCost
       -- Alonzo
       _coinsPerUTxOWord
       costModels
       _prices
       _maxTxExUnits
       _maBlockExUnits
       maxValSize
       collateralPercentage
       _MaxCollateralInputs
       -- Babbage
       coinsPerUTxOByte
       -- Conway
       _poolVotingThresholds
       _drepVotingThresholds
       _committeeMinSize
       committeeMaxTermLength
       govActionLifetime
       govActionDeposit
       dRepDeposit
       _drepActivity
       _minFeeRefScriptCostPerByte ->
          [ maxBBSize /=. lit (SJust 0)
          , maxTxSize /=. lit (SJust 0)
          , maxBHSize /=. lit (SJust 0)
          , maxValSize /=. lit (SJust 0)
          , collateralPercentage /=. lit (SJust 0)
          , committeeMaxTermLength /=. lit (SJust $ EpochInterval 0)
          , govActionLifetime /=. lit (SJust $ EpochInterval 0)
          , poolDeposit /=. lit (SJust mempty)
          , govActionDeposit /=. lit (SJust mempty)
          , dRepDeposit /=. lit (SJust mempty)
          , costModels ==. lit SNothing -- NOTE: this is because the cost
          , coinsPerUTxOByte /=. lit (SJust mempty)
          -- model generator is way too slow
          ]
