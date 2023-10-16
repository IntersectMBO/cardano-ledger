{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Cardano.Ledger.Constrained.V2.Conway.Gov where

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
  match ge $ \_ _ pp _ _ ->
    satisfies pp pparamsSpec

govRuleStateSpec ::
  IsConwayUniv fn =>
  GovEnv (ConwayEra StandardCrypto) ->
  Spec fn (GovRuleState (ConwayEra StandardCrypto))
govRuleStateSpec env =
  constrained $ \grs ->
    match grs $ \props _ -> satisfies props (govProposalsSpec env)

govProposalsSpec ::
  IsConwayUniv fn =>
  GovEnv (ConwayEra StandardCrypto) ->
  Spec fn (Proposals (ConwayEra StandardCrypto))
govProposalsSpec GovEnv {..} =
  constrained $ \ps ->
    assert $ rootActionsAre (gePParams ^. ppProtocolVersionL) gePrevGovActionIds ps

govProceduresSpec ::
  IsConwayUniv fn =>
  GovEnv (ConwayEra StandardCrypto) ->
  GovRuleState (ConwayEra StandardCrypto) ->
  Spec fn (GovProcedures (ConwayEra StandardCrypto))
govProceduresSpec ge@GovEnv {..} (GovRuleState ps _) =
  let actions f =
        [ (k, act)
        | (k, act) <- Map.toList $ proposalsGovActionStates ps
        , geEpoch <= act ^. gasExpiresAfterL
        , f (gasAction act)
        ]
      committeeVotableActionIds =
        map fst $ actions isCommitteeVotingAllowed
      drepVotableActionIds =
        map fst $ actions isDRepVotingAllowed
      stakepoolVotableActionIds =
        map fst $ actions isStakePoolVotingAllowed
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
                , wfGovAction ge ps govAction
                ]
          ]

-- TODO: The pointer to the previous action could also point to a
-- key in the `Proposals`
wfGovAction ::
  IsConwayUniv fn =>
  GovEnv (ConwayEra StandardCrypto) ->
  Proposals (ConwayEra StandardCrypto) ->
  Term fn (GovAction (ConwayEra StandardCrypto)) ->
  Pred fn
wfGovAction GovEnv {..} _proposals govAction =
  caseOn
    govAction
    -- ParameterChange
    --   !(StrictMaybe (PrevGovActionId 'PParamUpdatePurpose (EraCrypto era)))
    --   !(PParamsUpdate era)
    --   !(StrictMaybe (ScriptHash (EraCrypto era)))
    ( branch $ \mPrevActionId ppUpdate _ ->
        [ assert $ mPrevActionId ==. lit (pgaPParamUpdate gePrevGovActionIds)
        , wfPParamsUpdate ppUpdate
        ]
    )
    -- HardForkInitiation
    --   !(StrictMaybe (PrevGovActionId 'HardForkPurpose (EraCrypto era)))
    --   !ProtVer
    ( branch $ \mPrevActionId _protVer ->
        mPrevActionId ==. lit (pgaHardFork gePrevGovActionIds)
        -- TODO: here we need to say that the `protVer` can follow the last prot ver
    )
    -- TreasuryWithdrawals !(Map (RewardAcnt (EraCrypto era)) Coin)
    ( branch $ \withdrawMap _ ->
        forAll (dom_ withdrawMap) $ \rewAcnt ->
          match rewAcnt $ \net _ -> net ==. lit Testnet
    )
    -- NoConfidence
    --   !(StrictMaybe (PrevGovActionId 'CommitteePurpose (EraCrypto era)))
    ( branch $ \mPrevActionId ->
        mPrevActionId ==. lit (pgaCommittee gePrevGovActionIds)
    )
    -- UpdateCommittee
    --   !(StrictMaybe (PrevGovActionId 'CommitteePurpose (EraCrypto era)))
    --   !(Set (Credential 'ColdCommitteeRole (EraCrypto era)))
    --   !(Map (Credential 'ColdCommitteeRole (EraCrypto era)) EpochNo)
    --   !UnitInterval
    ( branch $ \mPrevActionId _removed added _quorum ->
        [ assert $ mPrevActionId ==. lit (pgaCommittee gePrevGovActionIds)
        , forAll (rng_ added) $ \epoch ->
            lit geEpoch <. epoch
        ]
    )
    -- NewConstitution
    --  !(StrictMaybe (PrevGovActionId 'ConstitutionPurpose (EraCrypto era)))
    --  !(Constitution era)
    ( branch $ \mPrevActionId _c ->
        mPrevActionId ==. lit (pgaConstitution gePrevGovActionIds)
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
         _cppCostModels
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
         _cppDRepActivity ->
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
            ]
    ]
