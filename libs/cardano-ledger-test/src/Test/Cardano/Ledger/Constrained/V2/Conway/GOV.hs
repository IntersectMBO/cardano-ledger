{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the GOV rule
module Test.Cardano.Ledger.Constrained.V2.Conway.GOV where

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

govProposalsSpec ::
  GovEnv (ConwayEra StandardCrypto) ->
  Spec fn (Proposals (ConwayEra StandardCrypto))
govProposalsSpec GovEnv {} = TrueSpec

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
        actions isCommitteeVotingAllowed
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
-- key in the `Proposals`
wfGovAction ::
  IsConwayUniv fn =>
  GovEnv (ConwayEra StandardCrypto) ->
  Term fn (GovAction (ConwayEra StandardCrypto)) ->
  Pred fn
wfGovAction GovEnv {..} govAction =
  caseOn
    govAction
    -- ParameterChange
    --   !(StrictMaybe (GovPurposeId 'PParamUpdatePurpose era))
    --   !(PParamsUpdate era)
    --   !(StrictMaybe (ScriptHash (EraCrypto era)))
    ( branch $ \mPrevActionId ppUpdate _ ->
        [ assert $ mPrevActionId ==. lit (gePrevGovActionIds ^. grPParamUpdateL)
        , wfPParamsUpdate ppUpdate
        ]
    )
    -- HardForkInitiation
    --   !(StrictMaybe (PrevGovActionId 'HardForkPurpose (EraCrypto era)))
    --   !ProtVer
    ( branch $ \mPrevActionId _protVer ->
        mPrevActionId ==. lit (gePrevGovActionIds ^. grHardForkL)
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
        mPrevActionId ==. lit (gePrevGovActionIds ^. grCommitteeL)
    )
    -- UpdateCommittee
    --   !(StrictMaybe (PrevGovActionId 'CommitteePurpose (EraCrypto era)))
    --   !(Set (Credential 'ColdCommitteeRole (EraCrypto era)))
    --   !(Map (Credential 'ColdCommitteeRole (EraCrypto era)) EpochNo)
    --   !UnitInterval
    ( branch $ \mPrevActionId _removed added _quorum ->
        [ assert $ mPrevActionId ==. lit (gePrevGovActionIds ^. grCommitteeL)
        , forAll (rng_ added) $ \epoch ->
            lit geEpoch <. epoch
        ]
    )
    -- NewConstitution
    --  !(StrictMaybe (PrevGovActionId 'ConstitutionPurpose (EraCrypto era)))
    --  !(Constitution era)
    ( branch $ \mPrevActionId _c ->
        mPrevActionId ==. lit (gePrevGovActionIds ^. grConstitutionL)
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
            ]
    ]
