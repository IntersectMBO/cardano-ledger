{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the EPOCH rule
module Test.Cardano.Ledger.Constrained.Conway.Epoch where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Shelley.API.Types
import Constrained.API
import Data.Foldable
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import Lens.Micro.Extras
import Test.Cardano.Ledger.Constrained.Conway.Gov
import Test.Cardano.Ledger.Constrained.Conway.Instances

data EpochExecEnv era = EpochExecEnv
  { eeeStakeDistr :: Map (Credential Staking) (CompactForm Coin)
  , eeeEpochNo :: EpochNo
  }
  deriving (Generic, Eq, Show)

epochEnvSpec :: Specification (EpochExecEnv ConwayEra)
epochEnvSpec = trueSpec

epochStateSpec ::
  Term EpochNo ->
  Specification (EpochState ConwayEra)
epochStateSpec epochNo = constrained $ \es ->
  match es $ \_accountState ledgerState _snapShots _nonMyopic ->
    match ledgerState $ \utxoState certState ->
      match utxoState $ \_utxo _deposited _fees govState _stakeDistr _donation ->
        match govState $ \ [var|proposals|] _committee constitution _curPParams _prevPParams _futPParams drepPulsingState ->
          [ match constitution $ \_ policy ->
              proposals `satisfies` proposalsSpec epochNo policy certState
          , caseOn
              drepPulsingState
              -- DRPulsing
              ( branch $ \pulser ->
                  match pulser $ \_size _stakeMap _index _stakeDistr _stakePoolDistr _drepDistr _drepState pulseEpoch _committeeState _enactState pulseProposals _proposalDeposits _poolParams ->
                    [ assert $ pulseEpoch ==. epochNo
                    , forAll pulseProposals $ \gas ->
                        match gas $ \gasIdPulse _ _ _ _ _ _ ->
                          proposalExists gasIdPulse proposals
                    , -- TODO: something is wrong in this case and I haven't figured out what yet
                      assert False
                    ]
              )
              -- DRComplete
              ( branch $ \_snap ratifyState ->
                  match ratifyState $ \enactState [var|enacted|] [var|expired|] _delayed ->
                    [ -- This may seem strange, but if expired is too big, the 'forAll expired' will fail
                      -- There can't exist more expired proposals, than there are proposals.
                      reify proposals (toInteger . proposalsSize) $ \ [var|sz|] ->
                        [ ifElse
                            (sz <=. 0)
                            (expired ==. mempty)
                            (sizeOf_ expired <. sz)
                        ]
                    , assert $ sizeOf_ expired <=. 30
                    , forAll expired $ \ [var| gasIdExpire |] ->
                        proposalExists gasIdExpire proposals
                    , -- TODO: this isn't enough, we need to ensure it's at most
                      -- one of each type of action that's being enacted
                      forAll enacted $ \ [var|govact|] ->
                        [ reify proposals enactableProposals $ \ [var|enactable|] -> govact `elem_` enactable
                        , assert $ not_ $ gasId_ govact `member_` expired
                        ]
                    , -- TODO: this is a hack to get around the todo above!
                      assert $ sizeOf_ enacted <=. 1
                    , match enactState $
                        \_cc _con _cpp _ppp _tr _wth prevGACTIDS ->
                          reify proposals (toPrevGovActionIds . view pRootsL) (prevGACTIDS ==.)
                    ]
              )
          ]

proposalExists ::
  Term GovActionId ->
  Term (Proposals ConwayEra) ->
  Pred
proposalExists gasId proposals =
  reify proposals proposalsActionsMap $ \ [var|actionMap|] ->
    gasId `mapMember_` actionMap

epochSignalSpec :: EpochNo -> Specification EpochNo
epochSignalSpec curEpoch = constrained $ \e ->
  elem_ e (lit [curEpoch, succ curEpoch])

enactableProposals :: Proposals era -> [GovActionState era]
enactableProposals proposals =
  [ gact'
  | gact <- toList (proposalsActions proposals)
  , gact' <- withGovActionParent gact [gact] $
      \_ mparent _ ->
        case mparent of
          SNothing -> [gact]
          SJust (GovPurposeId gpid')
            | isRoot gpid' proposals -> [gact]
            | otherwise -> []
  ]

isRoot :: GovActionId -> Proposals era -> Bool
isRoot gid (view pRootsL -> GovRelation {..}) =
  SJust gid
    `elem` [getGID grPParamUpdate, getGID grHardFork, getGID grCommittee, getGID grConstitution]
  where
    getGID = fmap unGovPurposeId . prRoot
