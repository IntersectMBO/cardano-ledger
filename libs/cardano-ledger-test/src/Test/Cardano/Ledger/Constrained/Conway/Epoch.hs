{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the EPOCH rule
module Test.Cardano.Ledger.Constrained.Conway.Epoch where

import Control.Lens
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Foldable
import Data.Sequence.Strict
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway (Conway, ConwayEra)
import Cardano.Ledger.Conway.Governance
import Control.Monad.Identity
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.API.Types
import Constrained
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Test.Cardano.Ledger.Constrained.Conway.Gov
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Cardano.Ledger.Conway.Governance

newtype EpochExecEnv era = EpochExecEnv
  { eeeStakeDistr :: Map (Credential 'Staking (EraCrypto era)) (CompactForm Coin)
  }
  deriving (Generic, Eq, Show)

epochEnvSpec :: Specification fn (EpochExecEnv Conway)
epochEnvSpec = TrueSpec

epochStateSpec ::
  Term ConwayFn EpochNo ->
  Specification ConwayFn (EpochState (ConwayEra StandardCrypto))
epochStateSpec epochNo = constrained $ \ es ->
  match es $ \_accountState ledgerState _snapShots _nonMyopic ->
    match ledgerState $ \utxoState certState ->
      match utxoState $ \_utxo _deposited _fees govState _stakeDistr _donation ->
        match govState $ \ [var| proposals |] _committee constitution _curPParams _prevPParams _futPParams drepPulsingState ->
          [ match constitution $ \_ policy ->
              proposals `satisfies` proposalsSpec epochNo policy certState
          , caseOn
              drepPulsingState
              -- DRPulsing
              ( branch $ \pulser ->
                  match pulser $ \_size _stakeMap _index _stakeDistr _stakePoolDistr _drepDistr _drepState pulseEpoch _committeeState _enactState pulseProposals _proposalDeposits _poolParams ->
                    [ assert $ pulseEpoch ==. epochNo
                    , forAll pulseProposals $ \gas ->
                        match gas $ \gasId _ _ _ _ _ _ ->
                          proposalExists gasId proposals
                    -- TODO: something is wrong in this case and I haven't figured out what yet
                    , assert False
                    ]
              )
              -- DRComplete
              ( branch $ \_snap ratifyState ->
                  match ratifyState $ \enactState [var| enacted |] expired _delayed ->
                    [ forAll expired $ \[var| gasId |] ->
                        proposalExists gasId proposals
                    -- TODO: this isn't enough, we need to ensure it's at most
                    -- one of each type of action that's being enacted
                    , forAll enacted $ \govact ->
                        [ reify proposals enactableProposals $ \ enactable -> govact `elem_` enactable
                        , assert $ not_ $ gasId_ govact `member_` expired
                        ]
                    -- TODO: this is a hack to get around the todo above!
                    , assert $ sizeOf_ enacted <=. 1
                    , match enactState $
                        \ _cc _con _cpp _ppp _tr _wth prevGACTIDS ->
                          reify proposals (toPrevGovActionIds . view pRootsL) (prevGACTIDS ==.)
                    ]
              )
          ]

proposalExists ::
  Term ConwayFn (GovActionId StandardCrypto) ->
  Term ConwayFn (Proposals Conway) ->
  Pred ConwayFn
proposalExists gasId proposals =
  reify proposals proposalsActionsMap $ \actionMap ->
    gasId `member_` dom_ actionMap

epochSignalSpec :: EpochNo -> Specification ConwayFn EpochNo
epochSignalSpec curEpoch = constrained $ \e ->
  elem_ e (lit [curEpoch, succ curEpoch])

enactableProposals :: Proposals era -> [GovActionState era]
enactableProposals proposals =
  [ gact' | gact <- toList (proposalsActions proposals)
          , gact' <- withGovActionParent gact [gact]
                      $ \ _ mparent _ ->
                          case mparent of
                            SNothing -> [gact]
                            SJust (GovPurposeId gpid')
                              | isRoot gpid' proposals -> [gact]
                              | otherwise              -> []
  ]

isRoot :: GovActionId (EraCrypto era) -> Proposals era -> Bool
isRoot gid (view pRootsL -> GovRelation{..}) =
  SJust gid `elem` [getGID grPParamUpdate, getGID grHardFork, getGID grCommittee, getGID grConstitution]
  where getGID = fmap unGovPurposeId . prRoot
