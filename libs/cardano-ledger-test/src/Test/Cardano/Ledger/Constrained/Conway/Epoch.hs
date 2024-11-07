{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the EPOCH rule
module Test.Cardano.Ledger.Constrained.Conway.Epoch where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway (Conway, ConwayEra)
import Cardano.Ledger.Conway.Governance (GovActionId, Proposals, proposalsActionsMap)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.API.Types
import Constrained
import Data.Map.Strict
import GHC.Generics (Generic)
import Test.Cardano.Ledger.Constrained.Conway.Gov
import Test.Cardano.Ledger.Constrained.Conway.Instances

newtype EpochExecEnv era = EpochExecEnv
  { eeeStakeDistr :: Map (Credential 'Staking (EraCrypto era)) (CompactForm Coin)
  }
  deriving (Generic, Eq, Show)

epochEnvSpec :: Specification fn (EpochExecEnv Conway)
epochEnvSpec = TrueSpec

epochStateSpec ::
  Term ConwayFn EpochNo ->
  Specification ConwayFn (EpochState (ConwayEra StandardCrypto))
epochStateSpec epochNo = constrained $ \es ->
  match es $ \_accountState ledgerState _snapShots _nonMyopic ->
    match ledgerState $ \utxoState certState ->
      match utxoState $ \_utxo _deposited _fees govState _stakeDistr _donation ->
        match govState $ \proposals _committee constitution _curPParams _prevPParams _futPParams drepPulsingState ->
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
                    ]
              )
              -- DRComplete
              ( branch $ \_snap ratifyState ->
                  match ratifyState $ \_enactState _enacted expired _delayed ->
                    forAll expired $ \gasId ->
                      proposalExists gasId proposals
              )
          ]

proposalExists ::
  Term ConwayFn (GovActionId StandardCrypto) ->
  Term ConwayFn (Proposals Conway) ->
  Pred ConwayFn
proposalExists gasId proposals =
  reify proposals proposalsActionsMap $ \actionMap ->
    [ gasId `member_` dom_ actionMap
    ]

epochSignalSpec :: EpochNo -> Specification ConwayFn EpochNo
epochSignalSpec curEpoch = constrained $ \e ->
  elem_ e (lit [curEpoch, succ curEpoch])
