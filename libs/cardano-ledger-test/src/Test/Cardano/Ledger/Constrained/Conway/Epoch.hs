{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the EPOCH rule
module Test.Cardano.Ledger.Constrained.Conway.Epoch where

import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.Gov
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway (Conway, ConwayEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley.API.Types
import Constrained
import Data.Map.Strict
import GHC.Generics (Generic)

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
  match es $ \ _accountState ledgerState _snapShots _nonMyopic ->
    match ledgerState $ \ utxoState certState ->
      match utxoState $ \ _utxo _deposited _fees govState _stakeDistr _donation ->
        match govState $ \ proposals _committee constitution _curPParams _prevPParams _futPParams _drepPulsingState ->
          match constitution $ \_ policy ->
          proposals `satisfies` proposalsSpec epochNo policy certState

epochSignalSpec :: EpochNo -> Specification ConwayFn EpochNo
epochSignalSpec curEpoch = constrained $ \ e ->
  elem_ e (lit [curEpoch, succ curEpoch])
