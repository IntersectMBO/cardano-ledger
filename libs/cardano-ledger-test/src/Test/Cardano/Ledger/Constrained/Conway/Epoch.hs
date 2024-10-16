{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the EPOCH rule
module Test.Cardano.Ledger.Constrained.Conway.Epoch where

import Test.Cardano.Ledger.Constrained.Conway.Instances
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

epochStateSpec :: Specification ConwayFn (EpochState (ConwayEra StandardCrypto))
epochStateSpec = constrained $ \ es ->
  let accountState = sel @0 es
      ledgerSTate  = sel @1 es
      snapShots    = sel @2 es
      nonMyopic    = sel @3 es
  in True

epochSignalSpec :: Specification fn EpochNo
epochSignalSpec = TrueSpec
