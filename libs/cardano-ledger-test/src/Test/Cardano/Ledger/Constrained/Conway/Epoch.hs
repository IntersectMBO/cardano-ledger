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
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Shelley.API.Types
import Constrained
import Data.Map.Strict
import GHC.Generics (Generic)

newtype EpochExecEnv era = EpochExecEnv
  { eeeStakeDistr :: Map (Credential 'Staking) (CompactForm Coin)
  }
  deriving (Generic, Eq, Show)

epochEnvSpec :: Specification fn (EpochExecEnv ConwayEra)
epochEnvSpec = TrueSpec

epochStateSpec :: Specification fn (EpochState ConwayEra)
epochStateSpec = TrueSpec

epochSignalSpec :: Specification fn EpochNo
epochSignalSpec = TrueSpec
