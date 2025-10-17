{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the POOL rule
module Test.Cardano.Ledger.Constrained.Conway.Pool where

import Cardano.Crypto.Hash.Class qualified as Hash
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Shelley.API.Types
import Cardano.Slotting.EpochInfo qualified as EI
import Constrained.API
import Control.Monad.Identity
import Data.Map.Strict qualified as Map
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Conway.Instances.Ledger
import Test.Cardano.Ledger.Constrained.Conway.PParams (pparamsSpec)
import Test.Cardano.Ledger.Constrained.Conway.WitnessUniverse
import Test.Cardano.Ledger.Core.Utils
import Test.Cardano.Slotting.Numeric ()

currentEpoch :: SlotNo -> EpochNo
currentEpoch = runIdentity . EI.epochInfoEpoch (epochInfoPure testGlobals)

poolEnvSpec ::
  forall era.
  EraSpecPParams era =>
  WitUniv era ->
  Specification (PoolEnv era)
poolEnvSpec _univ =
  constrained $ \pe ->
    match pe $ \_ pp ->
      satisfies pp (pparamsSpec @era)

pStateSpec ::
  forall era.
  Era era =>
  WitUniv era ->
  Specification (PState era)
pStateSpec univ = constrained $ \ps ->
  match ps $ \_ stakePools futureStakePools retiring ->
    [ witness univ (dom_ stakePools)
    , witness univ (rng_ stakePools)
    , witness univ (dom_ futureStakePools)
    , witness univ (rng_ futureStakePools)
    , witness univ (dom_ retiring)
    , assertExplain (pure "dom of retiring is a subset of dom of stakePoolParams") $
        dom_ retiring `subset_` dom_ stakePools
    , forAll' (rng_ stakePools) $ \_ _ _ _ _ _ _ _ [var|d|] ->
        assertExplain (pure "all deposits are greater then (Coin 0)") $ d >=. lit 0
    , assertExplain (pure "dom of stakePoolParams is disjoint from futureStakePoolParams") $
        dom_ stakePools `disjoint_` dom_ futureStakePools
    ]

poolCertSpec ::
  forall era.
  EraSpecPParams era =>
  WitUniv era ->
  PoolEnv era ->
  PState era ->
  Specification PoolCert
poolCertSpec univ (PoolEnv e pp) ps =
  constrained $ \pc ->
    (caseOn pc)
      -- RegPool !(PoolParams c)
      ( branchW 1 $ \poolParams ->
          match poolParams $ \_ _ _ cost _ rewAccnt _ _ mMetadata _ ->
            [ witness univ poolParams
            , match rewAccnt $ \net' _ ->
                net' ==. lit Testnet
            , onJust' mMetadata $ \metadata ->
                match metadata $ \_ hashstr -> strLen_ hashstr <=. lit (maxMetaLen - 1)
            , assert $ lit (pp ^. ppMinPoolCostL) <=. cost
            ]
      )
      -- RetirePool !(KeyHash 'StakePool c) !EpochNo
      ( branchW 1 $ \keyHash epochNo ->
          [ witness univ keyHash
          , assert $ epochNo <=. lit (maxEpochNo - 1)
          , assert $ lit e <. epochNo
          , assert $ elem_ keyHash $ lit rpools
          ]
      )
  where
    EpochInterval maxEp = pp ^. ppEMaxL
    maxEpochNo = EpochNo (fromIntegral maxEp)
    rpools = Map.keys $ psStakePools ps
    maxMetaLen = fromIntegral (Hash.sizeHash ([] @HASH))
