{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the POOL rule
module Test.Cardano.Ledger.Constrained.V2.Conway.POOL where

import Cardano.Crypto.Hash.Class qualified as Hash
import Cardano.Ledger.Api
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.CertState
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Shelley.API.Types
import Cardano.Slotting.EpochInfo qualified as EI
import Control.Monad.Identity
import Data.Map qualified as Map
import Lens.Micro

import Constrained

import Test.Cardano.Ledger.Constrained.V2.Conway
import Test.Cardano.Ledger.Constrained.V2.Conway.PParams

import Test.Cardano.Ledger.Core.Utils

currentEpoch :: SlotNo -> EpochNo
currentEpoch = runIdentity . EI.epochInfoEpoch (epochInfoPure testGlobals)

poolEnvSpec ::
  IsConwayUniv fn =>
  Spec fn (PoolEnv (ConwayEra StandardCrypto))
poolEnvSpec =
  constrained $ \pe ->
    match pe $ \_ pp ->
      satisfies pp pparamsSpec

pStateSpec ::
  IsConwayUniv fn =>
  Spec fn (PState (ConwayEra StandardCrypto))
pStateSpec = constrained $ \ps ->
  match ps $ \stakePoolParams futureStakePoolParams retiring deposits ->
    [ assertExplain ["dom of retiring is a subset of dom of stakePoolParams"] $
        dom_ retiring `subset_` dom_ stakePoolParams
    , assertExplain ["dom of deposits is dom of stakePoolParams"] $
        dom_ deposits ==. dom_ stakePoolParams
    , assertExplain ["no deposit is 0"] $
        not_ $
          lit (Coin 0) `elem_` rng_ deposits
    , assertExplain ["dom of stakePoolParams is disjoint from futureStakePoolParams"] $
        dom_ stakePoolParams `disjoint_` dom_ futureStakePoolParams
    ]

poolCertSpec ::
  IsConwayUniv fn =>
  PoolEnv (ConwayEra StandardCrypto) ->
  PState (ConwayEra StandardCrypto) ->
  Spec fn (PoolCert StandardCrypto)
poolCertSpec (PoolEnv s pp) ps =
  constrained $ \pc ->
    (caseOn pc)
      -- RegPool !(PoolParams c)
      ( branch $ \poolParams ->
          match poolParams $ \_ _ _ cost _ rewAccnt _ _ mMetadata ->
            [ match rewAccnt $ \net' _ ->
                net' ==. lit Testnet
            , onJust' mMetadata $ \metadata ->
                match metadata $ \_ hash -> strLen_ hash <=. lit (maxMetaLen - 1)
            , assert $ lit (pp ^. ppMinPoolCostL) <=. cost
            ]
      )
      -- RetirePool !(KeyHash 'StakePool c) !EpochNo
      ( branch $ \keyHash epochNo ->
          [ epochNo <=. lit (maxEpochNo - 1)
          , lit (currentEpoch s) <. epochNo
          , elem_ keyHash $ lit rpools
          ]
      )
  where
    EpochInterval maxEp = pp ^. ppEMaxL
    maxEpochNo = EpochNo (fromIntegral maxEp)
    rpools = Map.keys $ psStakePoolParams ps
    maxMetaLen = fromIntegral (Hash.sizeHash ([] @(HASH StandardCrypto)))
