{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the POOL rule
module Test.Cardano.Ledger.Constrained.Conway.Pool where

import Cardano.Crypto.Hash.Class qualified as Hash
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.CertState
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Crypto (Crypto (..), StandardCrypto)
import Cardano.Ledger.Shelley.API.Types
import Cardano.Slotting.EpochInfo qualified as EI
import Constrained
import Control.Monad.Identity
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.PParams (pparamsSpec)
import Test.Cardano.Ledger.Core.Utils
import Test.Cardano.Slotting.Numeric ()

currentEpoch :: SlotNo -> EpochNo
currentEpoch = runIdentity . EI.epochInfoEpoch (epochInfoPure testGlobals)

poolEnvSpec ::
  IsConwayUniv fn =>
  Specification fn (PoolEnv (ConwayEra StandardCrypto))
poolEnvSpec =
  constrained $ \pe ->
    match pe $ \_ pp ->
      satisfies pp pparamsSpec

pStateSpec ::
  IsConwayUniv fn =>
  Specification fn (PState (ConwayEra StandardCrypto))
pStateSpec = constrained $ \ps ->
  match ps $ \stakePoolParams futureStakePoolParams retiring deposits ->
    [ assertExplain (pure "dom of retiring is a subset of dom of stakePoolParams") $
        dom_ retiring `subset_` dom_ stakePoolParams
    , assertExplain (pure "dom of deposits is dom of stakePoolParams") $
        dom_ deposits ==. dom_ stakePoolParams
    , assertExplain (pure "no deposit is 0") $
        not_ $
          lit (Coin 0) `elem_` rng_ deposits
    , assertExplain (pure "dom of stakePoolParams is disjoint from futureStakePoolParams") $
        dom_ stakePoolParams `disjoint_` dom_ futureStakePoolParams
    ]

poolCertSpec ::
  IsConwayUniv fn =>
  PoolEnv (ConwayEra StandardCrypto) ->
  PState (ConwayEra StandardCrypto) ->
  Specification fn (PoolCert StandardCrypto)
poolCertSpec (PoolEnv _s pp) _ps =
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
      --
      -- TODO: We do not generate this certificate because the two predicate
      -- failures from Shelley are not yet implemented in the spec.
      --
      --   ( branch $ \keyHash epochNo -> False
      --       [ epochNo <=. lit (maxEpochNo - 1)
      --       , lit (currentEpoch s) <. epochNo
      --       , elem_ keyHash $ lit rpools
      --       ]
      --   )
      -- where
      --   EpochInterval maxEp = pp ^. ppEMaxL
      --   maxEpochNo = EpochNo (fromIntegral maxEp)
      --   rpools = Map.keys $ psStakePoolParams ps
      (branch $ \_ _ -> False)
  where
    maxMetaLen = fromIntegral (Hash.sizeHash ([] @(HASH StandardCrypto)))
