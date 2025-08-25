{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.State.StakePoolSpec (spec) where

import Cardano.Ledger.Coin
import Cardano.Ledger.State
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()

spec :: Spec
spec = do
  describe "StakePoolState" $ do
    prop "mkStakePoolState/stakePoolStateToPoolParams round-trip" $
      \(poolParams :: PoolParams, deposit :: CompactForm Coin) ->
        let poolId = ppId poolParams
            stakePoolState = mkStakePoolState deposit poolParams
            poolParams' = stakePoolStateToPoolParams poolId stakePoolState
         in poolParams === poolParams'
