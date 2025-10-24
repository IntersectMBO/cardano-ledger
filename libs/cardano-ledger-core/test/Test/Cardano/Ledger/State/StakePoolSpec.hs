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
    prop "mkStakePoolState/stakePoolStateToStakePoolParams round-trip" $
      \(stakePoolParams :: StakePoolParams, deposit :: CompactForm Coin) ->
        let poolId = sppId stakePoolParams
            stakePoolState = mkStakePoolState deposit stakePoolParams
            stakePoolParams' = stakePoolStateToStakePoolParams poolId stakePoolState
         in stakePoolParams === stakePoolParams'
