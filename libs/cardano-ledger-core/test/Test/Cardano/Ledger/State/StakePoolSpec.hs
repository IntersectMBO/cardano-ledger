{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.State.StakePoolSpec (spec) where

import Cardano.Ledger.Coin
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.State
import Data.Set (Set)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()

spec :: Spec
spec = do
  describe "StakePoolState" $ do
    prop "mkStakePoolState/stakePoolStateToPoolParams round-trip" $
      \( poolParams :: PoolParams
         , deposit :: CompactForm Coin
         , delegs :: Set (Credential 'Staking)
         ) ->
          let poolId = ppId poolParams
              stakePoolState = mkStakePoolState deposit delegs poolParams
              poolParams' = stakePoolStateToPoolParams poolId stakePoolState
           in poolParams === poolParams'
