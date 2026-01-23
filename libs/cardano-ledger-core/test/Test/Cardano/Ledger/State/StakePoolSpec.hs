{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.State.StakePoolSpec (spec) where

import Cardano.Ledger.Address (aaNetworkId)
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
      \( stakePoolParams :: StakePoolParams
         , deposit :: CompactForm Coin
         , delegs :: Set (Credential Staking)
         ) ->
          let poolId = sppId stakePoolParams
              network = aaNetworkId $ sppAccountAddress stakePoolParams
              stakePoolState = mkStakePoolState deposit delegs stakePoolParams
              stakePoolParams' = stakePoolStateToStakePoolParams poolId network stakePoolState
           in stakePoolParams === stakePoolParams'
