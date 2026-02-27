{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Test that modifications to the calculatePoolDistr function
--   made when building the Tickf benchmarks behave the same as
--   the code that was replaced.
module Test.Cardano.Ledger.Tickf (oldCalculatePoolDistr, calcPoolDistOldEqualsNew) where

import Cardano.Ledger.BaseTypes (unNonZero)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool))
import Cardano.Ledger.Shelley.Rules (calculatePoolDistr)
import Cardano.Ledger.State (
  IndividualPoolStake (..),
  PoolDistr (..),
  SnapShot (..),
  StakePoolSnapShot (spssVrf),
  StakeWithDelegation (..),
  sumAllActiveStake,
  unActiveStake,
 )
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import qualified Data.VMap as VMap
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Shelley.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()

-- =====================================

calcPoolDistOldEqualsNew :: Spec
calcPoolDistOldEqualsNew =
  describe "calculatePoolDistr" $ do
    prop "old==new" $
      withMaxSuccess 500 $ \snap ->
        counterexample "BAD" $
          oldCalculatePoolDistr (const True) snap === calculatePoolDistr snap

-- | The original version of calculatePoolDistr
oldCalculatePoolDistr :: (KeyHash StakePool -> Bool) -> SnapShot -> PoolDistr
oldCalculatePoolDistr includeHash (SnapShot activeStake _ stakePoolsSnapShot) =
  let nonZeroTotalActiveStake = sumAllActiveStake activeStake
      activeStakeMap = VMap.toMap $ unActiveStake activeStake
      sd =
        Map.fromListWith (\(cc, rat) (cc', rat') -> (cc <> cc', rat + rat')) $
          [ (d, (compactCoin, c % unCoin (unNonZero nonZeroTotalActiveStake)))
          | StakeWithDelegation nzc d <- Map.elems activeStakeMap
          , let compactCoin = unNonZero nzc
                Coin c = fromCompact compactCoin
          , includeHash d
          ]
   in PoolDistr
        ( Map.intersectionWith
            (\(cc, rat) vrf -> IndividualPoolStake rat cc vrf)
            sd
            (VMap.toMap (VMap.map spssVrf stakePoolsSnapShot))
        )
        nonZeroTotalActiveStake
