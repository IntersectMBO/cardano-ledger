{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Test that modifications to the calculatePoolDistr function
--   made when building the Tickf benchmarks behave the same as
--   the code that was replaced.
module Test.Cardano.Ledger.Tickf (oldCalculatePoolDistr, calcPoolDistOldEqualsNew) where

import Cardano.Ledger.BaseTypes (nonZeroOr, unNonZero)
import Cardano.Ledger.Coin (Coin (..), knownNonZeroCoin)
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool))
import Cardano.Ledger.Shelley.Rules (calculatePoolDistr)
import Cardano.Ledger.State (
  IndividualPoolStake (..),
  PoolDistr (..),
  SnapShot (..),
  Stake (..),
  StakePoolSnapShot (spssVrf),
  sumAllStake,
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
oldCalculatePoolDistr includeHash (SnapShot stake _ delegs stakePoolsSnapShot) =
  let totalActiveStake = sumAllStake stake
      nonZeroTotalActiveStake = totalActiveStake `nonZeroOr` knownNonZeroCoin @1
      withZeroStake = VMap.toMap (unStake stake) `Map.union` (mempty <$ VMap.toMap delegs)
      sd =
        Map.fromListWith (\(cc, rat) (cc', rat') -> (cc <> cc', rat + rat')) $
          [ (d, (compactCoin, c % unCoin (unNonZero nonZeroTotalActiveStake)))
          | (hk, compactCoin) <- Map.toAscList withZeroStake
          , let Coin c = fromCompact compactCoin
          , Just d <- [VMap.lookup hk delegs]
          , includeHash d
          ]
   in PoolDistr
        ( Map.intersectionWith
            (\(cc, rat) vrf -> IndividualPoolStake rat cc vrf)
            sd
            (VMap.toMap (VMap.map spssVrf stakePoolsSnapShot))
        )
        nonZeroTotalActiveStake
