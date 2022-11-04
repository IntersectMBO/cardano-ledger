{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Test that modifications to the calculatePoolDistr function
--   made when building the Tickf benchmarks behave the same as
--   the code that was replaced.
module Test.Cardano.Ledger.Tickf (oldCalculatePoolDistr, calcPoolDistOldEqualsNew) where

import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.EpochBoundary (SnapShot (..), Stake (..), sumAllStake)
import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool))
import Cardano.Ledger.PoolDistr (IndividualPoolStake (..), PoolDistr (..))
import Cardano.Ledger.Shelley.Rules (calculatePoolDistr)
import Cardano.Ledger.Shelley.TxBody (PoolParams (_poolVrf))
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import qualified Data.VMap as VMap
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Tasty
import Test.Tasty.QuickCheck

-- =====================================

calcPoolDistOldEqualsNew :: TestTree
calcPoolDistOldEqualsNew =
  testGroup
    "calculatePoolDistr"
    [ testProperty
        "old==new"
        ( withMaxSuccess
            500
            ( \snap ->
                counterexample
                  "BAD"
                  (oldCalculatePoolDistr @StandardCrypto (const True) snap === calculatePoolDistr snap)
            )
        )
    ]

-- | The original version of calculatePoolDistr
oldCalculatePoolDistr :: forall c. (KeyHash 'StakePool c -> Bool) -> SnapShot c -> PoolDistr c
oldCalculatePoolDistr includeHash (SnapShot stake delegs poolParams) =
  let Coin totalc = sumAllStake stake
      -- totalc could be zero (in particular when shrinking)
      nonZeroTotal = if totalc == 0 then 1 else totalc
      sd =
        Map.fromListWith (+) $
          [ (d, c % nonZeroTotal)
            | (hk, compactCoin) <- VMap.toAscList (unStake stake),
              let Coin c = fromCompact compactCoin,
              Just d <- [VMap.lookup hk delegs],
              includeHash d
          ]
   in PoolDistr $
        Map.intersectionWith
          IndividualPoolStake
          sd
          (VMap.toMap (VMap.map _poolVrf poolParams))
