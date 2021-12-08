{-# LANGUAGE ScopedTypeVariables #-}

module Bench.Cardano.Ledger.SumStake (sumStakeBenchmarks) where

import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible
import Control.Monad
import Criterion
import Data.Compact.ViewMap as VMap
import Data.Foldable as F
import Data.Map.Strict as Map
import System.Random.Stateful

--------------------------------------------------------------------------------
-- Benchmark suite
--------------------------------------------------------------------------------

sumStakeBenchmarks :: Benchmark
sumStakeBenchmarks =
  let stdGen = mkStdGen 2021
      n = 100000
      stake :: [(Word, CompactForm Coin)]
      stake =
        runStateGen_ stdGen $ \gen ->
          replicateM n $ do
            cred <- uniformM gen -- Key is irrelevant for this benchmark
            stakeAmount <- CompactCoin <$> uniformRM (0, 1000) gen
            pure (cred, stakeAmount)
   in bgroup
        "sumAllStake"
        [ env (pure (Map.fromList stake)) $
            bench "sum $ Map Word CompactCoin" . nf (Map.foldl' (\a (CompactCoin c) -> a + c) 0),
          env (pure (VMap.fromList stake :: VMap VB VP Word (CompactForm Coin))) $
            bench "sum $ VMap Word CompactCoin" . nf (VMap.foldl (\a (CompactCoin c) -> a + c) 0),
          env (pure (Map.fromList stake)) $
            bench "foldMap fromCompact $ Map Word CompactCoin" . nf (F.foldMap fromCompact),
          env (pure (VMap.fromList stake :: VMap VB VP Word (CompactForm Coin))) $
            bench "foldMap fromCompact $ VMap Word CompactCoin" . nf (VMap.foldMap fromCompact),
          env (pure (Map.fromList (fmap fromCompact <$> stake))) $
            bench "fold $ Map Word Coin" . nf F.fold,
          env (pure (VMap.fromList (fmap fromCompact <$> stake) :: VMap VB VB Word Coin)) $
            bench "fold $ VMap Word Coin" . nf VMap.fold
        ]
