{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Bench.Cardano.Ledger.Balance (balanceBenchmarks) where

import Bench.Cardano.Ledger.ApplyTx (ShelleyBench)
import Cardano.Ledger.Coin
import Cardano.Ledger.Shelley.UTxO (sumAllValue)
import Control.DeepSeq
import Control.Monad
import Criterion
import Data.Compact.KeyMap
import Data.Compact.SplitMap as SplitMap
import Data.Foldable as F
import Data.Map.Strict as Map
import System.Random.Stateful

--------------------------------------------------------------------------------
-- Benchmark suite
--------------------------------------------------------------------------------

newtype T = T {value :: Coin}
  deriving (NFData)

balanceBenchmarks :: Benchmark
balanceBenchmarks =
  let stdGen = mkStdGen 2021
      n = 100000
      utxo :: [((Int, Key), T)]
      utxo =
        runStateGen_ stdGen $ \gen ->
          replicateM n $ do
            txIn <- (,) <$> uniformRM (0, 15) gen <*> uniformM gen
            txOut <- T . Coin <$> uniformRM (0, 1000) gen
            pure (txIn, txOut)
   in bgroup
        "balance"
        [ bgroup
            "sumAllValue"
            [ env (pure (snd <$> utxo)) $
                bench "[Coin]" . nf sumAllValueList,
              env (pure (Map.fromList utxo)) $
                bench "Map TxIn Coin" . nf sumAllValueMap,
              env (pure (SplitMap.fromList utxo)) $
                bench "SplitMap TxIn Coin" . nf sumAllValueSplitMap
            ],
          bgroup
            "foldMap"
            [ env (pure (snd <$> utxo)) $
                bench "[Coin]" . nf foldMapList,
              env (pure (Map.fromList utxo)) $
                bench "Map TxIn Coin" . nf foldMapMap,
              env (pure (SplitMap.fromList utxo)) $
                bench "SplitMap TxIn Coin" . nf foldMapSplitMap
            ],
          bgroup
            "foldMap'"
            [ env (pure (snd <$> utxo)) $
                bench "[Coin]" . nf foldMap'List,
              env (pure (Map.fromList utxo)) $
                bench "Map TxIn Coin" . nf foldMap'Map,
              env (pure (SplitMap.fromList utxo)) $
                bench "SplitMap TxIn Coin" . nf foldMap'SplitMap
            ],
          bgroup
            "foldl'"
            [ env (pure (snd <$> utxo)) $
                bench "[Coin]" . nf foldl'List,
              env (pure (Map.fromList utxo)) $
                bench "Map TxIn Coin" . nf foldl'Map,
              env (pure (SplitMap.fromList utxo)) $
                bench "SplitMap TxIn Coin" . nf foldl'SplitMap
            ]
        ]

sumAllValueList :: [T] -> Coin
sumAllValueList xs = sumAllValue @ShelleyBench xs
{-# NOINLINE sumAllValueList #-}

sumAllValueMap :: Map (Int, Key) T -> Coin
sumAllValueMap xs = sumAllValue @ShelleyBench xs
{-# NOINLINE sumAllValueMap #-}

sumAllValueSplitMap :: SplitMap (Int, Key) T -> Coin
sumAllValueSplitMap xs = sumAllValue @ShelleyBench xs
{-# NOINLINE sumAllValueSplitMap #-}

foldMapList :: [T] -> Coin
foldMapList xs = F.foldMap value xs
{-# NOINLINE foldMapList #-}

foldMapMap :: Map (Int, Key) T -> Coin
foldMapMap xs = F.foldMap value xs
{-# NOINLINE foldMapMap #-}

foldMapSplitMap :: SplitMap (Int, Key) T -> Coin
foldMapSplitMap xs = F.foldMap value xs
{-# NOINLINE foldMapSplitMap #-}

foldMap'List :: [T] -> Coin
foldMap'List xs = F.foldMap' value xs
{-# NOINLINE foldMap'List #-}

foldMap'Map :: Map (Int, Key) T -> Coin
foldMap'Map xs = F.foldMap' value xs
{-# NOINLINE foldMap'Map #-}

foldMap'SplitMap :: SplitMap (Int, Key) T -> Coin
foldMap'SplitMap xs = F.foldMap' value xs
{-# NOINLINE foldMap'SplitMap #-}

foldl'List :: [T] -> Coin
foldl'List xs = F.foldl' (\acc tx -> acc <> value tx) mempty xs
{-# NOINLINE foldl'List #-}

foldl'Map :: Map (Int, Key) T -> Coin
foldl'Map xs = F.foldl' (\acc tx -> acc <> value tx) mempty xs
{-# NOINLINE foldl'Map #-}

foldl'SplitMap :: SplitMap (Int, Key) T -> Coin
foldl'SplitMap xs = F.foldl' (\acc tx -> acc <> value tx) mempty xs
{-# NOINLINE foldl'SplitMap #-}
