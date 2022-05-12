{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad
import Criterion.Main
import Data.Map.Strict as Map
import Data.VMap as VMap
import System.Random.Stateful as Random

type Key = Int

main :: IO ()
main = do
  let std1 = mkStdGen 2021
      n = 200000
      randList :: StdGen -> [(Key, Int)]
      randList gen = runStateGen_ gen (uniformListM n)
      prefixList :: StdGen -> [(Key, Int)]
      prefixList gen =
        runStateGen_ gen $ \g ->
          replicateM n $ do
            key <- uniformM g
            val <- uniformM g
            pure (key, val)
      sequentialList :: StdGen -> [(Key, Int)]
      sequentialList gen =
        runStateGen_ gen $ \g -> do
          forM [1 .. fromIntegral n] $ \k -> do
            val <- uniformM g
            pure (k, val)
  defaultMain
    [ bgroup
        "fromList"
        [ fromListBench "uniform" (randList std1),
          fromListBench "prefixed" (prefixList std1),
          fromListBench "sequential" (sequentialList std1)
        ],
      bgroup
        "toList"
        [ toListBench "uniform" (randList std1),
          toListBench "prefixed" (prefixList std1),
          toListBench "sequential" (sequentialList std1)
        ],
      bgroup
        "toMapThroughList"
        [ toMapThroughListBench "uniform" (randList std1),
          toMapThroughListBench "prefixed" (prefixList std1),
          toMapThroughListBench "sequential" (sequentialList std1)
        ],
      bgroup
        "fromMap"
        [ fromMapBench "uniform" (randList std1),
          fromMapBench "prefixed" (prefixList std1),
          fromMapBench "sequential" (sequentialList std1)
        ],
      bgroup
        "foldlWithKey"
        [ foldlWithKeyBench "uniform" (randList std1),
          foldlWithKeyBench "prefixed" (prefixList std1),
          foldlWithKeyBench "sequential" (sequentialList std1)
        ]
    ]

fromMapBench :: String -> [(Key, Int)] -> Benchmark
fromMapBench name xs =
  env (pure $ Map.fromList xs) $ \xsMap ->
    bgroup
      name
      [ bgroup
          "fromMap"
          [ bench "VMap" $ nf (VMap.fromMap :: Map Key Int -> VMap VB VP Key Int) xsMap
          ]
      ]

toMapThroughListBench :: String -> [(Key, Int)] -> Benchmark
toMapThroughListBench name xs =
  bgroup
    name
    [ bgroup
        "toMap"
        [ env (pure (Map.fromList xs)) $
            bench "Map" . nf (Map.fromDistinctAscList . Map.toList),
          env (pure (VMap.fromList xs :: VMap VB VP Key Int)) $
            bench "VMap" . nf VMap.toMap
        ]
    ]

toListBench :: String -> [(Key, Int)] -> Benchmark
toListBench name xs =
  bgroup
    name
    [ bgroup
        "toList"
        [ env (pure (Map.fromList xs)) $ bench "Map" . nf Map.toList,
          env (pure (VMap.fromList xs :: VMap VB VP Key Int)) $
            bench "VMap" . nf VMap.toList
        ],
      bgroup
        "keys"
        [ env (pure (Map.fromList xs)) $ bench "Map" . nf Map.keys,
          env (pure (VMap.fromList xs :: VMap VB VP Key Int)) $
            bench "VMap" . nf VMap.keys
        ],
      bgroup
        "elems"
        [ env (pure (Map.fromList xs)) $ bench "Map" . nf Map.elems,
          env (pure (VMap.fromList xs :: VMap VB VP Key Int)) $
            bench "VMap" . nf VMap.elems
        ]
    ]

fromListBench :: String -> [(Key, Int)] -> Benchmark
fromListBench name xs =
  env (pure xs) $ \xsnf ->
    bgroup
      name
      [ bench "Map" $ nf Map.fromList xsnf,
        bench "VMap" $
          nf (VMap.fromList :: [(Key, Int)] -> VMap VB VP Key Int) xsnf
      ]

foldlWithKeyBench :: String -> [(Key, Int)] -> Benchmark
foldlWithKeyBench name xs =
  bgroup
    name
    [ env (pure (Map.fromList xs)) $
        bench "Map" . nf (Map.foldlWithKey (\a !_ -> (a +)) 0),
      env (pure (VMap.fromList xs :: VMap VB VP Key Int)) $
        bench "VMap" . nf (VMap.foldlWithKey (\a !_ -> (a +)) 0)
    ]
