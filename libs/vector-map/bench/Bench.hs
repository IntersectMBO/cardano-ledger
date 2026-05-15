{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad
import Criterion.Main
import Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.VMap as VMap
import System.Random.Stateful as Random

type Key = Int

main :: IO ()
main = do
  let std1 = mkStdGen 2021
      n = 200000
      randList :: StdGen -> [(Key, Int)]
      randList gen = runStateGen_ gen (uniformListM n)
      duplicatedList :: StdGen -> [(Key, Int)]
      duplicatedList gen =
        replicate n $ runStateGen_ gen uniformM
      sequentialList :: StdGen -> [(Key, Int)]
      sequentialList gen =
        runStateGen_ gen $ \g -> do
          forM [1 .. fromIntegral n] $ \k -> do
            val <- uniformM g
            pure (k, val)
  defaultMain
    [ bgroup
        "fromList"
        [ fromListBench "uniform" (randList std1)
        , fromListBench "sequential" (sequentialList std1)
        , fromListBench "duplicated" (duplicatedList std1)
        ]
    , lookupBench (randList std1)
    , toListBench (randList std1)
    , toMapThroughListBench (randList std1)
    , fromMapBench (randList std1)
    , foldlWithKeyBench (randList std1)
    , mapMaybeWithKeyBench (randList std1)
    ]

fromMapBench :: [(Key, Int)] -> Benchmark
fromMapBench xs =
  env (pure $ Map.fromList xs) $ \xsMap ->
    bgroup
      "fromMap"
      [ bench "VMap" $ nf (VMap.fromMap :: Map Key Int -> VMap VB VP Key Int) xsMap
      ]

-- This benchmark compares `toMap` implementation to a similar one if it was written for `Map`
toMapThroughListBench :: [(Key, Int)] -> Benchmark
toMapThroughListBench xs =
  bgroup
    "toMapThroughList"
    [ bgroup
        "toMap"
        [ env (pure (Map.fromList xs)) $
            bench "Map" . nf (Map.fromDistinctAscList . Map.toList)
        , env (pure (VMap.fromList xs :: VMap VB VP Key Int)) $
            bench "VMap" . nf VMap.toMap
        ]
    ]

toListBench :: [(Key, Int)] -> Benchmark
toListBench xs =
  bgroup
    "toList"
    [ bgroup
        "toList"
        [ env (pure (Map.fromList xs)) $ bench "Map" . nf Map.toList
        , env (pure (VMap.fromList xs :: VMap VB VP Key Int)) $
            bench "VMap" . nf VMap.toList
        ]
    , bgroup
        "keys"
        [ env (pure (Map.fromList xs)) $ bench "Map" . nf Map.keys
        , env (pure (VMap.fromList xs :: VMap VB VP Key Int)) $
            bench "VMap" . nf VMap.keys
        ]
    , bgroup
        "elems"
        [ env (pure (Map.fromList xs)) $ bench "Map" . nf Map.elems
        , env (pure (VMap.fromList xs :: VMap VB VP Key Int)) $
            bench "VMap" . nf VMap.elems
        ]
    ]

fromListBench :: String -> [(Key, Int)] -> Benchmark
fromListBench name xs =
  bgroup
    name
    [ env (pure xs) $ bench "Map.fromList" . nf Map.fromList
    , env (pure xs) $
        bench "VMap.fromList"
          . nf (VMap.fromList :: [(Key, Int)] -> VMap VB VP Key Int)
    , env (pure (length xs, xs)) $ \ ~(n, xsnf) ->
        bench "VMap.fromListN" $
          nf (VMap.fromListN n :: [(Key, Int)] -> VMap VB VP Key Int) xsnf
    ]

foldlWithKeyBench :: [(Key, Int)] -> Benchmark
foldlWithKeyBench xs =
  bgroup
    "foldlWithKey"
    [ env (pure (Map.fromList xs)) $
        bench "Map" . nf (Map.foldlWithKey' (\a !_ -> (a +)) 0)
    , env (pure (VMap.fromList xs :: VMap VB VP Key Int)) $
        bench "VMap" . nf (VMap.foldlWithKey (\a !_ -> (a +)) 0)
    ]

mapMaybeWithKeyBench :: [(Key, Int)] -> Benchmark
mapMaybeWithKeyBench xs =
  bgroup
    "mapMaybeWithKey"
    [ env (pure (Map.fromList xs)) $
        bench "Map" . nf (Map.mapMaybeWithKey f)
    , env (pure (VMap.fromList xs :: VMap VB VP Key Int)) $
        bench "VMap" . nf (VMap.mapMaybeWithKey f)
    ]
  where
    f key val = if even key then Just (key + val) else Nothing

lookupBench :: [(Key, Int)] -> Benchmark
lookupBench xs =
  bgroup
    "lookup"
    [ env (pure (Prelude.map fst xs, Map.fromList xs)) $ \ ~(ks, m) ->
        bench "Map" $ whnfIO $ mapM_ (\k -> fromJust (Map.lookup k m) `seq` pure ()) ks
    , env (pure (Prelude.map fst xs, VMap.fromList xs :: VMap VB VP Key Int)) $ \ ~(ks, m) ->
        bench "VMap" $ whnfIO $ mapM_ (\k -> fromJust (VMap.lookup k m) `seq` pure ()) ks
    ]
