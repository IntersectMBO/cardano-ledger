{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad
import Criterion.Main
--import Data.Compact.HashMap as HMap
import Data.Compact.KeyMap as KeyMap
import Data.Compact.VMap as VMap
import Data.Map.Strict as Map
import System.Random.Stateful as Random

main :: IO ()
main = do
  let !(std1, std2) = Random.split $ mkStdGen 2021
      n = 10000
      randList :: StdGen -> [(Key, Int)]
      randList gen = runStateGen_ gen (uniformListM n)
      prefixList :: StdGen -> [(Key, Int)]
      prefixList gen =
        runStateGen_ gen $ \g ->
          replicateM n $ do
            key <- Key 0 0 0 <$> uniformM g
            val <- uniformM g
            pure (key, val)
      sequentialList :: StdGen -> [(Key, Int)]
      sequentialList gen =
        runStateGen_ gen $ \g -> do
          (p1, p2, p3) <- uniformM g
          forM [1 .. fromIntegral n] $ \k -> do
            val <- uniformM g
            pure (Key p1 p2 p3 k, val)
  defaultMain
    [ bgroup
        "fromList"
        [ fromListBench "uniform" (randList std1),
          fromListBench "prefixed" (prefixList std1),
          fromListBench "sequential" (sequentialList std1)
        ],
      bgroup
        "union (same)"
        [ unionBench "uniform" (randList std1) (randList std1),
          unionBench "prefixed" (prefixList std1) (prefixList std1),
          unionBench "sequential" (sequentialList std1) (sequentialList std1)
        ],
      bgroup
        "union (distinct)"
        [ unionBench "uniform" (randList std1) (randList std2),
          unionBench "prefixed" (prefixList std1) (prefixList std2),
          unionBench "sequential" (sequentialList std1) (sequentialList std2)
        ],
      bgroup
        "intersection (same)"
        [ intersectionBench "uniform" (randList std1) (randList std1),
          intersectionBench "prefixed" (prefixList std1) (prefixList std1),
          intersectionBench "sequential" (sequentialList std1) (sequentialList std1)
        ],
      bgroup
        "intersection (distinct)"
        [ intersectionBench "uniform" (randList std1) (randList std2),
          intersectionBench "prefixed" (prefixList std1) (prefixList std2),
          intersectionBench "sequential" (sequentialList std1) (sequentialList std2)
        ]
    ]

fromListBench :: String -> [(Key, Int)] -> Benchmark
fromListBench name xs =
  env (pure xs) $ \xsnf ->
    bgroup
      name
      [ bench "Map" $ nf Map.fromList xsnf,
        bench "KeyMap" $ nf KeyMap.fromList xsnf,
        bench "VMap" $
          nf (VMap.fromList :: [(Key, Int)] -> VMap VB VP Key Int) xsnf
      ]

unionBench :: String -> [(Key, Int)] -> [(Key, Int)] -> Benchmark
unionBench name xs1 xs2 =
  bgroup
    name
    [ env (pure (Map.fromList xs1, Map.fromList xs2)) $
        bench "Map" . nf (uncurry Map.union),
      env (pure (KeyMap.fromList xs1, KeyMap.fromList xs2)) $
        bench "KeyMap" . nf (uncurry KeyMap.union)
      -- env (pure (VMap.fromList xs1 :: VMap VB VP Key Int, VMap.fromList xs2)) $
      --   bench "VMap" . nf (uncurry VMap.union)
    ]


intersectionBench :: String -> [(Key, Int)] -> [(Key, Int)] -> Benchmark
intersectionBench name xs1 xs2 =
  bgroup
    name
    [ env (pure (Map.fromList xs1, Map.fromList xs2)) $
        bench "Map" . nf (uncurry Map.intersection),
      env (pure (KeyMap.fromList xs1, KeyMap.fromList xs2)) $
        bench "KeyMap" . nf (uncurry KeyMap.intersection)
      -- env (pure (VMap.fromList xs1 :: VMap VB VP Key Int, VMap.fromList xs2)) $
      --   bench "VMap" . nf (uncurry VMap.intersection)
    ]
