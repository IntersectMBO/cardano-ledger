{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad
import Criterion.Main
import Data.Compact.KeyMap as KeyMap
import Data.Compact.ViewMap as VMap
import Data.Foldable as F
import Data.Map.Strict as Map
import System.Random.Stateful as Random

main :: IO ()
main = do
  let (std1, std2) = Random.split $ mkStdGen 2021
      n = 200000
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
        ],
      bgroup
        "union (with itself)"
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
        "intersection (with itself)"
        [ intersectionBench "uniform" (randList std1) (randList std1),
          intersectionBench "prefixed" (prefixList std1) (prefixList std1),
          intersectionBench "sequential" (sequentialList std1) (sequentialList std1)
        ],
      bgroup
        "intersection (distinct)"
        [ intersectionBench "uniform" (randList std1) (randList std2),
          intersectionBench "prefixed" (prefixList std1) (prefixList std2),
          intersectionBench "sequential" (sequentialList std1) (sequentialList std2)
        ],
      env (pure $ randList std1) $ \xs ->
        bgroup
          "insert vs union"
          [ bench "fromList" $ nf KeyMap.fromList xs,
            bench "insert" $ nf (F.foldl' (\acc (k, v) -> KeyMap.insert k v acc) KeyMap.empty) xs,
            bench "unionWith" $
              nf (F.foldl' (\acc (k, v) -> KeyMap.union (KeyMap.singleton k v) acc) KeyMap.empty) xs
          ]
    ]

fromMapBench :: String -> [(Key, Int)] -> Benchmark
fromMapBench name xs =
  env (pure $ Map.fromList xs) $ \xsMap ->
    bgroup
      name
      [ bgroup
          "fromMap"
          [ bench "KeyMap" $ nf (KeyMap.fromList . Map.toList) xsMap,
            bench "VMap" $ nf (VMap.fromMap :: Map Key Int -> VMap VB VP Key Int) xsMap
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
          env (pure (KeyMap.fromList xs)) $
            bench "KeyMap" . nf (Map.fromDistinctAscList . KeyMap.toList),
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
          env (pure (KeyMap.fromList xs)) $ bench "KeyMap" . nf KeyMap.toList,
          env (pure (VMap.fromList xs :: VMap VB VP Key Int)) $
            bench "VMap" . nf VMap.toList
        ],
      bgroup
        "keys"
        [ env (pure (Map.fromList xs)) $ bench "Map" . nf Map.keys,
          env (pure (KeyMap.fromList xs)) $ bench "KeyMap" . nf (fmap fst . KeyMap.toList),
          env (pure (VMap.fromList xs :: VMap VB VP Key Int)) $
            bench "VMap" . nf VMap.keys
        ],
      bgroup
        "elems"
        [ env (pure (Map.fromList xs)) $ bench "Map" . nf Map.elems,
          env (pure (KeyMap.fromList xs)) $ bench "KeyMap" . nf (fmap snd . KeyMap.toList),
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
        bench "KeyMap" $ nf KeyMap.fromList xsnf,
        bench "VMap" $
          nf (VMap.fromList :: [(Key, Int)] -> VMap VB VP Key Int) xsnf
      ]

foldlWithKeyBench :: String -> [(Key, Int)] -> Benchmark
foldlWithKeyBench name xs =
  bgroup
    name
    [ env (pure (Map.fromList xs)) $
        bench "Map" . nf (Map.foldlWithKey (\a !_ -> (a +)) 0),
      env (pure (KeyMap.fromList xs)) $
        bench "KeyMap" . nf (KeyMap.foldWithAscKey (\a !_ -> (a +)) 0),
      env (pure (VMap.fromList xs :: VMap VB VP Key Int)) $
        bench "VMap" . nf (VMap.foldlWithKey (\a !_ -> (a +)) 0)
    ]

unionBench :: String -> [(Key, Int)] -> [(Key, Int)] -> Benchmark
unionBench name xs1 xs2 =
  bgroup
    name
    [ env (pure (Map.fromList xs1, Map.fromList xs2)) $
        bench "Map" . nf (uncurry Map.union),
      env (pure (KeyMap.fromList xs1, KeyMap.fromList xs2)) $
        bench "KeyMap" . nf (uncurry KeyMap.union)
        -- uncomment when implemented:
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
        -- uncomment when implemented:
        -- env (pure (VMap.fromList xs1 :: VMap VB VP Key Int, VMap.fromList xs2)) $
        --   bench "VMap" . nf (uncurry VMap.intersection)
    ]
