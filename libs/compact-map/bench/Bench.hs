{-# LANGUAGE BangPatterns #-}
module Main where

import Criterion.Main
import Control.Monad
import Data.Compact.KeyMap as KeyMap
import Data.Compact.HashMap as HMap
import Data.Compact.VMap as VMap
import Data.Map.Strict as Map
import System.Random.Stateful

main :: IO ()
main = do
  let !stdGen = mkStdGen 2021
      n = 10000
      randList :: [(Key, Int)]
      randList = runStateGen_ stdGen (uniformListM n)
      zeroPrefixList :: [(Key, Int)]
      zeroPrefixList = runStateGen_ stdGen $ \g ->
        replicateM n $ do
          key <- Key 0 0 0 <$> uniformM g
          val <- uniformM g
          pure (key, val)
  defaultMain
    [ env (pure randList) $ \rxsnf ->
        bgroup
          "uniform"
          [ bgroup
              "fromList"
              [ bench "Map" $ nf Map.fromList rxsnf
              , bench "KeyMap" $ nf KeyMap.fromList rxsnf
              , bench "HashMap" $ nf HMap.fromList rxsnf
              , bench "VMap" $
                nf (VMap.fromList :: [(Key, Int)] -> VMap VB VP Key Int) rxsnf
              ]
          ]
    , env (pure zeroPrefixList) $ \rxsnf ->
        bgroup
          "uniform"
          [ bgroup
              "fromList"
              [ bench "Map" $ nf Map.fromList rxsnf
              , bench "KeyMap" $ nf KeyMap.fromList rxsnf
              , bench "HashMap" $ nf HMap.fromList rxsnf
              , bench "VMap" $
                nf (VMap.fromList :: [(Key, Int)] -> VMap VB VP Key Int) rxsnf
              ]
          ]
    ]
