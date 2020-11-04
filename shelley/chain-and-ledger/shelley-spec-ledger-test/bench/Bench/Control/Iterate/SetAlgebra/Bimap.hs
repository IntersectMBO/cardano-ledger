{-# LANGUAGE ScopedTypeVariables #-}

-- | Benchmarks for Bimap
module Bench.Control.Iterate.SetAlgebra.Bimap (fromList) where

import Control.Iterate.SetAlgebra (biMapFromAscDistinctList, biMapFromList)
import Criterion.Main
import Test.QuickCheck (arbitrary, generate)

-- | Benchmark ways to decode a bimap from a list
fromList :: Benchmark
fromList = env (generate arbitrary) $ \(lst :: [(Int, Int)]) ->
  bgroup "fromList" $
    [ bench "biMapFromList" $ nf (biMapFromList const) lst,
      bench "biMapFromAscDistinctList" $ nf biMapFromAscDistinctList lst
    ]
