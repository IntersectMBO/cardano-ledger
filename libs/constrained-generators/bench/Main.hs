module Main where

import Constrained.Bench
import Criterion.Main (defaultMain)

main :: IO ()
main = defaultMain [benchmarks]
