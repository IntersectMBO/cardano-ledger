{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This benchmark file is a placholder for benchmarks
module Main where

import qualified Bench.Cardano.Ledger.ShelleyMA.Serialisation.Generators as SerGen
import Criterion.Main
  ( -- bench, bgroup, nf,
    defaultMain,
  )

main :: IO ()
main = defaultMain [SerGen.benchTxGeneration]
