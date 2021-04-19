{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This benchmark file is a placholder for benchmarks
module Main where

import qualified Bench.Cardano.Ledger.ApplyTx as ApplyTx
import qualified Bench.Cardano.Ledger.EpochBoundary as Epoch
import qualified Bench.Cardano.Ledger.Serialisation.Generators as SerGen
import Criterion.Main
  ( -- bench, bgroup, nf,
    defaultMain,
  )

main :: IO ()
main =
  defaultMain
    [ SerGen.benchTxGeneration,
      ApplyTx.applyTxBenchmarks,
      Epoch.aggregateUtxoBench
    ]
