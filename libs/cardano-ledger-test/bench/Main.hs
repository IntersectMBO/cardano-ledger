module Main where

import qualified Bench.Cardano.Ledger.ApplyTx as ApplyTx
import qualified Bench.Cardano.Ledger.Balance as Balance
import qualified Bench.Cardano.Ledger.EpochBoundary as Epoch
import qualified Bench.Cardano.Ledger.Serialisation.Generators as SerGen
import qualified Bench.Cardano.Ledger.SumStake as SumStake
import qualified Bench.Cardano.Ledger.TxOut as TxOut
import Criterion.Main (defaultMain)

main :: IO ()
main =
  defaultMain
    [ TxOut.benchTxOut,
      SerGen.benchTxGeneration,
      ApplyTx.applyTxBenchmarks,
      Epoch.aggregateUtxoBench,
      SumStake.sumStakeBenchmarks,
      Balance.balanceBenchmarks
    ]
