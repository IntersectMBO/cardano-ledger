module Main where

import qualified Bench.Cardano.Ledger.ApplyTx as ApplyTx

-- TODO: re-enable, once the benchmark is fixed
-- import qualified Bench.Cardano.Ledger.Balance as Balance
import qualified Bench.Cardano.Ledger.EpochBoundary as Epoch
import Bench.Cardano.Ledger.Incremental (slowVsIncremental)
import qualified Bench.Cardano.Ledger.Serialisation.Generators as SerGen
import qualified Bench.Cardano.Ledger.StakeDistr as StakeDistr (tickfRuleBench)
import qualified Bench.Cardano.Ledger.SumStake as SumStake
import qualified Bench.Cardano.Ledger.TxOut as TxOut
import Criterion.Main (defaultMain)

main :: IO ()
main = defaultMain [slowVsIncremental]

_main2 :: IO ()
_main2 =
  defaultMain
    [ StakeDistr.tickfRuleBench
    , TxOut.benchTxOut
    , SerGen.benchTxGeneration
    , ApplyTx.applyTxBenchmarks
    , Epoch.aggregateUtxoBench
    , SumStake.sumStakeBenchmarks
    -- Balance.balanceBenchmarks
    ]
