module Main where

import qualified Bench.Cardano.Ledger.ApplyTx as ApplyTx

-- TODO: re-enable, once the benchmark is fixed
-- import qualified Bench.Cardano.Ledger.Balance as Balance
import qualified Bench.Cardano.Ledger.EpochBoundary as Epoch
import qualified Bench.Cardano.Ledger.Serialisation.Generators as SerGen
import qualified Bench.Cardano.Ledger.StakeDistr as StakeDistr (tickfRuleBench)
import qualified Bench.Cardano.Ledger.SumStake as SumStake
import qualified Bench.Cardano.Ledger.TxOut as TxOut
import qualified Bench.Constrained.STS as ConstrainedSTS
import Criterion.Main (defaultMain)

main :: IO ()
main =
  defaultMain
    [ StakeDistr.tickfRuleBench
    , TxOut.benchTxOut
    , SerGen.benchTxGeneration
    , ApplyTx.applyTxBenchmarks
    , Epoch.aggregateUtxoBench
    , SumStake.sumStakeBenchmarks
    , ConstrainedSTS.stsBenchmarks
    -- Balance.balanceBenchmarks
    ]
