-- | Benchmarks for the serialisation generators
module Bench.Cardano.Ledger.Serialisation.Generators (
  benchTxGeneration,
) where

import Cardano.Ledger.Core
import Criterion.Main
import Test.Cardano.Ledger.EraBuffet
import Test.Cardano.Ledger.Mary.Arbitrary ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.QuickCheck

genTxShelley :: IO (Tx ShelleyEra)
genTxShelley = generate arbitrary

-- | Generate an arbitrary Allegra transaction
genTxAllegra :: IO (Tx AllegraEra)
genTxAllegra = generate arbitrary

-- | Generate an arbitrary Mary transaction
genTxMary :: IO (Tx MaryEra)
genTxMary = generate arbitrary

benchTxGeneration :: Benchmark
benchTxGeneration =
  bgroup
    "txgen"
    [ bench "genTxShelley" (whnfIO genTxShelley)
    , bench "genTxAllegra" (whnfIO genTxAllegra)
    , bench "genTxMary" (whnfIO genTxMary)
    ]
