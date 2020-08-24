{-# LANGUAGE TypeApplications #-}

-- | Benchmarks for Shelley test generators.
module Shelley.Spec.Ledger.Bench.Gen where

import Data.Proxy (Proxy (..))
import Shelley.Spec.Ledger.API (Tx)
import Shelley.Spec.Ledger.LedgerState
  ( emptyDPState,
  )
import Shelley.Spec.Ledger.Coin

import Test.QuickCheck (generate)
import Test.Shelley.Spec.Ledger.BenchmarkFunctions
  ( B,
    initUTxO,
    ledgerEnv,
  )
import Test.Shelley.Spec.Ledger.Generator.Presets (genEnv)
import qualified Test.Shelley.Spec.Ledger.Generator.Utxo as GenUTxO

--TODO set this in one place (where?)
type FixedValType = Coin

-- | Benchmark generating transaction given a UTxO size.
genTx :: Integer -> IO (Tx B FixedValType)
genTx n =
  let st = (initUTxO n, emptyDPState)
      ge = genEnv (Proxy @B)
   in generate $ GenUTxO.genTx ge ledgerEnv st
