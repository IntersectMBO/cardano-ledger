module Main where

import Criterion.Main (bench, bgroup, defaultMain, whnf)
import Test.Shelley.Spec.Ledger.BenchmarkFunctions
  ( ledgerSpendOneUTxO,
  )

-- Our benchmark harness.
main :: IO ()
main =
  defaultMain
    [ bgroup "ledger" $
        fmap
          (\n -> bench (show n) $ whnf ledgerSpendOneUTxO n)
          [50, 500, 5000, 50000]
    ]
