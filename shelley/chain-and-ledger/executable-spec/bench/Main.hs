{-# OPTIONS_GHC -fno-warn-orphans  -Wno-unused-binds  -Wno-unused-imports #-}

module Main where

import Criterion.Main -- (bench, bgroup, defaultMain, whnf)

import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes(UTxOState)

import Test.Shelley.Spec.Ledger.BenchmarkFunctions
  ( ledgerSpendOneUTxO,
    ledgerSpendOneGivenUTxO,
    initUTxO
  )

given:: Integer -> Benchmark
given n  = env (return $ initUTxO n) (\ state -> bench ("given "++show n) (whnf ledgerSpendOneGivenUTxO state))


includes_init :: IO ()
includes_init =
  defaultMain
    [ bgroup "ledger" $
        fmap
          (\n -> bench (show n) $ whnf ledgerSpendOneUTxO n)
          [50, 500, 5000, 50000]
    ]


excludes_init :: IO ()
excludes_init =
  defaultMain
    [ bgroup "ledger" $
       [ given 50, given 500, given 5000, given 50000, given 500000]
    ]

profile :: IO ()
profile = do
  putStrLn "Enter profiling"
  let ans = ledgerSpendOneGivenUTxO (initUTxO 500000)
  putStrLn ("Exit profiling "++show ans)

main :: IO ()
main = excludes_init