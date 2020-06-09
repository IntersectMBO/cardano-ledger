{-# LANGUAGE TypeSynonymInstances,
             FlexibleInstances,
             StandaloneDeriving
#-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.DeepSeq (NFData(rnf))
import Criterion.Main -- (bench, bgroup, defaultMain, whnf)

import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes(UTxOState)

import Test.Shelley.Spec.Ledger.BenchmarkFunctions
  ( ledgerSpendOneUTxO,
    ledgerSpendOneGivenUTxO,
    initUTxO
  )

-- deriving instance NFData UTxOState

-- given:: Integer -> Benchmark
given n  = (n,initUTxO n)
many = map given [5::Integer,50,500]

spend:: Integer -> UTxOState -> Benchmark
spend n state = bench ("given "++show n) (whnf ledgerSpendOneGivenUTxO state)


-- Our benchmark harness.
main :: IO ()
main =
  defaultMain
    [ bgroup "ledger" $
        fmap
          (\n -> bench (show n) $ whnf ledgerSpendOneUTxO n)
          [50, 500, 5000, 50000]
    ]
