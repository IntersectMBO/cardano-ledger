{-# LANGUAGE BangPatterns #-}

module Main where

import Weigh
import Cardano.Ledger.State.UTxO
import Cardano.Ledger.State.Massiv

main :: IO ()
main = do
  let fp = "/home/lehins/Downloads/mainnet-utxo-2021-09-15.json"
  let cols = [Case, Max, MaxOS, Live, Allocated, GCs, Check]
  -- utxo <- loadBinUTxO fp
  mainWith $ do
    setColumns cols
    -- action "loadLedgerState" $ do
    --   !_ <- loadLedgerState "/home/lehins/iohk/chain/mainnet/ledger-state.bin"
    --   pure ()
    io "UTxO (Map TxIn ())" loadUTxO' fp
    io "UTxO (IntMap (Map TxId ())" loadUTxOni' fp
    io "UTxO (IntMap (HashMap TxId ())" loadUTxOhm' fp
