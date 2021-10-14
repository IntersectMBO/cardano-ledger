{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Weigh
import Cardano.Ledger.State.UTxO
import Cardano.Ledger.State.Query
import Data.Map.Strict as Map
import qualified Data.Text as T

main :: IO ()
main = do
  let fp = "/home/lehins/Downloads/mainnet-utxo-2021-09-15.json"
      db = "/home/lehins/iohk/cardano-ledger-specs/ledger-state.sqlite" :: T.Text
  let cols = [Case, Max, MaxOS, Live, Allocated, GCs, Check]
  -- utxo <- loadBinUTxO fp
  mainWith $ do
    setColumns cols
    -- action "loadLedgerState" $ do
    --   !_ <- loadLedgerState "/home/lehins/iohk/chain/mainnet/ledger-state.bin"
    --   pure ()
    io "UTxO (Map TxIn ())" (foldUTxO (\ !m !(!k, _) -> Map.insert k () m) mempty) db
    io "UTxO (IntMap (Map TxId ())" (foldUTxO nestedInsertTxId' mempty) db
    -- io "UTxO (IntMap (HashMap TxId ())" (foldUTxO nestedInsertHM' mempty) db
    -- io "UTxO (Map TxIn ())" (loadUTxO') fp
    -- io "UTxO (IntMap (Map TxId ())" loadUTxOni' fp
    -- io "UTxO (IntMap (HashMap TxId ())" loadUTxOhm' fp
