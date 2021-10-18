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
  let -- fp = "/home/lehins/Downloads/mainnet-utxo-2021-09-15.json"
      db = "/home/lehins/iohk/chain/ledger-state.sqlite" :: T.Text
  let cols = [Case, Max, MaxOS, Live, Allocated, GCs]
  -- utxo <- loadBinUTxO fp
  mainWith $ do
    setColumns cols
    -- action "loadLedgerState" $ do
    --   !_ <- loadLedgerState "/home/lehins/iohk/chain/mainnet/ledger-state.bin"
    --   pure ()
    --wgroup "UTxO" $ do
    --   wgroup "No TxOut" $ do
    --     io "Map TxIn ()" (foldUTxO (\ !m !(!k, _) -> Map.insert k () m) mempty) db
    --     io "IntMap (Map TxId ()" (foldUTxO (\m txin -> nestedInsertTxId m (() <$ txin)) mempty) db
      -- wgroup "With TxOut" $ do
      --   io "Map TxIn TxOut" (foldUTxO (\ !m !(!k, !v) -> Map.insert k v m) mempty) db
      --   io "IntMap (Map TxId TxOut)" (foldUTxO nestedInsertTxId mempty) db
    wgroup "LedgerState" $ do
      io "TxOut (no-sharing)" getLedgerStateNoSharing db
      io "TxOut (DState sharing)" getLedgerStateSomeSharing db
      io "TxOut' (DState+TxOut sharing)" getLedgerStateWithSharing db
    -- io "UTxO (IntMap (HashMap TxId ())" (foldUTxO nestedInsertHM' mempty) db
    -- io "UTxO (Map TxIn ())" (loadUTxO') fp
    -- io "UTxO (IntMap (Map TxId ())" loadUTxOni' fp
    -- io "UTxO (IntMap (HashMap TxId ())" loadUTxOhm' fp
