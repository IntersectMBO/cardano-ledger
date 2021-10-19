{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Weigh
import Control.Monad
import Cardano.Ledger.State.UTxO
import Cardano.Ledger.State.Query
import Data.Map.Strict as Map
import qualified Data.Text as T
import Options.Applicative as O


data Opts = Opts
  { -- | Json file to use UTxO state from.
    optsUtxoJsonFile :: Maybe FilePath,
    -- | Path to the CBOR encoded NewEpochState data type, which will be used to
    -- load into sqlite database
    optsLedgerStateBinaryFile :: Maybe FilePath,
    -- | Path to Sqlite database file.
    optsSqliteDbFile :: Maybe FilePath
  }
  deriving (Show)



optsParser :: Parser Opts
optsParser =
  Opts <$>
  option
    (Just <$> str)
    (long "utxo-json" <>
     O.value Nothing <>
     help
       "Benchmark loading Json file with UTxO into memory.") <*>
  option
    (Just <$> str)
    (long "new-epoch-state-cbor" <>
     O.value Nothing <>
     help
       ("Benchmark loading CBOR encoded NewEpochState into memory.")) <*>
  option
    (Just <$> str)
    (long "new-epoch-state-sqlite" <>
     O.value Nothing <>
     help
       ("Run various benchmarks on LedgerState representations"))

main :: IO ()
main = do
  opts <-
    execParser $
    info
      (optsParser <*
       abortOption
         (ShowHelpText Nothing)
         (long "help" <> short 'h' <> help "Display this message."))
      (header "ledger-state:memory - Tool for analyzing memory consumption of ledger state")
  let cols = [Case, Max, MaxOS, Live, Allocated, GCs]
  mainWith $ do
    setColumns cols
    forM_ (optsUtxoJsonFile opts) $ \fp -> do
      wgroup "UTxO" $ do
        wgroup "No TxOut" $ do
          io "IntMap (KeyMap TxId ())" (loadJsonUTxO txIxSharingKeyMap_) fp
          io "IntMap (Map TxId ()" (loadJsonUTxO txIxSharing_) fp
          io "Map TxIn ()" (loadJsonUTxO noSharing_) fp
    forM_ (optsSqliteDbFile opts) $ \dbFpStr -> do
      let dbFp = T.pack dbFpStr
      wgroup "LedgerState" $ do
        io "DState+TxIx sharing+IntMap(KeyMap))" getLedgerStateSomeSharingKeyMap dbFp
        io "DState+TxIx sharing+KeyMap(IntMap))" getLedgerStateSomeSharingKeyMap' dbFp
        io "DState sharing" getLedgerStateSomeSharing dbFp
        io "no-sharing" getLedgerStateNoSharing dbFp
        -- io "TxOut' (DState+TxOut sharing)" getLedgerStateWithSharing dbFp

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
    -- io "UTxO (IntMap (HashMap TxId ())" (foldUTxO nestedInsertHM' mempty) db
    -- io "UTxO (Map TxIn ())" (loadUTxO') fp
    -- io "UTxO (IntMap (Map TxId ())" loadUTxOni' fp
    -- io "UTxO (IntMap (HashMap TxId ())" loadUTxOhm' fp
