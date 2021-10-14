{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.State.Query where

import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import qualified Cardano.Ledger.Shelley.UTxO as Shelley
import Cardano.Ledger.State.Schema
import Cardano.Ledger.State.UTxO
import qualified Cardano.Ledger.TxIn as TxIn
import Conduit
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import Database.Persist.Sqlite
import Control.Monad.Logger (NoLoggingT(..))
import Data.Text as T

insertUTxOState ::
  MonadIO m =>
  Shelley.UTxOState CurrentEra ->
  ReaderT SqlBackend m (Key UtxoState)
insertUTxOState Shelley.UTxOState {..} = do
  insert $
    UtxoState
      { utxoStateDeposited = _deposited,
        utxoStateFees = _fees,
        utxoStatePpups = _ppups
      }

insertUTxO ::
  MonadIO m =>
  Shelley.UTxO CurrentEra ->
  Key UtxoState ->
  ReaderT SqlBackend m ()
insertUTxO utxo stateKey = do
  mapM_ insertTxOut $ Map.toList (Shelley.unUTxO utxo)
  where
    insertTxOut (TxIn.TxIn txId txIx, out) = do
      txKey <-
        insert $ Tx {txInIx = fromIntegral txIx, txInId = txId, txOut = out}
      insert_ $ UtxoEntry {utxoEntryTx = txKey, utxoEntryState = stateKey}

insertLedgerState ::
  MonadIO m => Shelley.LedgerState CurrentEra -> ReaderT SqlBackend m ()
insertLedgerState Shelley.LedgerState {..} = do
  stateKey <- insertUTxOState _utxoState
  insertUTxO (Shelley._utxo _utxoState) stateKey
  insert_ $
    LedgerState
      { ledgerStateUtxo = stateKey,
        ledgerStateDstate = Shelley._dstate _delegationState,
        ledgerStatePstate = Shelley._pstate _delegationState
      }

sourceUTxO ::
     MonadResource m
  => ConduitM () (TxIn.TxIn C, Alonzo.TxOut CurrentEra) (ReaderT SqlBackend m) ()
sourceUTxO =
  selectSource [] []
    .| mapC (\(Entity _ Tx {..}) -> (TxIn.TxIn txInId (fromIntegral txInIx), txOut))


foldUTxO ::
     MonadUnliftIO m
  => (a -> (TxIn.TxIn C, Alonzo.TxOut CurrentEra) -> a) -- ^ Folding function
  -> a -- ^ Empty acc
  -> Text -- ^ Path to Sqlite db
  -> m a
foldUTxO f m fp = runSqlite fp (runConduit (sourceUTxO .| foldlC f m))

storeLedgerState ::
     MonadUnliftIO m => Text -> Shelley.LedgerState CurrentEra -> m ()
storeLedgerState fp ls = runSqlite fp $ do
  runMigration migrateAll
  insertLedgerState ls


-- runSqlite ::
--      MonadUnliftIO m
--   => T.Text
--   -> ReaderT SqlBackend (NoLoggingT m) a
--   -> m a
-- runSqlite dbfile = runNoLoggingT . withSqliteConn dbfile . runReaderT
