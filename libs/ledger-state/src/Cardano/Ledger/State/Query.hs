{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.State.Query where

import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import qualified Cardano.Ledger.Shelley.UTxO as Shelley
import Cardano.Ledger.State.Schema
import Cardano.Ledger.State.Transform
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
import Data.Int


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

sourceUTxOr ::
     MonadResource m
  => Int64 -> Int64 -> ConduitM () (TxIn.TxIn C, Alonzo.TxOut CurrentEra) (ReaderT SqlBackend m) ()
sourceUTxOr b t =
  selectSource [TxId >. TxKey (SqlBackendKey b) , TxId <. TxKey (SqlBackendKey t)] [] .|
  mapC (\(Entity _ Tx {..}) -> (TxIn.TxIn txInId (fromIntegral txInIx), txOut))


foldUTxO ::
     MonadUnliftIO m
  => (a -> (TxIn.TxIn C, Alonzo.TxOut CurrentEra) -> a) -- ^ Folding function
  -> a -- ^ Empty acc
  -> Text -- ^ Path to Sqlite db
  -> m a
foldUTxO f m fp = runSqlite fp (runConduit (sourceUTxO .| foldlC f m))

foldUTxOr ::
     MonadUnliftIO m
  => Int64
  -> Int64
  -> (a -> (TxIn.TxIn C, Alonzo.TxOut CurrentEra) -> a) -- ^ Folding function
  -> a -- ^ Empty acc
  -> Text -- ^ Path to Sqlite db
  -> m a
foldUTxOr b t f m fp = runSqlite fp (runConduit (sourceUTxOr b t .| foldlC f m))

getLedgerState ::
     MonadIO m
  => Shelley.UTxO CurrentEra
  -> ReaderT SqlBackend m (Shelley.LedgerState CurrentEra)
getLedgerState u = do
  Entity _ LedgerState {..} <- Prelude.head <$> selectList [] []
  UtxoState {..} <- getJust ledgerStateUtxo
  pure
    Shelley.LedgerState
      { Shelley._utxoState =
          Shelley.UTxOState
            { Shelley._utxo = u
            , Shelley._deposited = utxoStateDeposited
            , Shelley._fees = utxoStateFees
            , Shelley._ppups = utxoStatePpups
            }
      , Shelley._delegationState =
          Shelley.DPState
            { Shelley._dstate = ledgerStateDstate
            , Shelley._pstate = ledgerStatePstate
            }
      }

loadClassicLedgerState ::
     MonadUnliftIO m => Text -> m (Shelley.LedgerState CurrentEra)
loadClassicLedgerState fp =
  runSqlite fp $ do
    m <-
      runConduit
        (sourceUTxO .| foldlC (\ !m !(!k, !v) -> Map.insert k v m) mempty)
    ls <- getLedgerState $ Shelley.UTxO m
    pure ls

loadSharedLedgerState ::
     MonadUnliftIO m
  => Text
  -> m (Shelley.LedgerState CurrentEra, Map.Map (TxIn.TxIn C) TxOut')
loadSharedLedgerState fp =
  runSqlite fp $ do
    ls <- getLedgerState $ Shelley.UTxO mempty
    let stakeCredentials =
          Shelley._rewards $ Shelley._dstate $ Shelley._delegationState ls
    m <-
      runConduit
        (sourceUTxO .|
         foldlC
           (\ !m !(!k, !v) ->
              let !v' = toTxOut' stakeCredentials v
               in Map.insert k v' m)
           mempty)
    pure (ls, m)


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
