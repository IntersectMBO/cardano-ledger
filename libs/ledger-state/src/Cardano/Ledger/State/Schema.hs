{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.State.Schema where

import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import Cardano.Ledger.Coin
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import Cardano.Ledger.State.Orphans ()
import Cardano.Ledger.State.UTxO
import qualified Cardano.Ledger.TxIn as TxIn
import Data.Word
import Database.Persist.Sqlite
import Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Tx
  inIx Word64
  inId (TxIn.TxId C)
  out (Alonzo.TxOut CurrentEra)
  UniqueTx inIx inId
UtxoEntry
  tx TxId
  state UtxoStateId
UtxoState
  deposited Coin
  fees Coin
  ppups (Shelley.PPUPState CurrentEra)
LedgerState
  utxo UtxoStateId
  dstate (Shelley.DState C)
  pstate (Shelley.PState C)
|]
