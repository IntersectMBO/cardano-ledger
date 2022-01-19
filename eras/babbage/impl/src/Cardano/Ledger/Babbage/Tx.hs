module Cardano.Ledger.Babbage.Tx
  ( module X,
    TxBody (..),
  )
where

import Cardano.Ledger.Alonzo.Tx as X hiding (TxBody (..))
import Cardano.Ledger.Babbage.TxBody (TxBody (..))
