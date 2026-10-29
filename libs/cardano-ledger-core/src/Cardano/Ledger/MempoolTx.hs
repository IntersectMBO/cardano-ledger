-- | Transactions as they are received by the mempool. See `MempoolTx` for details.
module Cardano.Ledger.MempoolTx (
  MempoolTx,
  unwrapMempoolTx,
) where

import Cardano.Ledger.MempoolTx.Internal (MempoolTx, unwrapMempoolTx)
