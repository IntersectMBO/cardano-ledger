-- | TxSeq. This is effectively the block body, which consists of a sequence of
-- transactions with segregated witness and metadata information.
module Cardano.Ledger.Alonzo.TxSeq (
  AlonzoTxSeq (AlonzoTxSeq, txSeqTxns),
  TxSeq,
  hashTxSeq,
  hashAlonzoTxSeq,
)
where

import Cardano.Ledger.Alonzo.TxSeq.Internal
