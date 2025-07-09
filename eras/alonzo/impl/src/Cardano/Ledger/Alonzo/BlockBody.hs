-- | TxSeq. This is effectively the block body, which consists of a sequence of
-- transactions with segregated witness and metadata information.
module Cardano.Ledger.Alonzo.BlockBody (
  AlonzoBlockBody (AlonzoBlockBody),
) where

import Cardano.Ledger.Alonzo.BlockBody.Internal
