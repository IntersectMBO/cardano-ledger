{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Ledger.Alonzo.TxSeq
  {-# DEPRECATED "Use `Cardano.Ledger.Alonzo.BlockBody` instead" #-} (
  AlonzoTxSeq,
  pattern AlonzoTxSeq,
  hashAlonzoTxSeq,
  txSeqTxns,
) where

import Cardano.Ledger.Alonzo.BlockBody.Internal
import Cardano.Ledger.Alonzo.Tx
import Cardano.Ledger.Core
import Data.Sequence.Strict
import Lens.Micro ((^.))

type AlonzoTxSeq = AlonzoBlockBody

pattern AlonzoTxSeq ::
  ( AlonzoEraTx era
  , SafeToHash (TxWits era)
  ) =>
  StrictSeq (Tx era) -> AlonzoBlockBody era
pattern AlonzoTxSeq s = AlonzoBlockBody s

txSeqTxns :: EraBlockBody era => BlockBody era -> StrictSeq (Tx era)
txSeqTxns = (^. txSeqBlockBodyL)

hashAlonzoTxSeq :: EraBlockBody era => BlockBody era -> Hash HASH EraIndependentBlockBody
hashAlonzoTxSeq = hashBlockBody
