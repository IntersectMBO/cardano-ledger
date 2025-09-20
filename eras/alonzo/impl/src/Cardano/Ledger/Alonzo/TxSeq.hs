{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Alonzo.TxSeq
  {-# DEPRECATED "Use `Cardano.Ledger.Alonzo.BlockBody` instead" #-} (
  AlonzoTxSeq,
#if __GLASGOW_HASKELL__ >= 914
  data AlonzoTxSeq,
#else
  pattern AlonzoTxSeq,
#endif
  hashAlonzoTxSeq,
  txSeqTxns,
) where

import Cardano.Ledger.Alonzo.BlockBody
import Cardano.Ledger.Alonzo.Tx
import Cardano.Ledger.Core
import Data.Sequence.Strict

type AlonzoTxSeq = AlonzoBlockBody

pattern AlonzoTxSeq ::
  ( AlonzoEraTx era
  , SafeToHash (TxWits era)
  ) =>
  StrictSeq (Tx era) -> AlonzoBlockBody era
pattern AlonzoTxSeq s = AlonzoBlockBody s

txSeqTxns :: AlonzoBlockBody era -> StrictSeq (Tx era)
txSeqTxns = alonzoBlockBodyTxs

hashAlonzoTxSeq :: AlonzoBlockBody era -> Hash HASH EraIndependentBlockBody
hashAlonzoTxSeq = alonzoBlockBodyHash
