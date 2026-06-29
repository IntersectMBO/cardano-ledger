module Cardano.Ledger.Alonzo.BlockBody (
  AlonzoBlockBody (AlonzoBlockBody),
  mkBasicBlockBodyAlonzo,
  txSeqBlockBodyAlonzoL,
  alonzoBlockBodyHash,
  alonzoBlockBodyTxs,
) where

import Cardano.Crypto.Hash (Hash)
import Cardano.Ledger.Alonzo.BlockBody.Internal
import Cardano.Ledger.Core (EraIndependentBlockBody, HASH, Tx, TxLevel (..))
import Data.Sequence.Strict (StrictSeq)

alonzoBlockBodyHash :: AlonzoBlockBody era -> Hash HASH EraIndependentBlockBody
alonzoBlockBodyHash = abbHash

alonzoBlockBodyTxs :: AlonzoBlockBody era -> StrictSeq (Tx TopTx era)
alonzoBlockBodyTxs = abbTxs
