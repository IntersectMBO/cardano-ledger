module Cardano.Ledger.Shelley.BlockBody (
  ShelleyBlockBody (ShelleyBlockBody),
  txSeqBlockBodyShelleyL,
  mkBasicBlockBodyShelley,
  shelleyBlockBodyHash,
  shelleyBlockBodyTxs,
  auxDataSeqDecoder,
  hashShelleySegWits,
  bBodySize,
  slotToNonce,
  incrBlocks,
  coreAuxDataBytes,
) where

import Cardano.Crypto.Hash (Hash)
import Cardano.Ledger.Core (EraIndependentBlockBody, HASH, TopTx, Tx)
import Cardano.Ledger.Shelley.BlockBody.Internal
import Data.Sequence.Strict (StrictSeq)

shelleyBlockBodyHash :: ShelleyBlockBody era -> Hash HASH EraIndependentBlockBody
shelleyBlockBodyHash = sbbHash

shelleyBlockBodyTxs :: ShelleyBlockBody era -> StrictSeq (Tx TopTx era)
shelleyBlockBodyTxs = sbbTxs
