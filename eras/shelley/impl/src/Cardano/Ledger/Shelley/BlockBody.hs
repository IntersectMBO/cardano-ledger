module Cardano.Ledger.Shelley.BlockBody (
  ShelleyBlockBody (ShelleyBlockBody),
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
import Cardano.Ledger.Core (EraIndependentBlockBody, HASH, Tx)
import Cardano.Ledger.Shelley.BlockBody.Internal
import Data.Sequence.Strict (StrictSeq)

shelleyBlockBodyHash :: ShelleyBlockBody era -> Hash HASH EraIndependentBlockBody
shelleyBlockBodyHash = sbbHash

shelleyBlockBodyTxs :: ShelleyBlockBody era -> StrictSeq (Tx era)
shelleyBlockBodyTxs = sbbTxs
