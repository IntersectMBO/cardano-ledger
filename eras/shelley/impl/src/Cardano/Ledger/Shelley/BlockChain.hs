{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Ledger.Shelley.BlockChain
  {-# DEPRECATED "Use `Cardano.Ledger.Shelley.BlockBody` instead" #-} (
  ShelleyTxSeq,
  txSeqTxns,
  pattern ShelleyTxSeq,
  bbHash,
  hashShelleySegWits,
  bBodySize,
  slotToNonce,
  --
  incrBlocks,
  coreAuxDataBytes,
) where

import Cardano.Ledger.Core
import Cardano.Ledger.Shelley
import Cardano.Ledger.Shelley.BlockBody.Internal
import Data.Sequence.Strict

type ShelleyTxSeq = ShelleyBlockBody

pattern ShelleyTxSeq ::
  forall era.
  ( EraTx era
  , Tx era ~ ShelleyTx era
  , SafeToHash (TxWits era)
  ) =>
  StrictSeq (Tx era) ->
  ShelleyBlockBody era
pattern ShelleyTxSeq s = ShelleyBlockBody s

txSeqTxns :: ShelleyBlockBody era -> StrictSeq (ShelleyTx era)
txSeqTxns = sbbTxs

bbHash :: EraBlockBody era => BlockBody era -> Hash HASH EraIndependentBlockBody
bbHash = hashBlockBody
