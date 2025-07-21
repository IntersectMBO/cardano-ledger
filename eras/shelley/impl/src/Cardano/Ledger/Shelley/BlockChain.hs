{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
import Cardano.Ledger.Shelley.BlockBody
import Data.Sequence.Strict

type ShelleyTxSeq = ShelleyBlockBody

pattern ShelleyTxSeq ::
  forall era.
  ( EraTx era
  , SafeToHash (TxWits era)
  ) =>
  StrictSeq (Tx era) ->
  ShelleyBlockBody era
pattern ShelleyTxSeq s = ShelleyBlockBody s

txSeqTxns :: ShelleyBlockBody era -> StrictSeq (Tx era)
txSeqTxns = shelleyBlockBodyTxs

bbHash :: EraBlockBody era => BlockBody era -> Hash HASH EraIndependentBlockBody
bbHash = hashBlockBody
