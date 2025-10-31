{-# LANGUAGE CPP #-}
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
#if __GLASGOW_HASKELL__ >= 914
  data ShelleyTxSeq,
#else
  pattern ShelleyTxSeq,
#endif
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
  ( EraTx era
  , SafeToHash (TxWits era)
  ) =>
  StrictSeq (Tx TopTx era) ->
  ShelleyBlockBody era
pattern ShelleyTxSeq s = ShelleyBlockBody s

txSeqTxns :: ShelleyBlockBody era -> StrictSeq (Tx TopTx era)
txSeqTxns = shelleyBlockBodyTxs

bbHash :: EraBlockBody era => BlockBody era -> Hash HASH EraIndependentBlockBody
bbHash = hashBlockBody
