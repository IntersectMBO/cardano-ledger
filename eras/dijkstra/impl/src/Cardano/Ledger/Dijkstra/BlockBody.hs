module Cardano.Ledger.Dijkstra.BlockBody (
  DijkstraBlockBody (DijkstraBlockBody),
  mkBasicBlockBodyDijkstra,
  txSeqBlockBodyDijkstraL,
  dijkstraBlockBodyHash,
  dijkstraBlockBodyTxs,
) where

import Cardano.Crypto.Hash (Hash)
import Cardano.Ledger.Core (EraIndependentBlockBody, HASH, Tx, TxLevel (..))
import Cardano.Ledger.Dijkstra.BlockBody.Internal
import Data.Sequence.Strict (StrictSeq)

dijkstraBlockBodyHash :: DijkstraBlockBody era -> Hash HASH EraIndependentBlockBody
dijkstraBlockBodyHash = dbbHash

dijkstraBlockBodyTxs :: DijkstraBlockBody era -> StrictSeq (Tx TopTx era)
dijkstraBlockBodyTxs = dbbTxs
