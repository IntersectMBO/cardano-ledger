{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.BlockBody where

import Cardano.Ledger.Alonzo.BlockBody
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.Era
import Cardano.Ledger.Dijkstra.Tx ()
import Lens.Micro (lens)

instance EraBlockBody DijkstraEra where
  type BlockBody DijkstraEra = AlonzoBlockBody DijkstraEra
  mkBasicBlockBody = AlonzoBlockBody mempty
  txSeqBlockBodyL = lens alonzoBlockBodyTxs (\_ s -> AlonzoBlockBody s)
  hashBlockBody = alonzoBlockBodyHash
  numSegComponents = 4
