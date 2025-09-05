{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.BlockBody where

import Cardano.Ledger.Alonzo.BlockBody
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.Era
import Cardano.Ledger.Dijkstra.Tx ()

instance EraBlockBody DijkstraEra where
  type BlockBody DijkstraEra = AlonzoBlockBody DijkstraEra
  mkBasicBlockBody = mkBasicBlockBodyAlonzo
  txSeqBlockBodyL = txSeqBlockBodyAlonzoL
  hashBlockBody = alonzoBlockBodyHash
  numSegComponents = 4
