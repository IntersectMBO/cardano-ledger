{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.BlockBody where

import Cardano.Ledger.Alonzo.BlockBody
import Cardano.Ledger.Conway.Era
import Cardano.Ledger.Conway.Tx ()
import Cardano.Ledger.Core

instance EraBlockBody ConwayEra where
  type BlockBody ConwayEra = AlonzoBlockBody ConwayEra
  mkBasicBlockBody = mkBasicBlockBodyAlonzo
  txSeqBlockBodyL = txSeqBlockBodyAlonzoL
  hashBlockBody = alonzoBlockBodyHash
  numSegComponents = 4
