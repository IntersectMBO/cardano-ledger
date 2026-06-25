{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.BlockBody where

import Cardano.Ledger.Alonzo.BlockBody
import Cardano.Ledger.Babbage.Era
import Cardano.Ledger.Babbage.Tx ()
import Cardano.Ledger.Core

instance EraBlockBody BabbageEra where
  type BlockBody BabbageEra = AlonzoBlockBody BabbageEra
  mkBasicBlockBody = mkBasicBlockBodyAlonzo
  txSeqBlockBodyL = txSeqBlockBodyAlonzoL
  hashBlockBody = alonzoBlockBodyHash
  numSegComponents = 4
