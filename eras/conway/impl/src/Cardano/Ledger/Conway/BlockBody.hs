{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.BlockBody where

import Cardano.Ledger.Alonzo.BlockBody
import Cardano.Ledger.Conway.Era
import Cardano.Ledger.Conway.Tx ()
import Cardano.Ledger.Core
import Lens.Micro (lens)

instance EraBlockBody ConwayEra where
  type BlockBody ConwayEra = AlonzoBlockBody ConwayEra
  mkBasicBlockBody = AlonzoBlockBody mempty
  txSeqBlockBodyL = lens alonzoBlockBodyTxs (\_ s -> AlonzoBlockBody s)
  hashBlockBody = alonzoBlockBodyHash
  numSegComponents = 4
