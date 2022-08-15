{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.TxWits
  ( module BabbageTxWitsReExport,
  )
where

import Cardano.Ledger.Alonzo.TxWitness
  ( addrAlonzoWitsL,
    bootAddrAlonzoWitsL,
    datsAlonzoWitsL,
    rdmrsAlonzoWitsL,
    scriptAlonzoWitsL,
  )
import Cardano.Ledger.Alonzo.TxWitness as BabbageTxWitsReExport
  ( AlonzoEraWitnesses (..),
    TxWitness (..),
  )
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.Scripts ()
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CC

instance CC.Crypto c => EraTxWits (ConwayEra c) where
  type TxWits (ConwayEra c) = TxWitness (ConwayEra c)
  mkBasicWits = mempty
  addrWitsL = addrAlonzoWitsL
  bootAddrWitsL = bootAddrAlonzoWitsL
  scriptWitsL = scriptAlonzoWitsL

instance CC.Crypto c => AlonzoEraWitnesses (ConwayEra c) where
  datsWitsL = datsAlonzoWitsL
  rdmrsWitsL = rdmrsAlonzoWitsL
