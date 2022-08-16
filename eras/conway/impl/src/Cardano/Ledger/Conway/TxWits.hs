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

instance CC.Crypto c => EraWitnesses (ConwayEra c) where
  {-# SPECIALIZE instance EraWitnesses (ConwayEra CC.StandardCrypto) #-}

  type Witnesses (ConwayEra c) = TxWitness (ConwayEra c)

  mkBasicWitnesses = mempty

  addrWitsL = addrAlonzoWitsL
  {-# INLINE addrWitsL #-}

  bootAddrWitsL = bootAddrAlonzoWitsL
  {-# INLINE bootAddrWitsL #-}

  scriptWitsL = scriptAlonzoWitsL
  {-# INLINE scriptWitsL #-}

instance CC.Crypto c => AlonzoEraWitnesses (ConwayEra c) where
  {-# SPECIALIZE instance AlonzoEraWitnesses (ConwayEra CC.StandardCrypto) #-}

  datsWitsL = datsAlonzoWitsL
  {-# INLINE datsWitsL #-}

  rdmrsWitsL = rdmrsAlonzoWitsL
  {-# INLINE rdmrsWitsL #-}
