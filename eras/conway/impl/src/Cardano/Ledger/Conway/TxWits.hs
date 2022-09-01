{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.TxWits
  ( module BabbageTxWitsReExport,
  )
where

import Cardano.Ledger.Alonzo.TxWits
  ( addrAlonzoTxWitsL,
    bootAddrAlonzoTxWitsL,
    datsAlonzoTxWitsL,
    rdmrsAlonzoTxWitsL,
    scriptAlonzoTxWitsL,
  )
import Cardano.Ledger.Alonzo.TxWits as BabbageTxWitsReExport
  ( AlonzoEraTxWits (..),
    AlonzoTxWits (..),
  )
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.Scripts ()
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CC

instance CC.Crypto c => EraTxWits (ConwayEra c) where
  {-# SPECIALIZE instance EraTxWits (ConwayEra CC.StandardCrypto) #-}

  type TxWits (ConwayEra c) = AlonzoTxWits (ConwayEra c)

  mkBasicTxWits = mempty

  addrTxWitsL = addrAlonzoTxWitsL
  {-# INLINE addrTxWitsL #-}

  bootAddrTxWitsL = bootAddrAlonzoTxWitsL
  {-# INLINE bootAddrTxWitsL #-}

  scriptTxWitsL = scriptAlonzoTxWitsL
  {-# INLINE scriptTxWitsL #-}

instance CC.Crypto c => AlonzoEraTxWits (ConwayEra c) where
  {-# SPECIALIZE instance AlonzoEraTxWits (ConwayEra CC.StandardCrypto) #-}

  datsTxWitsL = datsAlonzoTxWitsL
  {-# INLINE datsTxWitsL #-}

  rdmrsTxWitsL = rdmrsAlonzoTxWitsL
  {-# INLINE rdmrsTxWitsL #-}
