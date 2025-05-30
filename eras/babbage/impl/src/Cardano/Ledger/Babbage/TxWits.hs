{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.TxWits (
  module BabbageTxWitsReExport,
) where

import Cardano.Ledger.Alonzo.TxWits (
  AlonzoEraTxWits (..),
  AlonzoTxWits (..),
  addrAlonzoTxWitsL,
  bootAddrAlonzoTxWitsL,
  datsAlonzoTxWitsL,
  rdmrsAlonzoTxWitsL,
  scriptAlonzoTxWitsL,
 )
import Cardano.Ledger.Alonzo.TxWits as BabbageTxWitsReExport (
  AlonzoEraTxWits (..),
  AlonzoTxWits (..),
 )
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.TxBody ()
import Cardano.Ledger.Core

instance EraTxWits BabbageEra where
  type TxWits BabbageEra = AlonzoTxWits BabbageEra

  mkBasicTxWits = mempty

  addrTxWitsL = addrAlonzoTxWitsL
  {-# INLINE addrTxWitsL #-}

  bootAddrTxWitsL = bootAddrAlonzoTxWitsL
  {-# INLINE bootAddrTxWitsL #-}

  scriptTxWitsL = scriptAlonzoTxWitsL
  {-# INLINE scriptTxWitsL #-}

instance AlonzoEraTxWits BabbageEra where
  datsTxWitsL = datsAlonzoTxWitsL
  {-# INLINE datsTxWitsL #-}

  rdmrsTxWitsL = rdmrsAlonzoTxWitsL
  {-# INLINE rdmrsTxWitsL #-}
