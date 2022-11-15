{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.TxWits
  ( module BabbageTxWitsReExport,
  )
where

import Cardano.Ledger.Alonzo.TxWits
  ( AlonzoEraTxWits (..),
    AlonzoTxWits (..),
    addrAlonzoTxWitsL,
    bootAddrAlonzoTxWitsL,
    datsAlonzoTxWitsL,
    rdmrsAlonzoTxWitsL,
    scriptAlonzoTxWitsL,
  )
import Cardano.Ledger.Alonzo.TxWits as BabbageTxWitsReExport
  ( AlonzoEraTxWits (..),
    AlonzoTxWits (..),
  )
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.Babbage.TxBody ()
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CC

instance CC.Crypto c => EraTxWits (BabbageEra c) where
  {-# SPECIALIZE instance EraTxWits (BabbageEra CC.StandardCrypto) #-}

  type TxWits (BabbageEra c) = AlonzoTxWits (BabbageEra c)

  mkBasicTxWits = mempty

  addrTxWitsL = addrAlonzoTxWitsL
  {-# INLINE addrTxWitsL #-}

  bootAddrTxWitsL = bootAddrAlonzoTxWitsL
  {-# INLINE bootAddrTxWitsL #-}

  scriptTxWitsL = scriptAlonzoTxWitsL
  {-# INLINE scriptTxWitsL #-}

instance CC.Crypto c => AlonzoEraTxWits (BabbageEra c) where
  {-# SPECIALIZE instance AlonzoEraTxWits (BabbageEra CC.StandardCrypto) #-}

  datsTxWitsL = datsAlonzoTxWitsL
  {-# INLINE datsTxWitsL #-}

  rdmrsTxWitsL = rdmrsAlonzoTxWitsL
  {-# INLINE rdmrsTxWitsL #-}
