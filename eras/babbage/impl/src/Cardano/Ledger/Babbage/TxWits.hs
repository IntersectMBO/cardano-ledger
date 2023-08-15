{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babbage.TxWits (
  module BabbageTxWitsReExport,
)
where

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
  upgradeRedeemers,
  upgradeTxDats,
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

  upgradeTxWits (AlonzoTxWits {txwitsVKey, txwitsBoot, txscripts, txdats, txrdmrs}) =
    AlonzoTxWits
      { txwitsVKey
      , txwitsBoot
      , txscripts = upgradeScript <$> txscripts
      , txdats = upgradeTxDats txdats
      , txrdmrs = upgradeRedeemers txrdmrs
      }

instance CC.Crypto c => AlonzoEraTxWits (BabbageEra c) where
  {-# SPECIALIZE instance AlonzoEraTxWits (BabbageEra CC.StandardCrypto) #-}

  datsTxWitsL = datsAlonzoTxWitsL
  {-# INLINE datsTxWitsL #-}

  rdmrsTxWitsL = rdmrsAlonzoTxWitsL
  {-# INLINE rdmrsTxWitsL #-}
