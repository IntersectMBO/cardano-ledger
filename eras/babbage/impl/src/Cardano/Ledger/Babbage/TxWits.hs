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
import Cardano.Ledger.Crypto

instance Crypto c => EraTxWits (BabbageEra c) where
  {-# SPECIALIZE instance EraTxWits (BabbageEra StandardCrypto) #-}

  type TxWits (BabbageEra c) = AlonzoTxWits (BabbageEra c)

  mkBasicTxWits = mempty

  addrTxWitsL = addrAlonzoTxWitsL
  {-# INLINE addrTxWitsL #-}

  bootAddrTxWitsL = bootAddrAlonzoTxWitsL
  {-# INLINE bootAddrTxWitsL #-}

  scriptTxWitsL = scriptAlonzoTxWitsL
  {-# INLINE scriptTxWitsL #-}

  upgradeTxWits atw =
    AlonzoTxWits
      { txwitsVKey = txwitsVKey atw
      , txwitsBoot = txwitsBoot atw
      , txscripts = upgradeScript <$> txscripts atw
      , txdats = upgradeTxDats (txdats atw)
      , txrdmrs = upgradeRedeemers (txrdmrs atw)
      }

instance Crypto c => AlonzoEraTxWits (BabbageEra c) where
  {-# SPECIALIZE instance AlonzoEraTxWits (BabbageEra StandardCrypto) #-}

  datsTxWitsL = datsAlonzoTxWitsL
  {-# INLINE datsTxWitsL #-}

  rdmrsTxWitsL = rdmrsAlonzoTxWitsL
  {-# INLINE rdmrsTxWitsL #-}
