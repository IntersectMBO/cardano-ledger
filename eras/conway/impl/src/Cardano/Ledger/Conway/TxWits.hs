{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.TxWits (
  module BabbageTxWitsReExport,
)
where

import Cardano.Ledger.Alonzo.TxWits (
  addrAlonzoTxWitsL,
  bootAddrAlonzoTxWitsL,
  datsAlonzoTxWitsL,
  rdmrsAlonzoTxWitsL,
  scriptAlonzoTxWitsL,
  upgradeRedeemers,
  upgradeTxDats,
 )
import Cardano.Ledger.Alonzo.TxWits as BabbageTxWitsReExport (
  AlonzoEraTxWits (..),
  AlonzoTxWits (..),
 )
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.Scripts ()
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto

instance Crypto c => EraTxWits (ConwayEra c) where
  {-# SPECIALIZE instance EraTxWits (ConwayEra StandardCrypto) #-}

  type TxWits (ConwayEra c) = AlonzoTxWits (ConwayEra c)

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

instance Crypto c => AlonzoEraTxWits (ConwayEra c) where
  {-# SPECIALIZE instance AlonzoEraTxWits (ConwayEra StandardCrypto) #-}

  datsTxWitsL = datsAlonzoTxWitsL
  {-# INLINE datsTxWitsL #-}

  rdmrsTxWitsL = rdmrsAlonzoTxWitsL
  {-# INLINE rdmrsTxWitsL #-}
