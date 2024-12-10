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

instance EraTxWits ConwayEra where
  type TxWits ConwayEra = AlonzoTxWits ConwayEra

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

instance AlonzoEraTxWits ConwayEra where
  datsTxWitsL = datsAlonzoTxWitsL
  {-# INLINE datsTxWitsL #-}

  rdmrsTxWitsL = rdmrsAlonzoTxWitsL
  {-# INLINE rdmrsTxWitsL #-}
