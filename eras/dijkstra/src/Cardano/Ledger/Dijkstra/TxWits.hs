{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.TxWits () where

import Cardano.Ledger.Alonzo.TxWits (
  addrAlonzoTxWitsL,
  bootAddrAlonzoTxWitsL,
  datsAlonzoTxWitsL,
  rdmrsAlonzoTxWitsL,
  scriptAlonzoTxWitsL,
  upgradeRedeemers,
  upgradeTxDats,
 )
import Cardano.Ledger.Conway.Core (AlonzoEraTxWits (..), EraScript (..), EraTxWits (..))
import Cardano.Ledger.Conway.TxWits (AlonzoTxWits (..))
import Cardano.Ledger.Dijkstra.Era (DijkstraEra)
import Cardano.Ledger.Dijkstra.Scripts ()

instance EraTxWits DijkstraEra where
  type TxWits DijkstraEra = AlonzoTxWits DijkstraEra

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

instance AlonzoEraTxWits DijkstraEra where
  datsTxWitsL = datsAlonzoTxWitsL
  {-# INLINE datsTxWitsL #-}

  rdmrsTxWitsL = rdmrsAlonzoTxWitsL
  {-# INLINE rdmrsTxWitsL #-}
