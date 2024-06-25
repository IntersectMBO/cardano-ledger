{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Babel.TxWits (
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
import Cardano.Ledger.Babel.Era (BabelEra)
import Cardano.Ledger.Babel.Scripts ()
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto

instance Crypto c => EraTxWits (BabelEra c) where
  {-# SPECIALIZE instance EraTxWits (BabelEra StandardCrypto) #-}

  type TxWits (BabelEra c) = AlonzoTxWits (BabelEra c)

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

instance Crypto c => AlonzoEraTxWits (BabelEra c) where
  {-# SPECIALIZE instance AlonzoEraTxWits (BabelEra StandardCrypto) #-}

  datsTxWitsL = datsAlonzoTxWitsL
  {-# INLINE datsTxWitsL #-}

  rdmrsTxWitsL = rdmrsAlonzoTxWitsL
  {-# INLINE rdmrsTxWitsL #-}
