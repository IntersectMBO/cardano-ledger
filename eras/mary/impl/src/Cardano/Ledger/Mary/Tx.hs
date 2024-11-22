{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Tx (
  validateTimelock,
)
where

import Cardano.Ledger.Allegra.Tx (validateTimelock)
import Cardano.Ledger.Core (EraTx (..), upgradeTxAuxData, upgradeTxBody, upgradeTxWits)
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.PParams ()
import Cardano.Ledger.Mary.TxAuxData ()
import Cardano.Ledger.Mary.TxBody ()
import Cardano.Ledger.Mary.TxWits ()
import Cardano.Ledger.Shelley.Tx (
  ShelleyTx (..),
  auxDataShelleyTxL,
  bodyShelleyTxL,
  mkBasicShelleyTx,
  shelleyMinFeeTx,
  sizeShelleyTxF,
  wireSizeShelleyTxF,
  witsShelleyTxL,
 )

-- ========================================

instance EraTx MaryEra where
  {-# SPECIALIZE instance EraTx MaryEra #-}

  type Tx MaryEra = ShelleyTx MaryEra

  mkBasicTx = mkBasicShelleyTx

  bodyTxL = bodyShelleyTxL
  {-# INLINE bodyTxL #-}

  witsTxL = witsShelleyTxL
  {-# INLINE witsTxL #-}

  auxDataTxL = auxDataShelleyTxL
  {-# INLINE auxDataTxL #-}

  sizeTxF = sizeShelleyTxF
  {-# INLINE sizeTxF #-}

  wireSizeTxF = wireSizeShelleyTxF
  {-# INLINE wireSizeTxF #-}

  validateNativeScript = validateTimelock
  {-# INLINE validateNativeScript #-}

  getMinFeeTx pp tx _ = shelleyMinFeeTx pp tx

  upgradeTx (ShelleyTx txb txwits txAux) =
    ShelleyTx
      <$> upgradeTxBody txb
      <*> pure (upgradeTxWits txwits)
      <*> pure (fmap upgradeTxAuxData txAux)
