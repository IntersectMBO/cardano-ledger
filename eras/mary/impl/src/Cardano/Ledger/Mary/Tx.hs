{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Tx (
  validateTimelock,
)
where

import Cardano.Ledger.Allegra.Tx (validateTimelock)
import Cardano.Ledger.Core (
  EraRequiredTxsData (RequiredTxs),
  EraTx (..),
  upgradeTxAuxData,
  upgradeTxBody,
  upgradeTxWits,
 )
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.PParams ()
import Cardano.Ledger.Mary.TxAuxData ()
import Cardano.Ledger.Mary.TxBody ()
import Cardano.Ledger.Mary.TxWits ()
import Cardano.Ledger.Shelley.Tx (
  ShelleyRequiredTx,
  ShelleyTx (..),
  auxDataShelleyTxL,
  bodyShelleyTxL,
  mkBasicShelleyTx,
  shelleyMinFeeTx,
  sizeShelleyTxF,
  witsShelleyTxL,
 )
import Lens.Micro (lens)

-- ========================================

instance Crypto c => EraRequiredTxsData (MaryEra c) where
  {-# SPECIALIZE instance EraRequiredTxsData (MaryEra StandardCrypto) #-}

  type RequiredTxs (MaryEra c) = ShelleyRequiredTx (MaryEra c)

instance Crypto c => EraTx (MaryEra c) where
  {-# SPECIALIZE instance EraTx (MaryEra StandardCrypto) #-}

  type Tx (MaryEra c) = ShelleyTx (MaryEra c)

  mkBasicTx = mkBasicShelleyTx

  bodyTxL = bodyShelleyTxL
  {-# INLINE bodyTxL #-}

  witsTxL = witsShelleyTxL
  {-# INLINE witsTxL #-}

  auxDataTxL = auxDataShelleyTxL
  {-# INLINE auxDataTxL #-}

  requiredTxsTxL = lens (const mempty) const
  {-# INLINE requiredTxsTxL #-}

  sizeTxF = sizeShelleyTxF
  {-# INLINE sizeTxF #-}

  validateNativeScript = validateTimelock
  {-# INLINE validateNativeScript #-}

  getMinFeeTx pp tx _ = shelleyMinFeeTx pp tx

  upgradeTx (ShelleyTx txb txwits txAux) =
    ShelleyTx
      <$> upgradeTxBody txb
      <*> pure (upgradeTxWits txwits)
      <*> pure (fmap upgradeTxAuxData txAux)
