{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.Tx (
  validateTimelock,
)
where

import Cardano.Ledger.Allegra.Tx (validateTimelock)
import Cardano.Ledger.Core (
  EraTx (..),
  PhasedScript (..),
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
  ShelleyTx (..),
  auxDataShelleyTxL,
  bodyShelleyTxL,
  mkBasicShelleyTx,
  shelleyMinFeeTx,
  sizeShelleyTxF,
  witsShelleyTxL,
 )

-- ========================================

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

  sizeTxF = sizeShelleyTxF
  {-# INLINE sizeTxF #-}

  validateScript (Phase1Script script) = validateTimelock @(MaryEra c) script
  {-# INLINE validateScript #-}

  getMinFeeTx = shelleyMinFeeTx

  upgradeTx (ShelleyTx txb txwits txAux) =
    ShelleyTx
      <$> upgradeTxBody txb
      <*> pure (upgradeTxWits txwits)
      <*> pure (fmap upgradeTxAuxData txAux)
