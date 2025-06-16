{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Tx (
  validateTimelock,
) where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.PParams ()
import Cardano.Ledger.Allegra.Scripts (AllegraEraScript (..), Timelock, evalTimelock)
import Cardano.Ledger.Allegra.TxAuxData ()
import Cardano.Ledger.Allegra.TxBody (AllegraEraTxBody (..))
import Cardano.Ledger.Allegra.TxWits ()
import Cardano.Ledger.Core (
  EraTx (..),
  EraTxAuxData (upgradeTxAuxData),
  EraTxWits (..),
  NativeScript,
  upgradeTxBody,
 )
import Cardano.Ledger.Keys.WitVKey (witVKeyHash)
import Cardano.Ledger.Shelley.Tx (
  ShelleyTx (..),
  auxDataShelleyTxL,
  bodyShelleyTxL,
  mkBasicShelleyTx,
  shelleyMinFeeTx,
  sizeShelleyTxF,
  witsShelleyTxL,
 )
import qualified Data.Set as Set (map)
import Lens.Micro ((^.))

-- ========================================

instance EraTx AllegraEra where
  type Tx AllegraEra = ShelleyTx AllegraEra

  mkBasicTx = mkBasicShelleyTx

  bodyTxL = bodyShelleyTxL
  {-# INLINE bodyTxL #-}

  witsTxL = witsShelleyTxL
  {-# INLINE witsTxL #-}

  auxDataTxL = auxDataShelleyTxL
  {-# INLINE auxDataTxL #-}

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

-- =======================================================
-- Validating timelock scripts
-- We extract ValidityInterval from TxBody with vldtTxBodyL getter
-- We still need to correctly compute the witness set for TxBody as well.

validateTimelock ::
  (EraTx era, AllegraEraTxBody era, AllegraEraScript era, NativeScript era ~ Timelock era) =>
  Tx era -> NativeScript era -> Bool
validateTimelock tx timelock = evalTimelock vhks (tx ^. bodyTxL . vldtTxBodyL) timelock
  where
    vhks = Set.map witVKeyHash (tx ^. witsTxL . addrTxWitsL)
{-# INLINEABLE validateTimelock #-}
