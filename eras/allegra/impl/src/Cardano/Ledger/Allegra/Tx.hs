{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.Tx
  ( validateTimelock,
  )
where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.PParams ()
import Cardano.Ledger.Allegra.Scripts (Timelock, evalTimelock)
import Cardano.Ledger.Allegra.TxAuxData ()
import Cardano.Ledger.Allegra.TxBody (AllegraEraTxBody (..))
import Cardano.Ledger.Allegra.TxWits ()
import Cardano.Ledger.Core (EraTx (..), EraTxWits (..), PhasedScript (..))
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Keys.WitVKey (witVKeyHash)
import Cardano.Ledger.Shelley.Tx
  ( ShelleyTx,
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

instance Crypto c => EraTx (AllegraEra c) where
  {-# SPECIALIZE instance EraTx (AllegraEra StandardCrypto) #-}

  type Tx (AllegraEra c) = ShelleyTx (AllegraEra c)

  mkBasicTx = mkBasicShelleyTx

  bodyTxL = bodyShelleyTxL
  {-# INLINE bodyTxL #-}

  witsTxL = witsShelleyTxL
  {-# INLINE witsTxL #-}

  auxDataTxL = auxDataShelleyTxL
  {-# INLINE auxDataTxL #-}

  sizeTxF = sizeShelleyTxF
  {-# INLINE sizeTxF #-}

  validateScript (Phase1Script script) tx = validateTimelock @(AllegraEra c) script tx
  {-# INLINE validateScript #-}

  getMinFeeTx = shelleyMinFeeTx

-- =======================================================
-- Validating timelock scripts
-- We extract ValidityInterval from TxBody with vldtTxBodyL getter
-- We still need to correctly compute the witness set for TxBody as well.

validateTimelock ::
  (EraTx era, AllegraEraTxBody era) => Timelock era -> Tx era -> Bool
validateTimelock timelock tx = evalTimelock vhks (tx ^. bodyTxL . vldtTxBodyL) timelock
  where
    vhks = Set.map witVKeyHash (tx ^. witsTxL . addrTxWitsL)
{-# INLINEABLE validateTimelock #-}
