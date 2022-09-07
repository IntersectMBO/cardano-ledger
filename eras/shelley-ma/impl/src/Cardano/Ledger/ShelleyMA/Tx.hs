{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.ShelleyMA.Tx
  ( validateTimelock,
  )
where

import Cardano.Ledger.Core (EraTx (..), EraTxWits (..), PhasedScript (..))
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys.WitVKey (witVKeyHash)
import Cardano.Ledger.Shelley.PParams (ShelleyPParamsHKD (..))
import Cardano.Ledger.Shelley.Tx
  ( ShelleyTx,
    auxDataShelleyTxL,
    bodyShelleyTxL,
    mkBasicShelleyTx,
    shelleyMinFeeTx,
    sizeShelleyTxF,
    witsShelleyTxL,
  )
import Cardano.Ledger.ShelleyMA.AuxiliaryData ()
import Cardano.Ledger.ShelleyMA.Era (MAClass, MaryOrAllegra (..), ShelleyMAEra)
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock, evalTimelock)
import Cardano.Ledger.ShelleyMA.TxBody (ShelleyMAEraTxBody (..))
import Cardano.Ledger.ShelleyMA.TxWits ()
import qualified Data.Set as Set (map)
import Lens.Micro ((^.))

-- ========================================

instance MAClass ma c => EraTx (ShelleyMAEra ma c) where
  {-# SPECIALIZE instance EraTx (ShelleyMAEra 'Mary StandardCrypto) #-}
  {-# SPECIALIZE instance EraTx (ShelleyMAEra 'Allegra StandardCrypto) #-}

  type Tx (ShelleyMAEra ma c) = ShelleyTx (ShelleyMAEra ma c)

  mkBasicTx = mkBasicShelleyTx

  bodyTxL = bodyShelleyTxL
  {-# INLINE bodyTxL #-}

  witsTxL = witsShelleyTxL
  {-# INLINE witsTxL #-}

  auxDataTxL = auxDataShelleyTxL
  {-# INLINE auxDataTxL #-}

  sizeTxF = sizeShelleyTxF
  {-# INLINE sizeTxF #-}

  validateScript (Phase1Script script) tx = validateTimelock @(ShelleyMAEra ma c) script tx
  {-# INLINE validateScript #-}

  getMinFeeTx = shelleyMinFeeTx

-- =======================================================
-- Validating timelock scripts
-- We extract ValidityInterval from TxBody with vldtTxBodyL getter
-- We still need to correctly compute the witness set for TxBody as well.

validateTimelock ::
  (EraTx era, ShelleyMAEraTxBody era) => Timelock era -> Tx era -> Bool
validateTimelock timelock tx = evalTimelock vhks (tx ^. bodyTxL . vldtTxBodyL) timelock
  where
    vhks = Set.map witVKeyHash (tx ^. witsTxL . addrTxWitsL)
{-# INLINEABLE validateTimelock #-}
