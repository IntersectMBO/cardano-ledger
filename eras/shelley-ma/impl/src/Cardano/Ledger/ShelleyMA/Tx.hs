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

import Cardano.Ledger.Core (Era (Crypto), EraTx (..), EraTxWits (..), PhasedScript (..))
import Cardano.Ledger.Keys.WitVKey (witVKeyHash)
import Cardano.Ledger.Shelley.Tx
  ( ShelleyTx,
    ShelleyWitnesses,
    addrShelleyWitsL,
    auxDataShelleyTxL,
    bodyShelleyTxL,
    bootAddrShelleyWitsL,
    mkBasicShelleyTx,
    scriptShelleyWitsL,
    sizeShelleyTxF,
    witsShelleyTxL,
  )
import Cardano.Ledger.ShelleyMA.AuxiliaryData ()
import Cardano.Ledger.ShelleyMA.Era (MAClass, ShelleyMAEra)
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock, evalTimelock)
import Cardano.Ledger.ShelleyMA.TxBody (ShelleyMAEraTxBody (..))
import qualified Data.Set as Set (map)
import Lens.Micro ((^.))

-- ========================================

instance MAClass ma crypto => EraTx (ShelleyMAEra ma crypto) where
  type Tx (ShelleyMAEra ma crypto) = ShelleyTx (ShelleyMAEra ma crypto)

  mkBasicTx = mkBasicShelleyTx

  bodyTxL = bodyShelleyTxL

  witsTxL = witsShelleyTxL

  auxDataTxL = auxDataShelleyTxL

  sizeTxF = sizeShelleyTxF

  validateScript (Phase1Script script) tx = validateTimelock @(ShelleyMAEra ma crypto) script tx

-- =======================================================
-- Validating timelock scripts
-- We extract ValidityInterval from TxBody with vldtTxBodyL getter
-- We still need to correctly compute the witness set for TxBody as well.

validateTimelock ::
  (EraTx era, ShelleyMAEraTxBody era) => Timelock (Crypto era) -> Tx era -> Bool
validateTimelock timelock tx = evalTimelock vhks (tx ^. bodyTxL . vldtTxBodyL) timelock
  where
    vhks = Set.map witVKeyHash (tx ^. witsTxL . addrWitsL)

instance MAClass ma crypto => EraTxWits (ShelleyMAEra ma crypto) where
  type TxWits (ShelleyMAEra ma crypto) = ShelleyWitnesses (ShelleyMAEra ma crypto)

  mkBasicWits = mempty

  scriptWitsL = scriptShelleyWitsL

  addrWitsL = addrShelleyWitsL

  bootAddrWitsL = bootAddrShelleyWitsL
