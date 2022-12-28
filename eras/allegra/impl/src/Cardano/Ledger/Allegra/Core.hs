{-# LANGUAGE FlexibleContexts #-}

module Cardano.Ledger.Allegra.Core (
  AllegraEraTxBody (..),
  module Cardano.Ledger.Shelley.Core,
)
where

import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.Shelley.Core
import Lens.Micro (Lens')

class ShelleyEraTxBody era => AllegraEraTxBody era where
  vldtTxBodyL :: Lens' (TxBody era) ValidityInterval
