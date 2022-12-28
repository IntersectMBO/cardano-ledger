{-# LANGUAGE ConstraintKinds #-}

module Cardano.Ledger.ShelleyMA.Core
  {-# DEPRECATED "Use `Cardano.Ledger.Allegra.Core` from 'cardano-ledger-allegra' or `Cardano.Ledger.Mary.Core` from 'cardano-ledger-mary' packages instead" #-} (
  module Cardano.Ledger.Mary.Core,
  ShelleyMAEraTxBody,
)
where

import Cardano.Ledger.Mary.Core

type ShelleyMAEraTxBody era = (AllegraEraTxBody era, MaryEraTxBody era)
