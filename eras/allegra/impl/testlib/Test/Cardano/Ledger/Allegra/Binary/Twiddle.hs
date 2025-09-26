{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Allegra.Binary.Twiddle (
  module Test.Cardano.Ledger.Shelley.Binary.Twiddle,
) where

import Cardano.Ledger.Allegra
import Cardano.Ledger.Allegra.Core
import Cardano.Ledger.Allegra.Scripts (Timelock)
import Cardano.Ledger.Allegra.TxAuxData (AllegraTxAuxData)
import Test.Cardano.Ledger.Shelley.Binary.Twiddle

instance Twiddle (PParams AllegraEra) where
  twiddle = twiddleTerm

instance Twiddle (PParamsUpdate AllegraEra) where
  twiddle = twiddleTerm

instance Twiddle (AllegraTxAuxData era)

instance Twiddle (Timelock era)

instance Twiddle (Tx AllegraEra) where
  twiddle = twiddleTerm

instance Twiddle (TxBody AllegraEra) where
  twiddle = twiddleTerm
