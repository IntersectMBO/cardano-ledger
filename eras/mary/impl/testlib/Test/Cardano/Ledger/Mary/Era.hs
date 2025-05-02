{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Mary.Era (
  module Test.Cardano.Ledger.Allegra.Era,
  MaryEraTest,
) where

import Cardano.Ledger.Mary
import Cardano.Ledger.Mary.Core
import Test.Cardano.Ledger.Allegra.Era
import Test.Cardano.Ledger.Mary.Arbitrary ()
import Test.Cardano.Ledger.Mary.TreeDiff ()

class
  ( AllegraEraTest era
  , MaryEraTxBody era
  ) =>
  MaryEraTest era

instance EraTest MaryEra

instance ShelleyEraTest MaryEra

instance AllegraEraTest MaryEra

instance MaryEraTest MaryEra
