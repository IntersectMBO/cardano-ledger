{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.Era (
  module Test.Cardano.Ledger.Alonzo.Era,
  ConwayEraTest,
) where

import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.State
import Test.Cardano.Ledger.Alonzo.Era
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.TreeDiff ()

class
  ( AlonzoEraTest era
  , ConwayEraTxBody era
  , ConwayEraPParams era
  , ConwayEraCertState era
  , ConwayEraGov era
  ) =>
  ConwayEraTest era

instance EraTest ConwayEra

instance ShelleyEraTest ConwayEra

instance MaryEraTest ConwayEra

instance AlonzoEraTest ConwayEra

instance ConwayEraTest ConwayEra
