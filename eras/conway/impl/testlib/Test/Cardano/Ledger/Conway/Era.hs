{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.Era (
  module Test.Cardano.Ledger.Babbage.Era,
  ConwayEraTest,
) where

import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.State
import Test.Cardano.Ledger.Babbage.Era
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.TreeDiff ()

class
  ( BabbageEraTest era
  , ConwayEraTxBody era
  , ConwayEraCertState era
  , ConwayEraGov era
  ) =>
  ConwayEraTest era

instance EraTest ConwayEra

instance ShelleyEraTest ConwayEra

instance AllegraEraTest ConwayEra

instance MaryEraTest ConwayEra

instance AlonzoEraTest ConwayEra

instance BabbageEraTest ConwayEra

instance ConwayEraTest ConwayEra
