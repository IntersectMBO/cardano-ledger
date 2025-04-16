{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babbage.Era () where

import Cardano.Ledger.Babbage
import Test.Cardano.Ledger.Alonzo.Era
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Babbage.TreeDiff ()

instance EraTest BabbageEra

instance ShelleyEraTest BabbageEra

instance MaryEraTest BabbageEra

instance AlonzoEraTest BabbageEra
