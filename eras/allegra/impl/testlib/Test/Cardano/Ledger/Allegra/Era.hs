{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Allegra.Era () where

import Cardano.Ledger.Allegra
import Test.Cardano.Ledger.Allegra.Arbitrary ()
import Test.Cardano.Ledger.Allegra.TreeDiff ()
import Test.Cardano.Ledger.Era
import Test.Cardano.Ledger.Shelley.Era

instance EraTest AllegraEra

instance ShelleyEraTest AllegraEra
