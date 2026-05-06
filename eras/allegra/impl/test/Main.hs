{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Cardano.Ledger.Allegra (AllegraEra)
import qualified Test.Cardano.Ledger.Allegra.Binary.CddlSpec as CddlSpec
import qualified Test.Cardano.Ledger.Allegra.Binary.Golden as Golden
import qualified Test.Cardano.Ledger.Allegra.BinarySpec as BinarySpec
import qualified Test.Cardano.Ledger.Allegra.Imp as Imp
import Test.Cardano.Ledger.Allegra.ImpTest ()
import Test.Cardano.Ledger.Core.JSON (roundTripJsonEraSpec)
import Test.Cardano.Ledger.Era
import Test.Cardano.Ledger.Shelley.JSON (roundTripJsonShelleyEraSpec)

instance EraSpec AllegraEra where
  eraImpSpec = Imp.spec

main :: IO ()
main =
  ledgerEraTestMain @AllegraEra $ do
    BinarySpec.spec
    CddlSpec.spec
    roundTripJsonEraSpec @AllegraEra
    roundTripJsonShelleyEraSpec @AllegraEra
    Golden.spec @AllegraEra
