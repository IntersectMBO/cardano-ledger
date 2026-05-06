{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Cardano.Ledger.Shelley (ShelleyEra)
import Test.Cardano.Ledger.Core.JSON (roundTripJsonEraSpec)
import Test.Cardano.Ledger.Era
import qualified Test.Cardano.Ledger.Shelley.Binary.CddlSpec as Cddl
import qualified Test.Cardano.Ledger.Shelley.BinarySpec as Binary
import qualified Test.Cardano.Ledger.Shelley.Imp as Imp
import Test.Cardano.Ledger.Shelley.JSON (roundTripJsonShelleyEraSpec)

instance EraSpec ShelleyEra where
  eraImpSpec = Imp.spec

main :: IO ()
main =
  ledgerEraTestMain @ShelleyEra $ do
    Binary.spec
    Cddl.spec
    roundTripJsonEraSpec @ShelleyEra
    roundTripJsonShelleyEraSpec @ShelleyEra
