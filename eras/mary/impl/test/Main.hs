{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Cardano.Ledger.Mary (MaryEra)
import qualified Test.Cardano.Ledger.Allegra.Binary.Golden as Golden
import Test.Cardano.Ledger.Core.JSON (roundTripJsonEraSpec)
import Test.Cardano.Ledger.Era
import qualified Test.Cardano.Ledger.Mary.Binary.CddlSpec as CddlSpec
import qualified Test.Cardano.Ledger.Mary.BinarySpec as BinarySpec
import qualified Test.Cardano.Ledger.Mary.Imp as Imp
import Test.Cardano.Ledger.Era
import Test.Cardano.Ledger.Mary.ImpTest ()
import qualified Test.Cardano.Ledger.Mary.ValueSpec as ValueSpec
import qualified Test.Cardano.Ledger.Shelley.Imp as Imp (shelleyEraSpecificSpec)
import Test.Cardano.Ledger.Shelley.JSON (roundTripJsonShelleyEraSpec)

instance EraSpec MaryEra where
  eraImpSpec era = do
    Imp.shelleyEraSpecificSpec era
    Imp.spec era

main :: IO ()
main =
  ledgerEraTestMain @MaryEra $ do
    ValueSpec.spec
    BinarySpec.spec
    CddlSpec.spec
    roundTripJsonEraSpec @MaryEra
    roundTripJsonShelleyEraSpec @MaryEra
    Golden.spec @MaryEra
