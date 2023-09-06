{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.BinarySpec (spec) where

import Cardano.Ledger.Alonzo
import Cardano.Ledger.Alonzo.Genesis
import Cardano.Ledger.Alonzo.Scripts
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Alonzo.Binary.RoundTrip (roundTripAlonzoCommonSpec)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Binary (BinaryUpgradeOpts (..), specUpgrade)
import Test.Cardano.Ledger.Core.Binary.RoundTrip (roundTripEraSpec)

spec :: Spec
spec = do
  -- Scripts are not upgradeable from Mary through their CBOR instances, since
  -- Mary had no concept of a prefix.
  -- Transactions are also not upgradeable through deserialisation, though we
  -- check them via the translateEra method
  specUpgrade @Alonzo (BinaryUpgradeOpts False False)
  describe "RoundTrip" $ do
    roundTripAlonzoCommonSpec @Alonzo
    -- AlonzoGenesis only makes sense in Alonzo era
    roundTripEraSpec @Alonzo @AlonzoGenesis
    -- CostModel serialization changes drastically for Conway, which requires a different
    -- QuickCheck generator, hence Arbitrary can't be reused
    roundTripEraSpec @Alonzo @CostModels
