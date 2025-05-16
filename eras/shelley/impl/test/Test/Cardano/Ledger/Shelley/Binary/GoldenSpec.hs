{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Binary.GoldenSpec (spec) where

import Cardano.Ledger.Shelley
import Paths_cardano_ledger_shelley (getDataFileName)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.JSON (goldenJsonPParamsSpec, goldenJsonPParamsUpdateSpec)
import Test.Cardano.Ledger.Shelley.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Binary.Golden (goldenNewEpochStateExpectation)
import Test.Cardano.Ledger.Shelley.Era ()

spec :: Spec
spec =
  describe "Golden" $ do
    prop "NewEpochState" $ goldenNewEpochStateExpectation @ShelleyEra
    beforeAll (getDataFileName "golden/pparams.json") $
      goldenJsonPParamsSpec @ShelleyEra
    beforeAll (getDataFileName "golden/pparams-update.json") $
      goldenJsonPParamsUpdateSpec @ShelleyEra
