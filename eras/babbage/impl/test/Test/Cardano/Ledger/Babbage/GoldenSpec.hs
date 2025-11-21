{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.GoldenSpec (spec) where

import Cardano.Ledger.Babbage
import Paths_cardano_ledger_babbage (getDataFileName)
import qualified Test.Cardano.Ledger.Alonzo.Binary.Golden as Golden
import Test.Cardano.Ledger.Babbage.Era ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.JSON (goldenJsonPParamsSpec, goldenJsonPParamsUpdateSpec)

spec :: Spec
spec =
  describe "Golden" $ do
    describe "JSON files" $ do
      beforeAll (getDataFileName "golden/pparams.json") $
        goldenJsonPParamsSpec @BabbageEra
      beforeAll (getDataFileName "golden/pparams-update.json") $
        goldenJsonPParamsUpdateSpec @BabbageEra
    describe "CBOR" $ Golden.spec @BabbageEra
