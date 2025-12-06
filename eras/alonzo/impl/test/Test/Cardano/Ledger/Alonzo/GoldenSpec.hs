{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.GoldenSpec (spec) where

import Cardano.Ledger.Alonzo
import Paths_cardano_ledger_alonzo (getDataFileName)
import qualified Test.Cardano.Ledger.Alonzo.Binary.Golden as Golden
import Test.Cardano.Ledger.Alonzo.Era ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.JSON (goldenJsonPParamsSpec, goldenJsonPParamsUpdateSpec)

spec :: Spec
spec =
  describe "Golden" $ do
    describe "JSON files" $ do
      beforeAll (getDataFileName "golden/pparams.json") $
        goldenJsonPParamsSpec @AlonzoEra
      beforeAll (getDataFileName "golden/pparams-update.json") $
        goldenJsonPParamsUpdateSpec @AlonzoEra
    describe "CBOR" $ Golden.spec @AlonzoEra
