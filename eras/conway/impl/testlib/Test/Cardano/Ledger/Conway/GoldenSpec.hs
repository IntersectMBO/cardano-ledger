{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.GoldenSpec (spec) where

import Cardano.Ledger.Conway
import Paths_cardano_ledger_conway (getDataFileName)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Era ()
import Test.Cardano.Ledger.Core.JSON (goldenJsonPParamsSpec, goldenJsonPParamsUpdateSpec)

spec :: Spec
spec =
  describe "Golden" $ do
    beforeAll (getDataFileName "golden/pparams.json") $
      goldenJsonPParamsSpec @ConwayEra
    beforeAll (getDataFileName "golden/pparams-update.json") $
      goldenJsonPParamsUpdateSpec @ConwayEra
