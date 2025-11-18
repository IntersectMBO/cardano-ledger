{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.GoldenSpec (spec) where

import Cardano.Ledger.Babbage
import Cardano.Ledger.Babbage.Core (eraProtVersions)
import Paths_cardano_ledger_babbage (getDataFileName)
import qualified Test.Cardano.Ledger.Alonzo.Binary.Golden as Golden
import Test.Cardano.Ledger.Babbage.Era ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.JSON (goldenJsonPParamsSpec, goldenJsonPParamsUpdateSpec)

spec :: Spec
spec =
  describe "Golden" $ do
    beforeAll (getDataFileName "golden/pparams.json") $
      goldenJsonPParamsSpec @BabbageEra
    beforeAll (getDataFileName "golden/pparams-update.json") $
      goldenJsonPParamsUpdateSpec @BabbageEra
    forM_ (eraProtVersions @BabbageEra) $ \version ->
      describe (show version) . describe "Alonzo era golden tests" $ Golden.spec @BabbageEra version
