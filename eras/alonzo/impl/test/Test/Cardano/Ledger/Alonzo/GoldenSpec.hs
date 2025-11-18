{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.GoldenSpec (spec) where

import Cardano.Ledger.Alonzo
import Cardano.Ledger.Core (eraProtVersions)
import Paths_cardano_ledger_alonzo (getDataFileName)
import qualified Test.Cardano.Ledger.Alonzo.Binary.Golden as Golden
import Test.Cardano.Ledger.Alonzo.Era ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.JSON (goldenJsonPParamsSpec, goldenJsonPParamsUpdateSpec)

spec :: Spec
spec =
  describe "Golden" $ do
    beforeAll (getDataFileName "golden/pparams.json") $
      goldenJsonPParamsSpec @AlonzoEra
    beforeAll (getDataFileName "golden/pparams-update.json") $
      goldenJsonPParamsUpdateSpec @AlonzoEra
    forM_ (eraProtVersions @AlonzoEra) $ \version ->
      describe (show version) $ Golden.spec @AlonzoEra version
