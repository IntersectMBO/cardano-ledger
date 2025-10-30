{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.GoldenSpec (spec) where

import Cardano.Ledger.Dijkstra (DijkstraEra)
import Paths_cardano_ledger_dijkstra (getDataFileName)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.JSON (goldenJsonPParamsSpec, goldenJsonPParamsUpdateSpec)
import Test.Cardano.Ledger.Dijkstra.Era ()

spec :: Spec
spec =
  describe "Golden" $ do
    beforeAll (getDataFileName "golden/pparams.json") $
      goldenJsonPParamsSpec @DijkstraEra
    beforeAll (getDataFileName "golden/pparams-update.json") $
      goldenJsonPParamsUpdateSpec @DijkstraEra
