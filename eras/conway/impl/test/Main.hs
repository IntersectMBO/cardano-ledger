{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Tx (tierRefScriptFee)
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Conway.Binary.CddlSpec as Cddl
import qualified Test.Cardano.Ledger.Conway.GenesisSpec as Genesis
import qualified Test.Cardano.Ledger.Conway.GoldenSpec as GoldenSpec
import qualified Test.Cardano.Ledger.Conway.GoldenTranslation as GoldenTranslation
import qualified Test.Cardano.Ledger.Conway.GovActionReorderSpec as GovActionReorder
import Test.Cardano.Ledger.Conway.Plutus.PlutusSpec as PlutusSpec
import qualified Test.Cardano.Ledger.Conway.Spec as ConwaySpec
import Test.Cardano.Ledger.Shelley.JSON (roundTripJsonShelleyEraSpec)

main :: IO ()
main = ledgerTestMain $ do
  describe "Conway era-generic" $ ConwaySpec.spec @ConwayEra
  describe "Conway era-specific" $ do
    GoldenTranslation.spec
    Genesis.spec
    GovActionReorder.spec
    describe "Plutus" $ do
      PlutusSpec.spec
    Cddl.spec
    GoldenSpec.spec
  describe "Various tests for functions defined in Conway" $ do
    prop "tierRefScriptFee is a linear function when growth is 1" $ \(Positive sizeIncrement) baseFee (NonNegative size) ->
      tierRefScriptFee 1 sizeIncrement baseFee size
        === Coin (floor (fromIntegral size * baseFee))
    it "tierRefScriptFee" $ do
      let step = 25600
      map (tierRefScriptFee 1.5 step 15) [0, step .. 204800]
        `shouldBe` map Coin [0, 384000, 960000, 1824000, 3120000, 5064000, 7980000, 12354000, 18915000]
  roundTripJsonShelleyEraSpec @ConwayEra
