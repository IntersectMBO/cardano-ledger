{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Spec (
  everyEraSpec,
) where

import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Era

goldenFilePath = "golden"

goldenJsonFilePath = goldenFilePath </> "json"

-- | This spec is applicable to all eras and will be executed for every era starting with Shelley.
everyEraSpec :: forall era. EraTest era => Spec
everyEraSpec =
  describe "Spec for every Era" $ do
    describe "Golden" $ do
      describe "JSON" $ do
        it "Genesis" $ do
          withImpInit @(LedgerSpec era) $ do
            let decodeJsonGenesis =
                  getEraDataFilePath (goldenJsonFilePath </> "genesis.json")
            mkGenesisWith decodeJsonGenesis $ \decodedGenesis -> do
              initializedGenesis <- initGenesis
              decodedGenesis `shouldBe` initializedGenesis
