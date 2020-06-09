module Test.Shelley.Spec.Ledger.Genesis.Properties
  ( genesis,
  )
where

import Data.Aeson (decode, encode, fromJSON, toJSON)
import Hedgehog (Property)
import qualified Hedgehog
import Test.Cardano.Prelude
import Test.Shelley.Spec.Ledger.Generator.Genesis
import Test.Shelley.Spec.Ledger.Genesis.Example
import Test.Tasty
import Test.Tasty.Hedgehog

prop_golden_ShelleyGenesis :: Property
prop_golden_ShelleyGenesis = goldenTestJSON exampleShelleyGenesis "test/Golden/ShelleyGenesis"

prop_roundtrip_GenesisDelegationPair_JSON :: Property
prop_roundtrip_GenesisDelegationPair_JSON =
  Hedgehog.property $ do
    dp <- Hedgehog.forAll genGenesisDelegationPair
    Hedgehog.tripping dp toJSON fromJSON
    Hedgehog.tripping dp encode decode

prop_roundtrip_ShelleyGenesis_JSON :: Property
prop_roundtrip_ShelleyGenesis_JSON = Hedgehog.withTests 500
  $ Hedgehog.property
  $ do
    sg <- Hedgehog.forAll genShelleyGenesis
    Hedgehog.tripping sg toJSON fromJSON
    Hedgehog.tripping sg encode decode

genesis :: TestTree
genesis =
  testGroup
    "Shelley Genesis"
    [ testProperty "Genesis Golden Test" prop_golden_ShelleyGenesis,
      testProperty "Genesis round trip" prop_roundtrip_ShelleyGenesis_JSON,
      testProperty "delegation pair round trip" prop_roundtrip_GenesisDelegationPair_JSON
    ]
