{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.Serialisation.Golden.Genesis
  ( tests,

    -- * Individual properties
    prop_golden_ShelleyGenesis,
  )
where

import Cardano.Crypto.Hash
import Hedgehog (Property)
import Shelley.Spec.Ledger.Genesis
import Test.Cardano.Prelude
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (ConcreteCrypto)
import Test.Shelley.Spec.Ledger.Examples (exampleShelleyGenesis)
import Test.Tasty
import Test.Tasty.Hedgehog

prop_golden_ShelleyGenesis :: Property
prop_golden_ShelleyGenesis = goldenTestJSONPretty example "test/Golden/ShelleyGenesis"
  where
    example :: ShelleyGenesis (ConcreteCrypto ShortHash)
    example = exampleShelleyGenesis

tests :: TestTree
tests =
  testGroup
    "Shelley Genesis golden tests"
    [ testProperty "ShelleyGenesis golden test" prop_golden_ShelleyGenesis
    ]
