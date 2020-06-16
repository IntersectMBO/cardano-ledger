{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Shelley.Spec.Ledger.Genesis.Properties
  ( genesis,

    -- * exported since they can be used for different cryptos
    prop_roundtrip_Address_JSON,
    prop_roundtrip_FundPair_JSON,
    prop_roundtrip_GenesisDelegationPair_JSON,
    prop_roundtrip_ShelleyGenesis_JSON,
  )
where

import Cardano.Crypto.Hash
import Data.Aeson (decode, encode, fromJSON, toJSON)
import Data.Proxy
import Hedgehog (Property)
import qualified Hedgehog
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.Genesis
import Test.Cardano.Prelude
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (ConcreteCrypto)
import Test.Shelley.Spec.Ledger.Generator.Genesis
import Test.Shelley.Spec.Ledger.Genesis.Example
import Test.Tasty
import Test.Tasty.Hedgehog

prop_golden_ShelleyGenesis :: Property
prop_golden_ShelleyGenesis = goldenTestJSON example "test/Golden/ShelleyGenesis"
  where
    example :: ShelleyGenesis (ConcreteCrypto ShortHash)
    example = exampleShelleyGenesis

prop_roundtrip_Address_JSON :: forall c. Crypto c => Proxy c -> Property
prop_roundtrip_Address_JSON _ =
  -- If this fails, FundPair and ShelleyGenesis can also fail.
  Hedgehog.property $ do
    addr <- Hedgehog.forAll $ genAddress @c
    Hedgehog.tripping addr toJSON fromJSON
    Hedgehog.tripping addr encode decode

prop_roundtrip_GenesisDelegationPair_JSON :: forall c. Crypto c => Proxy c -> Property
prop_roundtrip_GenesisDelegationPair_JSON _ =
  Hedgehog.property $ do
    dp <- Hedgehog.forAll $ genGenesisDelegationPair @c
    Hedgehog.tripping dp toJSON fromJSON
    Hedgehog.tripping dp encode decode

prop_roundtrip_FundPair_JSON :: forall c. Crypto c => Proxy c -> Property
prop_roundtrip_FundPair_JSON _ =
  -- If this fails, ShelleyGenesis can also fail.
  Hedgehog.property $ do
    fp <- Hedgehog.forAll $ genGenesisFundPair @c
    Hedgehog.tripping fp toJSON fromJSON
    Hedgehog.tripping fp encode decode

prop_roundtrip_ShelleyGenesis_JSON :: forall c. Crypto c => Proxy c -> Property
prop_roundtrip_ShelleyGenesis_JSON _ = Hedgehog.withTests 500
  $ Hedgehog.property
  $ do
    sg <- Hedgehog.forAll $ genShelleyGenesis @c
    Hedgehog.tripping sg toJSON fromJSON
    Hedgehog.tripping sg encode decode

genesis :: TestTree
genesis =
  testGroup
    "Shelley Genesis"
    [ testProperty "Genesis Golden Test" prop_golden_ShelleyGenesis,
      testProperty "Adress round trip" $
        prop_roundtrip_Address_JSON @(ConcreteCrypto ShortHash) Proxy,
      testProperty "Genesis round trip" $
        prop_roundtrip_ShelleyGenesis_JSON @(ConcreteCrypto ShortHash) Proxy,
      testProperty "fund pair round trip" $
        prop_roundtrip_FundPair_JSON @(ConcreteCrypto ShortHash) Proxy,
      testProperty "delegation pair round trip" $
        prop_roundtrip_GenesisDelegationPair_JSON @(ConcreteCrypto ShortHash) Proxy
    ]
