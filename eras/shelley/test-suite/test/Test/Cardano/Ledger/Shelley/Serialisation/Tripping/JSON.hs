{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Serialisation.Tripping.JSON (
  tests,
)
where

import Cardano.Ledger.Address (Addr)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys (GenDelegPair (..), KeyHash, KeyRole (Genesis))
import Cardano.Ledger.Shelley (Shelley)
import Cardano.Ledger.Shelley.API.Types (ShelleyGenesis)
import Data.Aeson (FromJSON, ToJSON (toJSON), decode, encode, fromJSON)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.QuickCheck (Property, (.&&.), (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

propRoundTripJSON :: (ToJSON a, FromJSON a, Show a, Eq a) => a -> Property
propRoundTripJSON a =
  (fromJSON (toJSON a) === pure a)
    .&&. (decode (encode a) === pure a)

tests :: TestTree
tests =
  testGroup
    "Shelley Genesis"
    [ testProperty "Genesis roundtrip" $
        propRoundTripJSON @(ShelleyGenesis Shelley)
    , testProperty "Coin roundtrip" $
        propRoundTripJSON @Coin
    , testProperty "Address roundtrip" $
        propRoundTripJSON @(Addr StandardCrypto)
    , testProperty "Genesis KeyHash " $
        propRoundTripJSON @(KeyHash 'Genesis StandardCrypto)
    , testProperty "GenDelegPair roundtrip" $
        propRoundTripJSON @(GenDelegPair StandardCrypto)
    ]
