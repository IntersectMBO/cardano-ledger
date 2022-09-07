{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Serialisation.Tripping.JSON
  ( tests,

    -- * exported since they can be used for different cryptos
    prop_roundtrip_Address_JSON,
    prop_roundtrip_FundPair_JSON,
    prop_roundtrip_GenesisDelegationPair_JSON,
    prop_roundtrip_ShelleyGenesis_JSON,
  )
where

import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era
import Data.Aeson (decode, encode, fromJSON, toJSON)
import Data.Proxy
import Hedgehog (Property)
import qualified Hedgehog
import Hedgehog.Internal.Property (PropertyName (..))
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (C, C_Crypto)
import Test.Cardano.Ledger.Shelley.Serialisation.Generators.Genesis
import Test.Tasty
import Test.Tasty.Hedgehog hiding (testProperty)

-- | testProperty has been deprecated. We make our own version here.
testProperty :: TestName -> Property -> TestTree
testProperty s p = testPropertyNamed s (Hedgehog.Internal.Property.PropertyName s) p

prop_roundtrip_Address_JSON ::
  forall c.
  CC.Crypto c =>
  Proxy c ->
  Property
prop_roundtrip_Address_JSON _ =
  -- If this fails, FundPair and ShelleyGenesis can also fail.
  Hedgehog.property $ do
    addr <- Hedgehog.forAll $ genAddress @c

    Hedgehog.tripping addr toJSON fromJSON
    Hedgehog.tripping addr encode decode

prop_roundtrip_GenesisDelegationPair_JSON ::
  forall c.
  CC.Crypto c =>
  Proxy c ->
  Property
prop_roundtrip_GenesisDelegationPair_JSON _ =
  Hedgehog.property $ do
    dp <- Hedgehog.forAll $ genGenesisDelegationPair @c

    Hedgehog.tripping dp toJSON fromJSON
    Hedgehog.tripping dp encode decode

prop_roundtrip_FundPair_JSON ::
  forall c.
  CC.Crypto c =>
  Proxy c ->
  Property
prop_roundtrip_FundPair_JSON _ =
  -- If this fails, ShelleyGenesis can also fail.
  Hedgehog.property $ do
    fp <- Hedgehog.forAll $ genGenesisFundPair @c

    Hedgehog.tripping fp toJSON fromJSON
    Hedgehog.tripping fp encode decode

prop_roundtrip_ShelleyGenesis_JSON ::
  forall era.
  Era era =>
  Proxy era ->
  Property
prop_roundtrip_ShelleyGenesis_JSON _ = Hedgehog.withTests 500 $
  Hedgehog.property $
    do
      sg <- Hedgehog.forAll $ genShelleyGenesis @era
      Hedgehog.tripping sg toJSON fromJSON
      Hedgehog.tripping sg encode decode

tests :: TestTree
tests =
  testGroup
    "Shelley Genesis"
    [ testProperty "Address round trip" $
        prop_roundtrip_Address_JSON @C_Crypto Proxy,
      testProperty "Genesis round trip" $
        prop_roundtrip_ShelleyGenesis_JSON @C Proxy,
      testProperty "fund pair round trip" $
        prop_roundtrip_FundPair_JSON @C_Crypto Proxy,
      testProperty "delegation pair round trip" $
        prop_roundtrip_GenesisDelegationPair_JSON @C_Crypto Proxy
    ]
