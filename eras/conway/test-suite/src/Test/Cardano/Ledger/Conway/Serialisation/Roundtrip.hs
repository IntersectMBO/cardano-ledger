{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.Serialisation.Roundtrip (allprops) where

import Cardano.Ledger.Alonzo.TxWits (Redeemers)
import Cardano.Ledger.Binary.Version (natVersion)
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Core (Era (..))
import Data.Data (Proxy (..), typeRep)
import Test.Cardano.Ledger.Binary.RoundTrip (
  embedTripAnnExpectation,
  roundTripAnnRangeExpectation,
  roundTripCborExpectation,
 )
import Test.Cardano.Ledger.Conway.Serialisation.Generators ()
import Test.Hspec (shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

allprops ::
  forall e.
  (Era e) =>
  TestTree
allprops =
  testGroup
    ("Conway CBOR roundtrip " <> show (typeRep (Proxy @e)))
    [ testProperty "ConwayGenesis" $
        roundTripCborExpectation @(ConwayGenesis (EraCrypto e))
    , testProperty "Redeemers" $
        roundTripAnnRangeExpectation @(Redeemers e) (natVersion @9) maxBound
    , testProperty "Redeemers - Conway can decode a Babbage encoding" $
        embedTripAnnExpectation @(Redeemers e) (natVersion @8) (natVersion @9) shouldBe
    ]
