{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.Serialisation.Roundtrip (allprops) where

import Cardano.Ledger.Binary.Version (natVersion)
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Core (Era (..))
import Data.Data (Proxy (..), typeRep)
import Test.Cardano.Ledger.Alonzo.Arbitrary (FlexibleCostModels)
import Test.Cardano.Ledger.Binary.Plain.RoundTrip as Plain (roundTripCborExpectation)
import Test.Cardano.Ledger.Binary.RoundTrip (roundTripCborRangeExpectation)
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

allprops :: forall e. Era e => TestTree
allprops =
  testGroup
    (show $ typeRep (Proxy @e))
    [ testProperty "ConwayGenesis" $
        roundTripCborRangeExpectation @(ConwayGenesis (EraCrypto e))
          (natVersion @2)
          maxBound
    , testProperty "ConwayGenesis (Plain)" $
        Plain.roundTripCborExpectation @(ConwayGenesis (EraCrypto e))
    , testProperty "v9 CostModels" $
        roundTripCborRangeExpectation @FlexibleCostModels
          (natVersion @9)
          (natVersion @9)
    ]
