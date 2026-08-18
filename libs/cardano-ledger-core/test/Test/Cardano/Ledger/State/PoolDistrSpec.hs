{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.State.PoolDistrSpec (spec) where

import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Binary (natVersion)
import Cardano.Ledger.State (BlsKey, IndividualPoolStake (..))
import Test.Cardano.Ledger.Binary.RoundTrip (
  cborTrip,
  embedTripExpectation,
  roundTripCborRangeExpectation,
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()

spec :: Spec
spec =
  describe "IndividualPoolStake" $ do
    prop "BlsKey round-trips from Dijkstra onwards" $
      \(ips :: IndividualPoolStake) (blsKey :: BlsKey) ->
        roundTripCborRangeExpectation
          (natVersion @12)
          maxBound
          ips {individualPoolStakeBls = SJust blsKey}
    prop "BlsKey is neither encoded nor decoded before Dijkstra" $
      \(ips :: IndividualPoolStake) (blsKey :: BlsKey) ->
        forM_ [minBound .. natVersion @11] $ \version ->
          embedTripExpectation
            version
            version
            cborTrip
            (\decoded original -> decoded `shouldBe` original {individualPoolStakeBls = SNothing})
            ips {individualPoolStakeBls = SJust blsKey}
