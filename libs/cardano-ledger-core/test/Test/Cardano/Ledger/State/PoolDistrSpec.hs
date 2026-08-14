{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.State.PoolDistrSpec (spec) where

import Cardano.Ledger.BaseTypes (StrictMaybe (SJust, SNothing))
import Cardano.Ledger.Binary (natVersion)
import Cardano.Ledger.State (IndividualPoolStake (..), LeiosKey)
import Test.Cardano.Ledger.Binary.RoundTrip (Trip, cborTrip, embedTripExpectation)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()

trip :: Trip IndividualPoolStake IndividualPoolStake
trip = cborTrip

-- | The generator leaves 'individualPoolStakeBls' empty, because a pool only
-- registers a BLS key from version 12 on. These properties set it, so they need
-- a key of their own.
withKey :: LeiosKey -> IndividualPoolStake -> IndividualPoolStake
withKey leiosKey ips = ips {individualPoolStakeBls = SJust leiosKey}

spec :: Spec
spec =
  describe "IndividualPoolStake" $ do
    prop "keeps individualPoolStakeBls at version 12" $ \leiosKey ips ->
      embedTripExpectation (natVersion @12) (natVersion @12) trip shouldBe $
        withKey leiosKey ips

    prop "leaves individualPoolStakeBls out before version 12" $ \leiosKey ips ->
      embedTripExpectation
        (natVersion @11)
        (natVersion @11)
        trip
        (\decoded original -> decoded `shouldBe` original {individualPoolStakeBls = SNothing})
        $ withKey leiosKey ips

    -- Unlike 'StakePoolSnapShot', the decoder here accepts both field counts at
    -- any version. The record also travels over node-to-client, where the peer
    -- picks the version.
    prop "reads a three field encoding at version 12" $ \leiosKey ips ->
      embedTripExpectation
        (natVersion @11)
        (natVersion @12)
        trip
        (\decoded original -> decoded `shouldBe` original {individualPoolStakeBls = SNothing})
        $ withKey leiosKey ips

    prop "reads a four field encoding before version 12" $ \leiosKey ips ->
      embedTripExpectation (natVersion @12) (natVersion @11) trip shouldBe $
        withKey leiosKey ips
