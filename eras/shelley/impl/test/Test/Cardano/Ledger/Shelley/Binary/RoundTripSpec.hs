{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Binary.RoundTripSpec (spec) where

import Cardano.Ledger.Shelley
import Cardano.Ledger.Shelley.Genesis
import Cardano.Ledger.Shelley.RewardUpdate
import Test.Cardano.Ledger.Binary.RoundTrip (roundTripCborSpec)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Binary.RoundTrip (roundTripEraSpec)
import Test.Cardano.Ledger.Core.Binary.Twiddle ()
import Test.Cardano.Ledger.Shelley.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Binary.RoundTrip (roundTripShelleyCommonSpec)
import Test.Cardano.Ledger.Shelley.TreeDiff ()

spec :: Spec
spec =
  describe "RoundTrip" $ do
    roundTripShelleyCommonSpec @ShelleyEra
    describe "Non era parametric Shelley types" $ do
      roundTripCborSpec @NominalDiffTimeMicro
      roundTripCborSpec @ShelleyGenesisStaking
      -- ShelleyGenesis only makes sense in Shelley era
      roundTripEraSpec @ShelleyEra @ShelleyGenesis
      roundTripCborSpec @RewardUpdate
      roundTripCborSpec @RewardSnapShot
      roundTripCborSpec @FreeVars
      roundTripCborSpec @Pulser
      roundTripCborSpec @PulsingRewUpdate
