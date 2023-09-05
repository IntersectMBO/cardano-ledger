{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Binary.RoundTripSpec (spec) where

import Cardano.Ledger.Crypto
import Cardano.Ledger.Shelley
import Cardano.Ledger.Shelley.Genesis
import Cardano.Ledger.Shelley.RewardUpdate
import Test.Cardano.Ledger.Binary.RoundTrip (roundTripCborSpec)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Binary.RoundTrip (roundTripEraSpec)
import Test.Cardano.Ledger.Shelley.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Binary.RoundTrip (roundTripShelleyCommonSpec)

spec :: Spec
spec =
  describe "RoundTrip" $ do
    roundTripShelleyCommonSpec @Shelley
    describe "Non era parametric Shelley types" $ do
      roundTripCborSpec @NominalDiffTimeMicro
      roundTripCborSpec @(ShelleyGenesisStaking StandardCrypto)
      -- ShelleyGenesis only makes sense in Shelley era
      roundTripEraSpec @Shelley @(ShelleyGenesis StandardCrypto)
      roundTripCborSpec @(RewardUpdate StandardCrypto)
      roundTripCborSpec @(RewardSnapShot StandardCrypto)
      roundTripCborSpec @(FreeVars StandardCrypto)
      roundTripCborSpec @(Pulser StandardCrypto)
      roundTripCborSpec @(PulsingRewUpdate StandardCrypto)
