{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.Plutus.PlutusSpec (spec) where

import Cardano.Ledger.Babbage.PParams (CoinPerByte)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.PParams (DRepVotingThresholds, PoolVotingThresholds)
import Cardano.Ledger.Core (PParamsUpdate)
import Cardano.Ledger.Crypto (StandardCrypto)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Plutus.ToPlutusData (roundTripPlutusDataSpec)

-- ================================

spec :: Spec
spec = do
  describe "roundtrip ToPlutusData Conway instances" $ do
    roundTripPlutusDataSpec @PoolVotingThresholds
    roundTripPlutusDataSpec @DRepVotingThresholds
    roundTripPlutusDataSpec @CoinPerByte
    roundTripPlutusDataSpec @(PParamsUpdate (ConwayEra StandardCrypto))
