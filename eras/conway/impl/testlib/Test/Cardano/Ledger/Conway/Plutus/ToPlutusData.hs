{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.PlutusSpec (spec) where

import Cardano.Ledger.Babbage.PParams (CoinPerByte)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.PParams (DRepVotingThresholds, PoolVotingThresholds)
import Cardano.Ledger.Core (PParamsUpdate)
import Cardano.Ledger.Crypto (StandardCrypto)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Plutus.ToPlutusData (roundtrip)

-- ================================

spec :: Spec
spec = do
  describe "roundtrip ToPlutusData Conway instances" $ do
    roundtrip @PoolVotingThresholds
    roundtrip @DRepVotingThresholds
    roundtrip @CoinPerByte
    roundtrip @(PParamsUpdate (ConwayEra StandardCrypto))
