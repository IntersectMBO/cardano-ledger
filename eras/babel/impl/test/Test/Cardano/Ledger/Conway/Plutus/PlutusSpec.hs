{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babel.Plutus.PlutusSpec (spec) where

import Cardano.Ledger.Babbage.PParams (CoinPerByte)
import Cardano.Ledger.Babel (BabelEra)
import Cardano.Ledger.Babel.PParams (DRepVotingThresholds, PoolVotingThresholds)
import Cardano.Ledger.Core (PParamsUpdate)
import Cardano.Ledger.Crypto (StandardCrypto)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Babel.Arbitrary ()
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Plutus.ToPlutusData (roundTripPlutusDataSpec)

-- ================================

spec :: Spec
spec = do
  describe "roundtrip ToPlutusData Babel instances" $ do
    roundTripPlutusDataSpec @PoolVotingThresholds
    roundTripPlutusDataSpec @DRepVotingThresholds
    roundTripPlutusDataSpec @CoinPerByte
    roundTripPlutusDataSpec @(PParamsUpdate (BabelEra StandardCrypto))
