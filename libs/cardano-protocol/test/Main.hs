{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Protocol.Crypto (StandardCrypto)
import qualified Cardano.Protocol.Leios.BlockHeader as Leios
import qualified Cardano.Protocol.Praos.BlockHeader as Praos
import Test.Cardano.Ledger.Binary (decoderEquivalenceSpec)
import Test.Cardano.Ledger.Binary.RoundTrip (roundTripCborSpec)
import Test.Cardano.Ledger.Common
import Test.Cardano.Protocol.Leios.Arbitrary ()
import Test.Cardano.Protocol.Praos.Arbitrary ()

main :: IO ()
main =
  ledgerTestMain $ do
    describe "RoundTrip" $ do
      roundTripCborSpec @(Praos.HeaderBody StandardCrypto)
      roundTripCborSpec @(Praos.Header StandardCrypto)
      roundTripCborSpec @(Leios.HeaderBody StandardCrypto)
      roundTripCborSpec @(Leios.Header StandardCrypto)
    describe "DecCBOR instances equivalence" $ do
      decoderEquivalenceSpec @(Praos.Header StandardCrypto) minBound maxBound
      decoderEquivalenceSpec @(Leios.Header StandardCrypto) minBound maxBound
