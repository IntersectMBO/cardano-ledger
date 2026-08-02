{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.BaseTypes (natVersion)
import Cardano.Protocol.Crypto (StandardCrypto)
import qualified Cardano.Protocol.Leios.BlockHeader as Leios
import qualified Cardano.Protocol.Praos.BlockHeader as Praos
import qualified Cardano.Protocol.TPraos.BlockHeader as TPraos
import Test.Cardano.Ledger.Binary (decoderEquivalenceSpec)
import Test.Cardano.Ledger.Binary.RoundTrip (roundTripCborSpec)
import Test.Cardano.Ledger.Common
import Test.Cardano.Protocol.Leios.BlockHeader.Arbitrary ()
import Test.Cardano.Protocol.Praos.BlockHeader.Arbitrary ()
import Test.Cardano.Protocol.TPraos.BlockHeader.Arbitrary ()

main :: IO ()
main =
  ledgerTestMain $ do
    describe "RoundTrip" $ do
      roundTripCborSpec @(TPraos.BHBody StandardCrypto)
      roundTripCborSpec @(TPraos.BHeader StandardCrypto)
      roundTripCborSpec @(Praos.HeaderBody StandardCrypto)
      roundTripCborSpec @(Praos.Header StandardCrypto)
      roundTripCborSpec @(Leios.HeaderBody StandardCrypto)
      roundTripCborSpec @(Leios.Header StandardCrypto)
    describe "DecCBOR instances equivalence" $ do
      decoderEquivalenceSpec @(TPraos.BHeader StandardCrypto) (natVersion @2) (natVersion @6)
      decoderEquivalenceSpec @(Praos.Header StandardCrypto) (natVersion @7) (natVersion @11)
      decoderEquivalenceSpec @(Leios.Header StandardCrypto) (natVersion @12) maxBound
