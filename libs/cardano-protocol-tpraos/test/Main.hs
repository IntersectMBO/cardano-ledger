{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Protocol.TPraos.BlockHeader (BHeader)
import Test.Cardano.Ledger.Allegra.Binary.Annotator ()
import Test.Cardano.Ledger.Alonzo.Binary.Annotator ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Mary.Binary.Annotator ()
import Test.Cardano.Ledger.Shelley.Binary.Annotator ()
import Test.Cardano.Protocol.Binary.Annotator ()
import qualified Test.Cardano.Protocol.Binary.BinarySpec as Binary
import qualified Test.Cardano.Protocol.Binary.CddlSpec as Cddl
import Test.Cardano.Protocol.Binary.RoundTrip
import Test.Cardano.Protocol.TPraos.Arbitrary ()

main :: IO ()
main =
  ledgerTestMain $ do
    Cddl.spec
    Binary.spec
    describe "RoundTrip" $ do
      roundTripBlockSpec @(BHeader StandardCrypto) @ShelleyEra
      roundTripBlockSpec @(BHeader StandardCrypto) @AllegraEra
      roundTripBlockSpec @(BHeader StandardCrypto) @MaryEra
      roundTripBlockSpec @(BHeader StandardCrypto) @AlonzoEra
