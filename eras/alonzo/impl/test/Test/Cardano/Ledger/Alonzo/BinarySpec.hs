{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.BinarySpec (spec) where

import Cardano.Ledger.Alonzo
import Cardano.Ledger.Alonzo.Genesis
import Cardano.Ledger.Alonzo.TxWits (Redeemers, TxDats)
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Alonzo.Binary.RoundTrip (roundTripAlonzoCommonSpec)
import Test.Cardano.Ledger.Alonzo.TreeDiff ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Binary as Binary (
  decoderEquivalenceCoreEraTypesSpec,
  decoderEquivalenceEraSpec,
  txSizeSpec,
 )
import Test.Cardano.Ledger.Core.Binary.RoundTrip (roundTripEraSpec)

spec :: Spec
spec = do
  describe "RoundTrip" $ do
    roundTripAlonzoCommonSpec @AlonzoEra
    -- AlonzoGenesis only makes sense in Alonzo era
    roundTripEraSpec @AlonzoEra @AlonzoGenesis
  describe "DecCBOR instances equivalence" $ do
    Binary.decoderEquivalenceCoreEraTypesSpec @AlonzoEra
    decoderEquivalenceEraSpec @AlonzoEra @(TxDats AlonzoEra)
    decoderEquivalenceEraSpec @AlonzoEra @(Redeemers AlonzoEra)
  Binary.txSizeSpec @AlonzoEra
