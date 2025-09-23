{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.BinarySpec (spec) where

import Cardano.Ledger.Alonzo
import Cardano.Ledger.Alonzo.Genesis
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Alonzo.TxWits (Redeemers, TxDats)
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus.Data (BinaryData, Data (..))
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Alonzo.Binary.RoundTrip (roundTripAlonzoCommonSpec)
import Test.Cardano.Ledger.Alonzo.Binary.Twiddle ()
import Test.Cardano.Ledger.Alonzo.TreeDiff ()
import Test.Cardano.Ledger.Binary.RoundTrip (
  roundTripTwiddledProperty,
 )
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
    -- TOOD:  https://github.com/IntersectMBO/cardano-ledger/issues/3025
    describe "Twiddled" $ do
      prop "Script" $ roundTripTwiddledProperty @(Script AlonzoEra)
      prop "Data" $ roundTripTwiddledProperty @(Data AlonzoEra)
      prop "BinaryData" $ roundTripTwiddledProperty @(BinaryData AlonzoEra)
      prop "TxBody" $ roundTripTwiddledProperty @(TxBody AlonzoEra)
  describe "DecCBOR instances equivalence" $ do
    Binary.decoderEquivalenceCoreEraTypesSpec @AlonzoEra
    decoderEquivalenceEraSpec @AlonzoEra @(TxDats AlonzoEra)
    decoderEquivalenceEraSpec @AlonzoEra @(Redeemers AlonzoEra)
  Binary.txSizeSpec @AlonzoEra
