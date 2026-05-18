{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babbage.BinarySpec (spec) where

import Cardano.Ledger.Alonzo.TxWits (Redeemers, TxDats)
import Cardano.Ledger.Babbage
import Test.Cardano.Ledger.Alonzo.Binary.RoundTrip (roundTripAlonzoCommonSpec)
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Babbage.Era ()
import Test.Cardano.Ledger.Babbage.TreeDiff ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Binary as Binary (
  decoderEquivalenceCoreEraTypesSpec,
  decoderEquivalenceEraSpec,
  txSizeSpec,
 )

spec :: Spec
spec = do
  describe "RoundTrip" $ do
    roundTripAlonzoCommonSpec @BabbageEra
  describe "DecCBOR instances equivalence" $ do
    Binary.decoderEquivalenceCoreEraTypesSpec @BabbageEra
    decoderEquivalenceEraSpec @BabbageEra @(TxDats BabbageEra)
    decoderEquivalenceEraSpec @BabbageEra @(Redeemers BabbageEra)
  Binary.txSizeSpec @BabbageEra
