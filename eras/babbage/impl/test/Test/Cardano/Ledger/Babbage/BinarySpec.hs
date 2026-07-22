{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babbage.BinarySpec (spec) where

import Cardano.Ledger.Alonzo.TxWits (Redeemers, TxDats)
import Cardano.Ledger.Babbage
import Cardano.Protocol.Crypto (StandardCrypto)
import qualified Cardano.Protocol.Praos.BlockHeader as Praos
import Test.Cardano.Ledger.Alonzo.Binary.RoundTrip (roundTripAlonzoCommonSpec)
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Babbage.Era ()
import Test.Cardano.Ledger.Babbage.TreeDiff ()
import Test.Cardano.Ledger.Binary (decoderEquivalenceSpec)
import Test.Cardano.Ledger.Binary.RoundTrip (roundTripCborSpec)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Binary as Binary (
  decoderEquivalenceCoreEraTypesSpec,
  decoderEquivalenceEraSpec,
  txSizeSpec,
 )
import Test.Cardano.Protocol.Praos.Arbitrary ()

spec :: Spec
spec = do
  describe "RoundTrip" $ do
    roundTripAlonzoCommonSpec @BabbageEra
    roundTripCborSpec @(Praos.HeaderBody StandardCrypto)
    roundTripCborSpec @(Praos.Header StandardCrypto)
  describe "DecCBOR instances equivalence" $ do
    Binary.decoderEquivalenceCoreEraTypesSpec @BabbageEra
    decoderEquivalenceEraSpec @BabbageEra @(TxDats BabbageEra)
    decoderEquivalenceEraSpec @BabbageEra @(Redeemers BabbageEra)
    decoderEquivalenceSpec @(Praos.Header StandardCrypto) minBound maxBound
  Binary.txSizeSpec @BabbageEra
