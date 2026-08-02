{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babbage.BinarySpec (spec) where

import Cardano.Ledger.Alonzo.TxWits (Redeemers, TxDats)
import Cardano.Ledger.Babbage
import Cardano.Ledger.Block (Block (Block))
import Cardano.Protocol.Crypto (StandardCrypto)
import qualified Cardano.Protocol.Praos.BlockHeader as Praos
import qualified Test.Cardano.Base.QuickCheck as BaseQC
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
import Test.Cardano.Ledger.Core.Binary.RoundTrip (
  roundTripAnnEraExpectation,
  roundTripEraExpectation,
 )
import Test.Cardano.Protocol.Praos.BlockHeader.Arbitrary ()

spec :: Spec
spec = do
  describe "RoundTrip" $ do
    roundTripAlonzoCommonSpec @BabbageEra
    prop "Block (Praos.Header)" $
      BaseQC.withNumTests 25 $
        forAll (Block <$> arbitrary <*> scale (`div` 2) arbitrary) $ \block ->
          conjoin
            [ roundTripEraExpectation @BabbageEra @(Block (Praos.Header StandardCrypto) BabbageEra) block
            , roundTripAnnEraExpectation @BabbageEra @(Block (Praos.Header StandardCrypto) BabbageEra) block
            ]
  describe "DecCBOR instances equivalence" $ do
    Binary.decoderEquivalenceCoreEraTypesSpec @BabbageEra
    decoderEquivalenceEraSpec @BabbageEra @(TxDats BabbageEra)
    decoderEquivalenceEraSpec @BabbageEra @(Redeemers BabbageEra)
  Binary.txSizeSpec @BabbageEra
