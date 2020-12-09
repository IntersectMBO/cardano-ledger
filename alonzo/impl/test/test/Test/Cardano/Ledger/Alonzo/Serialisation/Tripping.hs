{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Serialisation.Tripping where

import Cardano.Binary
import Cardano.Ledger.Alonzo
import Cardano.Ledger.Alonzo.TxWitness (TxWitness)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Coders
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
import Test.Tasty
import Test.Tasty.QuickCheck

trippingAnn ::
  ( Eq t,
    Show t,
    ToCBOR t,
    FromCBOR (Annotator t)
  ) =>
  t ->
  Property
trippingAnn x = case roundTripAnn x of
  Right (remaining, y) | BSL.null remaining -> x === y
  Right (remaining, _) ->
    counterexample
      ("Unconsumed trailing bytes:\n" <> BSL.unpack remaining)
      False
  Left stuff ->
    counterexample
      ("Failed to decode: " <> show stuff)
      False

tests :: TestTree
tests =
  testGroup
    "Alonzo CBOR round-trip"
    [ testProperty "alonzo/TxWitness" $
        trippingAnn @(TxWitness (AlonzoEra C_Crypto))
    ]
