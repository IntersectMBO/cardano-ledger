{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Serialisation.Tripping where

import Cardano.Binary
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Data (BinaryData, Data (..))
import qualified Cardano.Ledger.Alonzo.Data as Data
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxoPredFailure, AlonzoUtxosPredFailure, AlonzoUtxowPredFailure)
import Cardano.Ledger.Alonzo.Scripts (CostModels)
import qualified Cardano.Ledger.Alonzo.Scripts as Script
import Cardano.Ledger.Alonzo.TxBody (txBodyRawEq)
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits)
import Cardano.Ledger.Block (Block)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Metadata (ShelleyTxAuxData)
import Cardano.Protocol.TPraos.BHeader (BHeader)
import Codec.CBOR.Write (toLazyByteString)
import qualified Data.ByteString.Base16.Lazy as Base16
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Functor.Identity (Identity (..))
import Data.Roundtrip (roundTrip, roundTripAnn, roundTripAnnWithTwiddling, roundTripWithTwiddling)
import Data.Twiddle (Twiddle (..))
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.Tasty
import Test.Tasty.QuickCheck

trippingF ::
  (Eq src, Show src, Show target, ToCBOR src) =>
  (src -> Either target (BSL.ByteString, src)) ->
  src ->
  Property
trippingF f x = runIdentity $ trippingM f' (===) x
  where
    f' a = pure (f a, toLazyByteString $ toCBOR a)

trippingM ::
  (Monad m, Show target) =>
  (src -> m (Either target (BSL.ByteString, src), BSL.ByteString)) ->
  (src -> src -> Property) ->
  src ->
  m Property
trippingM f g x = do
  res <- f x
  pure $ case res of
    (Right (remaining, y), _)
      | BSL.null remaining ->
          g x y
    (Right (remaining, _), _) ->
      counterexample
        ("Unconsumed trailing bytes:\n" <> BSL.unpack remaining)
        False
    (Left stuff, bs) ->
      counterexample
        ( concat
            [ "Failed to decode: ",
              show stuff,
              "\nbytes: ",
              show (Base16.encode bs)
            ]
        )
        False

trippingAnn ::
  ( Eq t,
    Show t,
    ToCBOR t,
    FromCBOR (Annotator t)
  ) =>
  t ->
  Property
trippingAnn = trippingF roundTripAnn

trippingAnnWithTwiddling :: (Twiddle t, FromCBOR (Annotator t)) => (t -> t -> Property) -> t -> Property
trippingAnnWithTwiddling comp x = property $ trippingM roundTripAnnWithTwiddling comp x

tripping :: (Eq src, Show src, ToCBOR src, FromCBOR src) => src -> Property
tripping = trippingF roundTrip

trippingWithTwiddling :: (Twiddle src, FromCBOR src) => (src -> src -> Property) -> src -> Property
trippingWithTwiddling comp x = property $ trippingM roundTripWithTwiddling comp x

tests :: TestTree
tests =
  testGroup
    "Alonzo CBOR round-trip"
    [ testProperty "alonzo/Script" $
        trippingAnn @(Script (AlonzoEra C_Crypto)),
      skip $
        testTwiddlingAnnWith @(Script (AlonzoEra C_Crypto))
          "alonzo/Script twiddled"
          Script.contentsEq,
      testProperty "alonzo/Data" $
        trippingAnn @(Data (AlonzoEra C_Crypto)),
      skip $
        testTwiddlingAnnWith @(Data (AlonzoEra C_Crypto))
          "alonzo/Data twiddled"
          Data.contentsEq,
      testProperty "alonzo/BinaryData" $
        tripping @(BinaryData (AlonzoEra C_Crypto)),
      skip $
        testTwiddling @(BinaryData (AlonzoEra C_Crypto))
          "alonzo/BinaryData twiddled",
      testProperty "alonzo/TxAuxData" $
        trippingAnn @(ShelleyTxAuxData (AlonzoEra C_Crypto)),
      testProperty "alonzo/AlonzoTxWits" $
        trippingAnn @(AlonzoTxWits (AlonzoEra C_Crypto)),
      testProperty "alonzo/TxBody" $
        trippingAnn @(TxBody (AlonzoEra C_Crypto)),
      skip $
        testTwiddlingAnnWith
          @(TxBody (AlonzoEra C_Crypto))
          "alonzo/TxBody twiddled"
          txBodyRawEq,
      testProperty "alonzo/CostModels" $
        tripping @CostModels,
      testProperty "alonzo/PParams" $
        tripping @(PParams (AlonzoEra C_Crypto)),
      testProperty "alonzo/PParamsUpdate" $
        tripping @(PParamsUpdate (AlonzoEra C_Crypto)),
      testProperty "alonzo/AuxiliaryData" $
        trippingAnn @(TxAuxData (AlonzoEra C_Crypto)),
      testProperty "alonzo/AlonzoUtxowPredFailure" $
        tripping @(AlonzoUtxowPredFailure (AlonzoEra C_Crypto)),
      testProperty "alonzo/AlonzoUtxoPredFailure" $
        tripping @(AlonzoUtxoPredFailure (AlonzoEra C_Crypto)),
      testProperty "alonzo/AlonzoUtxosPredFailure" $
        tripping @(AlonzoUtxosPredFailure (AlonzoEra C_Crypto)),
      testProperty "Script" $
        trippingAnn @(Script (AlonzoEra C_Crypto)),
      testProperty "alonzo/Tx" $
        trippingAnn @(Tx (AlonzoEra C_Crypto)),
      testProperty "alonzo/Block" $
        trippingAnn @(Block (BHeader C_Crypto) (AlonzoEra C_Crypto))
    ]
  where
    testTwiddlingAnnWith ::
      forall a.
      (Twiddle a, Arbitrary a, Show a, FromCBOR (Annotator a)) =>
      TestName ->
      (a -> a -> Bool) ->
      TestTree
    testTwiddlingAnnWith name comp = testProperty name $ trippingAnnWithTwiddling (\x y -> counterexample (msg x y) $ comp x y)
      where
        msg x y = "Contents differ:\n" <> show x <> "\n" <> show y

    testTwiddling ::
      forall a.
      ( Arbitrary a,
        Twiddle a,
        FromCBOR a,
        Show a,
        Eq a
      ) =>
      TestName ->
      TestTree
    testTwiddling name = testProperty name $ trippingWithTwiddling @a (===)

    skip _ = testProperty "Test skipped" True
