{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Serialisation.Tripping where

import Cardano.Ledger.Alonzo (Alonzo)
import Cardano.Ledger.Alonzo.Data (BinaryData, Data (..))
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxoPredFailure, AlonzoUtxosPredFailure, AlonzoUtxowPredFailure)
import Cardano.Ledger.Alonzo.Scripts (CostModels)
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits)
import Cardano.Ledger.Block (Block)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Metadata (ShelleyTxAuxData)
import Cardano.Protocol.TPraos.BHeader (BHeader)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.Cardano.Ledger.Binary.RoundTrip
-- import Test.Cardano.Ledger.Binary.Twiddle (Twiddle (..))
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.Tasty
import Test.Tasty.QuickCheck

-- trippingF ::
--   (Eq src, Show src, Show target, ToCBOR src) =>
--   (src -> Either target (BSL.ByteString, src)) ->
--   src ->
--   Property
-- trippingF f x = runIdentity $ trippingM f' (===) x
--   where
--     f' a = pure (f a, toLazyByteString $ toCBOR a)

-- trippingM ::
--   (Monad m, Show target) =>
--   (src -> m (Either target (BSL.ByteString, src), BSL.ByteString)) ->
--   (src -> src -> Property) ->
--   src ->
--   m Property
-- trippingM f g x = do
--   res <- f x
--   pure $ case res of
--     (Right (remaining, y), _)
--       | BSL.null remaining ->
--           g x y
--     (Right (remaining, _), _) ->
--       counterexample
--         ("Unconsumed trailing bytes:\n" <> BSL.unpack remaining)
--         False
--     (Left stuff, bs) ->
--       counterexample
--         ( concat
--             [ "Failed to decode: ",
--               show stuff,
--               "\nbytes: ",
--               show (Base16.encode bs)
--             ]
--         )
--         False

-- trippingAnn ::
--   ( Eq t,
--     Show t,
--     ToCBOR t,
--     FromCBOR (Annotator t)
--   ) =>
--   t ->
--   Property
-- trippingAnn = trippingF roundTripAnn

-- trippingAnnWithTwiddling :: (Twiddle t, FromCBOR (Annotator t)) => (t -> t -> Property) -> t -> Property
-- trippingAnnWithTwiddling comp x = property $ trippingM roundTripAnnWithTwiddling comp x

-- tripping :: (Eq src, Show src, ToCBOR src, FromCBOR src) => src -> Property
-- tripping = trippingF roundTrip

-- trippingWithTwiddling :: (Twiddle src, FromCBOR src) => (src -> src -> Property) -> src -> Property
-- trippingWithTwiddling comp x = property $ trippingM roundTripWithTwiddling comp x

tests :: TestTree
tests =
  testGroup
    "Alonzo CBOR round-trip"
    [ testProperty "alonzo/Script" $
        roundTripAnnExpectation @(Script Alonzo) v,
      -- skip $
      --   testTwiddlingAnnWith @(Script Alonzo)
      --     "alonzo/Script twiddled"
      --     Script.contentsEq,
      testProperty "alonzo/Data" $
        roundTripAnnExpectation @(Data Alonzo) v,
      -- skip $
      --   testTwiddlingAnnWith @(Data Alonzo)
      --     "alonzo/Data twiddled"
      --     Data.contentsEq,
      testProperty "alonzo/BinaryData" $
        roundTripCborExpectation @(BinaryData Alonzo) v,
      -- skip $
      --   testTwiddling @(BinaryData Alonzo)
      --     "alonzo/BinaryData twiddled",
      testProperty "alonzo/TxAuxData" $
        roundTripAnnExpectation @(ShelleyTxAuxData Alonzo) v,
      testProperty "alonzo/AlonzoTxWits" $
        roundTripAnnExpectation @(AlonzoTxWits Alonzo) v,
      testProperty "alonzo/TxBody" $
        roundTripAnnExpectation @(TxBody Alonzo) v,
      -- skip $
      --   testTwiddlingAnnWith
      --     @(TxBody Alonzo)
      --     "alonzo/TxBody twiddled"
      --     txBodyRawEq,
      testProperty "alonzo/CostModels" $
        roundTripCborExpectation @CostModels v,
      testProperty "alonzo/PParams" $
        roundTripCborExpectation @(PParams Alonzo) v,
      testProperty "alonzo/PParamsUpdate" $
        roundTripCborExpectation @(PParamsUpdate Alonzo) v,
      testProperty "alonzo/AuxiliaryData" $
        roundTripAnnExpectation @(TxAuxData Alonzo) v,
      testProperty "alonzo/AlonzoUtxowPredFailure" $
        roundTripCborExpectation @(AlonzoUtxowPredFailure Alonzo) v,
      testProperty "alonzo/AlonzoUtxoPredFailure" $
        roundTripCborExpectation @(AlonzoUtxoPredFailure Alonzo) v,
      testProperty "alonzo/AlonzoUtxosPredFailure" $
        roundTripCborExpectation @(AlonzoUtxosPredFailure Alonzo) v,
      testProperty "Script" $
        roundTripAnnExpectation @(Script Alonzo) v,
      testProperty "alonzo/Tx" $
        roundTripAnnExpectation @(Tx Alonzo) v,
      testProperty "alonzo/Block" $
        roundTripAnnExpectation @(Block (BHeader StandardCrypto) Alonzo) v
    ]
  where
    v = eraProtVerHigh @Alonzo
    -- testTwiddlingAnnWith ::
    --   forall a.
    --   (Twiddle a, Arbitrary a, Show a, FromCBOR (Annotator a)) =>
    --   TestName ->
    --   (a -> a -> Bool) ->
    --   TestTree
    -- testTwiddlingAnnWith name comp =
    --   testProperty name $ trippingAnnWithTwiddling (\x y -> counterexample (msg x y) $ comp x y)
    --   where
    --     msg x y = "Contents differ:\n" <> show x <> "\n" <> show y

    -- testTwiddling ::
    --   forall a.
    --   ( Arbitrary a,
    --     Twiddle a,
    --     FromCBOR a,
    --     Show a,
    --     Eq a
    --   ) =>
    --   TestName ->
    --   TestTree
    -- testTwiddling name = testProperty name $ trippingWithTwiddling @a (===)

    _skip _ = testProperty "Test skipped" True
