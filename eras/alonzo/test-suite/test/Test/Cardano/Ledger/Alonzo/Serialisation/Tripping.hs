{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Serialisation.Tripping where

import Cardano.Binary
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Data (BinaryData, Data (..))
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxoPredFailure, AlonzoUtxosPredFailure, AlonzoUtxowPredFailure)
import Cardano.Ledger.Alonzo.Scripts (CostModels)
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits)
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxBody, txBodyRawEq)
import Cardano.Ledger.Alonzo.TxWitness (TxWitness)
import Cardano.Ledger.Block (Block)
import Cardano.Ledger.Core
import Cardano.Ledger.SafeHash (HashAnnotated (..), SafeToHash (makeHashWithExplicitProxys))
import Cardano.Ledger.Shelley.Metadata (Metadata)
import Cardano.Protocol.TPraos.BHeader (BHeader)
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Term
import qualified Data.ByteString.Base16.Lazy as Base16
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Data (Proxy (..))
import Data.Functor.Identity (Identity (..))
import Data.Roundtrip (roundTrip, roundTripAnn, roundTripAnnWithTwiddling)
import Data.Twiddle (Twiddle (..))
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.Tasty
import Test.Tasty.QuickCheck
import Codec.CBOR.Write (toLazyByteString)

trippingF ::
  (Eq src, Show src, Show target, ToCBOR src) =>
  (src -> Either target (BSL.ByteString, src)) ->
  src ->
  Property
trippingF f x = runIdentity $ trippingM g (===) x
  where
    g a = pure (f a, toLazyByteString $ toCBOR a)

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

compareTxBody ::
  TxBody (AlonzoEra C_Crypto) ->
  TxBody (AlonzoEra C_Crypto) ->
  Property
compareTxBody x y = counterexample "TxBody contents are different" (txBodyRawEq x y)

tests :: TestTree
tests =
  testGroup
    "Alonzo CBOR round-trip"
    [ testProperty "alonzo/Script" $
        trippingAnn @(Script (AlonzoEra C_Crypto)),
      testProperty "alonzo/Data" $
        trippingAnn @(Data (AlonzoEra C_Crypto)),
      testProperty "alonzo/BinaryData" $
        tripping @(BinaryData (AlonzoEra C_Crypto)),
      testProperty "alonzo/Metadata" $
        trippingAnn @(Metadata (AlonzoEra C_Crypto)),
      testProperty "alonzo/AlonzoTxWits" $
        trippingAnn @(AlonzoTxWits (AlonzoEra C_Crypto)),
      testProperty "alonzo/TxBody" $
        trippingAnn @(TxBody (AlonzoEra C_Crypto)),
      testProperty "alonzo/TxBody twiddled" $
        trippingAnnWithTwiddling @(TxBody (AlonzoEra C_Crypto)) compareTxBody,
      testProperty "alonzo/CostModels" $
        tripping @CostModels,
      testProperty "alonzo/PParams" $
        tripping @(PParams (AlonzoEra C_Crypto)),
      testProperty "alonzo/PParamsUpdate" $
        tripping @(PParamsUpdate (AlonzoEra C_Crypto)),
      testProperty "alonzo/AuxiliaryData" $
        trippingAnn @(AuxiliaryData (AlonzoEra C_Crypto)),
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

test :: IO ()
test = do
  x <- generate . resize 1 $ arbitrary @(AlonzoTxBody (AlonzoEra C_Crypto))
  let hash = makeHashWithExplicitProxys (Proxy @C_Crypto) (Proxy @BSL.ByteString)
  let xb = toStrictByteString $ toCBOR x
  putStrLn $ "Untwiddled hash: " <> show (hash xb)
  y <- generate $ twiddle x
  let yb = toStrictByteString $ encodeTerm y
  putStrLn $ "Twiddled hash: " <> show (hash yb)
  let yr = deserialiseFromBytes (fromCBOR @(Annotator (AlonzoTxBody (AlonzoEra C_Crypto)))) $ BSL.fromStrict yb
  let y' = case yr of
        Left err -> error $ show err
        Right (leftover, Annotator f)
          | BSL.null leftover -> f . Full $ BSL.fromStrict yb
          | otherwise -> error $ "Leftover bytes: " <> show leftover
  putStrLn $ "Twiddled roundtrip hash: " <> show (hashAnnotated y')
