{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Serialisation.Tripping where

import Cardano.Binary
import Cardano.Ledger.Alonzo.Scripts (CostModels)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure)
import Cardano.Ledger.Block (Block)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Metadata (ShelleyTxAuxData)
import Cardano.Protocol.TPraos.BHeader (BHeader)
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Term (decodeTerm)
import qualified Data.ByteString.Base16.Lazy as Base16
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Roundtrip (roundTrip, roundTripAnn)
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.Tasty
import Test.Tasty.QuickCheck
import Text.Show.Pretty (ppShow)

trippingF ::
  (Eq src, Show src, Show target, ToCBOR src) =>
  (src -> Either target (BSL.ByteString, src)) ->
  src ->
  Property
trippingF f x =
  case f x of
    Right (remaining, y)
      | BSL.null remaining ->
          x === y
    Right (remaining, y) ->
      counterexample
        ( "Unconsumed trailing bytes:\n"
            <> BSL.unpack remaining
            <> "\nbad res: "
            <> show y
            <> "\nfull term: "
            <> case deserialiseFromBytes decodeTerm (serialize x) of
              Left e -> ppShow e
              Right (_, terms) -> ppShow terms
            <> "\nremaining term: "
            <> case deserialiseFromBytes decodeTerm (serialize y) of
              Left e -> ppShow e
              Right (_, terms) -> ppShow terms
        )
        False
    Left stuff ->
      counterexample
        ( concat
            [ "Failed to decode: ",
              ppShow stuff,
              "\nbytes: ",
              ppShow (Base16.encode (serialize x)),
              "\nterm: ",
              case deserialiseFromBytes decodeTerm (serialize x) of
                Left e -> ppShow e
                Right (_, terms) -> ppShow terms
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

tripping :: (Eq src, Show src, ToCBOR src, FromCBOR src) => src -> Property
tripping = trippingF roundTrip

tests :: TestTree
tests =
  testGroup
    "Babbage CBOR round-trip"
    [ testProperty "babbage/Script" $
        trippingAnn @(Script (BabbageEra C_Crypto)),
      testProperty "babbage/Metadata" $
        trippingAnn @(ShelleyTxAuxData (BabbageEra C_Crypto)),
      testProperty "babbage/TxOut" $
        tripping @(TxOut (BabbageEra C_Crypto)),
      testProperty "babbage/TxBody" $
        trippingAnn @(TxBody (BabbageEra C_Crypto)),
      testProperty "babbage/CostModel" $
        tripping @CostModels,
      testProperty "babbage/PParams" $
        tripping @(PParams (BabbageEra C_Crypto)),
      testProperty "babbage/PParamsUpdate" $
        tripping @(PParamsUpdate (BabbageEra C_Crypto)),
      testProperty "babbage/AuxiliaryData" $
        trippingAnn @(TxAuxData (BabbageEra C_Crypto)),
      testProperty "Script" $
        trippingAnn @(Script (BabbageEra C_Crypto)),
      testProperty "babbage/Tx" $
        trippingAnn @(Tx (BabbageEra C_Crypto)),
      testProperty "babbage/BabbageUtxoPredFailure" $
        tripping @(BabbageUtxoPredFailure (BabbageEra C_Crypto)),
      testProperty "babbage/Block" $
        trippingAnn @(Block (BHeader C_Crypto) (BabbageEra C_Crypto))
    ]
