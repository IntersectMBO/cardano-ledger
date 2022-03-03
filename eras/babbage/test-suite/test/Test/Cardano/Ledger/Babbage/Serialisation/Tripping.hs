{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Serialisation.Tripping where

import Cardano.Binary
import Cardano.Ledger.Alonzo.Language (Language (PlutusV1))
import Cardano.Ledger.Alonzo.Scripts (decodeCostModel)
import Cardano.Ledger.Babbage (AuxiliaryData, BabbageEra, Script)
import Cardano.Ledger.Babbage.PParams (PParams, PParamsUpdate)
import Cardano.Ledger.Babbage.Rules.Utxo (BabbageUtxoPred)
import Cardano.Ledger.Babbage.TxBody (TxBody)
import Cardano.Ledger.Block (Block)
import Cardano.Ledger.Shelley.Metadata (Metadata)
import qualified Cardano.Ledger.Shelley.Tx as LTX
import Cardano.Protocol.TPraos.BHeader (BHeader)
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Term (decodeTerm)
import qualified Data.ByteString.Base16.Lazy as Base16
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Roundtrip (roundTrip, roundTrip', roundTripAnn)
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
    Right (remaining, _) ->
      counterexample
        ("Unconsumed trailing bytes:\n" <> BSL.unpack remaining)
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
trippingAnn x = trippingF roundTripAnn x

tripping :: (Eq src, Show src, ToCBOR src, FromCBOR src) => src -> Property
tripping x = trippingF roundTrip x

tests :: TestTree
tests =
  testGroup
    "Babbage CBOR round-trip"
    [ testProperty "babbage/Script" $
        trippingAnn @(Script (BabbageEra C_Crypto)),
      testProperty "babbage/Metadata" $
        trippingAnn @(Metadata (BabbageEra C_Crypto)),
      testProperty "babbage/TxBody" $
        trippingAnn @(TxBody (BabbageEra C_Crypto)),
      testProperty "babbage/CostModel" $
        trippingF (roundTrip' toCBOR (decodeCostModel PlutusV1)),
      testProperty "babbage/PParams" $
        tripping @(PParams (BabbageEra C_Crypto)),
      testProperty "babbage/PParamsUpdate" $
        tripping @(PParamsUpdate (BabbageEra C_Crypto)),
      testProperty "babbage/AuxiliaryData" $
        trippingAnn @(AuxiliaryData (BabbageEra C_Crypto)),
      testProperty "Script" $
        trippingAnn @(Script (BabbageEra C_Crypto)),
      testProperty "babbage/Tx" $
        trippingAnn @(LTX.Tx (BabbageEra C_Crypto)),
      testProperty "babbage/BabbageUtxoPred" $
        tripping @(BabbageUtxoPred (BabbageEra C_Crypto)),
      testProperty "babbage/Block" $
        trippingAnn @(Block (BHeader C_Crypto) (BabbageEra C_Crypto))
    ]
