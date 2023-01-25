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
import Cardano.Ledger.Alonzo.TxWitness (TxWitness)
import Cardano.Ledger.Block (Block)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Metadata (Metadata)
import Cardano.Protocol.TPraos.BHeader (BHeader)
import qualified Data.ByteString.Base16.Lazy as Base16
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Roundtrip (roundTrip, roundTripAnn)
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
              show stuff,
              "\nbytes: ",
              show (Base16.encode (serialize x))
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
    "Alonzo CBOR round-trip"
    [ testProperty "alonzo/Script" $
        trippingAnn @(Script (AlonzoEra C_Crypto)),
      testProperty "alonzo/Data" $
        trippingAnn @(Data (AlonzoEra C_Crypto)),
      testProperty "alonzo/BinaryData" $
        tripping @(BinaryData (AlonzoEra C_Crypto)),
      testProperty "alonzo/Metadata" $
        trippingAnn @(Metadata (AlonzoEra C_Crypto)),
      testProperty "alonzo/TxWitness" $
        trippingAnn @(TxWitness (AlonzoEra C_Crypto)),
      testProperty "alonzo/TxBody" $
        trippingAnn @(TxBody (AlonzoEra C_Crypto)),
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
        trippingAnn @(Block (BHeader C_Crypto C_Crypto) (AlonzoEra C_Crypto))
    ]
