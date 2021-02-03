{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Serialisation.Tripping where

import Cardano.Binary
import Cardano.Ledger.Alonzo
import Cardano.Ledger.Alonzo.Data (AuxiliaryData, Data)
import Cardano.Ledger.Alonzo.PParams (PParams, PParamsUpdate)
import Cardano.Ledger.Alonzo.Scripts (Script)
import Cardano.Ledger.Alonzo.Tx (CostModel, Tx)
import Cardano.Ledger.Alonzo.TxBody (TxBody)
import Cardano.Ledger.Alonzo.TxWitness
import qualified Data.ByteString.Base16.Lazy as Base16
import qualified Data.ByteString.Lazy.Char8 as BSL
import Shelley.Spec.Ledger.Metadata (Metadata)
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
      ( concat
          [ "Failed to decode: ",
            show stuff,
            "\nbytes: ",
            show (Base16.encode (serialize x))
          ]
      )
      False

tests :: TestTree
tests =
  testGroup
    "Alonzo CBOR round-trip"
    [ testProperty "alonzo/Script" $
        trippingAnn @(Script (AlonzoEra C_Crypto)),
      testProperty "alonzo/Data" $
        trippingAnn @(Data (AlonzoEra C_Crypto)),
      testProperty "alonzo/Metadata" $
        trippingAnn @(Metadata),
      testProperty "alonzo/TxWitness" $
        trippingAnn @(TxWitness (AlonzoEra C_Crypto)),
      testProperty "alonzo/TxBody" $
        trippingAnn @(TxBody (AlonzoEra C_Crypto)),
      testProperty "alonzo/Tx" $
        trippingAnn @(Tx (AlonzoEra C_Crypto)),
      testProperty "alonzo/CostModel" $
        trippingAnn @CostModel,
      testProperty "alonzo/PParams" $
        trippingAnn @(PParams (AlonzoEra C_Crypto)),
      testProperty "alonzo/PParamUpdate" $
        trippingAnn @(PParamsUpdate (AlonzoEra C_Crypto)),
      testProperty "alonzo/AuxiliaryData" $
        trippingAnn @(AuxiliaryData (AlonzoEra C_Crypto))
    ]
