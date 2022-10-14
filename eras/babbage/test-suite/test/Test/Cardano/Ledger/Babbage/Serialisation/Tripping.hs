{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Serialisation.Tripping where

import Cardano.Ledger.Alonzo.Scripts (CostModels)
import Cardano.Ledger.Babbage (Babbage)
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure)
import Cardano.Ledger.Block (Block)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Metadata (ShelleyTxAuxData)
import Cardano.Protocol.TPraos.BHeader (BHeader)
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Babbage CBOR round-trip"
    [ testProperty "babbage/Script" $
        roundTripAnnExpectation @(Script Babbage) v,
      testProperty "babbage/Metadata" $
        roundTripAnnExpectation @(ShelleyTxAuxData Babbage) v,
      testProperty "babbage/TxOut" $
        roundTripCborExpectation @(TxOut Babbage) v,
      testProperty "babbage/TxBody" $
        roundTripAnnExpectation @(TxBody Babbage) v,
      testProperty "babbage/CostModel" $
        roundTripCborExpectation @CostModels v,
      testProperty "babbage/PParams" $
        roundTripCborExpectation @(PParams Babbage) v,
      testProperty "babbage/PParamsUpdate" $
        roundTripCborExpectation @(PParamsUpdate Babbage) v,
      testProperty "babbage/AuxiliaryData" $
        roundTripAnnExpectation @(TxAuxData Babbage) v,
      testProperty "Script" $
        roundTripAnnExpectation @(Script Babbage) v,
      testProperty "babbage/Tx" $
        roundTripAnnExpectation @(Tx Babbage) v,
      testProperty "babbage/BabbageUtxoPredFailure" $
        roundTripCborExpectation @(BabbageUtxoPredFailure Babbage) v,
      testProperty "babbage/Block" $
        roundTripAnnExpectation @(Block (BHeader StandardCrypto) Babbage) v
    ]
  where
    -- TODO iterate over all protocol versions of an era
    v = eraProtVerHigh @Babbage
