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
        roundTripAnnExpectation @(Script Babbage)
    , testProperty "babbage/Metadata" $
        roundTripAnnExpectation @(TxAuxData Babbage)
    , testProperty "babbage/TxOut" $
        roundTripCborExpectation @(TxOut Babbage)
    , testProperty "babbage/TxBody" $
        roundTripAnnExpectation @(TxBody Babbage)
    , testProperty "babbage/CostModel" $
        roundTripCborExpectation @CostModels
    , testProperty "babbage/PParams" $
        roundTripCborExpectation @(PParams Babbage)
    , testProperty "babbage/PParamsUpdate" $
        roundTripCborExpectation @(PParamsUpdate Babbage)
    , testProperty "babbage/AuxiliaryData" $
        roundTripAnnExpectation @(TxAuxData Babbage)
    , testProperty "Script" $
        roundTripAnnExpectation @(Script Babbage)
    , testProperty "babbage/Tx" $
        roundTripAnnExpectation @(Tx Babbage)
    , testProperty "babbage/BabbageUtxoPredFailure" $
        roundTripCborExpectation @(BabbageUtxoPredFailure Babbage)
    , testProperty "babbage/Block" $
        roundTripAnnExpectation @(Block (BHeader StandardCrypto) Babbage)
    ]
