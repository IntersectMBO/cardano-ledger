{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Serialisation.Tripping where

import Cardano.Ledger.Alonzo.Scripts (CostModels)
import Cardano.Ledger.Babbage (Babbage)
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure)
import Cardano.Ledger.Binary.Version (natVersion)
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
        roundTripAnnRangeExpectation @(Script Babbage)
          (eraProtVerLow @Babbage)
          (eraProtVerHigh @Babbage)
    , testProperty "babbage/Metadata" $
        roundTripAnnRangeExpectation @(TxAuxData Babbage)
          (eraProtVerLow @Babbage)
          (eraProtVerHigh @Babbage)
    , testProperty "babbage/TxOut" $
        roundTripCborExpectation @(TxOut Babbage)
    , testProperty "babbage/TxBody" $
        roundTripAnnRangeExpectation @(TxBody Babbage)
          (eraProtVerLow @Babbage)
          (eraProtVerHigh @Babbage)
    , testProperty "babbage/CostModel" $
        roundTripCborRangeExpectation @CostModels
          (natVersion @2)
          maxBound
    , testProperty "babbage/PParams" $
        roundTripCborRangeExpectation @(PParams Babbage)
          (eraProtVerLow @Babbage)
          (eraProtVerHigh @Babbage)
    , testProperty "babbage/PParamsUpdate" $
        roundTripCborRangeExpectation @(PParamsUpdate Babbage)
          (eraProtVerLow @Babbage)
          (eraProtVerHigh @Babbage)
    , testProperty "babbage/AuxiliaryData" $
        roundTripAnnRangeExpectation @(TxAuxData Babbage)
          (eraProtVerLow @Babbage)
          (eraProtVerHigh @Babbage)
    , testProperty "Script" $
        roundTripAnnRangeExpectation @(Script Babbage)
          (eraProtVerLow @Babbage)
          (eraProtVerHigh @Babbage)
    , testProperty "babbage/Tx" $
        roundTripAnnRangeExpectation @(Tx Babbage)
          (eraProtVerLow @Babbage)
          (eraProtVerHigh @Babbage)
    , testProperty "babbage/BabbageUtxoPredFailure" $
        roundTripCborExpectation @(BabbageUtxoPredFailure Babbage)
    , testProperty "babbage/Block" $
        roundTripAnnRangeExpectation @(Block (BHeader StandardCrypto) Babbage)
          (eraProtVerLow @Babbage)
          (eraProtVerHigh @Babbage)
    ]
