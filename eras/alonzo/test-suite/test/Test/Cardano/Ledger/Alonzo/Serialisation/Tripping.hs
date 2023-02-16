{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Serialisation.Tripping where

import Cardano.Ledger.Alonzo (Alonzo)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
  AlonzoUtxowPredFailure,
 )
import Cardano.Ledger.Alonzo.Scripts (CostModels, eqAlonzoScriptRaw)
import Cardano.Ledger.Alonzo.Scripts.Data (BinaryData, Data (..))
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits)
import Cardano.Ledger.Binary.Version (natVersion)
import Cardano.Ledger.Block (Block)
import Cardano.Ledger.Core
import Cardano.Ledger.MemoBytes (zipMemoRawType)
import Cardano.Ledger.Shelley.TxAuxData (ShelleyTxAuxData)
import Cardano.Protocol.TPraos.BHeader (BHeader)
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.Cardano.Protocol.TPraos.Arbitrary ()
import Test.Cardano.Protocol.TPraos.ConcreteCryptoTypes (StandardCrypto)
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Alonzo CBOR round-trip"
    [ testProperty "alonzo/Script" $
        roundTripAnnRangeExpectation @(Script Alonzo)
          (eraProtVerLow @Alonzo)
          (eraProtVerHigh @Alonzo)
    , skip $
        testProperty "alonzo/Script twiddled" $
          roundTripAnnTwiddledProperty @(Script Alonzo) eqAlonzoScriptRaw
    , testProperty "alonzo/Data" $
        roundTripAnnRangeExpectation @(Data Alonzo)
          (eraProtVerLow @Alonzo)
          (eraProtVerHigh @Alonzo)
    , skip $
        testProperty "alonzo/Data twiddled" $
          roundTripAnnTwiddledProperty @(Data Alonzo) (zipMemoRawType (===))
    , testProperty "alonzo/BinaryData" $
        roundTripCborExpectation @(BinaryData Alonzo)
    , skip $
        testProperty "alonzo/BinaryData twiddled" $
          roundTripTwiddledProperty @(BinaryData Alonzo)
    , testProperty "alonzo/TxAuxData" $
        roundTripAnnRangeExpectation @(ShelleyTxAuxData Alonzo)
          (eraProtVerLow @Alonzo)
          (eraProtVerHigh @Alonzo)
    , testProperty "alonzo/AlonzoTxWits" $
        roundTripAnnRangeExpectation @(AlonzoTxWits Alonzo)
          (eraProtVerLow @Alonzo)
          (eraProtVerHigh @Alonzo)
    , testProperty "alonzo/TxBody" $
        roundTripAnnRangeExpectation @(TxBody Alonzo)
          (eraProtVerLow @Alonzo)
          (eraProtVerHigh @Alonzo)
    , skip $
        testProperty "alonzo/TxBody twiddled" $
          roundTripAnnTwiddledProperty @(TxBody Alonzo) (zipMemoRawType (===))
    , testProperty "alonzo/CostModels" $
        roundTripCborRangeExpectation @CostModels
          (natVersion @2)
          maxBound
    , testProperty "alonzo/PParams" $
        roundTripCborRangeExpectation @(PParams Alonzo)
          (eraProtVerLow @Alonzo)
          (eraProtVerHigh @Alonzo)
    , testProperty "alonzo/PParamsUpdate" $
        roundTripCborRangeExpectation @(PParamsUpdate Alonzo)
          (eraProtVerLow @Alonzo)
          (eraProtVerHigh @Alonzo)
    , testProperty "alonzo/AuxiliaryData" $
        roundTripAnnRangeExpectation @(TxAuxData Alonzo)
          (eraProtVerLow @Alonzo)
          (eraProtVerHigh @Alonzo)
    , testProperty "alonzo/AlonzoUtxowPredFailure" $
        roundTripCborExpectation @(AlonzoUtxowPredFailure Alonzo)
    , testProperty "alonzo/AlonzoUtxoPredFailure" $
        roundTripCborExpectation @(AlonzoUtxoPredFailure Alonzo)
    , testProperty "alonzo/AlonzoUtxosPredFailure" $
        roundTripCborExpectation @(AlonzoUtxosPredFailure Alonzo)
    , testProperty "Script" $
        roundTripAnnRangeExpectation @(Script Alonzo)
          (eraProtVerLow @Alonzo)
          (eraProtVerHigh @Alonzo)
    , testProperty "alonzo/Tx" $
        roundTripAnnRangeExpectation @(Tx Alonzo)
          (eraProtVerLow @Alonzo)
          (eraProtVerHigh @Alonzo)
    , testProperty "alonzo/Block" $
        roundTripAnnRangeExpectation @(Block (BHeader StandardCrypto) Alonzo)
          (eraProtVerLow @Alonzo)
          (eraProtVerHigh @Alonzo)
    ]
  where
    skip _ = testProperty "Test skipped" True
