{-# LANGUAGE AllowAmbiguousTypes #-}
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
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits, Redeemers)
import Cardano.Ledger.Block (Block)
import Cardano.Ledger.Core
import Cardano.Ledger.MemoBytes (zipMemoRawType)
import Cardano.Ledger.Shelley.TxAuxData (ShelleyTxAuxData)
import Cardano.Protocol.TPraos.BHeader (BHeader)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Alonzo CBOR round-trip"
    [ testProperty "alonzo/Script" $
        roundTripAnnExpectation @(Script Alonzo)
    , skip $
        testProperty "alonzo/Script twiddled" $
          roundTripAnnTwiddledProperty @(Script Alonzo) eqAlonzoScriptRaw
    , testProperty "alonzo/Data" $
        roundTripAnnExpectation @(Data Alonzo)
    , skip $
        testProperty "alonzo/Data twiddled" $
          roundTripAnnTwiddledProperty @(Data Alonzo) (zipMemoRawType (===))
    , testProperty "alonzo/BinaryData" $
        roundTripCborExpectation @(BinaryData Alonzo)
    , skip $
        testProperty "alonzo/BinaryData twiddled" $
          roundTripTwiddledProperty @(BinaryData Alonzo)
    , testProperty "alonzo/TxAuxData" $
        roundTripAnnExpectation @(ShelleyTxAuxData Alonzo)
    , testProperty "alonzo/Redeemers" $
        roundTripAnnExpectation @(Redeemers Alonzo)
    , testProperty "alonzo/AlonzoTxWits" $
        roundTripAnnExpectation @(AlonzoTxWits Alonzo)
    , testProperty "alonzo/TxBody" $
        roundTripAnnExpectation @(TxBody Alonzo)
    , skip $
        testProperty "alonzo/TxBody twiddled" $
          roundTripAnnTwiddledProperty @(TxBody Alonzo) (zipMemoRawType (===))
    , testProperty "alonzo/CostModels" $
        roundTripCborExpectation @CostModels
    , testProperty "alonzo/PParams" $
        roundTripCborExpectation @(PParams Alonzo)
    , testProperty "alonzo/PParamsUpdate" $
        roundTripCborExpectation @(PParamsUpdate Alonzo)
    , testProperty "alonzo/AuxiliaryData" $
        roundTripAnnExpectation @(TxAuxData Alonzo)
    , testProperty "alonzo/AlonzoUtxowPredFailure" $
        roundTripCborExpectation @(AlonzoUtxowPredFailure Alonzo)
    , testProperty "alonzo/AlonzoUtxoPredFailure" $
        roundTripCborExpectation @(AlonzoUtxoPredFailure Alonzo)
    , testProperty "alonzo/AlonzoUtxosPredFailure" $
        roundTripCborExpectation @(AlonzoUtxosPredFailure Alonzo)
    , testProperty "Script" $
        roundTripAnnExpectation @(Script Alonzo)
    , testProperty "alonzo/Tx" $
        roundTripAnnExpectation @(Tx Alonzo)
    , testProperty "alonzo/Block" $
        roundTripAnnExpectation @(Block (BHeader StandardCrypto) Alonzo)
    ]
  where
    skip _ = testProperty "Test skipped" True
