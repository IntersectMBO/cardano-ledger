{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Serialisation.Tripping where

import Cardano.Ledger.Alonzo (Alonzo)
import Cardano.Ledger.Alonzo.Data (BinaryData, Data (..))
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxoPredFailure, AlonzoUtxosPredFailure, AlonzoUtxowPredFailure)
import Cardano.Ledger.Alonzo.Scripts (CostModels)
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits)
import Cardano.Ledger.Block (Block)
import Cardano.Ledger.Core
import Cardano.Ledger.MemoBytes (zipMemoRawType)
import Cardano.Ledger.Shelley.Metadata (ShelleyTxAuxData)
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
        roundTripAnnExpectation @(Script Alonzo) v,
      -- skip $
      -- testProperty "alonzo/Script twiddled" $
      --   roundTripAnnTwiddledProperty @(Script Alonzo) (zipMemoRawType (===)) v,
      testProperty "alonzo/Data" $
        roundTripAnnExpectation @(Data Alonzo) v,
      skip $
      testProperty "alonzo/Data twiddled" $
        roundTripAnnTwiddledProperty @(Data Alonzo) (zipMemoRawType (===)) v,
      testProperty "alonzo/BinaryData" $
        roundTripCborExpectation @(BinaryData Alonzo) v,
      skip $
      testProperty "alonzo/BinaryData twiddled" $
        roundTripTwiddledProperty @(BinaryData Alonzo) v,
      testProperty "alonzo/TxAuxData" $
        roundTripAnnExpectation @(ShelleyTxAuxData Alonzo) v,
      testProperty "alonzo/AlonzoTxWits" $
        roundTripAnnExpectation @(AlonzoTxWits Alonzo) v,
      testProperty "alonzo/TxBody" $
        roundTripAnnExpectation @(TxBody Alonzo) v,
      skip $
      testProperty "alonzo/TxBody twiddled" $
        roundTripAnnTwiddledProperty @(TxBody Alonzo) (zipMemoRawType (===)) v,
      testProperty "alonzo/CostModels" $
        roundTripCborExpectation @CostModels v,
      testProperty "alonzo/PParams" $
        roundTripCborExpectation @(PParams Alonzo) v,
      testProperty "alonzo/PParamsUpdate" $
        roundTripCborExpectation @(PParamsUpdate Alonzo) v,
      testProperty "alonzo/AuxiliaryData" $
        roundTripAnnExpectation @(TxAuxData Alonzo) v,
      testProperty "alonzo/AlonzoUtxowPredFailure" $
        roundTripCborExpectation @(AlonzoUtxowPredFailure Alonzo) v,
      testProperty "alonzo/AlonzoUtxoPredFailure" $
        roundTripCborExpectation @(AlonzoUtxoPredFailure Alonzo) v,
      testProperty "alonzo/AlonzoUtxosPredFailure" $
        roundTripCborExpectation @(AlonzoUtxosPredFailure Alonzo) v,
      testProperty "Script" $
        roundTripAnnExpectation @(Script Alonzo) v,
      testProperty "alonzo/Tx" $
        roundTripAnnExpectation @(Tx Alonzo) v,
      testProperty "alonzo/Block" $
        roundTripAnnExpectation @(Block (BHeader StandardCrypto) Alonzo) v
    ]
  where
    v = eraProtVerHigh @Alonzo

    skip _ = testProperty "Test skipped" True
