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
import Cardano.Ledger.Alonzo.Scripts (eqAlonzoScriptRaw)
import Cardano.Ledger.Plutus.Data (BinaryData, Data (..))
import Cardano.Ledger.Block (Block)
import Cardano.Ledger.Core
import Cardano.Ledger.MemoBytes (zipMemoRawType)
import Cardano.Protocol.TPraos.BHeader (BHeader)
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
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
    [ skip $
        testProperty "alonzo/Script twiddled" $
          roundTripAnnTwiddledProperty @(Script Alonzo) eqAlonzoScriptRaw
    , skip $
        testProperty "alonzo/Data twiddled" $
          roundTripAnnTwiddledProperty @(Data Alonzo) (zipMemoRawType (===))
    , skip $
        testProperty "alonzo/BinaryData twiddled" $
          roundTripTwiddledProperty @(BinaryData Alonzo)
    , skip $
        testProperty "alonzo/TxBody twiddled" $
          roundTripAnnTwiddledProperty @(TxBody Alonzo) (zipMemoRawType (===))
    , testProperty "alonzo/AlonzoUtxowPredFailure" $
        roundTripCborExpectation @(AlonzoUtxowPredFailure Alonzo)
    , testProperty "alonzo/AlonzoUtxoPredFailure" $
        roundTripCborExpectation @(AlonzoUtxoPredFailure Alonzo)
    , testProperty "alonzo/AlonzoUtxosPredFailure" $
        roundTripCborExpectation @(AlonzoUtxosPredFailure Alonzo)
    , testProperty "alonzo/Block" $
        roundTripAnnRangeExpectation @(Block (BHeader StandardCrypto) Alonzo)
          (eraProtVerLow @Alonzo)
          (eraProtVerHigh @Alonzo)
    ]
  where
    skip _ = testProperty "Test skipped" True
