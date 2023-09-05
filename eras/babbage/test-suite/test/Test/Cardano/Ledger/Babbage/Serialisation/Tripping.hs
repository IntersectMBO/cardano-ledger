{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Serialisation.Tripping where

import Cardano.Ledger.Babbage (Babbage)
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure)
import Cardano.Ledger.Block (Block)
import Cardano.Ledger.Core
import Cardano.Protocol.TPraos.BHeader (BHeader)
import Test.Cardano.Ledger.Babbage.Arbitrary ()
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
    [ testProperty "babbage/BabbageUtxoPredFailure" $
        roundTripCborRangeExpectation @(BabbageUtxoPredFailure Babbage)
          (eraProtVerLow @Babbage)
          (eraProtVerHigh @Babbage)
    , testProperty "babbage/Block" $
        roundTripAnnRangeExpectation @(Block (BHeader StandardCrypto) Babbage)
          (eraProtVerLow @Babbage)
          (eraProtVerHigh @Babbage)
    ]
