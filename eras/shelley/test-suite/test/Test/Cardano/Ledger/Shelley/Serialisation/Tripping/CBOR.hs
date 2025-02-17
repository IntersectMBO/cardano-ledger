{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Shelley.Serialisation.Tripping.CBOR (
  tests,
)
where

import Cardano.Ledger.Core
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API as Ledger
import qualified Cardano.Ledger.Shelley.Rules as STS
import Cardano.Protocol.Crypto (StandardCrypto)
import qualified Cardano.Protocol.TPraos.BHeader as TP
import qualified Cardano.Protocol.TPraos.Rules.Prtcl as STS (PrtclState)
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

{-------------------------------------------------------------------------------
  Serialization Properties
-------------------------------------------------------------------------------}

testCoreTypes :: TestTree
testCoreTypes =
  testGroup
    "Core Types"
    [ testProperty "Header (Annotator)" $
        roundTripAnnExpectation @(TP.BHeader StandardCrypto)
    , testProperty "Header" $
        roundTripCborRangeExpectation @(TP.BHeader StandardCrypto) minBound maxBound
    , testProperty "Block Header Hash" $
        roundTripExpectation @TP.HashHeader cborTrip
    , testProperty "Protocol State" $
        roundTripExpectation @STS.PrtclState cborTrip
    ]

tests :: TestTree
tests =
  testGroup
    "Serialisation roundtrip Property Tests"
    [ testProperty "Block (Annotator)" $
        roundTripAnnRangeExpectation @(Block (TP.BHeader StandardCrypto) ShelleyEra)
          (eraProtVerLow @ShelleyEra)
          (eraProtVerHigh @ShelleyEra)
    , testProperty "Block" $
        roundTripCborRangeExpectation @(Block (TP.BHeader StandardCrypto) ShelleyEra)
          (eraProtVerLow @ShelleyEra)
          (eraProtVerHigh @ShelleyEra)
    , testProperty "LEDGER Predicate Failures" $
        roundTripExpectation @[STS.PredicateFailure (STS.ShelleyLEDGERS ShelleyEra)] cborTrip
    , testCoreTypes
    ]
