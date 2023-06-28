{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.Serialisation.Roundtrip (allprops) where

import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits)
import Cardano.Ledger.Binary.Version (natVersion)
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Conway.PParams (ConwayPParams)
import Cardano.Ledger.Core
import Data.Data (Proxy (..), typeRep)
import Data.Functor.Identity (Identity)
import Data.Maybe.Strict (StrictMaybe)
import Test.Cardano.Ledger.Alonzo.Arbitrary (FlexibleCostModels)
import Test.Cardano.Ledger.Binary.Plain.RoundTrip as Plain
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

allprops :: forall e. Era e => TestTree
allprops =
  testGroup
    (show (typeRep (Proxy @e)) <> " CBOR")
    [ testProperty "ConwayGenesis" $
        roundTripCborRangeExpectation @(ConwayGenesis (EraCrypto e))
          (natVersion @2)
          maxBound
    , testProperty "ConwayGenesis (Plain)" $
        Plain.roundTripCborExpectation @(ConwayGenesis (EraCrypto e))
    , testProperty "v9 CostModels" $
        roundTripCborRangeExpectation @FlexibleCostModels
          (eraProtVerLow @Conway)
          (eraProtVerHigh @Conway)
    , testProperty "Conway Script" $
        roundTripAnnRangeExpectation @(Script Conway)
          (eraProtVerLow @Conway)
          (eraProtVerHigh @Conway)
    , testProperty "Conway TxWits" $
        roundTripAnnRangeExpectation @(AlonzoTxWits Conway)
          (eraProtVerLow @Conway)
          (eraProtVerHigh @Conway)
    , testProperty "Conway AuxiliaryData" $
        roundTripAnnRangeExpectation @(TxAuxData Conway)
          (eraProtVerLow @Conway)
          (eraProtVerHigh @Conway)
    , testProperty "Conway Certificate" $
        roundTripCborRangeExpectation @(TxCert Conway)
          (eraProtVerLow @Conway)
          (eraProtVerHigh @Conway)
    , testProperty "Conway PParams" $
        roundTripCborRangeExpectation @(ConwayPParams Identity Conway)
          (eraProtVerLow @Conway)
          (eraProtVerHigh @Conway)
    , testProperty "Conway PParamsUpdate" $
        roundTripCborRangeExpectation @(ConwayPParams StrictMaybe Conway)
          (eraProtVerLow @Conway)
          (eraProtVerHigh @Conway)
    ]
