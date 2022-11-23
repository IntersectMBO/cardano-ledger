{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.Translation
  ( tests,
  )
where

import Cardano.Ledger.Alonzo (Alonzo)
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import qualified Cardano.Ledger.Alonzo.Translation as Translation (Tx (..))
import Cardano.Ledger.Alonzo.Tx (toCBORForSizeComputation)
import Cardano.Ledger.Binary (ToCBOR (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Mary (Mary)
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.ShelleyMA.AuxiliaryData (AllegraTxAuxData)
import Cardano.Ledger.ShelleyMA.TxBody (MATxBody)
import Data.Typeable (Typeable)
import Test.Cardano.Ledger.AllegraEraGen ()
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.Cardano.Ledger.TranslationTools
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion)
import Test.Tasty.QuickCheck (Arbitrary, testProperty)

tests :: TestTree
tests =
  testGroup
    "Translation"
    [ alonzoTranslationTests,
      alonzoEncodeDecodeTests
    ]

alonzoEncodeDecodeTests :: TestTree
alonzoEncodeDecodeTests =
  testGroup
    "encoded mary types can be decoded as alonzo types"
    [ testProperty "decoding auxilliary" $
        embedTripAnnExpectation @(AllegraTxAuxData Mary) @(TxAuxData Alonzo)
          (eraProtVerLow @Mary)
          (eraProtVerLow @Alonzo)
          (\_ _ -> pure ()),
      testProperty "decoding txbody" $
        embedTripAnnExpectation @(MATxBody Mary) @(TxBody Alonzo)
          (eraProtVerLow @Mary)
          (eraProtVerLow @Alonzo)
          (\_ _ -> pure ()),
      testProperty "decoding witnesses" $
        embedTripAnnExpectation @(TxWits Mary) @(TxWits Alonzo)
          (eraProtVerLow @Mary)
          (eraProtVerLow @Alonzo)
          (\_ _ -> pure ())
    ]

alonzoTranslationTests :: TestTree
alonzoTranslationTests =
  testGroup
    "Alonzo translation binary compatibiliby tests"
    [ testProperty "Tx compatibility" testTx,
      testProperty "ProposedPPUpdates compatibility" (test @API.ProposedPPUpdates),
      testProperty "PPUPState compatibility" (test @API.PPUPState),
      testProperty "UTxO compatibility" (test @API.UTxO),
      testProperty "UTxOState compatibility" (test @API.UTxOState),
      testProperty "LedgerState compatibility" (test @API.LedgerState)
    ]

deriving newtype instance
  (Arbitrary (Tx era)) =>
  Arbitrary (Translation.Tx era)

deriving newtype instance
  (Typeable era, ToCBOR (Tx era)) =>
  ToCBOR (Translation.Tx era)

deriving newtype instance
  (Show (Tx era)) =>
  Show (Translation.Tx era)

dummyAlonzoGenesis :: AlonzoGenesis
dummyAlonzoGenesis = error "Undefined AlonzoGenesis"

test ::
  forall f.
  ( ToCBOR (f Mary),
    ToCBOR (f Alonzo),
    TranslateEra Alonzo f,
    Show (TranslationError Alonzo f)
  ) =>
  f Mary ->
  Assertion
test = translateEraToCBOR ([] :: [Alonzo]) dummyAlonzoGenesis

testTx :: Translation.Tx Mary -> Assertion
testTx =
  translateEraEncoding @Alonzo
    dummyAlonzoGenesis
    (toCBORForSizeComputation . Translation.unTx)
    toCBOR
