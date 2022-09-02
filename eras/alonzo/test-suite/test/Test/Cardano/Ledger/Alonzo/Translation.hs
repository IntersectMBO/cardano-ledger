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

import Cardano.Binary
  ( ToCBOR (..),
  )
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import qualified Cardano.Ledger.Alonzo.Translation as Translation (Tx (..))
import Cardano.Ledger.Alonzo.Tx (toCBORForSizeComputation)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (TranslateEra (..))
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.ShelleyMA.AuxiliaryData (MAAuxiliaryData)
import Cardano.Ledger.ShelleyMA.TxBody (MATxBody)
import Data.Typeable (Typeable)
import Test.Cardano.Ledger.AllegraEraGen ()
import Test.Cardano.Ledger.EraBuffet
  ( MaryEra,
    StandardCrypto,
  )
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.Cardano.Ledger.TranslationTools
  ( decodeTestAnn,
    translationCompat,
    translationCompatToCBOR,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary, testProperty)

type Mary = MaryEra StandardCrypto

type Alonzo = AlonzoEra StandardCrypto

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
    [ testProperty
        "decoding auxilliary"
        (decodeTestAnn @(MAAuxiliaryData Mary) ([] :: [Core.AuxiliaryData Alonzo])),
      testProperty
        "decoding txbody"
        (decodeTestAnn @(MATxBody Mary) ([] :: [Core.TxBody Alonzo])),
      testProperty
        "decoding witnesses"
        (decodeTestAnn @(Core.TxWits Mary) ([] :: [Core.TxWits Alonzo]))
    ]

alonzoTranslationTests :: TestTree
alonzoTranslationTests =
  testGroup
    "Alonzo translation binary compatibiliby tests"
    [ testProperty "Core.Tx compatibility" testTx,
      testProperty "ProposedPPUpdates compatibility" (test @API.ProposedPPUpdates),
      testProperty "PPUPState compatibility" (test @API.PPUPState),
      testProperty "UTxO compatibility" (test @API.UTxO),
      testProperty "UTxOState compatibility" (test @API.UTxOState),
      testProperty "LedgerState compatibility" (test @API.LedgerState)
    ]

deriving newtype instance
  (Arbitrary (Core.Tx era)) =>
  Arbitrary (Translation.Tx era)

deriving newtype instance
  (Typeable era, ToCBOR (Core.Tx era)) =>
  ToCBOR (Translation.Tx era)

deriving newtype instance
  (Show (Core.Tx era)) =>
  Show (Translation.Tx era)

dummyAlonzoGenesis :: AlonzoGenesis
dummyAlonzoGenesis = undefined

test ::
  forall f.
  ( ToCBOR (f Mary),
    ToCBOR (f Alonzo),
    TranslateEra Alonzo f,
    Show (TranslationError Alonzo f)
  ) =>
  f Mary ->
  Bool
test = translationCompatToCBOR ([] :: [Alonzo]) dummyAlonzoGenesis

testTx :: Translation.Tx Mary -> Bool
testTx =
  translationCompat @Alonzo
    dummyAlonzoGenesis
    (toCBORForSizeComputation . Translation.unTx)
    toCBOR
