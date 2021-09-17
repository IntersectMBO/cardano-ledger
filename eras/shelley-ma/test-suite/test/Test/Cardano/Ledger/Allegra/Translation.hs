{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Allegra.Translation
  ( allegraTranslationTests,
    allegraEncodeDecodeTests,
  )
where

import Cardano.Binary
  ( ToCBOR (..),
  )
import Cardano.Ledger.Allegra.Translation ()
import Cardano.Ledger.Era (TranslateEra (..))
import qualified Cardano.Ledger.Shelley.API as S
import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as MA
import Test.Cardano.Ledger.EraBuffet
  ( AllegraEra,
    ShelleyEra,
    StandardCrypto,
  )
-- instance EraGen ShelleyEra
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.TranslationTools
  ( decodeTestAnn,
    translationCompatToCBOR,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

type Allegra = AllegraEra StandardCrypto

type Shelley = ShelleyEra StandardCrypto

allegraEncodeDecodeTests :: TestTree
allegraEncodeDecodeTests =
  testGroup
    "encoded shelley types can be decoded as allegra types"
    [ testProperty
        "decoding auxiliary data"
        (decodeTestAnn @(S.Metadata Allegra) ([] :: [MA.AuxiliaryData Allegra]))
    ]

allegraTranslationTests :: TestTree
allegraTranslationTests =
  testGroup
    "Allegra translation binary compatibiliby tests"
    [ testProperty "Tx compatibility" (test @S.Tx),
      testProperty "ShelleyGenesis compatibility" (test @S.ShelleyGenesis),
      testProperty "ProposedPPUpdates compatibility" (test @S.ProposedPPUpdates),
      testProperty "PPUPState compatibility" (test @S.PPUPState),
      testProperty "TxOut compatibility" (test @S.TxOut),
      testProperty "UTxO compatibility" (test @S.UTxO),
      testProperty "UTxOState compatibility" (test @S.UTxOState),
      testProperty "LedgerState compatibility" (test @S.LedgerState),
      testProperty "EpochState compatibility" (test @S.EpochState),
      testProperty "WitnessSet compatibility" (test @S.WitnessSet),
      testProperty "Update compatibility" (test @S.Update)
    ]

test ::
  forall f.
  ( ToCBOR (f Allegra),
    ToCBOR (f Shelley),
    TranslateEra Allegra f,
    Show (TranslationError Allegra f)
  ) =>
  f Shelley ->
  Bool
test = translationCompatToCBOR ([] :: [Allegra]) ()
