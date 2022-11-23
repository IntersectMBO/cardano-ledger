{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Allegra.Translation
  ( allegraTranslationTests,
    allegraEncodeDecodeTests,
  )
where

import Cardano.Ledger.Allegra (Allegra)
import Cardano.Ledger.Allegra.Translation ()
import Cardano.Ledger.Binary
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley (Shelley)
import qualified Cardano.Ledger.Shelley.API as S
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.TranslationTools (translateEraToCBOR)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion)
import Test.Tasty.QuickCheck (testProperty)

allegraEncodeDecodeTests :: TestTree
allegraEncodeDecodeTests =
  testGroup
    "encoded shelley types can be decoded as allegra types"
    [ testProperty
        "decoding auxiliary data"
        ( embedTripAnnExpectation @(TxAuxData Shelley) @(TxAuxData Allegra)
            (eraProtVerLow @Shelley)
            (eraProtVerLow @Allegra)
            (\_ _ -> pure ())
        )
    ]

allegraTranslationTests :: TestTree
allegraTranslationTests =
  testGroup
    "Allegra translation binary compatibiliby tests"
    [ testProperty "Tx compatibility" (testTranslation @S.ShelleyTx),
      testProperty "ShelleyGenesis compatibility" (testTranslation @S.ShelleyGenesis),
      testProperty "ProposedPPUpdates compatibility" (testTranslation @S.ProposedPPUpdates),
      testProperty "PPUPState compatibility" (testTranslation @S.PPUPState),
      testProperty "TxOut compatibility" (testTranslation @S.ShelleyTxOut),
      testProperty "UTxO compatibility" (testTranslation @S.UTxO),
      testProperty "UTxOState compatibility" (testTranslation @S.UTxOState),
      testProperty "LedgerState compatibility" (testTranslation @S.LedgerState),
      testProperty "EpochState compatibility" (testTranslation @S.EpochState),
      testProperty "ShelleyTxWits compatibility" (testTranslation @S.ShelleyTxWits),
      testProperty "Update compatibility" (testTranslation @S.Update)
    ]

testTranslation ::
  forall f.
  ( ToCBOR (f Allegra),
    ToCBOR (f Shelley),
    TranslateEra Allegra f,
    Show (TranslationError Allegra f)
  ) =>
  f Shelley ->
  Assertion
testTranslation = translateEraToCBOR ([] :: [Allegra]) ()
