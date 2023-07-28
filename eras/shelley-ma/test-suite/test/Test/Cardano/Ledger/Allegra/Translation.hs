{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Allegra.Translation (
  allegraTranslationTests,
  allegraEncodeDecodeTests,
)
where

import Cardano.Ledger.Allegra (Allegra)
import Cardano.Ledger.Binary
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley (Shelley)
import qualified Cardano.Ledger.Shelley.API as S
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.TranslationTools (translateEraEncCBOR, translateEraEncoding)
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
    [ testProperty "Tx compatibility" $
        translateEraEncoding @Allegra @S.ShelleyTx () toCBOR toCBOR
    , testProperty "ProposedPPUpdates compatibility" (testTranslation @S.ProposedPPUpdates)
    , testProperty "ShelleyPPUPState compatibility" $
        translateEraEncoding @Allegra @S.ShelleyPPUPState () toCBOR toCBOR
    , testProperty "TxOut compatibility" (testTranslation @S.ShelleyTxOut)
    , testProperty "UTxO compatibility" $
        translateEraEncoding @Allegra @S.UTxO () toCBOR toCBOR
    , testProperty "UTxOState compatibility" $
        translateEraEncoding @Allegra @S.UTxOState () toCBOR toCBOR
    , testProperty "LedgerState compatibility" $
        translateEraEncoding @Allegra @S.LedgerState () toCBOR toCBOR
    , testProperty "EpochState compatibility" $
        translateEraEncoding @Allegra @S.EpochState () toCBOR toCBOR
    , testProperty "ShelleyTxWits compatibility" $
        translateEraEncoding @Allegra @S.ShelleyTxWits () toCBOR toCBOR
    , testProperty "Update compatibility" (testTranslation @S.Update)
    ]

testTranslation ::
  forall f.
  ( EncCBOR (f Allegra)
  , EncCBOR (f Shelley)
  , TranslateEra Allegra f
  , Show (TranslationError Allegra f)
  , TranslationContextF Allegra f ~ ()
  ) =>
  f Shelley ->
  Assertion
testTranslation = translateEraEncCBOR ([] :: [Allegra]) ()
