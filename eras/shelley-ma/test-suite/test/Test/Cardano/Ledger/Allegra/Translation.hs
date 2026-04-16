{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Allegra.Translation (
  allegraTranslationTests,
  allegraEncodeDecodeTests,
) where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Binary (EncCBOR)
import Cardano.Ledger.Core
import Cardano.Ledger.Genesis (NoGenesis (..))
import Cardano.Ledger.Shelley (ShelleyEra)
import qualified Cardano.Ledger.Shelley.API as S
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Cardano.Ledger.Mary.Binary.Annotator ()
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.TranslationTools (translateEraEncCBOR)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion)
import Test.Tasty.QuickCheck (testProperty)

allegraEncodeDecodeTests :: TestTree
allegraEncodeDecodeTests =
  testGroup
    "encoded shelley types can be decoded as allegra types"
    [ testProperty
        "decoding auxiliary data (Annotator)"
        ( embedTripAnnExpectation @(TxAuxData ShelleyEra) @(TxAuxData AllegraEra)
            (eraProtVerLow @ShelleyEra)
            (eraProtVerLow @AllegraEra)
            (\_ _ -> pure ())
        )
    , testProperty
        "decoding auxiliary data"
        ( embedTripExpectation @(TxAuxData ShelleyEra) @(TxAuxData AllegraEra)
            (eraProtVerLow @ShelleyEra)
            (eraProtVerLow @AllegraEra)
            cborTrip
            (\_ _ -> pure ())
        )
    ]

allegraTranslationTests :: TestTree
allegraTranslationTests =
  testGroup
    "Allegra translation binary compatibiliby tests"
    [ testProperty "Tx compatibility" (testTranslation @(Tx TopTx))
    , testProperty "ProposedPPUpdates compatibility" (testTranslation @S.ProposedPPUpdates)
    , testProperty "ShelleyGovState compatibility" (testTranslation @S.ShelleyGovState)
    , testProperty "TxOut compatibility" (testTranslation @S.ShelleyTxOut)
    , testProperty "UTxO compatibility" (testTranslation @S.UTxO)
    , testProperty "UTxOState compatibility" (testTranslation @S.UTxOState)
    , testProperty "LedgerState compatibility" (testTranslation @S.LedgerState)
    , testProperty "EpochState compatibility" (testTranslation @S.EpochState)
    , testProperty "ShelleyTxWits compatibility" (testTranslation @S.ShelleyTxWits)
    , testProperty "Update compatibility" (testTranslation @S.Update)
    ]

testTranslation ::
  forall f.
  ( EncCBOR (f AllegraEra)
  , EncCBOR (f ShelleyEra)
  , TranslateEra AllegraEra f
  , Show (TranslationError AllegraEra f)
  ) =>
  f ShelleyEra ->
  Assertion
testTranslation = translateEraEncCBOR ([] :: [AllegraEra]) NoGenesis
