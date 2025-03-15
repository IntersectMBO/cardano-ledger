{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Allegra.Translation (
  allegraTranslationTests,
  allegraEncodeDecodeTests,
)
where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Allegra.State
import Cardano.Ledger.Binary
import Cardano.Ledger.Core
import Cardano.Ledger.Genesis (NoGenesis (..))
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.PParams
import Cardano.Ledger.Shelley.Tx
import Cardano.Ledger.Shelley.TxOut
import Cardano.Ledger.Shelley.TxWits
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
    [ testProperty "Tx compatibility" $
        translateEraEncoding @AllegraEra @ShelleyTx NoGenesis toCBOR toCBOR
    , testProperty "ProposedPPUpdates compatibility" (testTranslation @ProposedPPUpdates)
    , testProperty "ShelleyGovState compatibility" $
        translateEraEncoding @AllegraEra @ShelleyGovState NoGenesis toCBOR toCBOR
    , testProperty "TxOut compatibility" (testTranslation @ShelleyTxOut)
    , testProperty "UTxO compatibility" $
        translateEraEncoding @AllegraEra @UTxO NoGenesis toCBOR toCBOR
    , testProperty "UtxoState compatibility" $
        translateEraEncoding @AllegraEra @UtxoState NoGenesis toCBOR toCBOR
    , testProperty "LedgerState compatibility" $
        translateEraEncoding @AllegraEra @LedgerState NoGenesis toCBOR toCBOR
    , testProperty "EpochState compatibility" $
        translateEraEncoding @AllegraEra @EpochState NoGenesis toCBOR toCBOR
    , testProperty "ShelleyTxWits compatibility" $
        translateEraEncoding @AllegraEra @ShelleyTxWits NoGenesis toCBOR toCBOR
    , testProperty "Update compatibility" (testTranslation @Update)
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
