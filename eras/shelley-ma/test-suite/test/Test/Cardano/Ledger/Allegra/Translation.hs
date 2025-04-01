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
import Cardano.Ledger.Binary
import Cardano.Ledger.Core
import Cardano.Ledger.Genesis (NoGenesis (..))
import Cardano.Ledger.Shelley (ShelleyEra)
import qualified Cardano.Ledger.Shelley.API as S
import Test.Cardano.Ledger.Mary.Binary.Annotator ()
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
        translateEraEncoding @AllegraEra @S.ShelleyTx NoGenesis toCBOR toCBOR
    , testProperty "ProposedPPUpdates compatibility" (testTranslation @S.ProposedPPUpdates)
    , testProperty "ShelleyGovState compatibility" $
        translateEraEncoding @AllegraEra @S.ShelleyGovState NoGenesis toCBOR toCBOR
    , testProperty "TxOut compatibility" (testTranslation @S.ShelleyTxOut)
    , testProperty "UTxO compatibility" $
        translateEraEncoding @AllegraEra @S.UTxO NoGenesis toCBOR toCBOR
    , testProperty "UTxOState compatibility" $
        translateEraEncoding @AllegraEra @S.UTxOState NoGenesis toCBOR toCBOR
    , testProperty "LedgerState compatibility" $
        translateEraEncoding @AllegraEra @S.LedgerState NoGenesis toCBOR toCBOR
    , testProperty "EpochState compatibility" $
        translateEraEncoding @AllegraEra @S.EpochState NoGenesis toCBOR toCBOR
    , testProperty "ShelleyTxWits compatibility" $
        translateEraEncoding @AllegraEra @S.ShelleyTxWits NoGenesis toCBOR toCBOR
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
