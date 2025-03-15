{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Mary.Translation (
  maryTranslationTests,
  maryEncodeDecodeTests,
)
where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Binary
import Cardano.Ledger.Core
import Cardano.Ledger.Genesis (NoGenesis (..))
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.State as S
import Cardano.Ledger.Mary.Translation ()
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.PParams
import Cardano.Ledger.Shelley.Tx
import Cardano.Ledger.Shelley.TxOut
import Cardano.Ledger.Shelley.TxWits
import Test.Cardano.Ledger.AllegraEraGen ()
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Cardano.Ledger.Mary.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.Cardano.Ledger.TranslationTools (translateEraEncCBOR, translateEraEncoding)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion)
import Test.Tasty.QuickCheck (testProperty)

maryEncodeDecodeTests :: TestTree
maryEncodeDecodeTests =
  testGroup
    "encoded allegra types can be decoded as mary types"
    [ testProperty
        "decoding metadata (Annotator)"
        ( embedTripAnnExpectation @(TxAuxData AllegraEra) @(TxAuxData MaryEra)
            (eraProtVerLow @AllegraEra)
            (eraProtVerLow @MaryEra)
            (\_ _ -> pure ())
        )
    , testProperty
        "decoding metadata"
        ( embedTripExpectation @(TxAuxData AllegraEra) @(TxAuxData MaryEra)
            (eraProtVerLow @AllegraEra)
            (eraProtVerLow @MaryEra)
            cborTrip
            (\_ _ -> pure ())
        )
    ]

maryTranslationTests :: TestTree
maryTranslationTests =
  testGroup
    "Mary translation binary compatibiliby tests"
    [ testProperty "Tx compatibility" $
        translateEraEncoding @MaryEra @ShelleyTx NoGenesis toCBOR toCBOR
    , testProperty "ProposedPPUpdates compatibility" (test @ProposedPPUpdates)
    , testProperty "ShelleyGovState compatibility" $
        translateEraEncoding @MaryEra @ShelleyGovState NoGenesis toCBOR toCBOR
    , testProperty "TxOut compatibility" (test @ShelleyTxOut)
    , testProperty "UTxO compatibility" $
        translateEraEncoding @MaryEra @UTxO NoGenesis toCBOR toCBOR
    , testProperty "UtxoState compatibility" $
        translateEraEncoding @MaryEra @UtxoState NoGenesis toCBOR toCBOR
    , testProperty "LedgerState compatibility" $
        translateEraEncoding @MaryEra @LedgerState NoGenesis toCBOR toCBOR
    , testProperty "EpochState compatibility" $
        translateEraEncoding @MaryEra @EpochState NoGenesis toCBOR toCBOR
    , testProperty "ShelleyTxWits compatibility" $
        translateEraEncoding @MaryEra @ShelleyTxWits NoGenesis toCBOR toCBOR
    , testProperty "Update compatibility" (test @Update)
    ]

test ::
  forall f.
  ( EncCBOR (f AllegraEra)
  , EncCBOR (f MaryEra)
  , TranslateEra MaryEra f
  , Show (TranslationError MaryEra f)
  ) =>
  f AllegraEra ->
  Assertion
test = translateEraEncCBOR ([] :: [MaryEra]) NoGenesis
