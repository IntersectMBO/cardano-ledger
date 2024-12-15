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
import Cardano.Ledger.Mary.Translation ()
import qualified Cardano.Ledger.Shelley.API as S
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
        "decoding metadata"
        ( embedTripAnnExpectation @(TxAuxData AllegraEra) @(TxAuxData MaryEra)
            (eraProtVerLow @AllegraEra)
            (eraProtVerLow @MaryEra)
            (\_ _ -> pure ())
        )
    ]

maryTranslationTests :: TestTree
maryTranslationTests =
  testGroup
    "Mary translation binary compatibiliby tests"
    [ testProperty "Tx compatibility" $
        translateEraEncoding @MaryEra @S.ShelleyTx NoGenesis toCBOR toCBOR
    , testProperty "ProposedPPUpdates compatibility" (test @S.ProposedPPUpdates)
    , testProperty "ShelleyGovState compatibility" $
        translateEraEncoding @MaryEra @S.ShelleyGovState NoGenesis toCBOR toCBOR
    , testProperty "TxOut compatibility" (test @S.ShelleyTxOut)
    , testProperty "UTxO compatibility" $
        translateEraEncoding @MaryEra @S.UTxO NoGenesis toCBOR toCBOR
    , testProperty "UTxOState compatibility" $
        translateEraEncoding @MaryEra @S.UTxOState NoGenesis toCBOR toCBOR
    , testProperty "LedgerState compatibility" $
        translateEraEncoding @MaryEra @S.LedgerState NoGenesis toCBOR toCBOR
    , testProperty "EpochState compatibility" $
        translateEraEncoding @MaryEra @S.EpochState NoGenesis toCBOR toCBOR
    , testProperty "ShelleyTxWits compatibility" $
        translateEraEncoding @MaryEra @S.ShelleyTxWits NoGenesis toCBOR toCBOR
    , testProperty "Update compatibility" (test @S.Update)
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
