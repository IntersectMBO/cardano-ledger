{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Mary.Translation (
  maryTranslationTests,
  maryEncodeDecodeTests,
)
where

import Cardano.Ledger.Allegra (Allegra)
import Cardano.Ledger.Binary
import Cardano.Ledger.Core
import Cardano.Ledger.Mary (Mary)
import Cardano.Ledger.Mary.Translation ()
import Cardano.Ledger.Shelley (Shelley)
import qualified Cardano.Ledger.Shelley.API as S
import Test.Cardano.Ledger.AllegraEraGen ()
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.Cardano.Ledger.TranslationTools (translateEraEncoding, translateEraToCBOR)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion)
import Test.Tasty.QuickCheck (testProperty)

maryEncodeDecodeTests :: TestTree
maryEncodeDecodeTests =
  testGroup
    "encoded allegra types can be decoded as mary types"
    [ testProperty
        "decoding metadata"
        ( embedTripAnnExpectation @(TxAuxData Allegra) @(TxAuxData Mary)
            (eraProtVerLow @Shelley)
            (eraProtVerLow @Allegra)
            (\_ _ -> pure ())
        )
    ]

maryTranslationTests :: TestTree
maryTranslationTests =
  testGroup
    "Mary translation binary compatibiliby tests"
    [ testProperty "Tx compatibility" $
        translateEraEncoding @Mary @S.ShelleyTx () encCBOR encCBOR
    , testProperty "ProposedPPUpdates compatibility" (test @S.ProposedPPUpdates)
    , testProperty "ShelleyPPUPState compatibility" $
        translateEraEncoding @Mary @S.ShelleyPPUPState () encCBOR encCBOR
    , testProperty "TxOut compatibility" (test @S.ShelleyTxOut)
    , testProperty "UTxO compatibility" $
        translateEraEncoding @Mary @S.UTxO () encCBOR encCBOR
    , testProperty "UTxOState compatibility" $
        translateEraEncoding @Mary @S.UTxOState () encCBOR encCBOR
    , testProperty "LedgerState compatibility" $
        translateEraEncoding @Mary @S.LedgerState () encCBOR encCBOR
    , testProperty "EpochState compatibility" $
        translateEraEncoding @Mary @S.EpochState () encCBOR encCBOR
    , testProperty "ShelleyTxWits compatibility" $
        translateEraEncoding @Mary @S.ShelleyTxWits () encCBOR encCBOR
    , testProperty "Update compatibility" (test @S.Update)
    ]

test ::
  forall f.
  ( ToCBOR (f Allegra)
  , ToCBOR (f Mary)
  , TranslateEra Mary f
  , Show (TranslationError Mary f)
  ) =>
  f Allegra ->
  Assertion
test = translateEraToCBOR ([] :: [Mary]) ()
