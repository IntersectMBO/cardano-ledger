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
import Test.Cardano.Ledger.Mary.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.Cardano.Ledger.TranslationTools (translateEraEncCBOR, translateEraEncoding)
import Test.Cardano.Protocol.TPraos.ShelleyEraGen ()
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
        translateEraEncoding @Mary @S.ShelleyTx () toCBOR toCBOR
    , testProperty "ProposedPPUpdates compatibility" (test @S.ProposedPPUpdates)
    , testProperty "ShelleyPPUPState compatibility" $
        translateEraEncoding @Mary @S.ShelleyPPUPState () toCBOR toCBOR
    , testProperty "TxOut compatibility" (test @S.ShelleyTxOut)
    , testProperty "UTxO compatibility" $
        translateEraEncoding @Mary @S.UTxO () toCBOR toCBOR
    , testProperty "UTxOState compatibility" $
        translateEraEncoding @Mary @S.UTxOState () toCBOR toCBOR
    , testProperty "LedgerState compatibility" $
        translateEraEncoding @Mary @S.LedgerState () toCBOR toCBOR
    , testProperty "EpochState compatibility" $
        translateEraEncoding @Mary @S.EpochState () toCBOR toCBOR
    , testProperty "ShelleyTxWits compatibility" $
        translateEraEncoding @Mary @S.ShelleyTxWits () toCBOR toCBOR
    , testProperty "Update compatibility" (test @S.Update)
    ]

test ::
  forall f.
  ( EncCBOR (f Allegra)
  , EncCBOR (f Mary)
  , TranslateEra Mary f
  , Show (TranslationError Mary f)
  ) =>
  f Allegra ->
  Assertion
test = translateEraEncCBOR ([] :: [Mary]) ()
