{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Mary.Translation
  ( maryTranslationTests,
    maryEncodeDecodeTests,
  )
where

import Cardano.Binary
  ( ToCBOR (..),
  )
import Cardano.Ledger.Era (TranslateEra (..))
import Cardano.Ledger.Mary.Translation ()
import qualified Cardano.Ledger.Shelley.API as S
import Cardano.Ledger.ShelleyMA.AuxiliaryData (MAAuxiliaryData)
import Test.Cardano.Ledger.AllegraEraGen ()
-- import Allegra EraGen instance
import Test.Cardano.Ledger.EraBuffet
  ( AllegraEra,
    MaryEra,
    StandardCrypto,
  )
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.Cardano.Ledger.TranslationTools
  ( decodeTestAnn,
    translationCompatToCBOR,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

type Allegra = AllegraEra StandardCrypto

type Mary = MaryEra StandardCrypto

maryEncodeDecodeTests :: TestTree
maryEncodeDecodeTests =
  testGroup
    "encoded allegra types can be decoded as mary types"
    [ testProperty
        "decoding metadata"
        (decodeTestAnn @(S.Metadata Allegra) ([] :: [MAAuxiliaryData Mary]))
    ]

maryTranslationTests :: TestTree
maryTranslationTests =
  testGroup
    "Mary translation binary compatibiliby tests"
    [ testProperty "Tx compatibility" (test @S.ShelleyTx),
      testProperty "ProposedPPUpdates compatibility" (test @S.ProposedPPUpdates),
      testProperty "PPUPState compatibility" (test @S.PPUPState),
      testProperty "TxOut compatibility" (test @S.ShelleyTxOut),
      testProperty "UTxO compatibility" (test @S.UTxO),
      testProperty "UTxOState compatibility" (test @S.UTxOState),
      testProperty "LedgerState compatibility" (test @S.LedgerState),
      testProperty "EpochState compatibility" (test @S.EpochState),
      testProperty "WitnessSet compatibility" (test @S.ShelleyWitnesses),
      testProperty "Update compatibility" (test @S.Update)
    ]

test ::
  forall f.
  ( ToCBOR (f Allegra),
    ToCBOR (f Mary),
    TranslateEra Mary f,
    Show (TranslationError Mary f)
  ) =>
  f Allegra ->
  Bool
test = translationCompatToCBOR ([] :: [Mary]) ()
