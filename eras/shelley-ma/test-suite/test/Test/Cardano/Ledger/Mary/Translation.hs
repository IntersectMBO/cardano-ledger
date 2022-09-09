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
import Cardano.Ledger.Allegra (Allegra)
import Cardano.Ledger.Era (TranslateEra (..))
import Cardano.Ledger.Mary (Mary)
import Cardano.Ledger.Mary.Translation ()
import qualified Cardano.Ledger.Shelley.API as S
import Cardano.Ledger.ShelleyMA.AuxiliaryData (AllegraTxAuxData)
import Test.Cardano.Ledger.AllegraEraGen ()
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.Cardano.Ledger.TranslationTools (decodeTestAnn, translationCompatToCBOR)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

maryEncodeDecodeTests :: TestTree
maryEncodeDecodeTests =
  testGroup
    "encoded allegra types can be decoded as mary types"
    [ testProperty
        "decoding metadata"
        (decodeTestAnn @(S.ShelleyTxAuxData Allegra) ([] :: [AllegraTxAuxData Mary]))
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
      testProperty "ShelleyTxWits compatibility" (test @S.ShelleyTxWits),
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
