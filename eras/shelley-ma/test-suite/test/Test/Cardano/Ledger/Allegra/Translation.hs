{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Allegra.Translation
  ( allegraTranslationTests,
    allegraEncodeDecodeTests,
  )
where

import Cardano.Binary
  ( ToCBOR (..),
  )
import Cardano.Ledger.Allegra (Allegra)
import Cardano.Ledger.Allegra.Translation ()
import Cardano.Ledger.Era (TranslateEra (..))
import Cardano.Ledger.Shelley (Shelley)
import qualified Cardano.Ledger.Shelley.API as S
import Cardano.Ledger.ShelleyMA.AuxiliaryData
-- instance EraGen ShelleyEra
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.TranslationTools
  ( decodeTestAnn,
    translationCompatToCBOR,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

allegraEncodeDecodeTests :: TestTree
allegraEncodeDecodeTests =
  testGroup
    "encoded shelley types can be decoded as allegra types"
    [ testProperty
        "decoding auxiliary data"
        (decodeTestAnn @(S.Metadata Allegra) ([] :: [MAAuxiliaryData Allegra]))
    ]

allegraTranslationTests :: TestTree
allegraTranslationTests =
  testGroup
    "Allegra translation binary compatibiliby tests"
    [ testProperty "Tx compatibility" (test @S.ShelleyTx),
      testProperty "ShelleyGenesis compatibility" (test @S.ShelleyGenesis),
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
    ToCBOR (f Shelley),
    TranslateEra Allegra f,
    Show (TranslationError Allegra f)
  ) =>
  f Shelley ->
  Bool
test = translationCompatToCBOR ([] :: [Allegra]) ()
