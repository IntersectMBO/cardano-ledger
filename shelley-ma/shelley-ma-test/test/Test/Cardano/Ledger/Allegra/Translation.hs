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
import Cardano.Ledger.Allegra.Translation ()
import Cardano.Ledger.Era (TranslateEra (..))
import qualified Cardano.Ledger.ShelleyMA.Metadata as MA
import qualified Shelley.Spec.Ledger.API as S
import qualified Shelley.Spec.Ledger.EpochBoundary as EB
import Test.Cardano.Ledger.EraBuffet
  ( AllegraEra,
    ShelleyEra,
    StandardCrypto,
  )
import Test.Cardano.Ledger.TranslationTools
  ( decodeTestAnn,
    translationCompatToCBOR,
  )
-- instance EraGen ShelleyEra
import Test.Shelley.Spec.Ledger.Generator.ShelleyEraGen ()
import Test.Shelley.Spec.Ledger.Serialisation.Generators ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

type Allegra = AllegraEra StandardCrypto

type Shelley = ShelleyEra StandardCrypto

allegraEncodeDecodeTests :: TestTree
allegraEncodeDecodeTests =
  testGroup
    "encoded shelley types can be decoded as allegra types"
    [ testProperty
        "decoding metadata"
        (decodeTestAnn @S.MetaData ([] :: [MA.Metadata Allegra]))
    ]

allegraTranslationTests :: TestTree
allegraTranslationTests =
  testGroup
    "Allegra translation binary compatibiliby tests"
    [ testProperty "Tx compatibility" (test @S.Tx),
      testProperty "ShelleyGenesis compatibility" (test @S.ShelleyGenesis),
      testProperty "RewardAcnt compatibility" (test @S.RewardAcnt),
      testProperty "PoolParams compatibility" (test @S.PoolParams),
      testProperty "Addr compatibility" (test @S.Addr),
      testProperty "NonMyopic compatibility" (test @S.NonMyopic),
      testProperty "ScriptHash compatibility" (test @S.ScriptHash),
      testProperty "RewardUpdate compatibility" (test @S.RewardUpdate),
      testProperty "SnapShot compatibility" (test @S.SnapShot),
      testProperty "SnapShots compatibility" (test @S.SnapShots),
      testProperty "ProposedPPUpdates compatibility" (test @S.ProposedPPUpdates),
      testProperty "PPUPState compatibility" (test @S.PPUPState),
      testProperty "TxId compatibility" (test @S.TxId),
      testProperty "TxIn compatibility" (test @S.TxIn),
      testProperty "TxOut compatibility" (test @S.TxOut),
      testProperty "UTxO compatibility" (test @S.UTxO),
      testProperty "UTxOState compatibility" (test @S.UTxOState),
      testProperty "InstantaneousRewards compatibility" (test @S.InstantaneousRewards),
      testProperty "DState compatibility" (test @S.DState),
      testProperty "PState compatibility" (test @S.PState),
      testProperty "DPState compatibility" (test @S.DPState),
      testProperty "LedgerState compatibility" (test @S.LedgerState),
      testProperty "EpochState compatibility" (test @S.EpochState),
      testProperty "WitnessSet compatibility" (test @S.WitnessSet),
      testProperty "BootstrapWitness compatibility" (test @S.BootstrapWitness),
      testProperty "Wdrl compatibility" (test @S.Wdrl),
      testProperty "DCert compatibility" (test @S.DCert),
      testProperty "MIRCert compatibility" (test @S.MIRCert),
      testProperty "Update compatibility" (test @S.Update),
      testProperty "Credential compatibility" (test @(S.Credential 'S.Witness)),
      testProperty "WitVKey compatibility" (test @(S.WitVKey 'S.Witness)),
      testProperty "BlocksMade compatibility" (test @EB.BlocksMade)
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
test x = translationCompatToCBOR ([] :: [Allegra]) () x
