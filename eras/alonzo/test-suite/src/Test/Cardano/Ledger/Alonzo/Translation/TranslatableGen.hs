{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Test.Cardano.Ledger.Alonzo.Translation.TranslatableGen (
  TranslatableGen (..),
  translationInstances,
  epochInfo,
  systemStart,
) where

import Cardano.Ledger.Alonzo (Alonzo)
import Cardano.Ledger.Alonzo.TxInfo
import Cardano.Ledger.Core as Core
import Cardano.Ledger.Language (Language (..))
import Cardano.Ledger.UTxO (UTxO (..))
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..))
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Alonzo.Translation.TranslationInstance (TranslationInstance (..))
import Test.QuickCheck (
  Arbitrary,
  Gen,
  arbitrary,
  elements,
  vectorOf,
 )
import Test.QuickCheck.Gen (Gen (MkGen))
import Test.QuickCheck.Random (mkQCGen)

class EraTx era => TranslatableGen era where
  tgTx :: Language -> Gen (Core.Tx era)
  tgUtxo :: Language -> Core.Tx era -> Gen (UTxO era)

instance TranslatableGen Alonzo where
  tgTx _ = arbitrary :: Gen (Tx Alonzo)
  tgUtxo _ tx = do
    let ins = tx ^. bodyTxL ^. inputsTxBodyL
    outs <- vectorOf (length ins) (arbitrary :: Gen (TxOut Alonzo))
    pure $ UTxO (Map.fromList $ Set.toList ins `zip` outs)

translationInstances ::
  forall era.
  ( ExtendedUTxO era
  , TranslatableGen era
  , Arbitrary (PParams era)
  ) =>
  [Language] ->
  Int ->
  Int ->
  [TranslationInstance era]
translationInstances ls size seed =
  generateWithSeed seed $ vectorOf size (genTranslationInstance ls)

generateWithSeed :: Int -> Gen a -> a
generateWithSeed seed (MkGen g) = g (mkQCGen seed) 30

genTranslationInstance ::
  forall era.
  ( ExtendedUTxO era
  , TranslatableGen era
  , Arbitrary (PParams era)
  ) =>
  [Language] ->
  Gen (TranslationInstance era)
genTranslationInstance ls = do
  pp <- arbitrary :: Gen (PParams era)
  language <- elements ls
  tx <- tgTx @era language
  fullUtxo <- tgUtxo language tx
  let vtxInfoE = txInfo pp language epochInfo systemStart fullUtxo tx
  let vtxInfo = either (error . show) id vtxInfoE
  pure $ TranslationInstance pp language fullUtxo tx vtxInfo

epochInfo :: EpochInfo (Either a)
epochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)

systemStart :: SystemStart
systemStart = SystemStart $ posixSecondsToUTCTime 1684445839000 -- 18/05/2023
