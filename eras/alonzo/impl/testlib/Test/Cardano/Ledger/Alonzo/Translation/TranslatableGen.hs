{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Test.Cardano.Ledger.Alonzo.Translation.TranslatableGen (
  TranslatableGen (..),
  translationInstances,
  epochInfo,
  toVersionedTxInfo,
  systemStart,
) where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Plutus.Context (
  EraPlutusContext,
  LedgerTxInfo (..),
  PlutusTxInfo,
  SupportedLanguage (..),
  toPlutusTxInfo,
 )
import Cardano.Ledger.Alonzo.TxWits (Redeemers)
import Cardano.Ledger.Core as Core
import Cardano.Ledger.Plutus.Language (SLanguage (..))
import Cardano.Ledger.State (UTxO (..))
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..))
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Alonzo.Translation.TranslationInstance (
  TranslationInstance (..),
  VersionedTxInfo (..),
 )
import Test.Cardano.Ledger.Common

class (EraTx era, EraPlutusContext era, Arbitrary (Script era)) => TranslatableGen era where
  tgRedeemers :: Gen (Redeemers era)
  tgTx :: SupportedLanguage era -> Gen (Core.Tx era)
  tgUtxo :: SupportedLanguage era -> Core.Tx era -> Gen (UTxO era)

instance TranslatableGen AlonzoEra where
  tgRedeemers = arbitrary
  tgTx _ = arbitrary :: Gen (Tx AlonzoEra)
  tgUtxo _ tx = do
    let ins = tx ^. bodyTxL ^. inputsTxBodyL
    outs <- vectorOf (length ins) (arbitrary :: Gen (TxOut AlonzoEra))
    pure $ UTxO (Map.fromList $ Set.toList ins `zip` outs)

translationInstances ::
  TranslatableGen era =>
  Int ->
  Int ->
  [TranslationInstance era]
translationInstances size seed =
  runGen seed 30 $ vectorOf size genTranslationInstance

toVersionedTxInfo :: SLanguage l -> PlutusTxInfo l -> VersionedTxInfo
toVersionedTxInfo slang txInfo =
  case slang of
    SPlutusV1 -> TxInfoPV1 txInfo
    SPlutusV2 -> TxInfoPV2 txInfo
    SPlutusV3 -> TxInfoPV3 txInfo
    SPlutusV4 -> TxInfoPV4 txInfo

genTranslationInstance ::
  forall era.
  TranslatableGen era =>
  Gen (TranslationInstance era)
genTranslationInstance = do
  protVer <- arbitrary
  supportedLanguage :: SupportedLanguage era <- arbitrary
  tx <- tgTx supportedLanguage
  utxo <- tgUtxo supportedLanguage tx
  let lti =
        LedgerTxInfo
          { ltiProtVer = protVer
          , ltiEpochInfo = epochInfo
          , ltiSystemStart = systemStart
          , ltiUTxO = utxo
          , ltiTx = tx
          }
  case supportedLanguage of
    SupportedLanguage slang -> do
      case toPlutusTxInfo slang lti of
        Left err -> error $ show err
        Right txInfo ->
          pure $ TranslationInstance protVer supportedLanguage utxo tx $ toVersionedTxInfo slang txInfo

epochInfo :: EpochInfo (Either a)
epochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)

systemStart :: SystemStart
systemStart = SystemStart $ posixSecondsToUTCTime 1684445839000 -- 18/05/2023
