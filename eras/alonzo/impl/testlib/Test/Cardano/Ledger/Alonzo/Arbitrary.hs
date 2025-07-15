{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.Arbitrary (
  mkPlutusScript',
  alwaysSucceeds,
  alwaysSucceedsLang,
  alwaysFails,
  alwaysFailsLang,
  genEraLanguage,
  genAlonzoScript,
  genNativeScript,
  genNonEmptyRedeemers,
  genNonEmptyTxDats,
  genPlutusScript,
  genScripts,
  genValidCostModel,
  genValidAndUnknownCostModels,
  genAlonzoPlutusPurposePointer,
) where

import Cardano.Ledger.Allegra.Scripts (Timelock)
import Cardano.Ledger.Alonzo (AlonzoEra, Tx (..))
import Cardano.Ledger.Alonzo.BlockBody (AlonzoBlockBody (AlonzoBlockBody))
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import Cardano.Ledger.Alonzo.PParams (AlonzoPParams (AlonzoPParams), OrdExUnits (OrdExUnits))
import Cardano.Ledger.Alonzo.Plutus.Context (
  EraPlutusContext (ContextError),
  EraPlutusTxInfo,
  SupportedLanguage (..),
  mkSupportedPlutusScript,
  supportedLanguages,
 )
import Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError)
import Cardano.Ledger.Alonzo.Plutus.TxInfo (AlonzoContextError)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoPredFailure (..),
  AlonzoUtxosPredFailure (..),
  AlonzoUtxowPredFailure (..),
  FailureDescription (..),
  TagMismatchDescription (..),
 )
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoPlutusPurpose (..),
  AlonzoScript (..),
 )
import Cardano.Ledger.Alonzo.Transition (TransitionConfig (..))
import Cardano.Ledger.Alonzo.Tx (
  AlonzoTx (AlonzoTx),
  IsValid (IsValid),
  ScriptIntegrity (ScriptIntegrity),
  getLanguageView,
 )
import Cardano.Ledger.Alonzo.TxAuxData (
  AlonzoTxAuxData (..),
  mkAlonzoTxAuxData,
 )
import Cardano.Ledger.Alonzo.TxBody (TxBody (AlonzoTxBody))
import Cardano.Ledger.Alonzo.TxOut (AlonzoTxOut (AlonzoTxOut))
import Cardano.Ledger.Alonzo.TxWits (
  AlonzoTxWits (AlonzoTxWits),
  Redeemers (Redeemers),
  TxDats (TxDats),
 )
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Plutus.Data (hashData)
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import Cardano.Ledger.Plutus.Language (
  Language (..),
  Plutus (..),
  PlutusLanguage,
  asSLanguage,
  plutusLanguage,
 )
import Cardano.Ledger.Shelley.Rules (PredicateFailure, ShelleyUtxowPredFailure)
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE (toList)
import qualified Data.Map.Strict as Map
import qualified Data.MapExtras as Map (fromElems)
import qualified Data.Set as Set
import Data.Text (pack)
import Data.Word
import Generic.Random (genericArbitraryU)
import Numeric.Natural (Natural)
import Test.Cardano.Data (genNonEmptyMap)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary (
  genValidAndUnknownCostModels,
  genValidCostModel,
  genValidCostModels,
 )
import Test.Cardano.Ledger.Mary.Arbitrary ()
import Test.Cardano.Ledger.Plutus (alwaysFailsPlutus, alwaysSucceedsPlutus)

instance
  ( Arbitrary (AlonzoScript era)
  , AlonzoEraScript era
  ) =>
  Arbitrary (AlonzoTxAuxData era)
  where
  arbitrary = mkAlonzoTxAuxData @[] <$> arbitrary <*> arbitrary

instance
  (AlonzoEraScript era, Arbitrary (PlutusPurpose AsIx era)) =>
  Arbitrary (Redeemers era)
  where
  arbitrary = Redeemers <$> arbitrary

genNonEmptyRedeemers ::
  (AlonzoEraScript era, Arbitrary (PlutusPurpose AsIx era)) => Gen (Redeemers era)
genNonEmptyRedeemers = Redeemers <$> genNonEmptyMap arbitrary arbitrary

instance
  ( Era era
  , Arbitrary (Script era)
  , AlonzoEraScript era
  , Arbitrary (PlutusPurpose AsIx era)
  ) =>
  Arbitrary (AlonzoTxWits era)
  where
  arbitrary =
    AlonzoTxWits
      <$> arbitrary
      <*> arbitrary
      <*> genScripts
      <*> arbitrary
      <*> arbitrary

genScripts ::
  forall era.
  ( EraScript era
  , Arbitrary (Script era)
  ) =>
  Gen (Map.Map ScriptHash (Script era))
genScripts = Map.fromElems (hashScript @era) <$> (arbitrary :: Gen [Script era])

instance Era era => Arbitrary (TxDats era) where
  arbitrary = TxDats . Map.fromElems @[] hashData <$> arbitrary

genNonEmptyTxDats :: Era era => Gen (TxDats era)
genNonEmptyTxDats = TxDats . Map.fromElems @[] hashData <$> listOf1 arbitrary

instance
  ( EraTxOut era
  , Arbitrary (Value era)
  ) =>
  Arbitrary (AlonzoTxOut era)
  where
  arbitrary =
    AlonzoTxOut
      <$> arbitrary
      <*> scale (`div` 15) arbitrary
      <*> arbitrary

instance Arbitrary (TxBody AlonzoEra) where
  arbitrary =
    AlonzoTxBody
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> scale (`div` 15) arbitrary
      <*> arbitrary
      <*> scale (`div` 15) arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

deriving newtype instance Arbitrary IsValid

instance
  ( Arbitrary (TxBody era)
  , Arbitrary (TxWits era)
  , Arbitrary (TxAuxData era)
  ) =>
  Arbitrary (AlonzoTx era)
  where
  arbitrary =
    AlonzoTx
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

genEraLanguage :: forall era. AlonzoEraScript era => Gen Language
genEraLanguage = choose (minBound, eraMaxLanguage @era)

instance EraPlutusContext era => Arbitrary (SupportedLanguage era) where
  arbitrary = elements $ NE.toList (supportedLanguages @era)

instance
  ( EraPlutusContext era
  , Script era ~ AlonzoScript era
  , NativeScript era ~ Timelock era
  ) =>
  Arbitrary (AlonzoScript era)
  where
  arbitrary = arbitrary >>= genAlonzoScript

genAlonzoScript ::
  ( EraPlutusContext era
  , Script era ~ AlonzoScript era
  , NativeScript era ~ Timelock era
  ) =>
  SupportedLanguage era ->
  Gen (AlonzoScript era)
genAlonzoScript lang =
  frequency
    [ (2, fromPlutusScript <$> genPlutusScript lang)
    , (8, fromNativeScript <$> genNativeScript)
    ]

genNativeScript ::
  Arbitrary (NativeScript era) =>
  Gen (NativeScript era)
genNativeScript = arbitrary

genPlutusScript ::
  SupportedLanguage era ->
  Gen (PlutusScript era)
genPlutusScript lang =
  frequency
    [ (5, alwaysSucceedsLang lang <$> elements [1, 2, 3])
    , (5, alwaysFailsLang lang <$> elements [1, 2, 3])
    ]

instance Arbitrary (AlonzoPParams Identity era) where
  arbitrary =
    AlonzoPParams
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> genValidCostModels [PlutusV1, PlutusV2]
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

deriving instance Arbitrary OrdExUnits

instance Arbitrary (AlonzoPParams StrictMaybe era) where
  arbitrary =
    AlonzoPParams
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> oneof [pure SNothing, SJust <$> genValidCostModels [PlutusV1, PlutusV2]]
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary FailureDescription where
  arbitrary = PlutusFailure <$> (pack <$> arbitrary) <*> arbitrary

instance Arbitrary TagMismatchDescription where
  arbitrary =
    oneof [pure PassedUnexpectedly, FailedUnexpectedly <$> ((:|) <$> arbitrary <*> arbitrary)]

instance
  ( Era era
  , Arbitrary (EraRuleFailure "PPUP" era)
  , Arbitrary (PlutusPurpose AsItem era)
  , Arbitrary (ContextError era)
  ) =>
  Arbitrary (AlonzoUtxosPredFailure era)
  where
  arbitrary = genericArbitraryU

instance
  ( Era era
  , Arbitrary (PlutusPurpose AsItem era)
  , Arbitrary (ContextError era)
  ) =>
  Arbitrary (CollectError era)
  where
  arbitrary = genericArbitraryU

instance Era era => Arbitrary (AlonzoContextError era) where
  arbitrary = genericArbitraryU

instance
  ( EraTxOut era
  , Arbitrary (Value era)
  , Arbitrary (TxOut era)
  , Arbitrary (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  Arbitrary (AlonzoUtxoPredFailure era)
  where
  arbitrary = genericArbitraryU

instance
  ( Era era
  , Arbitrary (PredicateFailure (EraRule "UTXO" era))
  , Arbitrary (ShelleyUtxowPredFailure era)
  , Arbitrary (TxCert era)
  , Arbitrary (PlutusPurpose AsItem era)
  , Arbitrary (PlutusPurpose AsIx era)
  ) =>
  Arbitrary (AlonzoUtxowPredFailure era)
  where
  arbitrary = genericArbitraryU

deriving instance Arbitrary ix => Arbitrary (AsIx ix it)

deriving instance Arbitrary it => Arbitrary (AsItem ix it)

instance (Arbitrary ix, Arbitrary it) => Arbitrary (AsIxItem ix it) where
  arbitrary = AsIxItem <$> arbitrary <*> arbitrary

genAlonzoPlutusPurposePointer :: Word32 -> Gen (AlonzoPlutusPurpose AsIx era)
genAlonzoPlutusPurposePointer i =
  elements
    [ AlonzoSpending (AsIx i)
    , AlonzoMinting (AsIx i)
    , AlonzoCertifying (AsIx i)
    , AlonzoRewarding (AsIx i)
    ]

instance
  ( Era era
  , Arbitrary (TxCert era)
  ) =>
  Arbitrary (AlonzoPlutusPurpose AsItem era)
  where
  arbitrary =
    oneof
      [ AlonzoSpending <$> arbitrary
      , AlonzoMinting <$> arbitrary
      , AlonzoCertifying <$> arbitrary
      , AlonzoRewarding <$> arbitrary
      ]

instance
  ( Era era
  , Arbitrary (TxCert era)
  ) =>
  Arbitrary (AlonzoPlutusPurpose AsIxItem era)
  where
  arbitrary =
    oneof
      [ AlonzoSpending <$> arbitrary
      , AlonzoMinting <$> arbitrary
      , AlonzoCertifying <$> arbitrary
      , AlonzoRewarding <$> arbitrary
      ]

instance Era era => Arbitrary (AlonzoPlutusPurpose AsIx era) where
  arbitrary = arbitrary >>= genAlonzoPlutusPurposePointer

instance
  ( AlonzoEraScript era
  , AlonzoEraPParams era
  , Arbitrary (PParams era)
  , Arbitrary (PlutusPurpose AsIx era)
  ) =>
  Arbitrary (ScriptIntegrity era)
  where
  arbitrary =
    ScriptIntegrity
      <$> arbitrary
      <*> arbitrary
      -- FIXME: why singleton? We should generate empty as well as many value sets
      <*> (Set.singleton <$> (getLanguageView @era <$> arbitrary <*> arbitrary))

deriving instance Arbitrary CoinPerWord

instance Arbitrary AlonzoGenesis where
  arbitrary =
    AlonzoGenesis
      <$> arbitrary
      <*> genValidCostModels [PlutusV1, PlutusV2]
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

alwaysSucceeds ::
  forall l era.
  (HasCallStack, EraPlutusTxInfo l era) =>
  Natural ->
  Script era
alwaysSucceeds = fromPlutusScript . mkSupportedPlutusScript . alwaysSucceedsPlutus @l

alwaysFails ::
  forall l era.
  (HasCallStack, EraPlutusTxInfo l era) =>
  Natural ->
  Script era
alwaysFails = fromPlutusScript . mkSupportedPlutusScript . alwaysFailsPlutus @l

alwaysSucceedsLang ::
  SupportedLanguage era ->
  Natural ->
  PlutusScript era
alwaysSucceedsLang supportedLanguage n =
  case supportedLanguage of
    SupportedLanguage slang -> mkSupportedPlutusScript $ asSLanguage slang (alwaysSucceedsPlutus n)

alwaysFailsLang ::
  SupportedLanguage era ->
  Natural ->
  PlutusScript era
alwaysFailsLang supportedLanguage n =
  case supportedLanguage of
    SupportedLanguage slang -> mkSupportedPlutusScript $ asSLanguage slang (alwaysFailsPlutus n)

-- | Partial version of `mkPlutusScript`
mkPlutusScript' ::
  forall era l.
  (HasCallStack, AlonzoEraScript era, PlutusLanguage l) =>
  Plutus l ->
  Script era
mkPlutusScript' plutus =
  case mkPlutusScript plutus of
    Nothing ->
      error $
        "Plutus version " ++ show (plutusLanguage plutus) ++ " is not supported in " ++ eraName @era
    Just plutusScript -> fromPlutusScript plutusScript
{-# DEPRECATED mkPlutusScript' "In favor of `fromPlutusScript` . `mkSupportedPlutusScript`" #-}

instance Arbitrary (TransitionConfig AlonzoEra) where
  arbitrary = AlonzoTransitionConfig <$> arbitrary <*> arbitrary

deriving newtype instance Arbitrary (Tx AlonzoEra)

instance
  ( EraBlockBody era
  , AlonzoEraTx era
  , Arbitrary (Tx era)
  , SafeToHash (TxWits era)
  ) =>
  Arbitrary (AlonzoBlockBody era)
  where
  arbitrary = AlonzoBlockBody <$> arbitrary
