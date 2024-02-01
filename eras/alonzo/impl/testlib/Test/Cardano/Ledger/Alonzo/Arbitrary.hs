{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
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
  FlexibleCostModels (..),
  genAlonzoScript,
  genNativeScript,
  genPlutusScript,
  genScripts,
  genValidCostModel,
  genValidAndUnknownCostModels,
  genAlonzoPlutusPurposePointer,
) where

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import Cardano.Ledger.Alonzo.PParams (AlonzoPParams (AlonzoPParams), OrdExUnits (OrdExUnits))
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
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxBody (AlonzoTxBody))
import Cardano.Ledger.Alonzo.TxOut (AlonzoTxOut (AlonzoTxOut))
import Cardano.Ledger.Alonzo.TxWits (
  AlonzoTxWits (AlonzoTxWits),
  Redeemers (Redeemers),
  TxDats (TxDats),
 )
import Cardano.Ledger.BaseTypes (StrictMaybe)
import Cardano.Ledger.Plutus.Data (
  BinaryData,
  Data (..),
  Datum (..),
  dataToBinaryData,
  hashData,
 )
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..))
import Cardano.Ledger.Plutus.Language (
  Language (..),
  Plutus (..),
  PlutusLanguage,
  asSLanguage,
  plutusLanguage,
  withSLanguage,
 )
import Cardano.Ledger.Shelley.LedgerState (PPUPPredFailure)
import Cardano.Ledger.Shelley.Rules (PredicateFailure, ShelleyUtxowPredFailure)
import Cardano.Ledger.Shelley.TxWits (keyBy)
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (pack)
import Data.Word
import Numeric.Natural (Natural)
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary (
  FlexibleCostModels (..),
  genValidAndUnknownCostModels,
  genValidCostModel,
 )
import Test.Cardano.Ledger.Mary.Arbitrary ()
import Test.Cardano.Ledger.Plutus (alwaysFailsPlutus, alwaysSucceedsPlutus)

instance Era era => Arbitrary (Data era) where
  arbitrary = Data <$> arbitrary

instance Era era => Arbitrary (BinaryData era) where
  arbitrary = dataToBinaryData <$> arbitrary

instance Arbitrary PV1.Data where
  arbitrary = resize 5 (sized gendata)
    where
      gendata n
        | n > 0 =
            oneof
              [ PV1.I <$> arbitrary
              , PV1.B <$> arbitrary
              , PV1.Map <$> listOf ((,) <$> gendata (n `div` 2) <*> gendata (n `div` 2))
              , PV1.Constr
                  <$> fmap fromIntegral (arbitrary :: Gen Natural)
                  <*> listOf (gendata (n `div` 2))
              , PV1.List <$> listOf (gendata (n `div` 2))
              ]
      gendata _ = oneof [PV1.I <$> arbitrary, PV1.B <$> arbitrary]

instance
  ( Arbitrary (AlonzoScript era)
  , AlonzoEraScript era
  ) =>
  Arbitrary (AlonzoTxAuxData era)
  where
  arbitrary = mkAlonzoTxAuxData @[] <$> arbitrary <*> arbitrary

instance
  (AlonzoEraScript era, Arbitrary (PlutusPurpose AsIndex era)) =>
  Arbitrary (Redeemers era)
  where
  arbitrary = Redeemers <$> arbitrary

instance
  ( Era era
  , Arbitrary (Script era)
  , AlonzoEraScript era
  , Arbitrary (PlutusPurpose AsIndex era)
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
  Gen (Map.Map (ScriptHash (EraCrypto era)) (Script era))
genScripts = keyBy (hashScript @era) <$> (arbitrary :: Gen [Script era])

instance Era era => Arbitrary (TxDats era) where
  arbitrary = TxDats . keyBy hashData <$> arbitrary

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

instance
  ( EraTxOut era
  , EraTxCert era
  , Arbitrary (TxOut era)
  , Arbitrary (PParamsHKD StrictMaybe era)
  , Arbitrary (TxCert era)
  ) =>
  Arbitrary (AlonzoTxBody era)
  where
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

instance (AlonzoEraScript era, Script era ~ AlonzoScript era) => Arbitrary (AlonzoScript era) where
  arbitrary = do
    lang <- elements [minBound .. eraMaxLanguage @era]
    genAlonzoScript lang

genAlonzoScript ::
  ( AlonzoEraScript era
  , Script era ~ AlonzoScript era
  ) =>
  Language ->
  Gen (AlonzoScript era)
genAlonzoScript lang =
  frequency
    [ (2, genPlutusScript lang)
    , (8, genNativeScript)
    ]

genNativeScript :: AlonzoEraScript era => Gen (AlonzoScript era)
genNativeScript = TimelockScript <$> arbitrary

genPlutusScript ::
  ( AlonzoEraScript era
  , Script era ~ AlonzoScript era
  ) =>
  Language ->
  Gen (AlonzoScript era)
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
      <*> arbitrary
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
      <*> arbitrary
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
  (Era era, Arbitrary (PPUPPredFailure era)) =>
  Arbitrary (AlonzoUtxosPredFailure era)
  where
  arbitrary =
    oneof
      [ ValidationTagMismatch <$> arbitrary <*> arbitrary
      , UpdateFailure <$> arbitrary
      ]

instance
  ( EraTxOut era
  , Era era
  , Arbitrary (Value era)
  , Arbitrary (TxOut era)
  , Arbitrary (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  Arbitrary (AlonzoUtxoPredFailure era)
  where
  arbitrary =
    oneof
      [ BadInputsUTxO <$> arbitrary
      , OutsideValidityIntervalUTxO <$> arbitrary <*> arbitrary
      , MaxTxSizeUTxO <$> arbitrary <*> arbitrary
      , pure InputSetEmptyUTxO
      , FeeTooSmallUTxO <$> arbitrary <*> arbitrary
      , ValueNotConservedUTxO <$> arbitrary <*> arbitrary
      , OutputTooSmallUTxO <$> arbitrary
      , UtxosFailure <$> arbitrary
      , WrongNetwork <$> arbitrary <*> arbitrary
      , WrongNetworkWithdrawal <$> arbitrary <*> arbitrary
      , OutputBootAddrAttrsTooBig <$> arbitrary
      , pure TriesToForgeADA
      , OutputTooBigUTxO <$> arbitrary
      , InsufficientCollateral <$> arbitrary <*> arbitrary
      , ScriptsNotPaidUTxO <$> arbitrary
      , ExUnitsTooBigUTxO <$> arbitrary <*> arbitrary
      , CollateralContainsNonADA <$> arbitrary
      ]

instance
  ( Era era
  , Arbitrary (PredicateFailure (EraRule "UTXO" era))
  , Arbitrary (ShelleyUtxowPredFailure era)
  , Arbitrary (TxCert era)
  , Arbitrary (PlutusPurpose AsItem era)
  ) =>
  Arbitrary (AlonzoUtxowPredFailure era)
  where
  arbitrary =
    oneof
      [ ShelleyInAlonzoUtxowPredFailure <$> arbitrary
      , MissingRedeemers <$> arbitrary
      , MissingRequiredDatums <$> arbitrary <*> arbitrary
      , PPViewHashesDontMatch <$> arbitrary <*> arbitrary
      ]

deriving instance Arbitrary ix => Arbitrary (AsIndex ix it)

deriving instance Arbitrary it => Arbitrary (AsItem ix it)

instance (Arbitrary ix, Arbitrary it) => Arbitrary (AsIxItem ix it) where
  arbitrary = AsIxItem <$> arbitrary <*> arbitrary

genAlonzoPlutusPurposePointer :: Word32 -> Gen (AlonzoPlutusPurpose AsIndex era)
genAlonzoPlutusPurposePointer i =
  elements
    [ AlonzoSpending (AsIndex i)
    , AlonzoMinting (AsIndex i)
    , AlonzoCertifying (AsIndex i)
    , AlonzoRewarding (AsIndex i)
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

instance Era era => Arbitrary (AlonzoPlutusPurpose AsIndex era) where
  arbitrary = arbitrary >>= genAlonzoPlutusPurposePointer

instance
  ( AlonzoEraScript era
  , AlonzoEraPParams era
  , Arbitrary (PParams era)
  , Arbitrary (PlutusPurpose AsIndex era)
  ) =>
  Arbitrary (ScriptIntegrity era)
  where
  arbitrary =
    ScriptIntegrity
      <$> arbitrary
      <*> arbitrary
      -- FIXME: why singleton? We should generate empty as well as many value sets
      <*> (Set.singleton <$> (getLanguageView @era <$> arbitrary <*> arbitrary))

instance
  Era era =>
  Arbitrary (Datum era)
  where
  arbitrary =
    oneof
      [ pure NoDatum
      , DatumHash <$> arbitrary
      , Datum . dataToBinaryData <$> arbitrary
      ]

deriving instance Arbitrary CoinPerWord

instance Arbitrary AlonzoGenesis where
  arbitrary =
    AlonzoGenesis
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

alwaysSucceeds ::
  forall l era.
  (HasCallStack, PlutusLanguage l, AlonzoEraScript era) =>
  Natural ->
  Script era
alwaysSucceeds n = mkPlutusScript' (alwaysSucceedsPlutus @l n)

alwaysFails ::
  forall l era.
  (HasCallStack, PlutusLanguage l, AlonzoEraScript era) =>
  Natural ->
  Script era
alwaysFails n = mkPlutusScript' (alwaysFailsPlutus @l n)

alwaysSucceedsLang :: (HasCallStack, AlonzoEraScript era) => Language -> Natural -> Script era
alwaysSucceedsLang lang n =
  withSLanguage lang $ \slang -> mkPlutusScript' $ asSLanguage slang (alwaysSucceedsPlutus n)

alwaysFailsLang :: (HasCallStack, AlonzoEraScript era) => Language -> Natural -> Script era
alwaysFailsLang lang n =
  withSLanguage lang $ \slang -> mkPlutusScript' $ asSLanguage slang (alwaysFailsPlutus n)

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
