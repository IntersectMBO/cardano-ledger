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
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.Arbitrary (
  alwaysSucceeds,
  alwaysFails,
  costModelParamsCount,
  FlexibleCostModels (..),
  genAlonzoScript,
) where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import Cardano.Ledger.Alonzo.Language (Language)
import Cardano.Ledger.Alonzo.PParams (AlonzoPParams (AlonzoPParams), OrdExUnits (OrdExUnits))
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoPredFailure (..),
  AlonzoUtxosPredFailure (..),
  AlonzoUtxowPredFailure (..),
  FailureDescription (..),
  TagMismatchDescription (..),
 )
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoScript (..),
  CostModel,
  CostModels (..),
  ExUnits (..),
  Prices (..),
  Tag (..),
  mkCostModel,
  mkCostModelsLenient,
 )
import Cardano.Ledger.Alonzo.Scripts.Data (
  BinaryData,
  Data (..),
  Datum (..),
  dataToBinaryData,
  hashData,
 )
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (AlonzoTx), IsValid (IsValid), ScriptIntegrity (ScriptIntegrity), ScriptPurpose (..), getLanguageView)
import Cardano.Ledger.Alonzo.TxAuxData (
  AlonzoTxAuxData (..),
  mkAlonzoTxAuxData,
 )
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxBody (AlonzoTxBody))
import Cardano.Ledger.Alonzo.TxOut (AlonzoTxOut (AlonzoTxOut))
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits (AlonzoTxWits), RdmrPtr (RdmrPtr), Redeemers (Redeemers), TxDats (TxDats))
import Cardano.Ledger.BaseTypes (StrictMaybe)
import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Language (Language (..), nonNativeLanguages)
import Cardano.Ledger.Shelley.LedgerState (PPUPPredFailure)
import Cardano.Ledger.Shelley.Rules (PredicateFailure, ShelleyUtxowPredFailure)
import Cardano.Ledger.Shelley.TxWits (keyBy)
import Control.Monad (replicateM)
import Data.Either (fromRight)
import Data.Functor.Identity (Identity)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (pack)
import Data.Word (Word8)
import Numeric.Natural (Natural)
import qualified PlutusLedgerApi.Test.Examples as Plutus (
  alwaysFailingNAryFunction,
  alwaysSucceedingNAryFunction,
 )
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3
import PlutusPrelude (enumerate)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Mary.Arbitrary ()

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
  , Era era
  ) =>
  Arbitrary (AlonzoTxAuxData era)
  where
  arbitrary = mkAlonzoTxAuxData @[] <$> arbitrary <*> arbitrary

instance Arbitrary Tag where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary RdmrPtr where
  arbitrary = RdmrPtr <$> arbitrary <*> arbitrary

instance Arbitrary ExUnits where
  arbitrary = ExUnits <$> genUnit <*> genUnit
    where
      genUnit = fromIntegral <$> choose (0, maxBound :: Int64)

instance Era era => Arbitrary (Redeemers era) where
  arbitrary = Redeemers <$> arbitrary

instance
  ( Era era
  , Arbitrary (Script era)
  , AlonzoScript era ~ Script era
  , EraScript era
  ) =>
  Arbitrary (AlonzoTxWits era)
  where
  arbitrary =
    AlonzoTxWits
      <$> arbitrary
      <*> arbitrary
      <*> genScripts
      <*> genData
      <*> arbitrary

genScripts ::
  forall era.
  ( EraScript era
  , Arbitrary (Script era)
  ) =>
  Gen (Map.Map (ScriptHash (EraCrypto era)) (Script era))
genScripts = keyBy (hashScript @era) <$> (arbitrary :: Gen [Script era])

genData :: Era era => Gen (TxDats era)
genData = TxDats . keyBy hashData <$> arbitrary

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
  , EraDCert era
  , Arbitrary (TxOut era)
  , Arbitrary (PParamsHKD StrictMaybe era)
  , Arbitrary (DCert era)
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

genAlonzoScript ::
  forall era.
  ( Era era
  ) =>
  [Language] ->
  Gen (AlonzoScript era)
genAlonzoScript langs = do
  lang <- elements langs -- The language is not present in the Script serialization
  frequency
    [ (1, pure (alwaysSucceeds lang 1))
    , (1, pure (alwaysFails lang 1))
    , (10, TimelockScript <$> arbitrary)
    ]

instance Crypto c => Arbitrary (AlonzoScript (AlonzoEra c)) where
  arbitrary = genAlonzoScript [PlutusV1]

instance Arbitrary Language where
  arbitrary = elements nonNativeLanguages

instance Arbitrary Prices where
  arbitrary = Prices <$> arbitrary <*> arbitrary

costModelParamsCount :: Language -> Int
costModelParamsCount lang = case lang of
  PlutusV1 -> length (enumerate @PV1.ParamName)
  PlutusV2 -> length (enumerate @PV2.ParamName)
  PlutusV3 -> length (enumerate @PV3.ParamName)

genCostModel :: Language -> Gen (Language, CostModel)
genCostModel lang = (,) lang <$> genValidCostModel lang

instance Arbitrary CostModel where
  arbitrary = snd <$> (elements nonNativeLanguages >>= genCostModel)

genValidCostModel :: Language -> Gen CostModel
genValidCostModel lang = do
  newParamValues <- (vectorOf (costModelParamsCount lang) (arbitrary :: Gen Integer))
  pure $ fromRight (error "Corrupt cost model") (mkCostModel lang newParamValues)

genValidCostModelPair :: Language -> Gen (Language, CostModel)
genValidCostModelPair lang = (,) lang <$> genValidCostModel lang

-- | This Arbitrary instance assumes the inflexible deserialization
-- scheme prior to version 9.
instance Arbitrary CostModels where
  arbitrary = do
    langs <- sublistOf nonNativeLanguages
    cms <- mapM genValidCostModelPair langs
    pure $ CostModels (Map.fromList cms) mempty mempty

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
  , Arbitrary (DCert era)
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

instance
  ( Era era
  , Arbitrary (DCert era)
  ) =>
  Arbitrary (ScriptPurpose era)
  where
  arbitrary =
    oneof
      [ Minting <$> arbitrary
      , Spending <$> arbitrary
      , Rewarding <$> arbitrary
      , Certifying <$> arbitrary
      ]

instance
  ( AlonzoEraPParams era
  , Arbitrary (PParams era)
  ) =>
  Arbitrary (ScriptIntegrity era)
  where
  arbitrary =
    ScriptIntegrity
      <$> arbitrary
      <*> genData
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

alwaysSucceeds :: Language -> Natural -> AlonzoScript era
alwaysSucceeds lang n = PlutusScript lang (Plutus.alwaysSucceedingNAryFunction n)

alwaysFails :: Language -> Natural -> AlonzoScript era
alwaysFails lang n = PlutusScript lang (Plutus.alwaysFailingNAryFunction n)

-- | This Arbitrary instance assumes the flexible deserialization
-- scheme of 'CostModels' starting at version 9.
newtype FlexibleCostModels = FlexibleCostModels CostModels
  deriving (Show, Eq, Ord)
  deriving newtype (EncCBOR, DecCBOR)

instance Arbitrary FlexibleCostModels where
  arbitrary = do
    known <- genKnownCostModels
    unknown <- genUnknownCostModels
    let cms = known `Map.union` unknown
    pure . FlexibleCostModels $ mkCostModelsLenient cms

genUnknownCostModels :: Gen (Map Word8 [Integer])
genUnknownCostModels = Map.fromList <$> listOf genUnknownCostModelValues

genKnownCostModels :: Gen (Map Word8 [Integer])
genKnownCostModels = do
  langs <- sublistOf nonNativeLanguages
  cms <- mapM genCostModelValues langs
  return $ Map.fromList cms

genUnknownCostModelValues :: Gen (Word8, [Integer])
genUnknownCostModelValues = do
  lang <- chooseInt (firstInvalid, fromIntegral (maxBound :: Word8))
  vs <- arbitrary
  return (fromIntegral . fromEnum $ lang, vs)
  where
    firstInvalid = fromEnum (maxBound :: Language) + 1

genCostModelValues :: Language -> Gen (Word8, [Integer])
genCostModelValues lang =
  (lang',)
    <$> oneof
      [ listAtLeast (costModelParamsCount lang) -- Valid Cost Model for known language
      , take tooFew <$> arbitrary -- Invalid Cost Model for known language
      ]
  where
    lang' = fromIntegral (fromEnum lang)
    tooFew = costModelParamsCount lang - 1

listAtLeast :: Int -> Gen [Integer]
listAtLeast x = do
  y <- getNonNegative <$> arbitrary
  replicateM (x + y) arbitrary
