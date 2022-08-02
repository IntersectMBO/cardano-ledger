{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.Serialisation.Generators where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Data (AlonzoAuxiliaryData (..), BinaryData, Data (..), dataToBinaryData)
import Cardano.Ledger.Alonzo.Language
import Cardano.Ledger.Alonzo.PParams
import Cardano.Ledger.Alonzo.Rules
  ( AlonzoUtxoPredFailure (..),
    AlonzoUtxosPredFailure (..),
    AlonzoUtxowPredFailure (..),
    FailureDescription (..),
    TagMismatchDescription (..),
  )
import Cardano.Ledger.Alonzo.Scripts
  ( AlonzoScript (..),
    CostModels (..),
    ExUnits (..),
    Prices (..),
    Tag (..),
    mkCostModel,
  )
import Cardano.Ledger.Alonzo.Tx
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxOut (..))
import Cardano.Ledger.Alonzo.TxWitness
import Cardano.Ledger.Core
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Numeric.Natural (Natural)
import qualified Plutus.V1.Ledger.Api as PV1
import qualified Plutus.V2.Ledger.Api as PV2
import Test.Cardano.Ledger.Alonzo.Scripts (alwaysFails, alwaysSucceeds)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators (genMintValues)
import Test.QuickCheck

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
              [ PV1.I <$> arbitrary,
                PV1.B <$> arbitrary,
                PV1.Map <$> listOf ((,) <$> gendata (n `div` 2) <*> gendata (n `div` 2)),
                PV1.Constr <$> fmap fromIntegral (arbitrary :: Gen Natural)
                  <*> listOf (gendata (n `div` 2)),
                PV1.List <$> listOf (gendata (n `div` 2))
              ]
      gendata _ = oneof [PV1.I <$> arbitrary, PV1.B <$> arbitrary]

instance
  ( Script era ~ AlonzoScript era,
    Arbitrary (Script era),
    Era era
  ) =>
  Arbitrary (AlonzoAuxiliaryData era)
  where
  arbitrary = AlonzoAuxiliaryData <$> arbitrary <*> arbitrary

instance Arbitrary Tag where
  arbitrary = elements [Spend, Mint, Cert, Rewrd]

instance Arbitrary RdmrPtr where
  arbitrary = RdmrPtr <$> arbitrary <*> arbitrary

instance Arbitrary ExUnits where
  arbitrary = ExUnits <$> genUnit <*> genUnit
    where
      genUnit = fromIntegral <$> choose (0, maxBound :: Int64)

instance (Era era) => Arbitrary (Redeemers era) where
  arbitrary = Redeemers <$> arbitrary

instance
  ( Mock (Crypto era),
    Arbitrary (Script era),
    AlonzoScript era ~ Script era,
    EraScript era
  ) =>
  Arbitrary (TxWitness era)
  where
  arbitrary =
    TxWitness
      <$> arbitrary
      <*> arbitrary
      <*> genScripts
      <*> genData
      <*> arbitrary

keyBy :: Ord k => (a -> k) -> [a] -> Map k a
keyBy f xs = Map.fromList ((\x -> (f x, x)) <$> xs)

genScripts ::
  forall era.
  ( Script era ~ AlonzoScript era,
    EraScript era,
    Arbitrary (AlonzoScript era)
  ) =>
  Gen (Map (ScriptHash (Crypto era)) (Script era))
genScripts = keyBy (hashScript @era) <$> (arbitrary :: Gen [Script era])

genData :: forall era. Era era => Gen (TxDats era)
genData = TxDats . keyBy hashData <$> arbitrary

instance
  ( EraTxOut era,
    Mock (Crypto era),
    Arbitrary (Value era)
  ) =>
  Arbitrary (AlonzoTxOut era)
  where
  arbitrary =
    AlonzoTxOut
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Mock c => Arbitrary (AlonzoTxBody (AlonzoEra c)) where
  arbitrary =
    AlonzoTxBody
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> genMintValues @c
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

deriving newtype instance Arbitrary IsValid

instance Mock c => Arbitrary (AlonzoTx (AlonzoEra c)) where
  arbitrary =
    AlonzoTx
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Mock c => Arbitrary (AlonzoScript (AlonzoEra c)) where
  arbitrary = do
    lang <- arbitrary -- The language is not present in the Script serialization
    frequency
      [ (1, pure (alwaysSucceeds lang 1)),
        (1, pure (alwaysFails lang 1)),
        (10, TimelockScript <$> arbitrary)
      ]

-- ==========================
--

instance Arbitrary Language where
  arbitrary = elements nonNativeLanguages

instance Arbitrary Prices where
  arbitrary = Prices <$> arbitrary <*> arbitrary

mkNullCostModel :: Set Text -> Map Text Integer
mkNullCostModel = Map.fromSet (const 0)

genCM :: Language -> Set Text -> Gen CostModel
genCM lang costModelParamNames = do
  newCMPs <- traverse (const arbitrary) (mkNullCostModel costModelParamNames)
  either (error "Corrupt cost model") pure $ mkCostModel lang newCMPs

genCostModel :: Language -> Gen (Language, CostModel)
genCostModel PlutusV1 = (PlutusV1,) <$> genCM PlutusV1 PV1.costModelParamNames
genCostModel PlutusV2 = (PlutusV2,) <$> genCM PlutusV2 PV2.costModelParamNames

instance Arbitrary CostModel where
  arbitrary = snd <$> (elements nonNativeLanguages >>= genCostModel)

instance Arbitrary CostModels where
  arbitrary = CostModels . Map.fromList <$> (sublistOf nonNativeLanguages >>= mapM genCostModel)

instance Arbitrary (AlonzoPParams era) where
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

instance Arbitrary (AlonzoPParamsUpdate era) where
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

instance Mock c => Arbitrary (AlonzoUtxosPredFailure (AlonzoEra c)) where
  arbitrary =
    oneof
      [ ValidationTagMismatch <$> arbitrary <*> arbitrary,
        UpdateFailure <$> arbitrary
      ]

instance Mock c => Arbitrary (AlonzoUtxoPredFailure (AlonzoEra c)) where
  arbitrary =
    oneof
      [ BadInputsUTxO <$> arbitrary,
        OutsideValidityIntervalUTxO <$> arbitrary <*> arbitrary,
        MaxTxSizeUTxO <$> arbitrary <*> arbitrary,
        pure InputSetEmptyUTxO,
        FeeTooSmallUTxO <$> arbitrary <*> arbitrary,
        ValueNotConservedUTxO <$> arbitrary <*> arbitrary,
        OutputTooSmallUTxO <$> arbitrary,
        UtxosFailure <$> arbitrary,
        WrongNetwork <$> arbitrary <*> arbitrary,
        WrongNetworkWithdrawal <$> arbitrary <*> arbitrary,
        OutputBootAddrAttrsTooBig <$> arbitrary,
        pure TriesToForgeADA,
        OutputTooBigUTxO <$> arbitrary,
        InsufficientCollateral <$> arbitrary <*> arbitrary,
        ScriptsNotPaidUTxO <$> arbitrary,
        ExUnitsTooBigUTxO <$> arbitrary <*> arbitrary,
        CollateralContainsNonADA <$> arbitrary
      ]

instance Mock c => Arbitrary (AlonzoUtxowPredFailure (AlonzoEra c)) where
  arbitrary =
    oneof
      [ ShelleyInAlonzoUtxowPredFailure <$> arbitrary,
        MissingRedeemers <$> arbitrary,
        MissingRequiredDatums <$> arbitrary <*> arbitrary,
        PPViewHashesDontMatch <$> arbitrary <*> arbitrary
      ]

instance Mock c => Arbitrary (ScriptPurpose c) where
  arbitrary =
    oneof
      [ Minting <$> arbitrary,
        Spending <$> arbitrary,
        Rewarding <$> arbitrary,
        Certifying <$> arbitrary
      ]

instance Mock c => Arbitrary (ScriptIntegrity (AlonzoEra c)) where
  arbitrary =
    ScriptIntegrity
      <$> arbitrary
      <*> genData
      <*> (Set.singleton <$> (getLanguageView @(AlonzoEra c) <$> arbitrary <*> arbitrary))
