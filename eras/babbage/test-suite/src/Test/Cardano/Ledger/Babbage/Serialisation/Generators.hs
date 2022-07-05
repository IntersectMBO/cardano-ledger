{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babbage.Serialisation.Generators where

import Cardano.Binary (ToCBOR)
import Cardano.Ledger.Alonzo.Data (dataToBinaryData)
import Cardano.Ledger.Alonzo.Rules
  ( UtxoPredicateFailure (..),
    UtxosPredicateFailure (..),
    UtxowPredicateFail (..),
  )
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..))
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.PParams
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPred (..), BabbageUtxowPred (..))
import Cardano.Ledger.Babbage.Tx
import Cardano.Ledger.Babbage.TxBody
  ( BabbageEraTxBody,
    BabbageTxOut (..),
    Datum (..),
  )
import Cardano.Ledger.Core
import Cardano.Ledger.Serialization (Sized, mkSized)
import qualified Data.Set as Set
import Test.Cardano.Ledger.Alonzo.Scripts (alwaysFails, alwaysSucceeds)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators (genData)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators (genMintValues)
import Test.QuickCheck

instance (ToCBOR a, Arbitrary a) => Arbitrary (Sized a) where
  arbitrary = mkSized <$> arbitrary

instance
  ( EraTxOut era,
    Mock (Crypto era),
    Arbitrary (Value era),
    Arbitrary (Script era)
  ) =>
  Arbitrary (BabbageTxOut era)
  where
  arbitrary =
    BabbageTxOut
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance
  ( Mock (Crypto era),
    ToCBOR (Script era),
    BabbageEraTxBody era,
    Arbitrary (Value era),
    Arbitrary (Script era)
  ) =>
  Arbitrary (BabbageTxBody era)
  where
  arbitrary =
    BabbageTxBody
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
      <*> genMintValues
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Mock c => Arbitrary (AlonzoTx (BabbageEra c)) where
  arbitrary =
    AlonzoTx
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Mock c => Arbitrary (AlonzoScript (BabbageEra c)) where
  arbitrary = do
    lang <- arbitrary -- The language is not present in the Script serialization
    frequency
      [ (1, pure (alwaysSucceeds lang 1)),
        (1, pure (alwaysFails lang 1)),
        (10, TimelockScript <$> arbitrary)
      ]

-- ==========================
--

instance Arbitrary (BabbagePParams era) where
  arbitrary =
    BabbagePParams
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

instance Arbitrary (BabbagePParamsUpdate era) where
  arbitrary =
    BabbagePParams
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

instance Mock c => Arbitrary (UtxosPredicateFailure (BabbageEra c)) where
  arbitrary =
    oneof
      [ ValidationTagMismatch <$> arbitrary <*> arbitrary,
        UpdateFailure <$> arbitrary
      ]

instance Mock c => Arbitrary (UtxoPredicateFailure (BabbageEra c)) where
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

instance Mock c => Arbitrary (UtxowPredicateFail (BabbageEra c)) where
  arbitrary =
    oneof
      [ WrappedShelleyEraFailure <$> arbitrary,
        MissingRedeemers <$> arbitrary,
        MissingRequiredDatums <$> arbitrary <*> arbitrary,
        PPViewHashesDontMatch <$> arbitrary <*> arbitrary
      ]

instance Mock c => Arbitrary (ScriptIntegrity (BabbageEra c)) where
  arbitrary =
    ScriptIntegrity
      <$> arbitrary
      <*> genData
      <*> (Set.singleton <$> (getLanguageView @(BabbageEra c) <$> arbitrary <*> arbitrary))

instance
  Mock (Crypto era) =>
  Arbitrary (Datum era)
  where
  arbitrary =
    oneof
      [ pure NoDatum,
        DatumHash <$> arbitrary,
        Datum . dataToBinaryData <$> arbitrary
      ]

instance Mock c => Arbitrary (BabbageUtxoPred (BabbageEra c)) where
  arbitrary =
    oneof
      [ FromAlonzoUtxoFail <$> arbitrary,
        IncorrectTotalCollateralField <$> arbitrary <*> arbitrary
      ]

instance Mock c => Arbitrary (BabbageUtxowPred (BabbageEra c)) where
  arbitrary =
    oneof
      [ FromAlonzoUtxowFail <$> arbitrary,
        UtxoFailure <$> arbitrary,
        MalformedScriptWitnesses <$> arbitrary,
        MalformedReferenceScripts <$> arbitrary
      ]
