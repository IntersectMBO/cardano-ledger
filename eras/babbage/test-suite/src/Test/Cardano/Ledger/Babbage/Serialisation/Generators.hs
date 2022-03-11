{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans -fno-warn-unused-imports #-}

module Test.Cardano.Ledger.Babbage.Serialisation.Generators where

-- instances
import Cardano.Ledger.Alonzo.Data (AuxiliaryData (..), BinaryData, Data (..), dataToBinaryData)
import Cardano.Ledger.Alonzo.Language
import Cardano.Ledger.Alonzo.Rules.Utxo (UtxoPredicateFailure (..))
import Cardano.Ledger.Alonzo.Rules.Utxos (TagMismatchDescription (..), UtxosPredicateFailure (..))
import Cardano.Ledger.Alonzo.Rules.Utxow (UtxowPredicateFail (..))
import Cardano.Ledger.Alonzo.Scripts
  ( CostModel (..),
    ExUnits (..),
    Prices (..),
    Script (..),
    Tag (..),
  )
import Cardano.Ledger.Alonzo.TxInfo (FailureDescription (..), ScriptResult (..))
import Cardano.Ledger.Alonzo.TxWitness
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.PParams
import Cardano.Ledger.Babbage.Rules.Utxo (BabbageUtxoPred (..))
import Cardano.Ledger.Babbage.Tx
import Cardano.Ledger.Babbage.TxBody
  ( BabbageBody,
    Datum (..),
    TxBody (..),
    TxOut (..),
  )
import Cardano.Ledger.CompactAddress (compactAddr)
import Cardano.Ledger.Compactible (toCompact)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era, ValidateScript (..))
import Cardano.Ledger.Hashes (ScriptHash)
import Cardano.Ledger.Shelley.Constraints (UsesScript, UsesValue)
import Control.Applicative (liftA2)
import Data.Int (Int64)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Set as Set
import Data.Text (pack)
import Numeric.Natural (Natural)
import qualified PlutusTx as Plutus
import Test.Cardano.Ledger.Alonzo.Scripts (alwaysFails, alwaysSucceeds)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators (genData)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators (genMintValues)
import Test.QuickCheck

instance
  ( Era era,
    UsesValue era,
    Mock (Crypto era),
    Arbitrary (Core.Value era),
    Arbitrary (Core.Script era)
  ) =>
  Arbitrary (TxOut era)
  where
  arbitrary =
    TxOut
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance
  ( UsesValue era,
    Mock (Crypto era),
    BabbageBody (era),
    Arbitrary (Core.Value era),
    Arbitrary (Core.Script era)
  ) =>
  Arbitrary (TxBody era)
  where
  arbitrary =
    TxBody
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

instance Mock c => Arbitrary (ValidatedTx (BabbageEra c)) where
  arbitrary =
    ValidatedTx
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Mock c => Arbitrary (Script (BabbageEra c)) where
  arbitrary = do
    lang <- arbitrary -- The language is not present in the Script serialization
    frequency
      [ (1, pure (alwaysSucceeds lang 1)),
        (1, pure (alwaysFails lang 1)),
        (10, TimelockScript <$> arbitrary)
      ]

-- ==========================
--

instance Arbitrary (PParams era) where
  arbitrary =
    PParams
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

instance Arbitrary (PParamsUpdate era) where
  arbitrary =
    PParams
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
      [ (BadInputsUTxO) <$> arbitrary,
        OutsideValidityIntervalUTxO <$> arbitrary <*> arbitrary,
        MaxTxSizeUTxO <$> arbitrary <*> arbitrary,
        pure InputSetEmptyUTxO,
        FeeTooSmallUTxO <$> arbitrary <*> arbitrary,
        (ValueNotConservedUTxO) <$> arbitrary <*> arbitrary,
        (OutputTooSmallUTxO) <$> arbitrary,
        (UtxosFailure) <$> arbitrary,
        (WrongNetwork) <$> arbitrary <*> arbitrary,
        (WrongNetworkWithdrawal) <$> arbitrary <*> arbitrary,
        (OutputBootAddrAttrsTooBig) <$> arbitrary,
        pure TriesToForgeADA,
        (OutputTooBigUTxO) <$> arbitrary,
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
        FromAlonzoUtxowFail <$> arbitrary,
        UnequalCollateralReturn <$> arbitrary <*> arbitrary,
        DanglingWitnessDataHash <$> arbitrary
      ]
