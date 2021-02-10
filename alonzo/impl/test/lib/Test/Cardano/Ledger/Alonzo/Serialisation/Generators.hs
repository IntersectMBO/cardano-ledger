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
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.Serialisation.Generators where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Data (AuxiliaryData (..), Data (..), PlutusData (..))
import Cardano.Ledger.Alonzo.Language
import Cardano.Ledger.Alonzo.PParams
import Cardano.Ledger.Alonzo.Scripts (CostModel (..), ExUnits (..), Prices (..), Script (..), Tag (..))
import Cardano.Ledger.Alonzo.Tx
import Cardano.Ledger.Alonzo.TxBody
  ( TxOut (..),
  )
import Cardano.Ledger.Alonzo.TxWitness
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.SafeHash (HasAlgorithm, SafeHash, unsafeMakeSafeHash)
import Cardano.Ledger.Shelley.Constraints (UsesScript, UsesValue)
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators (genMintValues)
import Test.QuickCheck
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (Mock)
import Test.Shelley.Spec.Ledger.Serialisation.EraIndepGenerators ()

instance Arbitrary (Data era) where
  arbitrary = Data <$> arbitrary

instance Arbitrary PlutusData where
  arbitrary = pure NotReallyData

instance
  ( UsesScript era,
    Ord (Core.Script era),
    Arbitrary (Core.Script era)
  ) =>
  Arbitrary (AuxiliaryData era)
  where
  arbitrary = AuxiliaryData <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Tag where
  arbitrary = elements [Spend, Mint, Cert, Rewrd]

instance Arbitrary RdmrPtr where
  arbitrary = RdmrPtr <$> arbitrary <*> arbitrary

instance Arbitrary ExUnits where
  arbitrary = ExUnits <$> arbitrary <*> arbitrary

instance
  ( Era era,
    UsesValue era,
    Mock (Crypto era),
    Arbitrary (Core.Script era),
    UsesScript era
  ) =>
  Arbitrary (TxWitness era)
  where
  arbitrary =
    TxWitness
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance HasAlgorithm c => Arbitrary (SafeHash c i) where
  arbitrary = unsafeMakeSafeHash <$> arbitrary

instance
  ( Era era,
    UsesValue era,
    Mock (Crypto era),
    Arbitrary (Core.Value era)
  ) =>
  Arbitrary (TxOut era)
  where
  arbitrary =
    TxOut
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance
  forall c.
  (Mock c) =>
  Arbitrary (TxBody (AlonzoEra c))
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
      <*> genMintValues @c
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

deriving newtype instance Arbitrary IsValidating

instance Mock c => Arbitrary (Tx (AlonzoEra c)) where
  arbitrary =
    Tx
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Mock c => Arbitrary (Script (AlonzoEra c)) where
  arbitrary = frequency [(1, pure PlutusScript), (9, NativeScript <$> arbitrary)]

-- ==========================
--

instance Arbitrary Language where
  arbitrary = elements [PlutusV1]

instance Arbitrary Prices where
  arbitrary = Prices <$> arbitrary <*> arbitrary

instance Arbitrary CostModel where
  arbitrary = CostModel <$> arbitrary

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
