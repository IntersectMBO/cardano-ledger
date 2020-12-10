{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.Serialisation.Generators where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Data (Data (..), DataHash (..))
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Alonzo.TxBody
  ( IsFee (..),
    TxBody (TxBody),
    TxIn (..),
    TxOut (..),
  )
import Cardano.Ledger.Alonzo.TxWitness
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Shelley.Constraints (ShelleyBased)
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators (genMintValues)
import Test.QuickCheck
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (Mock)
import Test.Shelley.Spec.Ledger.Serialisation.EraIndepGenerators ()

-- TODO correct arbitrary generator for Data
instance Arbitrary (Data era) where
  arbitrary = pure NotReallyData

instance Arbitrary Tag where
  arbitrary = elements [Input, Mint, Cert, Wdrl]

instance Arbitrary RdmrPtr where
  arbitrary = RdmrPtr <$> arbitrary <*> arbitrary

instance Arbitrary ExUnits where
  arbitrary = ExUnits <$> arbitrary <*> arbitrary

instance
  ( ShelleyBased era,
    Mock (Crypto era),
    Arbitrary (Core.Script era)
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

deriving newtype instance CC.Crypto c => Arbitrary (ScriptDataHash c)

deriving newtype instance Era era => Arbitrary (DataHash era)

deriving newtype instance Arbitrary IsFee

instance
  ( CC.Crypto c
  ) =>
  Arbitrary (TxIn c)
  where
  arbitrary =
    TxInCompact
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance
  ( ShelleyBased era,
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
      <*> genMintValues
      <*> arbitrary
      <*> arbitrary
