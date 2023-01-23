{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Shelley.Serialisation.Generators () where

import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.API (ShelleyTxBody (ShelleyTxBody))
import Cardano.Ledger.Shelley.LedgerState (PPUPPredFailure)
import Cardano.Ledger.Shelley.PParams
import qualified Cardano.Ledger.Shelley.Rules as STS
import Control.Monad.Identity (Identity)
import Data.Maybe.Strict (StrictMaybe)
import Generic.Random (genericArbitraryU)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.QuickCheck (
  Arbitrary,
  arbitrary,
  shrink,
 )

{-------------------------------------------------------------------------------
  ShelleyEra Generators

  These are generators for roundtrip tests, so the generated values are not
  necessarily valid
-------------------------------------------------------------------------------}

instance
  (EraTxOut era, Mock (EraCrypto era), Arbitrary (TxOut era)) =>
  Arbitrary (ShelleyTxBody era)
  where
  arbitrary =
    ShelleyTxBody
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance
  ( Era era
  , Mock (EraCrypto era)
  , Arbitrary (Value era)
  , Arbitrary (TxOut era)
  , Arbitrary (PPUPPredFailure era)
  ) =>
  Arbitrary (STS.ShelleyUtxoPredFailure era)
  where
  arbitrary = genericArbitraryU
  shrink _ = []

instance Era era => Arbitrary (ShelleyPParams Identity era) where
  arbitrary = genericArbitraryU

instance Era era => Arbitrary (ShelleyPParams StrictMaybe era) where
  arbitrary = genericArbitraryU

deriving instance (Era era, Arbitrary (PParamsHKD Identity era)) => Arbitrary (PParams era)

deriving instance (Era era, Arbitrary (PParamsHKD StrictMaybe era)) => Arbitrary (PParamsUpdate era)
