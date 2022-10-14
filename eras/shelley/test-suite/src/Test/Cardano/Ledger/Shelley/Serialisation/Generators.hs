{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Shelley.Serialisation.Generators () where

import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.API (ShelleyTxBody (ShelleyTxBody))
import Cardano.Ledger.Shelley.PParams (ShelleyPParams)
import qualified Cardano.Ledger.Shelley.Rules as STS
import Generic.Random (genericArbitraryU)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.QuickCheck
  ( Arbitrary,
    arbitrary,
    shrink,
  )

{-------------------------------------------------------------------------------
  ShelleyEra Generators

  These are generators for roundtrip tests, so the generated values are not
  necessarily valid
-------------------------------------------------------------------------------}

instance
  (EraTxOut era, Mock (EraCrypto era), Arbitrary (Value era)) =>
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
  ( Era era,
    Mock (EraCrypto era),
    Arbitrary (Value era),
    Arbitrary (TxOut era),
    Arbitrary (STS.PredicateFailure (EraRule "PPUP" era))
  ) =>
  Arbitrary (STS.ShelleyUtxoPredFailure era)
  where
  arbitrary = genericArbitraryU
  shrink _ = []

-- | Note that this instance is a little off - it is an era-independent
-- generator for something which is only valid in certain eras. Its sole use is
-- for `ShelleyGenesis`, a somewhat confusing type which is in fact used as the
-- genesis for multiple eras.
instance Arbitrary (ShelleyPParams era) where
  arbitrary = genericArbitraryU
