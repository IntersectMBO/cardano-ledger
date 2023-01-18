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

import Cardano.Binary (ToCBOR)
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Shelley.API (ShelleyTxBody (ShelleyTxBody))
import Cardano.Ledger.Shelley.PParams (ShelleyPParams)
import qualified Cardano.Ledger.Shelley.Rules.Utxo as STS
import Generic.Random (genericArbitraryU)
-- import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
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
  (EraTxOut era, CC.Crypto (Crypto era), Arbitrary (Value era), ToCBOR (PParamsUpdate era)) =>
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
    CC.Crypto (Crypto era),
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
