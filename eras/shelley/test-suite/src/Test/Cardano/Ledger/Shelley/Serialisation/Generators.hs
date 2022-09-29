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
import Cardano.Ledger.Shelley.API (ShelleyTxBody (ShelleyTxBody), ShelleyPParamsHKD)
import qualified Cardano.Ledger.Shelley.Rules as STS
import Generic.Random (genericArbitraryU)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.QuickCheck
  ( Arbitrary,
    arbitrary,
    shrink,
  )
import Control.Monad.Identity (Identity)
import Data.Maybe.Strict (StrictMaybe)

{-------------------------------------------------------------------------------
  ShelleyEra Generators

  These are generators for roundtrip tests, so the generated values are not
  necessarily valid
-------------------------------------------------------------------------------}

instance
  (EraTxOut era, Mock (EraCrypto era), Arbitrary (Value era), ToCBOR (PParamsUpdate era)) =>
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

instance Arbitrary (ShelleyPParamsHKD Identity era) where
  arbitrary = genericArbitraryU

instance Arbitrary (ShelleyPParamsHKD StrictMaybe era) where
  arbitrary = genericArbitraryU
