{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Alonzo.Serialisation.Generators where

import Cardano.Ledger.Alonzo.Data (Data (..))
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Alonzo.TxWitness
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Era (Crypto)
import Cardano.Ledger.Shelley.Constraints (ShelleyBased)
import Cardano.Ledger.ShelleyMA.Timelocks
import Cardano.Slotting.Slot (SlotNo (..))
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
