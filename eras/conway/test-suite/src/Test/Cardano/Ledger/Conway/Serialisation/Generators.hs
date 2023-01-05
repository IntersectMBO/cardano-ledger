{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.Serialisation.Generators () where

import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Crypto (Crypto)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import Test.QuickCheck (Arbitrary (..))

instance Crypto c => Arbitrary (ConwayGenesis c) where
  arbitrary = ConwayGenesis <$> arbitrary <*> arbitrary
