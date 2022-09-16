{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.Serialisation.Generators () where

import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Crypto (Crypto)
import Test.Cardano.Ledger.Babbage.Serialisation.Generators ()
import Test.QuickCheck (Arbitrary (..))

deriving instance Crypto c => Arbitrary (ConwayGenesis c)
