{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.BinarySpec (spec) where

import Cardano.Ledger.Conway
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Core.Binary as UpgradeSpec

spec :: Spec
spec = specUpgrade @Conway True
