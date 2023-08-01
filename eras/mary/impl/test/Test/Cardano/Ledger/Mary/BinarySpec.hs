{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Mary.BinarySpec (spec) where

import Cardano.Ledger.Mary
import Test.Cardano.Ledger.Mary.Arbitrary ()
import Cardano.Ledger.Allegra.TxAuxData
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Binary as UpgradeSpec

spec :: Spec
spec = specUpgrade @Mary @AllegraTxAuxData True
