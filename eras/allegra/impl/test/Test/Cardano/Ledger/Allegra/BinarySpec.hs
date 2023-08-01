{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Allegra.BinarySpec (spec) where

import Cardano.Ledger.Allegra
import Cardano.Ledger.Allegra.TxAuxData
import Test.Cardano.Ledger.Allegra.Arbitrary ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Binary as UpgradeSpec

spec :: Spec
spec = specUpgrade @Allegra @AllegraTxAuxData True
