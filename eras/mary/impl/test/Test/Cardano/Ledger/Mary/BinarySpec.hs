{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Mary.BinarySpec (spec) where

import Cardano.Ledger.Allegra.TxAuxData
import Cardano.Ledger.Mary
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Binary as UpgradeSpec
import Test.Cardano.Ledger.Mary.Arbitrary ()

spec :: Spec
spec =
  specUpgrade @Mary @AllegraTxAuxData @ShelleyTxWits
    @MaryTxBody
    @ShelleyTx
    True
