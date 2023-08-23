{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.BinarySpec (spec) where

import Cardano.Ledger.Alonzo
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Binary as UpgradeSpec

spec :: Spec
spec =
  -- Scripts are not upgradeable from Mary through their CBOR instances, since Mary had no
  -- concept of a prefix.
  specUpgrade @Alonzo False
