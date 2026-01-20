module Test.Cardano.Ledger.Conway.Scls (
  spec,
) where

import Cardano.Ledger.Conway.SCLS.Arbitrary ()
import Cardano.SCLS.Testlib
import Test.Cardano.Ledger.Common

spec :: Spec
spec = do
  describe "SCLS" testAllNS
