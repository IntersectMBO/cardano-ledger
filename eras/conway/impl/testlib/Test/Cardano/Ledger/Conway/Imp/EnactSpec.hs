{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.Imp.EnactSpec (spec) where

-- import Cardano.Ledger.Conway.Governance
-- import Cardano.Ledger.Core
-- import Cardano.Ledger.Shelley.LedgerState
-- import Lens.Micro.Mtl
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Shelley.ImpTest

spec :: forall era. EraImpTest era => Spec
spec =
  describe "ENACT" $ do
    itM @era "TreasuryWithdrawal" $ do
      -- enactState <~ impNESL . newEpochStateGovStateL . rsEnactStateL
      pure ()
