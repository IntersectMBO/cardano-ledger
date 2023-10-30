{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Imp (
  spec,
) where

import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Shelley.Imp.EpochSpec as Epoch
import qualified Test.Cardano.Ledger.Shelley.Imp.LedgerSpec as Ledger
import Test.Cardano.Ledger.Shelley.ImpTest (ShelleyEraImp)

spec :: forall era. ShelleyEraImp era => Spec
spec =
  describe "ShelleyImpSpec" $ do
    Ledger.spec @era
    Epoch.spec @era
