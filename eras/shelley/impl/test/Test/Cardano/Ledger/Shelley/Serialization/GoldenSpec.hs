{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Serialization.GoldenSpec (spec) where

import Cardano.Ledger.Binary.Plain
import Cardano.Ledger.Shelley
import Cardano.Ledger.Shelley.LedgerState
import Test.Cardano.Ledger.Binary.Plain.Golden
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Shelley.Arbitrary ()

spec :: Spec
spec =
  describe "Golden" $ do
    prop "NewEpochState" $ \(nes@NewEpochState {..} :: NewEpochState Shelley) ->
      expectGoldenEncCBOR nes $
        mconcat
          [ E (TkListLen 7)
          , E nesEL
          , E nesBprev
          , E nesBcur
          , E nesEs
          , E nesRu
          , E nesPd
          , E stashedAVVMAddresses
          ]
