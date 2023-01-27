{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Binary.Golden (
  goldenNewEpochStateExpectation,
) where

import Cardano.Ledger.Binary.Plain
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState
import Test.Cardano.Ledger.Binary.Plain.Golden
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Shelley.Arbitrary ()

goldenNewEpochStateExpectation ::
  ( EraTxOut era
  , EncCBOR (StashedAVVMAddresses era)
  , EncCBOR (PPUPState era)
  , EncCBOR (TallyState era)
  ) =>
  NewEpochState era ->
  Expectation
goldenNewEpochStateExpectation nes@NewEpochState {..} = do
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
