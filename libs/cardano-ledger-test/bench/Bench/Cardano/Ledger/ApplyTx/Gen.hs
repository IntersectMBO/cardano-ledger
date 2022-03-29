{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Generates fixed transaction/state pairs for benchmarking.
module Bench.Cardano.Ledger.ApplyTx.Gen (generateForEra) where

import Cardano.Ledger.Core (EraRule)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Shelley.API (Globals, ShelleyBasedEra)
import Cardano.Ledger.Shelley.LedgerState (LedgerState)
import Control.State.Transition (State)
import Control.State.Transition.Trace
import Control.State.Transition.Trace.Generator.QuickCheck
import Data.Default.Class (Default)
import Data.Proxy
import Test.Cardano.Ledger.AllegraEraGen ()
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen)
import Test.Cardano.Ledger.Shelley.Generator.Presets
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Generator.Trace.Ledger (mkGenesisLedgerState)
import Test.Cardano.Ledger.Shelley.Utils (testGlobals)
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)

-- | Generate a ledger state and transaction in a given era, given a seed.
generateForEra ::
  forall era.
  ( EraGen era,
    HasTrace (EraRule "LEDGER" era) (GenEnv era),
    Default (State (EraRule "PPUP" era)),
    BaseEnv (EraRule "LEDGER" era) ~ Globals,
    ShelleyBasedEra era
  ) =>
  Proxy era ->
  Int ->
  (LedgerState era, Core.Tx era)
generateForEra eraProxy seed =
  let ge = genEnv eraProxy
      qcSeed = mkQCGen seed
      tr =
        unGen
          ( traceFromInitState
              @(EraRule "LEDGER" era)
              testGlobals
              20
              ge
              (Just $ mkGenesisLedgerState ge)
          )
          qcSeed
          30
      sst = last $ sourceSignalTargets tr
   in (source sst, signal sst)
