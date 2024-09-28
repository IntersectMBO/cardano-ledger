{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Generates fixed transaction/state pairs for benchmarking.
module Bench.Cardano.Ledger.ApplyTx.Gen (generateApplyTxEnvForEra, ApplyTxEnv (..)) where

import Cardano.Ledger.Shelley.API (
  AccountState (..),
  Coin (..),
  Globals,
  LedgerEnv (..),
  MempoolEnv,
 )
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (LedgerState)
import Cardano.Ledger.Slot (SlotNo (SlotNo))
import Control.DeepSeq (NFData (..))
import Control.State.Transition (Environment, Signal, State)
import Data.Proxy (Proxy)
import GHC.Generics (Generic)
import Test.Cardano.Ledger.AllegraEraGen ()
import Test.Cardano.Ledger.Shelley.Constants (defaultConstants)
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv (..))
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen)
import Test.Cardano.Ledger.Shelley.Generator.Presets (genEnv)
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Generator.Trace.Ledger (mkGenesisLedgerState)
import Test.Cardano.Ledger.Shelley.Utils (testGlobals)
import Test.Control.State.Transition.Trace (
  SourceSignalTarget (signal, source),
  Trace (..),
  sourceSignalTargets,
 )
import Test.Control.State.Transition.Trace.Generator.QuickCheck (
  HasTrace (BaseEnv),
  traceFromInitState,
 )
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)

-- | Static mempool environment. We apply Txs in some future slot. The account
-- state shouldn't matter much.
applyTxMempoolEnv :: PParams era -> MempoolEnv era
applyTxMempoolEnv pp =
  LedgerEnv
    { ledgerSlotNo = SlotNo 0
    , ledgerIx = minBound
    , ledgerPp = pp
    , ledgerAccount = AccountState (Coin 45000000000) (Coin 45000000000)
    , ledgerMempool = False
    }

data ApplyTxEnv era = ApplyTxEnv
  { ateGlobals :: !Globals
  , ateMempoolEnv :: !(MempoolEnv era)
  , ateState :: !(LedgerState era)
  , ateTx :: !(Tx era)
  }
  deriving (Generic)

instance NFData (ApplyTxEnv era) where
  rnf (ApplyTxEnv g me s t) = seq g (seq me (seq s (seq t ())))

-- | Generate a ledger state and transaction in a given era, given a seed.
generateApplyTxEnvForEra ::
  forall era.
  ( EraGen era
  , HasTrace (EraRule "LEDGER" era) (GenEnv era)
  , BaseEnv (EraRule "LEDGER" era) ~ Globals
  , Signal (EraRule "LEDGER" era) ~ Tx era
  , Environment (EraRule "LEDGER" era) ~ LedgerEnv era
  , State (EraRule "LEDGER" era) ~ LedgerState era
  , EraGov era
  ) =>
  Proxy era ->
  Int ->
  ApplyTxEnv era
generateApplyTxEnvForEra eraProxy seed =
  let ge = genEnv eraProxy defaultConstants
      qcSeed = mkQCGen seed
      traceGen =
        traceFromInitState
          @(EraRule "LEDGER" era)
          testGlobals
          20
          ge
          (Just $ mkGenesisLedgerState ge)
      tr = unGen traceGen qcSeed 30
      sst = last $ sourceSignalTargets tr
   in ApplyTxEnv
        { ateGlobals = testGlobals
        , ateMempoolEnv = applyTxMempoolEnv (ledgerPp (_traceEnv tr))
        , ateState = source sst
        , ateTx = signal sst
        }
