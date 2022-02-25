{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Benchmarks for transaction application
module Bench.Cardano.Ledger.ApplyTx (applyTxBenchmarks, ShelleyBench) where

import Bench.Cardano.Ledger.ApplyTx.Gen (generateForEra)
import Cardano.Binary
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Rules.Ledger ()
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era (Crypto))
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API
  ( AccountState (..),
    ApplyTx,
    Coin (..),
    Globals,
    LedgerEnv (..),
    MempoolEnv,
    MempoolState,
    ShelleyBasedEra,
    applyTxsTransition,
  )
import Cardano.Ledger.Shelley.LedgerState (DPState, UTxOState)
import Cardano.Ledger.Shelley.PParams (PParams' (..))
import Cardano.Ledger.Slot (SlotNo (SlotNo))
import Control.DeepSeq (NFData (..))
import Control.State.Transition (State)
import Control.State.Transition.Trace.Generator.QuickCheck (BaseEnv, HasTrace)
import Criterion
import Data.Default.Class (Default, def)
import Data.Proxy (Proxy (..))
import qualified Data.Sequence as Seq
import Data.Sharing (fromNotSharedCBOR)
import Data.Typeable (typeRep)
import GHC.Generics (Generic)
import Test.Cardano.Ledger.Alonzo.AlonzoEraGen ()
import Test.Cardano.Ledger.MaryEraGen ()
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (C_Crypto)
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen)
import Test.Cardano.Ledger.Shelley.Utils (testGlobals)

type ShelleyBench = ShelleyEra C_Crypto

type AllegraBench = AllegraEra C_Crypto

type MaryBench = MaryEra C_Crypto

type AlonzoBench = AlonzoEra C_Crypto

--------------------------------------------------------------------------------
-- Applying a Shelley transaction in multiple eras.
--
-- This benchmark starts with a fixed Shelley transaction. We decode it in the
-- correct transaction format for subsequent eras, and benchmark applying it to
-- a given ledger state (also translated for each era.)
--------------------------------------------------------------------------------

-- | Static mempool environment. We apply Txs in some future slot. The account
-- state shouldn't matter much.
applyTxMempoolEnv :: Default (Core.PParams era) => MempoolEnv era
applyTxMempoolEnv =
  LedgerEnv
    { ledgerSlotNo = SlotNo 0,
      ledgerIx = minBound,
      ledgerPp = def,
      ledgerAccount = AccountState (Coin 45000000000) (Coin 45000000000)
    }

data ApplyTxRes era = ApplyTxRes
  { atrGlobals :: Globals,
    atrMempoolEnv :: MempoolEnv era,
    atrState :: MempoolState era,
    atrTx :: Core.Tx era
  }
  deriving (Generic)

instance NFData (ApplyTxRes era) where
  rnf (ApplyTxRes g me s t) = seq g (seq me (seq s (seq t ())))

--------------------------------------------------------------------------------
-- Fixed generators
--------------------------------------------------------------------------------

benchmarkSeed :: Int
benchmarkSeed = 24601

benchWithGenState ::
  ( NFData a,
    EraGen era,
    HasTrace (Core.EraRule "LEDGER" era) (GenEnv era),
    ShelleyBasedEra era,
    Default (State (Core.EraRule "PPUP" era)),
    BaseEnv (Core.EraRule "LEDGER" era) ~ Globals
  ) =>
  Proxy era ->
  (((UTxOState era, DPState (Crypto era)), Core.Tx era) -> IO a) ->
  (a -> Benchmarkable) ->
  Benchmark
benchWithGenState px prepEnv mkBench =
  env (prepEnv $ generateForEra px benchmarkSeed) $ bench (show $ typeRep px) . mkBench

benchApplyTx ::
  forall era.
  ( Era era,
    EraGen era,
    ApplyTx era,
    ShelleyBasedEra era,
    HasTrace (Core.EraRule "LEDGER" era) (GenEnv era),
    BaseEnv (Core.EraRule "LEDGER" era) ~ Globals,
    Default (State (Core.EraRule "PPUP" era)),
    NFData (State (Core.EraRule "PPUP" era)),
    NFData (Core.PParams era),
    NFData (Core.TxOut era),
    NFData (Core.Value era)
  ) =>
  Proxy era ->
  Benchmark
benchApplyTx px = benchWithGenState px genRes $ \txRes ->
  let ApplyTxRes {atrGlobals, atrMempoolEnv, atrState, atrTx} = txRes
   in nf
        ( either (error . show) id
            . applyTxsTransition atrGlobals atrMempoolEnv (Seq.singleton atrTx)
        )
        atrState
  where
    genRes (state, tx) = pure $ ApplyTxRes testGlobals applyTxMempoolEnv state tx

--------------------------------------------------------------------------------
-- Deserialising resources
--------------------------------------------------------------------------------

-- | Benchmark deserialising a shelley transaction as if it comes from the given
-- era.
deserialiseTxEra ::
  forall era.
  ( EraGen era,
    ShelleyBasedEra era,
    Default (State (Core.EraRule "PPUP" era)),
    BaseEnv (Core.EraRule "LEDGER" era) ~ Globals,
    HasTrace (Core.EraRule "LEDGER" era) (GenEnv era),
    NFData (Core.Tx era)
  ) =>
  Proxy era ->
  Benchmark
deserialiseTxEra px =
  benchWithGenState px (pure . serialize . snd) $
    nf (either (error . show) (id @((Core.Tx era))) . decodeAnnotator "tx" fromCBOR)

--------------------------------------------------------------------------------
-- Benchmark suite
--------------------------------------------------------------------------------

applyTxBenchmarks :: Benchmark
applyTxBenchmarks =
  bgroup
    "applyTxBenchmarks"
    [ bgroup
        "ApplyTxInEra"
        [ benchApplyTx (Proxy @ShelleyBench),
          benchApplyTx (Proxy @AllegraBench),
          benchApplyTx (Proxy @MaryBench)
          -- benchApplyTx (Proxy @AlonzoBench)
        ],
      bgroup
        "Deserialise Shelley Tx"
        [ deserialiseTxEra (Proxy @ShelleyBench),
          deserialiseTxEra (Proxy @AllegraBench),
          deserialiseTxEra (Proxy @MaryBench)
          --deserialiseTxEra (Proxy @AlonzoBench)
        ]
    ]

instance FromCBOR (UTxOState ShelleyBench) where
  fromCBOR = fromNotSharedCBOR

instance FromCBOR (UTxOState AllegraBench) where
  fromCBOR = fromNotSharedCBOR

instance FromCBOR (UTxOState MaryBench) where
  fromCBOR = fromNotSharedCBOR

instance FromCBOR (UTxOState AlonzoBench) where
  fromCBOR = fromNotSharedCBOR

instance FromCBOR (DPState C_Crypto) where
  fromCBOR = fromNotSharedCBOR
