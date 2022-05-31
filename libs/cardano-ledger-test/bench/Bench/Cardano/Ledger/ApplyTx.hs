{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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

import Bench.Cardano.Ledger.ApplyTx.Gen (ApplyTxEnv (..), generateApplyTxEnvForEra)
import Cardano.Binary (FromCBOR (fromCBOR), decodeAnnotator, serialize)
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.PParams (PParams' (..))
import Cardano.Ledger.Alonzo.Rules.Ledger ()
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API
  ( ApplyTx,
    Globals,
    ShelleyBasedEra,
    applyTxsTransition,
  )
import Cardano.Ledger.Shelley.LedgerState (DPState, UTxOState)
import Cardano.Ledger.Shelley.PParams (PParams' (..))
import Control.DeepSeq (NFData (..))
import Control.State.Transition (State)
import Control.State.Transition.Trace.Generator.QuickCheck (BaseEnv, HasTrace)
import Criterion
import Data.Default.Class (Default)
import Data.Proxy (Proxy (..))
import qualified Data.Sequence as Seq
import Data.Sharing (fromNotSharedCBOR)
import Data.Typeable (typeRep)
import Test.Cardano.Ledger.Alonzo.AlonzoEraGen ()
import Test.Cardano.Ledger.Alonzo.Trace ()
import Test.Cardano.Ledger.MaryEraGen ()
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (C_Crypto)
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen)

type ShelleyBench = ShelleyEra C_Crypto

type AllegraBench = AllegraEra C_Crypto

type MaryBench = MaryEra C_Crypto

type AlonzoBench = AlonzoEra C_Crypto

--------------------------------------------------------------------------------
-- Applying a Shelley transaction in multiple eras.
--
-- This benchmark starts by generating a fixed Shelley transaction from a fixed
-- seed.
--------------------------------------------------------------------------------

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
  (ApplyTxEnv era -> IO a) ->
  (a -> Benchmarkable) ->
  Benchmark
benchWithGenState px prepEnv mkBench =
  env (prepEnv $ generateApplyTxEnvForEra px benchmarkSeed) $ bench (show $ typeRep px) . mkBench

benchApplyTx ::
  forall era.
  ( EraGen era,
    ApplyTx era,
    ShelleyBasedEra era,
    HasTrace (Core.EraRule "LEDGER" era) (GenEnv era),
    BaseEnv (Core.EraRule "LEDGER" era) ~ Globals,
    Default (State (Core.EraRule "PPUP" era)),
    NFData (State (Core.EraRule "PPUP" era)),
    NFData (Core.TxOut era)
  ) =>
  Proxy era ->
  Benchmark
benchApplyTx px =
  benchWithGenState px pure $ \applyTxEnv ->
    let ApplyTxEnv {ateGlobals, ateMempoolEnv, ateState, ateTx} = applyTxEnv
     in nf
          ( either (error . show) id
              . applyTxsTransition ateGlobals ateMempoolEnv (Seq.singleton ateTx)
          )
          ateState

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
  benchWithGenState px (pure . serialize . ateTx) $
    nf (either (error . show) (id @(Core.Tx era)) . decodeAnnotator "tx" fromCBOR)

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
          benchApplyTx (Proxy @MaryBench),
          benchApplyTx (Proxy @AlonzoBench)
        ],
      bgroup
        "Deserialise Shelley Tx"
        [ deserialiseTxEra (Proxy @ShelleyBench),
          deserialiseTxEra (Proxy @AllegraBench),
          deserialiseTxEra (Proxy @MaryBench),
          deserialiseTxEra (Proxy @AlonzoBench)
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
