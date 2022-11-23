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
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.PParams (AlonzoPParamsHKD (..))
import Cardano.Ledger.Alonzo.Rules ()
import Cardano.Ledger.Binary
  ( FromCBOR (fromCBOR),
    decodeFullAnnotator,
    fromNotSharedCBOR,
    serialize,
  )
import Cardano.Ledger.Core
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API
  ( ApplyTx,
    Globals,
    LedgerEnv,
    LedgerState,
    applyTxsTransition,
  )
import Cardano.Ledger.Shelley.LedgerState (DPState, UTxOState)
import Cardano.Ledger.Shelley.PParams (ShelleyPParamsHKD (..))
import Control.DeepSeq (NFData (..))
import Control.State.Transition (Environment, Signal, State)
import Control.State.Transition.Trace.Generator.QuickCheck (BaseEnv, HasTrace)
import Criterion
import Data.Default.Class (Default)
import Data.Proxy (Proxy (..))
import qualified Data.Sequence as Seq
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
    HasTrace (EraRule "LEDGER" era) (GenEnv era),
    Default (State (EraRule "PPUP" era)),
    BaseEnv (EraRule "LEDGER" era) ~ Globals,
    Signal (EraRule "LEDGER" era) ~ Tx era,
    Environment (EraRule "LEDGER" era) ~ LedgerEnv era,
    State (EraRule "LEDGER" era) ~ LedgerState era
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
    HasTrace (EraRule "LEDGER" era) (GenEnv era),
    BaseEnv (EraRule "LEDGER" era) ~ Globals,
    Default (State (EraRule "PPUP" era)),
    NFData (State (EraRule "PPUP" era))
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
    Default (State (EraRule "PPUP" era)),
    BaseEnv (EraRule "LEDGER" era) ~ Globals,
    HasTrace (EraRule "LEDGER" era) (GenEnv era),
    State (EraRule "LEDGER" era) ~ LedgerState era,
    Environment (EraRule "LEDGER" era) ~ LedgerEnv era,
    Signal (EraRule "LEDGER" era) ~ Tx era,
    NFData (Tx era)
  ) =>
  Proxy era ->
  Benchmark
deserialiseTxEra px =
  benchWithGenState px (pure . serialize v . ateTx) $
    nf (either (error . show) (id @(Tx era)) . decodeFullAnnotator v "tx" fromCBOR)
  where
    v = eraProtVerHigh @era

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
