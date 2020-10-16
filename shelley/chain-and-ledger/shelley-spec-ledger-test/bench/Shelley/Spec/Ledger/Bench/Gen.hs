{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Benchmarks for Shelley test generators.
module Shelley.Spec.Ledger.Bench.Gen
  ( genTriple,
    genBlock,
    genChainState,
  )
where

import Cardano.Ledger.Era (Crypto)
import Control.State.Transition.Extended
import Data.Either (fromRight)
import Data.Proxy
import Data.Sequence (Seq)
import Shelley.Spec.Ledger.API
  ( Block,
    ChainState (..),
    Tx,
  )
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase)
import Shelley.Spec.Ledger.LedgerState
  ( DPState (..),
    EpochState (..),
    LedgerState (..),
    NewEpochState (..),
    UTxOState (..),
  )
import Shelley.Spec.Ledger.STS.Ledger (LEDGER, LedgerEnv)
import Shelley.Spec.Ledger.STS.Ledgers (LEDGERS, LedgersEnv)
import Test.QuickCheck (generate)
import Test.Shelley.Spec.Ledger.BenchmarkFunctions (ledgerEnv)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (Mock)
import qualified Test.Shelley.Spec.Ledger.Generator.Block as GenBlock
import Test.Shelley.Spec.Ledger.Generator.Constants
  ( Constants
      ( maxGenesisUTxOouts,
        maxMinFeeA,
        minGenesisUTxOouts
      ),
  )
import Test.Shelley.Spec.Ledger.Generator.Core (GenEnv, geConstants)
import Test.Shelley.Spec.Ledger.Generator.Presets (genEnv)
import Test.Shelley.Spec.Ledger.Generator.Trace.Chain (mkGenesisChainState)
import Test.Shelley.Spec.Ledger.Generator.Utxo (genTx)
import Test.Shelley.Spec.Ledger.Serialisation.Generators ()
import Test.Shelley.Spec.Ledger.Utils (ShelleyTest)

-- =============================================================================

-- | Generate a genesis chain state given a UTxO size
genChainState ::
  ( ShelleyTest era
  ) =>
  Int ->
  GenEnv era ->
  IO (ChainState era)
genChainState n ge =
  let cs =
        (geConstants ge)
          { minGenesisUTxOouts = n,
            maxGenesisUTxOouts = n,
            -- We are using real crypto types here, which can be larger than
            -- those expected by the mock fee calculations. Since this is
            -- unimportant for now, we set the A part of the fee to 0
            maxMinFeeA = 0
          }
   in fromRight (error "genChainState failed")
        <$> ( generate $
                mkGenesisChainState cs (IRC ())
            )

-- | Benchmark generating a block given a chain state.
genBlock ::
  ( Mock (Crypto era),
    ShelleyTest era,
    STS (LEDGERS era),
    BaseM (LEDGERS era) ~ ShelleyBase,
    Environment (LEDGERS era) ~ LedgersEnv era,
    State (LEDGERS era) ~ LedgerState era,
    Signal (LEDGERS era) ~ Seq (Tx era),
    STS (LEDGER era),
    Environment (LEDGER era) ~ LedgerEnv era,
    State (LEDGER era) ~ (UTxOState era, DPState era),
    Signal (LEDGER era) ~ Tx era
  ) =>
  GenEnv era ->
  ChainState era ->
  IO (Block era)
genBlock ge cs = generate $ GenBlock.genBlock ge cs

-- The order one does this is important, since all these things must flow from the same
-- GenEnv, so that the addresses ind signatures in the UTxO are known and consistent.
-- 1) genEnv from a (Proxy era)
-- 2) genChainState from a GenEnv
-- 3) get a UTxOState from the ChainState
-- 4) get a DPState from the ChainState
-- 5) get a Transaction (Tx) from GenEnv and ChainState

genTriple ::
  ( Mock (Crypto era),
    ShelleyTest era
  ) =>
  Proxy era ->
  Int ->
  IO (GenEnv era, ChainState era, GenEnv era -> IO (Tx era))
genTriple proxy n = do
  let ge = genEnv proxy
  cs <- genChainState n ge
  let nes = chainNes cs -- NewEpochState
  let es = nesEs nes -- EpochState
  let (LedgerState utxoS dpstate) = esLState es -- LedgerState
  let fun genenv = generate $ genTx genenv ledgerEnv (utxoS, dpstate)
  pure (ge, cs, fun)
