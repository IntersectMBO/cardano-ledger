{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Benchmarks for Shelley test generators.
module Cardano.Ledger.Shelley.Bench.Gen
  ( genTriple,
    genBlock,
    genChainState,
  )
where

import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto)
-- Use Another constraint, so this works in all Eras

import Cardano.Ledger.Shelley.API
  ( ApplyBlock,
    Block,
    DCert,
    DPState,
    DelplEnv,
    GetLedgerView,
    LEDGERS,
    Tx,
  )
import Cardano.Ledger.Shelley.LedgerState
  ( EpochState (..),
    LedgerState (..),
    NewEpochState (..),
  )
import Cardano.Ledger.Shelley.Tx (TxIn)
import Control.State.Transition.Extended
import qualified Control.State.Transition.Trace.Generator.QuickCheck as QC
import Data.Either (fromRight)
import qualified Data.Map as Map
import Data.Proxy
import Data.Set (Set)
import GHC.Records (HasField (..))
import Test.Cardano.Ledger.Shelley.BenchmarkFunctions (ledgerEnv)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import qualified Test.Cardano.Ledger.Shelley.Generator.Block as GenBlock
import Test.Cardano.Ledger.Shelley.Generator.Constants
  ( Constants
      ( maxGenesisUTxOouts,
        maxMinFeeA,
        minGenesisUTxOouts
      ),
  )
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv (..), ScriptSpace (..))
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen, MinLEDGER_STS)
import Test.Cardano.Ledger.Shelley.Generator.Presets (genEnv)
import Test.Cardano.Ledger.Shelley.Generator.Trace.Chain (mkGenesisChainState)
import Test.Cardano.Ledger.Shelley.Generator.Trace.DCert (CERTS)
import Test.Cardano.Ledger.Shelley.Generator.Utxo (genTx)
import Test.Cardano.Ledger.Shelley.Rules.Chain (ChainState (..))
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.Shelley.Utils (ShelleyTest)
import Test.QuickCheck (generate)

-- ===============================================================

-- | Generate a genesis chain state given a UTxO size
genChainState ::
  ( ShelleyTest era,
    EraGen era
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
      ge' = GenEnv (geKeySpace ge) (ScriptSpace [] [] Map.empty Map.empty) cs
   in fromRight (error "genChainState failed")
        <$> ( generate $
                mkGenesisChainState ge' (IRC ())
            )

-- | Benchmark generating a block given a chain state.
genBlock ::
  ( Mock (Crypto era),
    ShelleyTest era,
    EraGen era,
    MinLEDGER_STS era,
    GetLedgerView era,
    Core.EraRule "LEDGERS" era ~ LEDGERS era,
    QC.HasTrace (LEDGERS era) (GenEnv era),
    ApplyBlock era
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
  ( EraGen era,
    Mock (Crypto era),
    Embed (Core.EraRule "DELPL" era) (CERTS era),
    Environment (Core.EraRule "DELPL" era) ~ DelplEnv era,
    State (Core.EraRule "DELPL" era) ~ DPState (Crypto era),
    Signal (Core.EraRule "DELPL" era) ~ DCert (Crypto era),
    ShelleyTest era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era)))
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
