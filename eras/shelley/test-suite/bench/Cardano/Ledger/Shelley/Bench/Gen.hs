{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Benchmarks for Shelley test generators.
module Cardano.Ledger.Shelley.Bench.Gen (
  genTriple,
  genBlock,
  genChainState,
)
where

import Cardano.Ledger.Coin
import Cardano.Ledger.Shelley.API (
  ApplyBlock,
  Block,
  CertState,
  DelplEnv,
  ShelleyLEDGERS,
  ShelleyTx,
 )
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  EpochState (..),
  NewEpochState (..),
 )
import Cardano.Ledger.UTxO (EraUTxO)
import Cardano.Protocol.TPraos.API (GetLedgerView)
import Cardano.Protocol.TPraos.BHeader (BHeader)
import Control.State.Transition.Extended
import Data.Either (fromRight)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Test.Cardano.Ledger.Shelley.BenchmarkFunctions (ledgerEnv)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import Test.Cardano.Ledger.Shelley.Constants (
  Constants (
    maxGenesisUTxOouts,
    maxMinFeeA,
    minGenesisUTxOouts
  ),
  defaultConstants,
 )
import qualified Test.Cardano.Ledger.Shelley.Generator.Block as GenBlock
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv (..), ScriptSpace (..))
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen, MinLEDGER_STS)
import Test.Cardano.Ledger.Shelley.Generator.Presets (genEnv)
import Test.Cardano.Ledger.Shelley.Generator.Trace.Chain (mkGenesisChainState)
import Test.Cardano.Ledger.Shelley.Generator.Trace.TxCert (CERTS)
import Test.Cardano.Ledger.Shelley.Generator.Utxo (genTx)
import Test.Cardano.Ledger.Shelley.Rules.Chain (ChainState (..))
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import qualified Test.Control.State.Transition.Trace.Generator.QuickCheck as QC
import Test.QuickCheck (generate)

-- ===============================================================

-- | Generate a genesis chain state given a UTxO size
genChainState ::
  ( EraGen era
  , EraGov era
  ) =>
  Int ->
  GenEnv era ->
  IO (ChainState era)
genChainState n ge =
  let cs =
        (geConstants ge)
          { minGenesisUTxOouts = n
          , maxGenesisUTxOouts = n
          , -- We are using real crypto types here, which can be larger than
            -- those expected by the mock fee calculations. Since this is
            -- unimportant for now, we set the A part of the fee to 0
            maxMinFeeA = Coin 0
          }
      ge' = GenEnv (geKeySpace ge) (ScriptSpace [] [] Map.empty Map.empty) cs
   in fromRight (error "genChainState failed")
        <$> ( generate $
                mkGenesisChainState ge' (IRC ())
            )

-- | Benchmark generating a block given a chain state.
genBlock ::
  ( Mock (EraCrypto era)
  , EraGen era
  , MinLEDGER_STS era
  , GetLedgerView era
  , EraRule "LEDGERS" era ~ ShelleyLEDGERS era
  , QC.HasTrace (ShelleyLEDGERS era) (GenEnv era)
  , ApplyBlock era
  ) =>
  GenEnv era ->
  ChainState era ->
  IO (Block (BHeader (EraCrypto era)) era)
genBlock ge cs = generate $ GenBlock.genBlock ge cs

-- The order one does this is important, since all these things must flow from the same
-- GenEnv, so that the addresses ind signatures in the UTxO are known and consistent.
-- 1) genEnv from a (Proxy era)
-- 2) genChainState from a GenEnv
-- 3) get a UTxOState from the ChainState
-- 4) get a CertState from the ChainState
-- 5) get a Transaction (Tx) from GenEnv and ChainState

genTriple ::
  ( EraGen era
  , EraUTxO era
  , Mock (EraCrypto era)
  , Embed (EraRule "DELPL" era) (CERTS era)
  , Environment (EraRule "DELPL" era) ~ DelplEnv era
  , State (EraRule "DELPL" era) ~ CertState era
  , Signal (EraRule "DELPL" era) ~ TxCert era
  , Tx era ~ ShelleyTx era
  , EraGov era
  , ProtVerAtMost era 4
  , ProtVerAtMost era 6
  ) =>
  Proxy era ->
  Int ->
  IO (GenEnv era, ChainState era, GenEnv era -> IO (ShelleyTx era))
genTriple proxy n = do
  let ge = genEnv proxy defaultConstants
  cs <- genChainState n ge
  let fun genenv = generate $ genTx genenv ledgerEnv (esLState (nesEs (chainNes cs)))
  pure (ge, cs, fun)
