{-# LANGUAGE TypeApplications #-}

-- | Benchmarks for Shelley test generators.
module Shelley.Spec.Ledger.Bench.Gen where

import Control.State.Transition.Extended (IRC (..))
import Data.Either (fromRight)
import Data.Proxy (Proxy (..))
import Shelley.Spec.Ledger.API
  ( Block,
    ChainState,
    Tx,
  )
import Shelley.Spec.Ledger.LedgerState
  ( emptyDPState,
  )
import Test.QuickCheck (generate)
import Test.Shelley.Spec.Ledger.BenchmarkFunctions
  ( B,
    initUTxO,
    ledgerEnv,
  )
import qualified Test.Shelley.Spec.Ledger.Generator.Block as GenBlock
import Test.Shelley.Spec.Ledger.Generator.Constants
  ( Constants
      ( maxGenesisUTxOouts,
        maxMinFeeA,
        minGenesisUTxOouts
      ),
  )
import Test.Shelley.Spec.Ledger.Generator.Core (geConstants)
import Test.Shelley.Spec.Ledger.Generator.Presets (genEnv)
import Test.Shelley.Spec.Ledger.Generator.Trace.Chain (mkGenesisChainState)
import qualified Test.Shelley.Spec.Ledger.Generator.Utxo as GenUTxO

-- | Benchmark generating transaction given a UTxO size.
genTx :: Integer -> IO (Tx B)
genTx n =
  let st = (initUTxO n, emptyDPState)
      ge = genEnv (Proxy @B)
   in generate $ GenUTxO.genTx ge ledgerEnv st

-- | Generate a genesis chain state given a UTxO size
genChainState :: Int -> IO (ChainState B)
genChainState n =
  let ge = genEnv (Proxy @B)
      cs =
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
genBlock :: ChainState B -> IO (Block B)
genBlock cs =
  let ge = genEnv (Proxy @B)
   in generate $ GenBlock.genBlock ge cs
