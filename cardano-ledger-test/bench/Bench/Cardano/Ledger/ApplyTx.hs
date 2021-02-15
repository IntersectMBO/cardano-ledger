{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

-- | Benchmarks for transaction application
module Bench.Cardano.Ledger.ApplyTx (applyTxBenchmarks) where

import Cardano.Binary
import Cardano.Ledger.Allegra (AllegraEra)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import Control.DeepSeq (NFData (..))
import Criterion
import qualified Data.ByteString.Lazy as BSL
import Data.Default.Class (Default, def)
import Data.Proxy (Proxy (..))
import qualified Data.Sequence as Seq
import Data.Typeable (typeRep)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.API
  ( AccountState (..),
    ApplyTx,
    Coin (..),
    Globals,
    LedgersEnv (..),
    MempoolEnv,
    MempoolState,
    Tx,
    applyTxsTransition,
  )
import Shelley.Spec.Ledger.PParams (PParams' (..))
import Shelley.Spec.Ledger.Slot (SlotNo (SlotNo))
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C_Crypto)
import Test.Shelley.Spec.Ledger.Utils (testGlobals)

type ShelleyBench = ShelleyEra C_Crypto

type AllegraBench = AllegraEra C_Crypto

type MaryBench = MaryEra C_Crypto

--------------------------------------------------------------------------------
-- Applying a Shelley transaction in multiple eras.
--
-- This benchmark starts with a fixed Shelley transaction. We decode it in the
-- correct transaction format for subsequent eras, and benchmark applying it to
-- a given ledger state (also translated for each era.)
-----------------------------------f---------------------------------------------

-- | Static mempool environment. We apply Txs in some future slot. The account
-- state shouldn't matter much.
applyTxMempoolEnv :: Default (Core.PParams era) => MempoolEnv era
applyTxMempoolEnv =
  LedgersEnv
    { ledgersSlotNo = SlotNo 100,
      ledgersPp = def,
      ledgersAccount = AccountState (Coin 45000000000) (Coin 45000000000)
    }

data ApplyTxRes era = ApplyTxRes
  { atrGlobals :: Globals,
    atrMempoolEnv :: MempoolEnv era,
    atrState :: MempoolState era,
    atrTx :: Tx era
  }
  deriving (Generic)

instance NFData (ApplyTxRes era) where
  rnf (ApplyTxRes g me s t) = seq g (seq me (seq s (seq t ())))

resource_0_ledgerstate :: FilePath
resource_0_ledgerstate = "bench/resources/0_ledgerstate.cbor"

resource_0_tx :: FilePath
resource_0_tx = "bench/resources/0_tx.cbor"

applyTxEra ::
  forall era.
  ( Era era,
    ApplyTx era,
    Default (Core.PParams era),
    FromCBOR (MempoolState era)
  ) =>
  Proxy era ->
  Benchmark
applyTxEra p = env loadRes go
  where
    loadRes :: IO (ApplyTxRes era)
    loadRes = do
      state <- unsafeDeserialize <$> BSL.readFile resource_0_ledgerstate
      tx <-
        either (\err -> error $ "Failed to decode tx: " <> show err) id
          . decodeAnnotator "tx" fromCBOR
          <$> BSL.readFile resource_0_tx
      pure $ ApplyTxRes testGlobals applyTxMempoolEnv state tx
    go :: ApplyTxRes era -> Benchmark
    go ~ApplyTxRes {atrGlobals, atrMempoolEnv, atrState, atrTx} =
      bench (show $ typeRep p) $
        whnf
          ( applyTxsTransition @era @(Either _)
              atrGlobals
              atrMempoolEnv
              (Seq.singleton atrTx)
          )
          atrState

applyTxBenchmarks :: Benchmark
applyTxBenchmarks =
  bgroup
    "applyTxBenchmarks"
    [ bgroup
        "Shelley Tx"
        [ applyTxEra (Proxy @ShelleyBench),
          applyTxEra (Proxy @AllegraBench),
          applyTxEra (Proxy @MaryBench)
        ]
    ]
