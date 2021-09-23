{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Benchmarks for transaction application
module Bench.Cardano.Ledger.ApplyTx (applyTxBenchmarks) where

import Cardano.Binary
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era, ValidateScript)
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
    Tx,
    applyTxsTransition,
  )
import Cardano.Ledger.Slot (SlotNo (SlotNo))
import Control.DeepSeq (NFData (..))
import Criterion
import qualified Data.ByteString.Lazy as BSL
import Data.Default.Class (Default, def)
import Data.Proxy (Proxy (..))
import qualified Data.Sequence as Seq
import Data.Typeable (typeRep)
import GHC.Generics (Generic)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (C_Crypto)
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
    { ledgerSlotNo = SlotNo 71,
      ledgerIx = 0,
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

resource_n_ledgerstate :: Int -> FilePath
resource_n_ledgerstate n = "bench/resources/" <> show n <> "_ledgerstate.cbor"

resource_n_tx :: Int -> FilePath
resource_n_tx n = "bench/resources/" <> show n <> "_tx.cbor"

-- | Apply the transaction as if it's a transaction from a given era.
applyTxEra ::
  forall era.
  ( Era era,
    ApplyTx era,
    Default (Core.PParams era),
    FromCBOR (MempoolState era)
  ) =>
  Proxy era ->
  FilePath ->
  FilePath ->
  Benchmark
applyTxEra p lsFile txFile = env loadRes go
  where
    loadRes :: IO (ApplyTxRes era)
    loadRes = do
      state <-
        either (\err -> error $ "Failed to decode state: " <> show err) id
          . decodeFullDecoder "state" fromCBOR
          <$> BSL.readFile lsFile
      tx <-
        either (\err -> error $ "Failed to decode tx: " <> show err) id
          . decodeAnnotator "tx" fromCBOR
          <$> BSL.readFile txFile
      pure $! ApplyTxRes testGlobals applyTxMempoolEnv state tx
    go :: ApplyTxRes era -> Benchmark
    go ~ApplyTxRes {atrGlobals, atrMempoolEnv, atrState, atrTx} =
      bench (show $ typeRep p) $
        whnf
          ( either (error . show) id
              . applyTxsTransition @era @(Either _)
                atrGlobals
                atrMempoolEnv
                (Seq.singleton atrTx)
          )
          atrState

applyTxGroup :: Benchmark
applyTxGroup =
  bgroup
    "Apply Shelley Tx"
    [ withRes 0,
      withRes 1
    ]
  where
    withRes n =
      let ls = resource_n_ledgerstate n
          tx = resource_n_tx n
       in bgroup
            (show n)
            [ applyTxEra (Proxy @ShelleyBench) ls tx,
              applyTxEra (Proxy @AllegraBench) ls tx,
              applyTxEra (Proxy @MaryBench) ls tx
            ]

-- | Benchmark deserialising a shelley transaction as if it comes from the given
-- era.
deserialiseTxEra ::
  forall era.
  ( Era era,
    ValidateScript era,
    FromCBOR (Annotator (Core.TxBody era)),
    FromCBOR (Annotator (Core.AuxiliaryData era)),
    FromCBOR (Annotator (Core.Witnesses era))
  ) =>
  Proxy era ->
  Benchmark
deserialiseTxEra p =
  bench (show $ typeRep p) $
    whnfIO $
      either (\err -> error $ "Failed to decode tx: " <> show err) (id @(Tx era))
        . decodeAnnotator "tx" fromCBOR
        <$> BSL.readFile (resource_n_tx 0)

applyTxBenchmarks :: Benchmark
applyTxBenchmarks =
  bgroup
    "applyTxBenchmarks"
    [ applyTxGroup,
      bgroup
        "Deserialise Shelley Tx"
        [ deserialiseTxEra (Proxy @ShelleyBench),
          deserialiseTxEra (Proxy @AllegraBench),
          deserialiseTxEra (Proxy @MaryBench),
          deserialiseTxEra (Proxy @AlonzoBench)
        ]
    ]
