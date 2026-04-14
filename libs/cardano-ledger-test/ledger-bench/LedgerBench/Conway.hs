{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

-- | Conway era benchmark environment generation using the constrained-generators.
--
-- == What this module does
--
-- 'generateConwayApplyTxEnv' produces everything needed for one round of
-- benchmarking: a concrete Conway transaction and the ledger state it is valid
-- against.  The result is passed to 'applyTx' in a tight loop by the
-- benchmark harness.
--
-- == Why constrained-generators?
--
-- Conway does not have an 'EraGen' instance (the QuickCheck trace generator
-- used for Shelley–Alonzo).  Instead it uses the constrained-generators
-- framework ('runGenRS' + 'genAlonzoTx'), which is the same infrastructure
-- that drives the Conway test suite.  This produces __realistic__ transactions
-- that may include:
--
--   * Plutus scripts (spending, minting, certifying, voting)
--   * Reference inputs and inline datums
--   * Governance certificates (DRep registration, committee actions)
--   * Multi-asset values
--
-- Because 'invalidScriptFreq' is set to 0, every generated script is
-- intentionally valid — 'applyTx' would call 'error' on a phase-2 failure.
--
-- == Why slot 100 instead of slot 0?
--
-- 'genWithdrawals' only generates stake-key withdrawal entries when the
-- current slot is in epoch 0 (slot < 100 with 'testGlobals' epoch size of
-- 100).  Conway requires every withdrawal key to also be DRep-delegated, but
-- 'registerNewAccount' in the generic generator does not set up DRep
-- delegations.  A withdrawal without DRep delegation would be rejected with
-- 'ConwayWdrlNotDelegatedToDRep', causing 'applyTx' to fail and 'error' to
-- be called.  Using slot 100 (the first slot of epoch 1) sidesteps withdrawal
-- generation entirely without changing any other transaction content.
module LedgerBench.Conway (ApplyTxEnv (..), generateConwayApplyTxEnv) where

import qualified Data.Map.Strict as Map
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.BaseTypes (Inject (inject), Network (Testnet), ProtVer (..), natVersion)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core (ppMaxTxSizeL, ppProtocolVersionL)
import Cardano.Ledger.Core (TopTx, Tx, mkBasicTxOut)
import Cardano.Ledger.Credential (Credential (KeyHashObj), StakeReference (StakeRefNull))
import Cardano.Ledger.Keys (hashKey)
import Cardano.Ledger.Shelley.API (Globals, LedgerEnv (..), MempoolEnv)
import Cardano.Ledger.Shelley.LedgerState (CanSetUTxO (..), LedgerState, UTxO (..))
import Cardano.Ledger.Slot (SlotNo (..))
import Cardano.Ledger.State (ChainAccountState (..))
import Cardano.Ledger.TxIn (TxId (..), mkTxInPartial)
import Lens.Micro ((%~), (&), (.~))
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkKeyPair)
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash, testGlobals)
import Test.Cardano.Ledger.Generic.GenState (
  GenSize (..),
  gePParams,
  gsGenEnv,
  initialLedgerState,
  runGenRS,
 )
import qualified Test.Cardano.Ledger.Generic.GenState as GenSize
import Test.Cardano.Ledger.Generic.Instances ()
import Test.Cardano.Ledger.Generic.TxGen (genAlonzoTx)
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)

-- | All inputs required to call 'applyTx':
--
--   * 'ateGlobals'     — protocol constants (epoch size, max epoch, etc.)
--   * 'ateMempoolEnv'  — slot number, protocol parameters, chain account state;
--                        this is what the mempool passes when validating a tx
--   * 'ateState'       — the current UTxO set, delegation state, governance state
--   * 'ateTx'          — the transaction to validate
--
-- Because 'applyTx' is a __pure function__, none of these fields are mutated
-- during benchmarking.  The same 'ApplyTxEnv' can be reused for every
-- iteration without copying.
data ApplyTxEnv era = ApplyTxEnv
  { ateGlobals :: Globals
  , ateMempoolEnv :: MempoolEnv era
  , ateState :: LedgerState era
  , ateTx :: Tx TopTx era
  }

-- | Generate a Conway 'ApplyTxEnv' deterministically from an integer seed.
--
-- Steps performed:
--
-- 1. Build a 'GenSize.small' generator configuration with
--    @invalidScriptFreq = 0@ so that all generated Plutus scripts are
--    intentionally valid.
--
-- 2. Run 'genAlonzoTx' via 'runGenRS' at slot 100 (epoch 1) to produce a
--    transaction and the corresponding generator state ('gs').  The generator
--    state captures the UTxO entries, keys, scripts, and protocol parameters
--    that were created during generation.
--
-- 3. Reconstruct the ledger state from 'gs' using 'initialLedgerState'.
--    This gives a 'LedgerState' that contains exactly the UTxO entries the
--    generated transaction spends, so 'applyTx' will find all inputs present.
--
-- 4. Patch the protocol parameters: pin the protocol version to Conway
--    (major 10) and raise 'ppMaxTxSize' to 'maxBound' so the generated
--    transaction is never rejected on size alone.
--
-- 5. Build a 'LedgerEnv' (the mempool environment) with the patched
--    parameters and a plausible chain account state.
generateConwayApplyTxEnv :: Int -> Int -> ApplyTxEnv ConwayEra
generateConwayApplyTxEnv seed backgroundUtxoSize =
  let -- 'small' keeps generation fast; invalidScriptFreq = 0 ensures every
      -- generated Plutus script is valid so applyTx never fails.
      genSize = GenSize.small {invalidScriptFreq = 0}

      -- Slot 100 = first slot of epoch 1 (testGlobals uses EpochSize 100).
      -- See module note for why we avoid epoch 0.
      slot = SlotNo 100

      -- Run the constrained generator.  'unGen' seeds the QuickCheck RNG
      -- with 'mkQCGen seed'; the size parameter (30) controls how large
      -- internally-generated structures can grow.
      ((_, tx), gs) = unGen (runGenRS genSize (genAlonzoTx slot)) (mkQCGen seed) 30

      -- Reconstruct the ledger state from the generator state.  This contains
      -- exactly the UTxO entries that 'tx' spends.
      lState = addBackgroundUtxo backgroundUtxoSize (initialLedgerState gs)

      -- Patch protocol parameters from the generator environment.
      pp =
        gePParams (gsGenEnv gs)
          -- Raise the max-tx-size limit so the generated tx is never rejected
          -- purely on size.  We want to measure validation cost, not size checks.
          & ppMaxTxSizeL .~ maxBound
          -- Pin the protocol version to Conway (major 10, minor 0).
          -- Some Conway-specific rules are gated on the protocol version.
          & ppProtocolVersionL .~ ProtVer (natVersion @10) 0

      -- The mempool environment provides the slot, epoch, transaction index,
      -- protocol parameters, and chain account balances to the LEDGER rule.
      mempoolEnv =
        LedgerEnv
          { ledgerSlotNo = slot
          , ledgerEpochNo = Nothing   -- not needed for LEDGER rule
          , ledgerIx = minBound       -- first transaction in the block
          , ledgerPp = pp
          -- Treasury and reserves; the exact values do not affect tx validation
          -- but must be present to satisfy the 'ChainAccountState' record.
          , ledgerAccount = ChainAccountState (Coin 45_000_000_000) (Coin 45_000_000_000)
          }
   in ApplyTxEnv
        { ateGlobals = testGlobals
        , ateMempoolEnv = mempoolEnv
        , ateState = lState
        , ateTx = tx
        }

-- | Inject @n@ background UTxO entries into a 'LedgerState'.
--
-- The entries use 'StakeRefNull' addresses, so they do not affect
-- 'utxosInstantStake' and the UTxOState invariant is preserved.
-- TxIds are derived from 'mkDummySafeHash', which is deterministic and
-- distinct from the random TxIds produced by the constrained generator.
addBackgroundUtxo :: Int -> LedgerState ConwayEra -> LedgerState ConwayEra
addBackgroundUtxo 0  ls = ls
addBackgroundUtxo n ls =
  let bgGroupSize  = 65_536
      numBgGroups  = max 1 ((n + bgGroupSize - 1) `div` bgGroupSize)
      bgTxIdPool   = [TxId (mkDummySafeHash j) | j <- [0 .. numBgGroups - 1]]
      bgAddr       = Addr Testnet (KeyHashObj (hashKey (vKey (mkKeyPair 0)))) StakeRefNull
      bgTxOut      = mkBasicTxOut bgAddr (inject (Coin 2_000_000))
      bgTxIns      = take n
        [ mkTxInPartial tid (fromIntegral k)
        | tid <- bgTxIdPool
        , k   <- [0 .. bgGroupSize - 1 :: Int]
        ]
      bgMap = Map.fromList [(txIn, bgTxOut) | txIn <- bgTxIns]
   in ls & utxoL %~ \(UTxO m) -> UTxO (Map.union m bgMap)
