{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Conway ledger throughput benchmark.
--
-- == What this benchmark measures
--
-- This benchmark measures the raw throughput of the Conway ledger's transaction
-- validation function ('applyTx') in isolation.  There is no network stack, no
-- mini-protocol overhead, no mempool queue, and no block assembly — just the
-- pure ledger rule.
--
-- == How it works
--
-- One transaction and one matching ledger state are generated (see below).
-- Then 'applyTx' is called N times in a loop, passing the same transaction and
-- the same initial ledger state every time.
--
-- == What 'applyTx' does internally
--
-- 'applyTx' runs the @LEDGER@ STS (Small-Step Transition System) rule, which
-- chains the following sub-rules:
--
--   1. @UTXOW@ (UTxO with witnesses):
--        * Verifies every required Ed25519 key witness (@verifySignature@).
--        * Executes any Plutus scripts with budget metering.
--        * Checks that all required signers are present.
--
--   2. @UTXO@ (UTxO validity):
--        * Confirms every input exists in the UTxO set.
--        * Enforces fee ≥ minimum fee (linear formula).
--        * Checks value conservation: Σ inputs = Σ outputs + fee.
--        * Validates output sizes, value sizes, and TTL\/validity interval.
--
--   3. @CERTS@: processes any delegation, pool registration, or governance
--      certificates embedded in the transaction.
--
--   4. @GOV@: processes any governance proposals or votes.
--
-- == Benchmark types
--
-- * __Sized benchmarks__ (default): hand-crafted transactions with N inputs
--   and N Ed25519 witnesses, scaling signature-verification work directly.
--   Three sizes are available: Small (~200 B, 1 witness), Medium (~1 430 B,
--   10 witnesses), Large (~16 000 B, 116 witnesses).
--
-- * __Random benchmark__ (@--random@): uses the constrained-generator to
--   produce a randomly generated Conway transaction that may include Plutus
--   scripts, reference inputs, inline datums, and governance certificates.
--   Seed the generator with @--seed@ to get a different transaction.
--
-- == Usage
--
-- @
-- cabal run ledger-bench                               -- all three sizes (default)
-- cabal run ledger-bench -- --tx-size Medium           -- Medium only
-- cabal run ledger-bench -- --tx-size Small --tx-size Large
-- cabal run ledger-bench -- --random                   -- add constrained-generator benchmark
-- cabal run ledger-bench -- --utxo-size 1000000        -- add 1M background UTxO entries
-- cabal run ledger-bench -- --verbose                   -- print full CBOR hex
-- cabal run ledger-bench -- --count 50000              -- more iterations
-- cabal run ledger-bench -- --seed 42                  -- different generated tx
-- @
module Main where

import qualified Cardano.Ledger.Binary.Plain as Plain
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy as BL
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Shelley.API (applyTx)
import Control.Exception (evaluate)
import Control.Monad (void)
import Data.Char (toLower)
import LedgerBench.Conway (ApplyTxEnv (..), generateConwayApplyTxEnv)
import LedgerBench.SizedConway (SizedTx (..), generateSizedConwayApplyTxEnv, numInputsForSize)
import LedgerBench.Throughput (ThroughputResult (..), measureThroughput, printReport)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

data Config = Config
  { cfgCount :: Int
  -- ^ Number of 'applyTx' calls to time per benchmark.
  -- More iterations give a more stable median but take longer to run.
  , cfgSeed :: Int
  -- ^ Integer seed for the QuickCheck RNG used by the trace generator.
  -- Different seeds produce different transactions; the sized benchmarks
  -- also use this seed to derive their key pairs deterministically.
  , cfgSizes :: [String]
  -- ^ Size categories requested via @--tx-size@.
  -- Empty means run all three sizes (Small, Medium, Large).
  , cfgRandom :: Bool
  -- ^ When True, also run the constrained-generator random benchmark.
  -- Enabled with @--random@.
  , cfgUtxoSize :: Int
  -- ^ Number of background UTxO entries to add alongside the spendable
  -- inputs.  0 (default) means only the N spendable entries are present.
  -- Use e.g. 1_000_000 to approximate a mainnet-sized UTxO set and stress
  -- O(log M) map-lookup cost.  Enabled with @--utxo-size N@.
  -- Note: has no effect on the @--trace@ benchmark.
  , cfgVerbose :: Bool
  -- ^ When True, print the full CBOR hex of each transaction rather than
  -- a truncated preview.  Enabled with @--verbose@.
  }

defaultConfig :: Config
defaultConfig =
  Config
    { cfgCount = 10_000
    , cfgSeed = 24601
    , cfgSizes = []
    , cfgRandom = False
    , cfgUtxoSize = 0
    , cfgVerbose = False
    }

usageText :: String
usageText =
  unlines
    [ "Usage: cabal run ledger-bench -- [OPTIONS] [+RTS ...]"
    , ""
    , "Measure Conway ledger throughput (applyTx KB/s)."
    , "Reports median latency and P95 latency; throughput is derived from P95 (conservative guarantee)."
    , ""
    , "Options:"
    , "  --tx-size SIZE     Run only this size: Small | Medium | Large (repeatable)."
    , "                     Default: run all three sizes."
    , "  --random           Also run a randomly generated transaction using the"
    , "                     constrained-generator (may include Plutus scripts, reference"
    , "                     inputs, inline datums, governance certificates)."
    , "  --count N          Number of applyTx iterations per benchmark (default: 10000)."
    , "  --seed N           Integer seed for key-pair and trace-generator RNG (default: 24601)."
    , "  --utxo-size N      Add N background UTxO entries to stress O(log M) map lookups."
    , "                     0 (default) = only the spendable inputs are present."
    , "  --verbose          Print full CBOR hex of each transaction (default: first 128 chars)."
    , "  --help             Print this message and exit."
    , ""
    , "Examples:"
    , "  cabal run ledger-bench                                   -- all three sizes"
    , "  cabal run ledger-bench -- --tx-size Medium               -- Medium only"
    , "  cabal run ledger-bench -- --random                       -- add constrained-generator tx"
    , "  cabal run ledger-bench -- --utxo-size 1000000            -- 1M background UTxO entries"
    , "  cabal run ledger-bench -- --count 50000                  -- more iterations"
    , ""
    , "For stable results, reduce GC noise with RTS options:"
    , "  cabal run ledger-bench -- +RTS -A128m -N1 -RTS"
    , "  -A128m  enlarges the allocation area, reducing GC frequency"
    , "  -N1     pins execution to one OS thread, eliminating scheduler jitter"
    ]

-- | Parse command-line arguments into a 'Config'.
-- Unknown flags are silently ignored.
parseArgs :: [String] -> IO Config
parseArgs args = go defaultConfig args
  where
    go cfg [] = pure cfg
    go _ ("--help" : _) = putStr usageText >> exitSuccess
    go cfg ("--count" : n : rest) = case readMaybe n of
      Just i -> go cfg {cfgCount = i} rest
      Nothing -> errorWithoutStackTrace $ "ledger-bench: invalid --count value: " <> n
    go cfg ("--seed" : s : rest) = case readMaybe s of
      Just i -> go cfg {cfgSeed = i} rest
      Nothing -> errorWithoutStackTrace $ "ledger-bench: invalid --seed value: " <> s
    go cfg ("--tx-size" : s : rest) = go cfg {cfgSizes = cfgSizes cfg ++ [s]} rest
    go cfg ("--random" : rest) = go cfg {cfgRandom = True} rest
    go cfg ("--utxo-size" : s : rest) = case readMaybe s of
      Just i -> go cfg {cfgUtxoSize = i} rest
      Nothing -> errorWithoutStackTrace $ "ledger-bench: invalid --utxo-size value: " <> s
    go cfg ("--verbose" : rest) = go cfg {cfgVerbose = True} rest
    go cfg (_ : rest) = go cfg rest

--------------------------------------------------------------------------------
-- Benchmarks
--------------------------------------------------------------------------------

-- | Constrained Generator Conway benchmark.
--
-- Calls 'generateConwayApplyTxEnv' to produce a randomly generated Conway
-- transaction using the constrained-generator (potentially including Plutus
-- scripts, reference inputs, inline datums, and governance certificates).
-- Then times N calls to 'applyTx' against the same initial ledger state.
--
-- The call to 'evaluate $!' forces the result of 'applyTx' to WHNF before the
-- timer for this iteration stops.  Without this, GHC's lazy evaluation would
-- defer the actual computation to the next 'getCurrentTime' call, bleeding
-- work across iteration boundaries and making individual timings meaningless.
benchConway :: Config -> IO ThroughputResult
benchConway Config {cfgCount, cfgSeed, cfgUtxoSize, cfgVerbose} = do
  let env = generateConwayApplyTxEnv cfgSeed cfgUtxoSize
      serialised = Plain.serialize (ateTx env)
      txBytes = fromIntegral $ BL.length serialised
      utxoDesc =
        if cfgUtxoSize == 0
          then "generated UTxO"
          else "generated UTxO + " <> show cfgUtxoSize <> " background entries"
  printf
    "  [Conway/Random] 1 transaction generated (%d B), %s, applied %d times\n"
    txBytes
    utxoDesc
    cfgCount
  printf "  CBOR hex: %s\n" (cborHexPreview cfgVerbose serialised)
  measureThroughput "Conway/Random" txBytes cfgCount (applyTxAction env)

-- | Sized Conway benchmark.
--
-- Calls 'generateSizedConwayApplyTxEnv' to produce a hand-crafted Conway
-- transaction with exactly 'numInputsForSize' inputs and witnesses.
-- All inputs are key-hash locked, so every validation call performs exactly N
-- Ed25519 signature verifications — the dominant ledger validation cost.
--
benchSized :: SizedTx -> Config -> IO ThroughputResult
benchSized size Config {cfgCount, cfgSeed, cfgUtxoSize, cfgVerbose} = do
  let env = generateSizedConwayApplyTxEnv size cfgSeed cfgUtxoSize
      serialised = Plain.serialize (ateTx env)
      txBytes = fromIntegral $ BL.length serialised
      nInputs = numInputsForSize size
      utxoDesc =
        if cfgUtxoSize == 0
          then show nInputs <> " UTxO entries"
          else show nInputs <> " + " <> show cfgUtxoSize <> " background UTxO entries"
      label = "Conway/" <> show size <> " (" <> show nInputs <> " inputs)"
  printf
    "  [%s] 1 transaction generated (%d B), %s, applied %d times\n"
    label
    txBytes
    utxoDesc
    cfgCount
  printf "  CBOR hex: %s\n" (cborHexPreview cfgVerbose serialised)
  measureThroughput label txBytes cfgCount (applyTxAction env)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

-- | Maps size-category names to their benchmark actions.
sizeTable :: Config -> [(String, IO ThroughputResult)]
sizeTable cfg =
  [ ("Small", benchSized Small cfg)
  , ("Medium", benchSized Medium cfg)
  , ("Large", benchSized Large cfg)
  ]

-- | Case-insensitive filter over a named table.
filterBy :: [String] -> [(String, a)] -> [(String, a)]
filterBy keys =
  let lowerKeys = map (map toLower) keys
   in filter (\(k, _) -> map toLower k `elem` lowerKeys)

-- | Serialize a transaction to CBOR and return a hex preview string.
-- In non-verbose mode only the first 64 bytes are decoded, avoiding a full
-- allocation of the hex string for large transactions.
cborHexPreview :: Bool -> BL.ByteString -> String
cborHexPreview True  bs = BC8.unpack . B16.encode . BL.toStrict $ bs
cborHexPreview False bs = (BC8.unpack . B16.encode . BL.toStrict . BL.take 64 $ bs) <> "..."

-- | Apply a transaction once and force the result to WHNF, discarding the
-- new ledger state.  This is the timed action passed to 'measureThroughput'.
applyTxAction :: ApplyTxEnv ConwayEra -> IO ()
applyTxAction ApplyTxEnv {ateGlobals, ateMempoolEnv, ateState, ateTx} =
  void $ evaluate $! either (error . show) fst $
    applyTx ateGlobals ateMempoolEnv ateState ateTx

main :: IO ()
main = do
  cfg <- getArgs >>= parseArgs

  -- Select benchmarks:
  --   * Sized benchmarks: all three sizes by default; filtered by --tx-size if given.
  --   * Random benchmark: only when --random is given.
  let sizeBenches
        | null (cfgSizes cfg) = map snd (sizeTable cfg)
        | otherwise = map snd $ filterBy (cfgSizes cfg) (sizeTable cfg)
      randomBenches
        | cfgRandom cfg = [benchConway cfg]
        | otherwise = []
      selected = sizeBenches ++ randomBenches

  printf
    "Conway Ledger Benchmark\n\
    \=======================\n\
    \Seed: %d  |  Iterations: %d\n\
    \Method: 1 tx generated, applied N times to the same state (no mempool, no network).\n\
    \Stats: median and P95 latency reported; throughput = txSize / P95 (conservative guarantee).\n\n\
    \Generating test environment (this may take a few seconds)...\n"
    (cfgSeed cfg)
    (cfgCount cfg)

  results <- sequence selected

  printReport results

  -- Exit with a non-zero code if any benchmark is below the 200 KB/s target,
  let failing = filter (not . trMeetsTarget) results
  if null failing
    then exitSuccess
    else exitFailure
