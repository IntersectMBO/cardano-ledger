{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Core timing harness for the Conway ledger benchmark.
--
-- == Methodology
--
-- 'measureThroughput' times each 'applyTx' call individually using
-- wall-clock time ('getCurrentTime', which wraps
-- @clock_gettime(CLOCK_REALTIME)@).  The overhead of the clock call itself
-- is roughly 100–500 ns per sample, which is negligible compared to the
-- ~0.5 ms validation cost of a typical Conway transaction.
--
-- All per-iteration durations are collected into a list, sorted, and then
-- summarised with order-statistics (min, median, P95) rather than a mean.
--
-- == Why median instead of mean?
--
-- GHC's stop-the-world garbage collector pauses whichever 'applyTx' call
-- happens to be running, producing occasional spikes of 100–600 ms.  With
-- 10 000 iterations these spikes represent < 0.1% of samples, so they
-- are __invisible to the median__ but inflate the mean by 10–15% and shift
-- it unpredictably between runs.  Median gives a stable, reproducible
-- measure of steady-state validation cost.
--
-- == Why P95 instead of max?
--
-- The __max__ latency is always a GC-pause outlier — it tells you nothing
-- about the ledger.  __P95__ (the 95th-percentile duration) captures the
-- realistic worst-case cost: the slowest 1-in-20 call, which reflects
-- occasional cache misses and minor OS scheduling jitter rather than GC.
--
-- == Throughput calculation
--
-- Throughput is derived from the P95 latency:
--
-- @
-- throughput (KB/s) = txSizeBytes / 1024 / p95Latency (seconds)
-- @
--
-- Using P95 gives a conservative guarantee: if even the 95th-percentile
-- call sustains ≥ 200 KB/s, the target is met under realistic worst-case
-- conditions, not just in the average case.
-- The 200 KB/s target comes from the Cardano block-size budget.
module LedgerBench.Throughput (
  ThroughputResult (..),
  measureThroughput,
  printReport,
  throughputTargetKBs,
) where

import Control.Monad (replicateM)
import Data.List (sort)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Text.Printf (printf)

-- | The 200 KB/s throughput target.
throughputTargetKBs :: Double
throughputTargetKBs = 200.0

-- | Result of timing N sequential 'applyTx' calls.
data ThroughputResult = ThroughputResult
  { trEra :: String
  -- ^ Benchmark label (e.g. "Conway/Medium (10 inputs)")
  , trTxSizeBytes :: Int
  -- ^ Serialised transaction size in bytes (CBOR encoding)
  , trIterations :: Int
  -- ^ Number of timed 'applyTx' calls
  , trDurationSec :: Double
  -- ^ Total elapsed wall-clock time of the timed phase (seconds)
  , trMinLatencySec :: Double
  -- ^ Fastest single call — best-case warm-cache cost
  , trMedianLatencySec :: Double
  -- ^ Median call time — primary statistic, immune to GC-pause outliers
  , trP95LatencySec :: Double
  -- ^ 95th-percentile call time — realistic worst-case (not a GC spike)
  , trThroughputKBs :: Double
  -- ^ Derived throughput in KB/s: @txSizeBytes / 1024 / p95Latency@.
  -- Using P95 gives a conservative guarantee: 95% of calls are faster,
  -- so the reported throughput is one the ledger sustains even in the
  -- realistic worst case (excluding GC-pause outliers above P95).
  , trMeetsTarget :: Bool
  -- ^ True when 'trThroughputKBs' ≥ 'throughputTargetKBs'
  }

-- | Time @n@ executions of @action@, collecting each call's individual
-- wall-clock duration.
--
-- The @action@ must force evaluation of the ledger result to WHNF before
-- returning (use 'Control.Exception.evaluate' with @($!)@).  Without this,
-- GHC's lazy evaluation defers computation to the next call, making
-- individual timings meaningless.
measureThroughput ::
  String ->
  -- ^ Benchmark label
  Int ->
  -- ^ Transaction size in bytes
  Int ->
  -- ^ Number of iterations
  IO () ->
  -- ^ The timed action — must force computation before returning
  IO ThroughputResult
measureThroughput eraName txSizeBytes n action = do
  samples <- replicateM n $ do
    t0 <- getCurrentTime
    action
    t1 <- getCurrentTime
    pure (realToFrac (diffUTCTime t1 t0) :: Double)
  let sorted = sort samples
      durationSec = sum samples
      minSec = head sorted
      medianSec = percentile 0.50 sorted
      p95Sec = percentile 0.95 sorted
      throughputKBs = fromIntegral txSizeBytes / 1024.0 / p95Sec
  pure
    ThroughputResult
      { trEra = eraName
      , trTxSizeBytes = txSizeBytes
      , trIterations = n
      , trDurationSec = durationSec
      , trMinLatencySec = minSec
      , trMedianLatencySec = medianSec
      , trP95LatencySec = p95Sec
      , trThroughputKBs = throughputKBs
      , trMeetsTarget = throughputKBs >= throughputTargetKBs
      }

-- | Nearest-rank percentile from a __sorted__ sample list.
-- @p@ must be in [0, 1].  Returns the element at index @floor(p * n)@,
-- clamped to [0, n-1].
percentile :: Double -> [Double] -> Double
percentile p xs =
  let n = length xs
      idx = min (n - 1) (max 0 (floor (p * fromIntegral n)))
   in xs !! idx

-- | Print a formatted summary table and overall verdict.
--
-- Columns: label, tx size, iterations, min latency, median latency,
-- P95 latency, throughput (KB/s), target verdict.
-- Latency columns are in seconds.  The label column width is computed
-- dynamically so rows always align regardless of benchmark name length.
printReport :: [ThroughputResult] -> IO ()
printReport results = do
  let labelW = maximum (map (length . trEra) results) `max` 3
      hdrFmt = "%-" ++ show labelW ++ "s %9s %10s %11s %11s %11s %14s %8s\n"
      rowFmt = "%-" ++ show labelW ++ "s %7d B %10d %9.6f s %9.6f s %9.6f s %11.1f KB/s %8s\n"
      sep k = replicate k '-' :: String
  putStrLn ""
  printf
    hdrFmt
    ("Era" :: String)
    ("Tx Size" :: String)
    ("Iters" :: String)
    ("Min" :: String)
    ("Median" :: String)
    ("P95" :: String)
    ("Throughput" :: String)
    ("Target" :: String)
  printf hdrFmt (sep labelW) (sep 9) (sep 10) (sep 11) (sep 11) (sep 11) (sep 14) (sep 8)
  mapM_ (printRow rowFmt) results
  putStrLn ""
  let failing = filter (not . trMeetsTarget) results
  if null failing
    then printf "PASS: All benchmarks meet the %.0f KB/s throughput target.\n" throughputTargetKBs
    else do
      printf "FAIL: The following benchmarks do NOT meet the %.0f KB/s target:\n" throughputTargetKBs
      mapM_ (\r -> printf "  - %s: %.1f KB/s\n" (trEra r) (trThroughputKBs r)) failing
  where
    printRow fmt ThroughputResult {trEra, trTxSizeBytes, trIterations, trMinLatencySec, trMedianLatencySec, trP95LatencySec, trThroughputKBs, trMeetsTarget} =
      printf
        fmt
        trEra
        trTxSizeBytes
        trIterations
        trMinLatencySec
        trMedianLatencySec
        trP95LatencySec
        trThroughputKBs
        (if trMeetsTarget then "OK" else "FAIL" :: String)
