# ledger-bench

A throughput benchmark for the Conway ledger's transaction validation function (`applyTx`).
It measures how fast the ledger can validate transactions in isolation — no network stack,
no mempool queue, no block assembly.

## What it measures

`applyTx` runs the `LEDGER` STS rule, which chains:

1. **`UTXOW`** — verifies Ed25519 key witnesses and executes Plutus scripts
2. **`UTXO`** — checks inputs exist, enforces fee ≥ minimum, validates value conservation
3. **`CERTS`** — processes delegation and pool registration certificates
4. **`GOV`** — processes governance proposals and votes

The dominant cost for non-script transactions is Ed25519 signature verification (one per input).

## Running

```
cabal run ledger-bench                                    # all three sized benchmarks
cabal run ledger-bench -- --tx-size Medium                # Medium only
cabal run ledger-bench -- --tx-size Small --tx-size Large
cabal run ledger-bench -- --random                        # add constrained-generator benchmark
cabal run ledger-bench -- --utxo-size 1000000             # 1M background UTxO entries
cabal run ledger-bench -- --count 50000                   # more iterations
cabal run ledger-bench -- --seed 42                       # different generated tx
cabal run ledger-bench -- --verbose                       # print full CBOR hex
```

## Benchmark modes

### Sized (default)

Hand-crafted transactions with exactly N inputs and N Ed25519 witnesses. All inputs are
key-hash locked, so every validation call performs exactly N signature verifications.

| Size   | Inputs | Approx size |
|--------|--------|-------------|
| Small  |      1 |     ~200 B  |
| Medium |     10 |   ~1,430 B  |
| Large  |     116 |  ~16,000 B  |

Use `--tx-size` to select one or more sizes. Default runs all three.

### Random (`--random`)

Uses the constrained-generator framework (`genAlonzoTx`) to produce a realistic Conway
transaction. This may include Plutus scripts, reference inputs, inline datums, multi-asset
values, and governance certificates.

## Background UTxO (`--utxo-size N`)

By default, the ledger state contains only the entries the transaction actually spends.
`--utxo-size N` adds N additional dummy entries, making UTxO map lookups O(log M) against
a realistically-sized map. Use `--utxo-size 1000000` to approximate mainnet conditions.

Works for both sized and random benchmarks.

## Output

```
Era                          Tx Size      Iters         Min      Median         P95      Throughput   Target
-----------------------------  --------- ---------- ----------- ----------- ----------- -------------- --------
Conway/Small (1 inputs)        200 B      10000  0.000312 s  0.000318 s  0.000341 s      0.6 KB/s       OK
Conway/Medium (10 inputs)    1,430 B      10000  0.001201 s  0.001230 s  0.001310 s      1.1 KB/s       OK
Conway/Large (116 inputs)   16,000 B      10000  0.013500 s  0.013800 s  0.014200 s      1.1 KB/s       OK
```

- **Min** — fastest single call (warm cache, no contention)
- **Median** — primary statistic; stable against GC-pause outliers
- **P95** — 95th-percentile latency; realistic worst case excluding GC spikes
- **Throughput** — derived from P95: `txSizeBytes / 1024 / p95Latency` (KB/s)
- **Target** — `OK` if throughput ≥ 200 KB/s (Cardano block-size budget); `FAIL` otherwise

The process exits non-zero if any benchmark falls below the 200 KB/s target.

## Test suite

```
cabal test ledger-bench-test
```

Verifies that:
- Each sized transaction (Small/Medium/Large) and the random generator produce non-empty
  transactions
- `applyTx` accepts all generated transactions without error
- `measureThroughput` returns positive latency and throughput values
