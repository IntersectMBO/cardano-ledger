# Revision history for `cardano-ledger-byron`

## 1.1.0.0

* Switch from cryptonite library (deprecated) to crypton (a drop in replacement)

## 1.0.2.1

*

## 1.0.2.0

* Move `mainnet-genesis.json` to the test package to make `ts_prop_elaboratedCertsValid` pass when run with `cabal`. #4586
* `force` `startTime` in the evaluation of genesis-data. #4574
* Lower bound on `cardano-ledger-binary >=1.5` in cabal file

## 1.0.1.0

* Replaced `small-steps-test` dependency with `small-steps:testlib`

## 1.0.0.4

*

## 1.0.0.3

* Update `streaming-binary` dependency to 0.4.

## 1.0.0.2

*

## 1.0.0.1

* Make it build with `ghc-9.6`.

## 1.0.0.0

* First properly versioned release.
