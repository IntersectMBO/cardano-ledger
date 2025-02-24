# Version history for `cardano-ledger-shelley-test`

## 1.6.0.0

* Add `genCoreNodeKeys` and `genIssuerKeys`
* Move `VRFNatVal` into `cardano-protocol-tpraos:testlib`
* Account for removal of crypto parametrization
* Remove crypto parametrization from `PoolSetUpArgs`, `PoolInfo`, `RewardUpdateOld`
* Add `MockCrypto`
* Remove `TestCrypto`, `C_Crypto` in favor of `MockCrypto`
* Remove `B` and `B_Crypto`
* Remove useless synonyms `Mock` and `ExMock`
* Remove deprecated `mkBlockHeader` and `unitIntervalToNatural`
* Remove `KeyPairWits`
* Remove `ShelleyBasedEra'`

## 1.5.1.0

* Update `ShelleyPoolPredFailure` arbitrary instance

## 1.5.0.0

* Remove `myDiscard`

## 1.4.0.3

*

## 1.4.0.2

*

## 1.4.0.1

*

## 1.4.0.0

* Replaced `small-steps-test` dependency with `small-steps:testlib`
* Change `testSTS` to use `NonEmpty (PredicateFailure _)` instead of `[PredicateFailure _]`
* Remove `testBootstrapSpending`, `testBootstrapNotSpending`, `bootstrapHashTest` and `genSignature`
* Moved `genBootstrapAddress` to `cardano-ledger-core:testlib:Test.Cardano.Ledger.Core.KeyPairs`

## 1.3.0.1

*

## 1.3.0.0

* Move `CDDLUtils` functionality into `cardano-ledger-binary`
* Move `cddl-files` to `cardano-ledger-shelley`

## 1.2.0.6

*

## 1.2.0.5

*

## 1.2.0.4

*

## 1.2.0.3

*

## 1.2.0.2

* GHC-9.6 compatibility

## 1.2.0.1

* Changed bounds on cardano-ledger-shelley-test
