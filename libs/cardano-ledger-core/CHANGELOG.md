# Version history for `cardano-ledger-core`

## 1.1.0.0

* Add `ToJSON (PParamsHKD f era)` superclass constraints for `EraPParams`.
* Add `ToJSON (TxOut era)` superclass constraints for `EraTxOut`.
* Add superclass constraints for `Val`:
  * `NoThunks`, `EncCBOR`, `DecCBOR`, `ToJSON`, `NFData`, `Show`
  * `EncCBOR (CompactForm t)`, `DecCBOR (CompactForm t)`
* Add `ToJSONKey`/`FromJSONKey` instances for `ScriptHash`
* Add `ToJSON`/`FromJSON` instances for `CompactForm Coin`, `SafeHash` and `TxId`
* Add `ToJSON` instances for:
  * `Ptr`, `CertIx`, `TxIx`
  * `Trip` and `UMap`
  * `DeltaCoin` and `CompactForm DeltaCoin`
  * `InstantaneousRewards`, `FutureGenDeleg`, `PState`, `DState` and `DPState`.
  * `UTxO` and `TxIn`
  * `Stake`, `SnapShot`, `SnapShots`, `PoolDistr` and `IndividualPoolStake`
  * `Reward` and `RewardType`
  * `AuxiliaryDataHash`
  * `Credential`
* Make `getConsumedValue` accept a deposit lookup function instead of a `DPState`
* Add `lookupDepositDState` and `lookupRewardDState`. Former can be used with
  `getConsumedValue` to regain previous behavior.
* Add `hashScriptTxWitsL`
* Remove custom `Fail` type, in favor of
  [`FailT`](https://hackage.haskell.org/package/FailT) package

### `testlib`

* Remove `runFailError` in favor of `errorFail` from `FailT` package

## 1.0.0.0

* First properly versioned release.
