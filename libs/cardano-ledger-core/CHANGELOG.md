# Version history for `cardano-ledger-core`

## 1.1.0.0

* Add `ToJSON` superclass constraints for `EraTxOut`.
* Add superclass constraints for `Val`:
  * `NoThunks`, `EncCBOR`, `DecCBOR`, `ToJSON`, `NFData`, `Show`
  * `EncCBOR (CompactForm t)`, `DecCBOR (CompactForm t)`
* Add `ToJSON`/`FromJSON` instances for `CompactForm Coin`, `SafeHash` and `TxId`
* Add `ToJSON` instances for:
  * `Ptr`, `CertIx`, `TxIx`
  * `Trip` and `UMap`
  * `DeltaCoin` and `CompactForm DeltaCoin`
  * `InstantaneousRewards`, `FutureGenDeleg`, `PState`, `DState` and `DPState`.
  * `UTxO` and `TxIn`

## 1.0.0.0

* First properly versioned release.
