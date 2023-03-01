# Version history for `cardano-ledger-shelley`

## 1.1.0.0

* Add `ToJSON` instance for:
  * `ShelleyTxOut`
  * `AlonzoPParams StrictMaybe`
  * `ProposedPPUpdates` and `ShelleyPPUPState`
* Add `ToJSON (GovernanceState era)` superclass constraint for `EraCovernance`

## 1.0.0.0

* First properly versioned release.
