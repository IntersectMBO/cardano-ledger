# Changelog for `cardano-ledger-pretty`

## 1.1.1.0

 * Added `PrettyA` instances for:
   * `Delegatee era` #3372
   * `ConwayDelegCert c` #3372
 * Plutus V3 is now pretty

## 1.1.0.0

* Added `PrettyA` instances for:
  * `Ratio a` #3291
  * `Set a` #3291
  * `StrictSeq a` #3291
  * `StrictMaybe a` #3291
  * `VoteDecision` #3291
* Added `PrettyA` instances for new Conway data types:
  * `GovernanceActionMetadata` #3291
  * `RatifyEnv` #3291
  * `RatifySignal` #3291
* Changed the prettyprinter of `ConwayTxBody` to account for the new
  `ctbGovProcs` field #3291
* Removed `PrettyA` instances for:
  * `Vote` #3291
  * `GovernanceActionInfo` #3291

## 1.0.0.0

* First properly versioned release.
