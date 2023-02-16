# Version history for `cardano-ledger-conway`

## 1.1.0.0

Added:
 * Added `RATIFY` rule
 * Added `GovernanceActionMetadata`
 * Added `RatifyEnv` and `RatifySignal`
 * Added lenses:
   * `cgTallyL`
   * `cgRatifyL`
   * `cgVoterRolesL`

Removed:
 * Removed `GovernanceActionInfo`
 * Removed `Vote`

Changed:
 * Consolidate all `Arbitrary` instances from the test package to under a new `testlib`. #3285
 * Replaced `ctbrVotes` and `ctbrGovActions` with `ctbrGovProcedure`
 * Renamed `ENACTMENT` to `ENACT`

## 1.0.0.0

* First properly versioned release.
