# Changelog
All notable changes to this repository will be documented in this file.
At at later date we may adopt per-Haskell-package changelogs.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html)
in the naming of release branches.

## [Unreleased]
### Added
-  Added `coinsPerUTxOByteToCoinsPerUTxOWord` helper function for Babbage
   #2896
### Changed
- Some types have been moved:
  - The `WitVKey` type has been moved into its own module in core.
  - The `HKD` type has been moved to `cardano-ledger-core`.
  - The `PoolParams` type has been moved into its own module
  - The `DCert` type and related functionality from `TxBody` to `Cardano.Ledger.Shelley.Delegation.Certificates`.
  #2880
- The initial funds and staking in the Shelley genesis type (used only for testing) now use `ListMap` instead of `Map`.
  #2871, #2890, #2892, #2895
### Deprecated
- The provenance for the reward calculation has been removed.
  The type signature to the API function `getRewardProvenance` has not change,
  it just returns an empty provenance value.
  The type signature will be changed once the corresponding query has been
  deprecated in the ouroboros-network repository.
  #2879
### Removed
- The `StakeCreds` type was unused and is now removed.
  #2880
- The`Ord` instance for `MemoBytes` was removed.
  #2881
### Fixed
- Ensure Babbage TxOut decoder can't fail due to malformed Ptr.
  This bug was manifesting in the cardano-node as:
  if a node is running in the Babbage era and shuts down, when it comes back up it has to re-sync from genesis.
  #2897
- The error message for failed Plutus V2 scripts was being obscured by a bug which has now been fixed.
  #2888

## Release branch 1.0.0

The first release branch in the cardano-ledger repository,
namely `release/1.0.0`, branches from the
cardano-ledger commit used for the 1.35.0 release of cardano-node release.
