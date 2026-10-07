# Changelog

This development package is excluded from the ledger release process.

## 9.9.9.9

* Translate protected address identity and body-local `Receiving` purposes for
  executable Dijkstra comparisons, retaining the actual protocol version and
  top-level collateral return/total collateral.
* Add generated Receiving domain/pointer comparisons and register supported
  structural, fixup and transaction-budget integration comparisons; document
  concrete evaluator, fee, integrity, token and pointer representation boundaries.
* Reject protected stake pointers explicitly when the model cannot represent them.
* Translate upstream Leios parameters and registered stake-pool key/epoch state;
  reject nonempty committee translation where ledger seats omit model pool IDs.
* Verify BLS proofs of possession from faithful fixed-size byte representations;
  add matching/wrong-key/malformed proof and committee-boundary regressions.
* Require `cardano-crypto-class >=2.5.1` for its fixed-size crypto codecs.
