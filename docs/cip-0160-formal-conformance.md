# CIP-160 executable model

The ledger pins development artifact
[`9f359b1e534419c24323a53181aa8e33ea17fbac`](https://github.com/colll78/formal-ledger-specifications/tree/9f359b1e534419c24323a53181aa8e33ea17fbac),
genuinely extracted from signed source
[`06dbab86667ad6efd3151b460d63daeae74ca042`](https://github.com/colll78/formal-ledger-specifications/commit/06dbab86667ad6efd3151b460d63daeae74ca042)
in [formal source PR 1348](https://github.com/IntersectMBO/formal-ledger-specifications/pull/1348).
That source includes upstream master through
`f4f95d3349a26c8bfcc483dda58fbfab80e3d85c`. Cabal and Nix consume the same
artifact revision. Its 789 generated files have SHA-256 manifest
`11d34721c5ad304d6ed0f3109d758e6f3dee18c21f7bb29a244bed024be5fa1b`.
Full Agda specification/proof checks, the example library and property dashboard
passed; GHC compiled the actual public API and ran all 41 persistent model
regressions. The formal Haskell artifact CI job now runs those regressions before
upload. [Successful public Haskell CI](https://github.com/IntersectMBO/formal-ledger-specifications/actions/runs/37572929146/job/112635555193)
also extracted and ran all 41 regressions on synthetic merge `7adffdf`, containing
the documentation-only `b4b7923` followup. This is separate from the pinned
artifact's exact `06dbab86` extraction provenance. Upstream source acceptance and
generated artifact ancestry remain required by the ledger CI gate described below.

`BaseAddr.protected` is part of address equality. Bootstrap addresses have no
protection flag. `protect` preserves the payment and staking credentials by
reflexivity. Conway output admission requires protection to be false; Dijkstra
ordinary outputs admit protection only from protocol major version 12; this
applies independently to each top-level and child body. The address model
already excludes stake pointers. Protected pointer wire/phase-1 rejection is consequently checked by
the ledger's OutputValiditySpec, UtxoSpec and SubUtxoSpec rather than projected
into this model and falsely described as a successful comparison.

`receivingCredentials` traverses each body's ordinary outputs. It produces a
set of protected payment credentials. `receivingScriptHashes` and
`receivingKeyHashes` project that set. The Receive script purpose identifies a
payment script hash, without staking credentials. Its foreign pointer index
sorts the unique natural-number hash representations, independently of witness
contents or native/Plutus script classification. Native hashes occupy slots.
The ledger's fixed-length hash encoding as a big-endian natural number preserves
lexicographic byte ordering. Duplicated output addresses or differing staking
credentials never create duplicate execution purposes. Parent and child target
domains are computed separately. The collector deduplicates purpose/credential
identities before constructing evaluator arguments, using existing semantic
proposal identity for Propose and exact tag/payload equality for other purposes.
Different purposes remain separate even under identical foreign contexts.

Receiving keys are required in each body's key-witness set. Receiving scripts
join the existing batch witness/reference-script pool, native validation,
non-native exact-redeemer domain and integrity/language-view rules. Newly created
outputs contribute no scripts to that pool: a reference script attached only to
a new protected output cannot authorize its own creation. Receiving introduces
no implicit datum; existing optional output datum rules apply. Legacy V1–V3
contexts reject protection on consumed inputs and ordinary outputs. V2/V3 also
reject protected reference inputs, which they expose; V1 has no reference-input
context field, so protection alone on a hidden reference input adds no rejection.
Its inherited bootstrap and inline-datum reference checks remain in force.

The top-level transaction alone carries collateral inputs, collateral return and
total collateral. The model rejects a protected return unconditionally in phase
1, including script-free transactions. Returned coin cannot exceed collateral
coin, and collateral balance equals return value plus the injected collected
coin. This equation permits full return of collateral's other assets in the
abstract token algebra. Invalid phase-2 batches preserve ordinary inputs and
create no ordinary outputs; they consume collateral, insert the unprotected
return at the next ordinary-output index and increase fees by the collected
coin. All bodies' declared execution budgets contribute to transaction and block
limits and the abstract script-fee calculation.

## Evaluator and representation premises

The model parameter `validPlutusScript` is an abstract evaluator. The existing
ledger conformance runner sets its foreign boolean to the transaction's declared
phase-2-valid flag. Successful conformance therefore establishes structural
witness/authorization and state-transition agreement *under that evaluator
premise*. It cannot establish that a concrete compiled validator accepted the
output, that its script context was encoded correctly, or that a claimed-valid
transaction whose actual validator fails was rejected. Concrete validators and
validity mismatches are independently checked by the ledger's Receiving Imp
suite and Plutus context tests.

Other existing foreign abstractions remain explicit: the executable token
algebra is ADA-only and the ledger translation keeps only coin; datum and
redeemer representations are hashes; `valContext` returns an abstract constant;
script-integrity hashes/language views, script fees and value-size estimation
are abstracted. Thus multiasset collateral equality, actual CBOR/context bytes,
datum/business rules, actual script fees and serialized-size limits require
the named ledger/unit/interop tests. None of these abstractions is an oracle
import of the ledger's Receiving helper.

The generated foreign API exports Receiving domain and pointer functions for
both transaction levels. The conformance suite compares those functions to the
ledger using generated protected/unprotected key/script destinations, duplicate
outputs and varying stake credentials; full LEDGER comparisons cover actual
submission and state effects through the existing Imp hook. Comparison success
is not proof of concrete evaluator correctness.
The foreign next-output index is the number of authored ordinary outputs; the
ledger translation provides contiguous indexes beginning at zero. Arbitrarily
sparse foreign output maps are outside that translation premise.

## Ledger integration and release input

The Receiving generated properties live in
`libs/cardano-ledger-conformance/test/Test/Cardano/Ledger/Conformance/Spec/Dijkstra/Receiving.hs`.
Dijkstra `Imp.UtxowSpec`, `Imp.SubUtxowSpec`, `Imp.UtxoSpec`, `Imp.SubUtxoSpec`
and `Imp.ReceivingFixupSpec`, plus `Imp.ReceivingAdversarialSpec.structuralSpec`
and `Imp.ReceivingAccountingSpec.transactionBudgetSpec`,
run beneath the existing
`submitTxConformanceHook` with the composed executable `LEDGER` rule. The hook
compares success/failure and translated resulting ledger states. It does not
compare predicate-failure constructor payloads: the preexisting formal runner
returns textual errors and accepts either system's rejection. Pure
`OutputValiditySpec` and context/compiled-validator tests provide the additional
wire, payload and evaluator checks described above.

A local path to extracted `dist/hs` is a development dependency only.
`.github/workflows/haskell.yml` checks that the formal artifact dependency is an
ancestor of upstream `master-artifacts`. An unmerged fork artifact cannot satisfy
that enforced release condition. Source review/typecheck, generated artifact
hashes, conformance results and the upstream source/artifact merge references
must all be attached to the release input; a local pass does not remove the
external merge requirement.

## Concrete evaluator coverage boundary

The complete ledger ReceivingAdversarialSpec runs structuralSpec and
concreteEvaluatorSpec. The formal runner registers the thirteen structural comparison
cases, including a separate accepted-invalid grouped-output collateral
transition. The following four concreteEvaluatorSpec tests run against actual
compiled validators in the ledger suite and are not formal comparisons:

- rejects claimed-invalid Receiving when every script succeeds, with no state effect;
- rejects claimed-valid Receiving when its script fails, with no state effect;
- a valid first grouped output cannot hide an odd second output; failure creates no ordinary output;
- creates a protected output under Receiving and spends it under the same validator.

The grouped-output test checks grouped validator behavior and declared-validity rejection;
its accepted-invalid state path also has the separate structural comparison.
The registered Receiving domain, structural, fixup and transaction-budget
comparisons have no skipped or pending cases. Protected-pointer rejection remains
complementary ledger coverage under the explicit representation boundary above.
These four concrete tests supply compiled evaluator and purpose-dispatch evidence
outside these formal comparisons. The foreign constant context and boolean
evaluator cannot establish that compiled purpose dispatch is correct.

## Accounting coverage boundary

The transaction execution-budget comparison is registered separately from the
thirteen adversarial structural comparisons. The extracted foreign budget
ordering compares memory and steps componentwise, with independently chosen
below-limit, equal-limit and each-dimension-over-limit fixtures. The ledger
PParams translation retains the actual protocol version for activation checks.

ReceivingAccountingSpec's concrete ledger suite additionally checks actual
script prices, removal of a balanced fee, script-integrity mutation, the BBODY
execution-budget limit and both full-return/omitted-asset collateral outcomes.
The fee and integrity callbacks and Coin projection cannot establish those
properties; the LEDGER hook also does not compare BBODY. These remain enabled
concrete tests with the boundaries stated in the test module. Mempool admission
cases run the actual mempool ledger transition independently of confirmed state.

## Upstream Leios representation boundary

The upstream formal API retains a registered pool's BLS key and registration
epoch, and drops the proof after its registration check. The ledger adapter
serializes the actual key/proof bytes and calls the real BLS proof verifier for
registration. State comparison consequently does not establish preservation of
the ledger's retained proof bytes. Those crypto checks have matching-proof,
wrong-key and malformed-encoding regression cases.

Formal committee seats retain pool IDs, while ledger seats retain only key and
weight. A nonempty committee has no direct faithful translation without an
additional snapshot-origin premise identifying each pool; this adapter returns
an explicit translation error. Empty committees translate normally. Receiving
comparisons use disabled Leios. This is an inherited representation boundary,
with explicit empty/nonempty regression cases, and adds no Receiving skips.
The incoming foreign `ebSize` callback is zero, so this integration makes no
endorser-block size-conformance claim.
