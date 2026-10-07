# CIP-160 executable model

The ledger pins development artifact
[`68ed72e91b519f065cc0df0a5e7f7bfdf5d39b5b`](https://github.com/colll78/formal-ledger-specifications/tree/68ed72e91b519f065cc0df0a5e7f7bfdf5d39b5b),
genuinely extracted from signed source
[`78ef3333bf5803cd76c9880d99b120f287f733c5`](https://github.com/colll78/formal-ledger-specifications/commit/78ef3333bf5803cd76c9880d99b120f287f733c5)
in [formal source PR 1348](https://github.com/IntersectMBO/formal-ledger-specifications/pull/1348).
That source includes upstream master through
`f4f95d3349a26c8bfcc483dda58fbfab80e3d85c`. Cabal and Nix consume the same
artifact revision. Its 789 generated files have SHA-256 manifest
`1d6e312a2710a93d32875c2b3bdcd1f798670d4d69483adbbf677d645b863b81`.

Full Agda proof closure, examples, interface checks and the 39-entry property
scanner pass. Genuine Shake extraction and GHC execution from the exact signed
source pass all 69 persistent regressions: 56 Receiving checks, four complete
hard-fork/epoch version-state checks and nine committee-selection checks.
Receiving checks include separate duplicate-output arguments/budgets and paired
valid/missing child-redeemer cases through composed `LEDGER`. The formal Haskell
artifact CI workflow runs these regressions before upload. Focused integrated
ledger comparisons against this pin pass; the full suite remains pending.
Upstream source acceptance and artifact ancestry remain required by the existing
ledger CI gate below.

`BaseAddr.protected` is part of address equality. Bootstrap addresses have no
protection flag. `protect` preserves the payment and staking credentials by
reflexivity. Conway output admission requires protection to be false; Dijkstra
ordinary outputs admit protection only from protocol major version 12; this
applies independently to each top-level and child body. The address model
already excludes stake pointers. Protected pointer wire/phase-1 rejection is consequently checked by
the ledger's OutputValiditySpec, UtxoSpec and SubUtxoSpec rather than projected
into this model and falsely described as a successful comparison.

`receivingCredentials` traverses each body's ordinary outputs and collects
protected payment credentials for key/native authorization and script lookup.
Those credential/hash sets are not execution domains. `receivingOutputs` keeps
indexed protected script outputs from the actual body-local output map. The
Receive purpose contains the original output index and resolved `TxOut`; its
redeemer pointer is `(Receive, originalIndex)`. Two identical protected Plutus
outputs at different indices require two executions, redeemers and budgets.
Ordinary, key and native outputs do not renumber Plutus output indices. Parent
and child purposes, redeemers and contexts are resolved independently.

The collector deduplicates semantic purpose/credential identities before
constructing evaluator arguments. Receiving identity uses the output index,
which uniquely identifies its resolved output in the body map; it never groups
outputs by address or script hash. Existing semantic proposal identity remains
in use for Propose. Distinct purposes remain separate even under identical
foreign contexts.

This per-output design replaces our earlier development choice of one execution
per distinct script hash. The pinned [CIP-160 receiving rule](https://github.com/cardano-foundation/CIPs/blob/b4a593c960f2751fef2ddc8df28bec7b22c68eb5/CIP-0160/README.md#receiving-validation-rule)
describes validation per transaction output, and [lehins's Ledger Working Group summary](https://github.com/cardano-foundation/CIPs/pull/1063#issuecomment-3222306948)
calls for the resolved output in the purpose. These support our per-output
implementation decision; they do not explicitly approve the exact raw-index,
data-encoding or activation choices here.

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

## Integrated validation

The GHC 9.6.7 conformance executable builds against artifact `68ed72e9`.
Its focused runs with seed 2023 and `+RTS -N2 -RTS` pass:

| Selection | Examples | Failures |
| --- | ---: | ---: |
| `Receiving` | 24 | 0 |
| `Foreign interface premises` | 16 | 0 |

All four generated Receiving interface properties ran 100 samples, including
duplicate outputs, body-local indices and the explicit protected-pointer
translation boundary. The Receiving selection also covers six fixup checks and
the aggregate duplicate-output execution-budget limit. Foreign checks exercise
BLS proof verification and complete new-epoch-state translation, including
ranked committee identities, zero-stake seats, key expiry and inconsistent
snapshot rejection. Selection counts are not a combined suite total.

The full conformance suite is running. The thirteen registered adversarial
structural comparisons have not yet been rerun as a separate complete selection;
the Receiving name filter covers only some of those cases.

Reproduce from `libs/cardano-ledger-conformance` by running its built `tests`
executable with `--seed=2023 --match <selection> +RTS -N2 -RTS`, using either
selection string above.

## Concrete evaluator coverage boundary

The complete ledger ReceivingAdversarialSpec runs structuralSpec and
concreteEvaluatorSpec. The formal runner registers the thirteen structural comparison
cases, including a separate accepted-invalid multiple-output collateral
transition. The following seven concreteEvaluatorSpec tests run against actual
compiled validators in the ledger suite and are not formal comparisons:

- rejects claimed-invalid Receiving when every script succeeds, with no state effect;
- rejects claimed-valid Receiving when its script fails, with no state effect;
- a valid first output cannot hide an odd second output; failure creates no ordinary output;
- creates a protected output under Receiving and spends it under the same validator.
- evaluates duplicate-hash outputs with their own datum, redeemer and declared budget;
- collects and evaluates two byte-identical outputs with separate redeemers and budgets;
- a wrong second redeemer fails only its own evaluation and rejects the entire transaction.

The multiple-output test checks independent validator behavior and declared-validity rejection;
its accepted-invalid state path also has the separate structural comparison.
The registered Receiving domain, structural, fixup and transaction-budget
comparisons have no skipped or pending cases. Protected-pointer rejection remains
complementary ledger coverage under the explicit representation boundary above.
These seven concrete tests supply compiled evaluator and purpose-dispatch evidence
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
weight. The adapter recovers IDs by independently ranking all pools in the
retained selection snapshot by descending stake and ascending pool ID. It
validates every stored weight and honored key using actual runtime globals and
real proof-of-possession verification, rejecting inconsistent seats. Zero-stake,
keyless and duplicate-key seats retain their distinct pool identities. The old
requested committee size is absent from the snapshot: its stored seat count
provides the retained top-K bound, without proving that historical size setting.

The model includes registered zero-stake pools, preserving its existing
fractional weights and ranking. Its fixed foreign globals imply a four-epoch
key age; arbitrary network-global agreement is outside that premise. Receiving
comparisons use disabled Leios. The incoming foreign `ebSize` callback is zero,
so this integration makes no endorser-block size-conformance claim.
