# Contract use of proposed protected addresses and Receiving

This guide describes the per-output CIP-160 contract proposed for Dijkstra
protocol major version 12 and receiving-aware Plutus V4. The
[CIP amendment](https://github.com/cardano-foundation/CIPs/pull/1286) and
[Plutus interface](https://github.com/IntersectMBO/plutus/pull/7982) are published
for review; upstream format agreement and network activation remain outstanding.
Integration and activation require coordinated ledger, Plutus, formal, API, CLI
and node releases. Review evidence and release requirements are linked below;
this is not a mainnet deployment guide.

Execution per output is this implementation's proposed reconciliation of
[CIP-160's per-output validation rule](https://github.com/cardano-foundation/CIPs/blob/b4a593c960f2751fef2ddc8df28bec7b22c68eb5/CIP-0160/README.md#receiving-validation-rule)
and [lehins's suggestion to resolve a TxOut into the purpose](https://github.com/cardano-foundation/CIPs/pull/1063#issuecomment-3222306948).
Those sources support individual output validation and visibility; the exact raw
output-index mapping and Data fields below are our proposed implementation
decisions, rather than evidence of upstream agreement on those formats.

## Recipient authorization

A protected address requires authorization when an ordinary transaction output is
created at that address. A protected key recipient must sign the creating body.
For a child output, the signature must cover that child's body hash; a signature
over the enclosing body is insufficient. A protected native-script recipient
supplies a satisfied native script. A protected Plutus recipient supplies the
script, a body-local Receiving redeemer and execution budget. Subsequent spending
uses the existing Spending purpose and payment credential; protection does not
introduce a second spending rule. Receiving key signatures do not implicitly add explicit guards.

The same credential can occur at an ordinary unprotected address. Contracts
relying on recipient authorization must inspect the address protection form in
outputs, consumed inputs and reference inputs where that matters. Protection
alone does not enforce global state uniqueness, one token or UTxO per protocol,
or every invariant historically enforced by state tokens.

## Validate the specific receiving output

Each protected Plutus output requires its own Receiving execution, redeemer and
budget, even when several outputs have the same payment hash or identical
contents. The pointer is the raw original zero-based index in the containing
body's authored output sequence, not a filtered rank or a sorted hash position.
Key, native and ordinary outputs do not shift or compress those indexes. Native
scripts use existing phase-1 checks and require no Plutus Receiving redeemer.
Use the ledger's output-purpose/pointer interfaces for construction and lookup.

The receiving-aware context identifies this specific output and its original
index. `scriptContextScriptHash` still identifies the executing recipient script.
Validate the selected output's datum, value and staking conditions. A validator
may additionally inspect other visible outputs for contract-specific invariants,
but a successful invocation cannot authorize a second protected Plutus output.
Even byte-identical duplicates have different purposes and independent budgets.

Receiving remains local to the containing body: a parent and child with the same
hash and output index have separate purposes, redeemers and integrity domains.
There is no implicit datum argument; datum contents come from the selected
output or existing permitted datum witnesses.

The [receivingEvenDatum fixture](../libs/plutus-preprocessor/src/Cardano/Ledger/Plutus/Preprocessor/Source/V4.hs)
checks an even inline integer datum on its resolved Receiving output and an even
datum when Spending. Other purposes fail. The `receivingRedeemerMatchesDatum`
fixture additionally checks the raw output index, resolved output and recipient
hash, then matches its redeemer to that output's inline integer datum. This
allows separate same-hash outputs to receive different instructions.

The [ledger lifecycle tests](../eras/dijkstra/impl/testlib/Test/Cardano/Ledger/Dijkstra/Imp/ReceivingAdversarialSpec.hs)
define creation and subsequent Spending with the same validator, independent
same-hash/identical-output purposes and malformed-output rejection. The creating
body uses Receiving; the consuming body uses Spending. These cases exercise
compiled validators and distinct output contexts, rather than abstract evaluator
results alone.

An inline datum exposes contents directly. A datum hash alone does not reveal its
preimage or create an automatic phase-1 preimage requirement. A contract can
instead look up contents in existing permitted datum witnesses. The fixture
intentionally requires inline datums rather than accepting hashes it cannot
inspect. V4 preserves `Maybe AccountId` staking representation; it does not
reintroduce pointer-capable V1 staking credentials.

Receiving sees the body containing its purpose, including its outputs, consumed
inputs and reference inputs. It does not implicitly see siblings or the enclosing
body. Wider batch conditions need the supported Guarding mechanisms. Guarding's
full and simplified views preserve protected addresses and recognize Receiving
redeemer hashes.

## Witnesses and failure behavior

Scripts on selected consumed or reference inputs may satisfy Receiving through
the existing script availability rules. A reference script attached only to a new
output cannot authorize its own creation. The same script may run under Receiving
and Spending with distinct redeemers and budgets. Budgets and fees aggregate over
the batch, while redeemer pointers and script-integrity hashes belong to bodies.

A child-only Plutus Receiving invocation requires the top-level collateral checks.
Key/native-only Receiving adds no Plutus collateral requirement. Protected
collateral-return outputs are rejected in phase 1. Failed Receiving creates no
ordinary outputs in any body. A matching phase-2-invalid transaction can still
follow the existing collateral-only path; a transaction claiming validity is
rejected when its Plutus scripts fail.

V1-V3 contexts cannot represent protected addresses or Receiving purposes.
Collection fails in phase 1 when the required legacy view includes protected
outputs or consumed inputs, including key or native-script recipients. V2 and V3
also expose reference inputs and reject protection there. V1 hides reference
inputs, so their protection alone does not prevent collection; its existing
missing-input, Byron-address and inline-datum validation still applies.
Dijkstra already rejects legacy languages in subtransactions. These restrictions
are applied to actual context visibility and do not imply a blanket ban on all
mixed-language batches.

## Migration and reproducibility

A transaction executing an old-language contract may be unable to create a
protected replacement output because its context cannot represent that output.
Do not promise an atomic legacy-to-protected migration. A supported strategy can
require an intermediate unprotected output or a contract-specific upgrade path;
each stage needs its own authorization and invariant analysis. Recompiling under
V4 can change the script hash. There is no general promise that an existing hash
is retained.

The proposed V4 Address Data schema uses
`Constr 0 [paymentCredential, optionalAccount]` for ordinary addresses and
`Constr 1 [paymentCredential, optionalAccount]` for protected addresses. Existing
V4 clients must update and V4 validators must be recompiled. Released V1-V3
schemas remain unchanged. Receiving uses Data constructor index 7 with an
output-specific payload: `Receiving ScriptHash Integer` is
`Constr 7 [scriptHash, originalOutputIndex]`;
`ReceivingScript Integer TxOut` is `Constr 7 [originalOutputIndex, resolvedOutput]`.
Ledger item and pointer views both carry the original `Word32` output index;
lookup verifies that index is a protected script output in the same body. Ledger
CBOR redeemer tag 7 and Plutus Data constructor index 7 are separate assignments;
Guarding retains tag/index 6.

Compile the fixture source through the repository's real Plutus preprocessor:

```sh
cabal run plutus-preprocessor
```

This generates the public test fixture module
`libs/cardano-ledger-core/testlib/Test/Cardano/Ledger/Plutus/Examples.hs`.
The command requires the patched receiving-aware Plutus dependency pinned in
[cabal.project](../cabal.project). A released Plutus 1.71 package does not contain
this proposed interface. The current proposal is
[`dcbb7e3232c3322557410fe341ec84f3cd78dc04`](https://github.com/colll78/plutus/tree/dcbb7e3232c3322557410fe341ec84f3cd78dc04).
Two genuine fixture generations at this dependency revision produced identical
bytes and preserved all 41 existing V1-V3 fixture byte strings.

The matching formal source is
[`87072fed43a085bbbaaeb5888a7792ec5f8a164a`](https://github.com/IntersectMBO/formal-ledger-specifications/commit/87072fed43a085bbbaaeb5888a7792ec5f8a164a),
with generated artifact
[`b747be78f6e001d41395974251cf0b42f45b68c4`](https://github.com/colll78/formal-ledger-specifications/commit/b747be78f6e001d41395974251cf0b42f45b68c4)
pinned by Cabal and Nix. Its 789 generated files were matched byte-for-byte to
genuine signed-source Shake extraction; manifest SHA-256 is
`8d0d123d8eb3728884ad272c00f18169c1244fe43563df18af28d813c41ff535`.
Model execution has documented foreign-evaluator and context abstractions;
concrete ledger validator tests and integrated conformance provide complementary
checks. See the [formal conformance guide](cip-0160-formal-conformance.md).

## Validation checkpoints and release requirements

The 2026-10-07 ledger checkpoint passed 1152 Dijkstra examples with zero failures
and two inherited pending cases, 291 ledger API tests, 63 focused Receiving tests
and four independent interoperability tests. Shared tests completed 58 examples
with zero failures and five inherited pending cases. Receiving benchmark smoke
checks covered every 16/256/4096-output group and paired translation setup
equality; those checks do not establish statistical performance. Two fresh CDDL
generations matched the tracked files and two fresh `hie.yaml` generations were
unchanged. The [ledger review](https://github.com/IntersectMBO/cardano-ledger/pull/6153)
records source applicability, integrated conformance and subsequent validation
results.

At the same dated checkpoint, Plutus `dcbb7e3` passed 450 public API tests, six
compiled-plugin cases and five remote checks. Formal source `87072fed` passed the
complete Agda proof closure, examples, interfaces, the 39-entry property scanner
and all 73 genuinely extracted runtime assertions. These results support the
proposed interfaces; they do not establish upstream format approval or network
activation. Review evidence is maintained in the
[Plutus proposal](https://github.com/IntersectMBO/plutus/pull/7982) and
[formal proposal](https://github.com/IntersectMBO/formal-ledger-specifications/pull/1348).

Downstream API source `7164f6d7d4a02d49164d7fbf33d10f80b4b50efe` passed strict
compilation of 147 production modules, 293 unit tests, 125 golden tests and 171
RPC tests. The native WASM golden check passed one case; it does not validate
browser execution or the JavaScript binding boundary. CLI checks passed strict
compilation of 186 production modules, 81 unit tests and 820 goldens. Consensus
release checks passed 11 admission/capacity tests, 240 tracing tests and two
encoding goldens in the scoped combined local dependency graph. Those scoped
results do not replace complete CDDL compliance or node integration. Current
coordinated evidence is tracked by the
[API review](https://github.com/IntersectMBO/cardano-api/pull/1370),
[consensus release review](https://github.com/IntersectMBO/ouroboros-consensus/pull/2373)
and ledger review, including CLI delivery.

Release requires upstream agreement on the address/language formats and
activation version, compatible published dependencies, full integrated
conformance and remote CI. The existing CI gate requires the formal artifact to
become an ancestor of upstream `master-artifacts`; development-fork publication
does not satisfy that condition. Browser WASM execution and binding compatibility
need their own checks. Node coverage must establish legacy-contract migration,
activation, persisted-state restart and restoration, rollback, and Leios
endorser-block Receiving accounting and validation. Ledger translation and
snapshot tests alone do not establish those node workflows. No mainnet readiness
or cost/efficiency claim follows from the checkpoints above.
