# Contract use of proposed protected addresses and Receiving

This guide describes the local CIP-160 implementation for proposed Dijkstra
activation at protocol major version 12. Its Plutus V4 interface is an unfrozen
proposal. The [Plutus interface proposal](https://github.com/IntersectMBO/plutus/pull/7982)
is published for review and is not yet agreed or released upstream. This is not a
mainnet deployment guide. Proposed cardano-api/CLI construction examples and
node/testnet migration workflows have not yet been validated end to end.

## Recipient authorization

A protected address requires authorization when an ordinary transaction output is
created at that address. A protected key recipient must sign the creating body.
For a child output, the signature must cover that child's body hash; a signature
over the enclosing body is insufficient. A protected native-script recipient supplies a satisfied native
script. A protected Plutus recipient supplies the script, a body-local Receiving
redeemer and execution budget. Subsequent spending uses the existing Spending
purpose and payment credential; protection does not introduce a second spending
rule. Receiving key signatures do not implicitly add explicit guards.

The same credential can occur at an ordinary unprotected address. Contracts
relying on recipient authorization must inspect the address protection form in
outputs, consumed inputs and reference inputs where that matters. Protection
alone does not enforce global state uniqueness, one token or UTxO per protocol,
or every invariant historically enforced by state tokens.

## Inspect every output in a group

The receiving target domain contains distinct protected script hashes in one
transaction body. One Receiving invocation authorizes every protected output for
its hash in that body. Native scripts occupy positions in the script-hash domain;
key recipients do not occupy Receiving redeemer positions. A position is not a
transaction output index, and the Data constructor index is not a CBOR redeemer
tag. Use ledger purpose/pointer interfaces instead of duplicating enumeration.

`ReceivingScript` has no datum field. The executing recipient hash comes from
`scriptContextScriptHash`. The V4 helper `protectedOutputsAt hash txInfo` returns
all matching `(originalBodyOutputIndex, output)` pairs in authored order and
excludes ordinary addresses with the same credential. Never check only the first
matching output: another output in the group may violate the contract.

The local [receivingEvenDatum validator](../libs/plutus-preprocessor/src/Cardano/Ledger/Plutus/Preprocessor/Source/V4.hs)
is the smallest compiled fixture demonstrating that rule. Its Receiving branch
requires a nonempty group and an even inline integer datum on every matching
output. Its Spending branch requires an even spending datum. All other purposes
fail. Thus a malformed second protected output makes the whole group fail,
regardless of whether the first output is valid. A production validator should
add the relevant value, account and datum constraints for its own contract.

The [ledger lifecycle example](../eras/dijkstra/impl/testlib/Test/Cardano/Ledger/Dijkstra/Imp/ReceivingAdversarialSpec.hs)
creates a protected output with inline integer datum 2 using this validator, then
consumes that actual output with the same script's Spending branch. The creating
body has a Receiving redeemer; the consuming body has a Spending redeemer. Both
retain the same payment script hash. This ledger lifecycle and its three
companion grouped-output/failure cases passed focused execution: four examples,
zero failures. This ledger evidence does not validate the proposed CLI workflow
or a node/testnet activation rehearsal.

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

The proposed V4 Address Data schema changes the previous draft list product to
`Constr 0 [paymentCredential, optionalAccount]` for ordinary addresses and
`Constr 1 [paymentCredential, optionalAccount]` for protected addresses. Existing
V4 clients must update and V4 validators must be recompiled. Released V1-V3
schemas remain unchanged. Receiving ScriptPurpose is `Constr 7 [scriptHash]` and
ReceivingScript is `Constr 7 []`.

Compile the fixture source through the repository's real Plutus preprocessor:

```sh
cabal run plutus-preprocessor
```

This generates the public test fixture module
`libs/cardano-ledger-core/testlib/Test/Cardano/Ledger/Plutus/Examples.hs`.
The command requires the patched receiving-aware Plutus dependency; an ordinary
released 1.71 package does not contain this proposed interface. The exact source
revision is pinned in [cabal.project](../cabal.project) at
`14d7686b4bcb51dbf35c24269ec2cc999824cb3e`. Its Haskell library/test sources match
`9d927c19a756cb15c7eba9e331d1a87878215fd1`, whose API package suite passed all 446 tests with
strict warnings under GHC 9.6.7; its compiled normal/data-backed selection tests
also passed. The preprocessor generated the fixture through the real Plutus
compiler at revision `3ddfba3e01998eb98e2c906c1caecebee242b609`; the current pin only adds a
test-import correction and a Receiving case in the off-chain analyser, preserving
the compiler and production API sources. The latest pin additionally fixes
Windows CI flags and guards fork documentation deployment; those CI changes
require no fixture regeneration.
Two successive runs produced identical complete fixture files, preserving all 41
released V1-V3 fixture byte strings. Local fixture execution is distinct
from a supported CLI construction path or a testnet activation rehearsal. No
cost or efficiency claim is made without measurements.
