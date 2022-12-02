---
slug: 6
title: |
  6. Cost model serialization
authors: [Jared Corduan]
tags: [Accepted]
---

## Status

Accepted

## Context

The current cost model serialization scheme is problematic:

```
 costmdls = 
   { ? 0 : [ 166*166 int ] ; Plutus v1 
   , ? 1 : [ 175*175 int ] ; Plutus v2 
   } 
```

The are two problems:

* We cannot add a V3 cost model in the same protocol parameter update that initiates a hard fork.
  We can only count on nodes to be updated after a hard fork
  (chainChecks provides this fantastic guarantee), and so nodes that have not updated would not be
  able to deserialize a V3 cost model. This is operationally annoying for us,
  and also means we have to wait an extra epoch to use new versions of Plutus.
* We cannot add new fields to existing cost models.

We need a more flexible serialization scheme that address these two problems.
Note that the ledger deserializes the CBOR list of integers into a map by using keys provided to
us by the Plutus library
(where the list is assumed to be ordered corresponding to the alphabetical sorting of the key).

See #2902.

## Decision

* The Plutus function `mkEvaluationContext` will be changed to take `[Integer]`
  instead of `Map Text Integer` (matching the wire spec).
  * If `mkEvaluationContext` receives fewer integers than it needs (for a given version of Plutus),
    it will return an error message
  * If `mkEvaluationContext` receives at least as many integers as it needs,
    it will return an evaluation context, possibly with a warning that too many integers were
    supplied and ignored
  
* The `CostModel` serialization will be made more permissive.
  * It will accept any `[int]`
  * It will store whatever `[int]` given to it, and also the results of `mkEvaluationContext`
    * If it is for a version of Plutus that the ledger is unaware of,
      it will not call `mkEvaluationContext` but instead store an appropriate error
  * We will use the [versioned serialization scheme](docs/adr/2022-12-05_002-version-serialization-schemes.md)
    to change the serialization at a given new major protocol version.

### How this works, new language built-ins

Suppose version `x` of the node supports Plutus V2 cost models with 10 fields.
Suppose version `x+1` of the node supports Plutus V2 cost models with 11 fields.
Let `f` be the new field, let `m` be the current major protocol version, and suppose that
Plutus V2 will not support `f` until `m+1`.

During major protocol `m`:

* Node `x` sees a cost model with 11 fields, it makes an `EvaluationContext` with the 10 fields
  it knows about, but also stores a warning and the original 11 fields.
  * It happily evaluates all V2 scripts, and Plutus will gracefully fail (phase 2)
    if it sees `f` (it will fail to deserialize).
* Node `x+1` sees a cost model with 11 fields, it makes an `EvaluationContext` with all of them.
  * It also happily evaluates all V2 scripts, and Plutus will gracefully fail (phase 2)
    if it sees `f` (based on `m`).
* Node `x` shuts down, the operator updates the software, and comes back up with a node `x+1`.
  Upon re-serialization of the ledger state, the operator is now in the case above (a normal `x+1` node).

During major protocol `m+1`:
* Node `x` can no longer participate in the chain, due to the `ObsoleteNode` error.
* Node `x+1` is ready to process scripts with `f` in V2.

### How this works, new Plutus versions

Suppose version `x` of the node does not know anything about Plutus V3.
Suppose version `x+1` of the node adds support for Plutus V3. Let `m` be the
current major protocol version, and suppose that Plutus V3 is introduced at `m+1`.

During major protocol `m`:

* Node `x` sees a cost model for V3. It stores the cost model integers, but in place of an
  `EvaluationContext` it stores an error.
  * Any V3 script will be rejected since `x` does not know how to deserialize V3.
* Node `x+1` sees a cost model for V3, it makes an `EvaluationContext` for it.
  * Any V3 script will be rejected by the Plutus evaluator based on `m`.
* Node `x` shuts down, the operator updates the software, and comes back up with a node `x+1`.
  Upon re-serialization of the ledger state, the operator is now in the case above
  (a normal `x+1` node).

## Consequences

We will be able to add new fields to existing cost models,
and we will be able to add cost models for new languages ahead of the hard forks
that introduce the languages.
