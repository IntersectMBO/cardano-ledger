---
slug: 2
title: |
  2. Versioned Serialization Schemes
authors: [Jared Corduan]
tags: [Accepted]
---

## Status

Accepted

## Context

The cardando ledger uses CBOR to serialize blocks and transactions on the network.
When we encounter a problem with our deserializers, such as in #3003, #2965 and #2444,
it can be very difficult to implement a fix.
It is difficult because we can only fix such issues during a hard fork,
and leading up to the hard fork we must maintain two serializations for the same type in order
to not cause unintended network splitting
(the problematic version must be used before the hard fork, and the fixed version is used afterwards).
This can be especially tricky with the FromCBOR typeclass,
since it is not always easy to search for where all the problematic uses are located.

This issue is captured in #3014.

## Decision

Create a new typeclass to replace ToCBOR and FromCBOR which depends not only on the type
but also on the protocol version (or, alternatively, the ledger era).
The first instances will all be implemented identically to our current
ToCBOR and FromCBOR instances for all protocol versions up to the current one.

## Consequences

This change affects downstream components that rely on the ledger serializers,
since they will now need to supply either the protocol version or a ledger era
(using a ledger era is less precise, due to intra-era hard forks).

This change will let us easily fix several outstanding issues, and will make fixing future
issues with the serialization much easier.

A deprecation cycle was proposed in [CIP-80](https://github.com/cardano-foundation/CIPs/pull/372).
