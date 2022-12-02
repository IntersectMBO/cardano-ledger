---
slug: 5
title: |
  5. Remove support for pointer addresses
authors: [Jared Corduan]
tags: [Accepted]
---

## Status

Accepted

## Context

[CRC-2](https://github.com/cardano-foundation/CIPs/pull/374) provides context.

The removal of pointer addresses is needed for
[Ouroboros Leios](https://iohk.io/en/research/library/papers/ouroboros-leios-design-goals-and-concepts/).

## Decision

We will remove support for pointer addresses.
We will accomplish this in two steps:

1. First, we will prevent the creation of new pointer addresses by using the versioned serialization
   scheme to reject deserializing transaction outputs that contain pointer addresses starting at
   major protocol version 9.
2. Next, after mainnet is on major protocol version 9, we will make the ledger era that follows
   translate the pointer addresses to enterprise addresses.


## Consequences

Pointer addresses will no longer be available.
The existing ~11 pointer addresses will be converted to enterprise addresses.
(We could attempt to resolve the valid pointers at a hard fork that removes them,
but this would involved getting the millions of stake credentials into the
[translation context](https://github.com/input-output-hk/cardano-ledger/blob/8b6f8e1a75034ca66fd66a39d437252eec927d71/eras/conway/impl/src/Cardano/Ledger/Conway/Translation.hs#L66)
which is used by the consensus hard fork combinator, which would be very difficult and hence
not worth the effort.)

The size in memory of the ledger state will be reduced
(there are millions of stake credentials).

The hurdle to Ouroboros Leios will be removed.
