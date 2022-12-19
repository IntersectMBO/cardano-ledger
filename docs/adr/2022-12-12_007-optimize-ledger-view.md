---
slug: 7
title: |
  7. Optimize TICKF
authors: [Jared Corduan]
tags: [Accepted]
---

## Status

Accepted

## Context

Large computations on the epoch boundary are problematic.
In the past they have led to delayed block production in new epochs.
We have removed most of the problems surrounding large computations in the ledger on the epoch
boundary (such as making the reward calculation and the stake distribution calculation incremental),
but benchmarks from the consensus team still indicate performance problems in the ledger.
In particular, the `TICKF` transition, which is used for forecasting the ledger state in order
to check block headers and to check for leadership, still take too long.
The node needs to check for leadership once per slot (so once per second on mainnet),
so if `TICKF` takes a second to compute then the node is guaranteed to start skipping leadership checks.
The performance problems with `TICKF` can exacerbate situations like
[this](https://github.com/input-output-hk/cardano-node/issues/4421).

Upon examination, the `TICKF` rule is still performing at least one large computation which can
be optimized, namely computing the per-pool stake distribution from the per-stake-credential
stake distribution using the delegation relation.
The computation is currently being done on the epoch boundary when it is first needed,
but could be performed on the previous epoch boundary.
In particular, the leadership check appears to be computing it repeatedly on every slot
crossing the epoch boundary until the first block in the new epoch is made.

The per-pool stake distribution, however, only appears to account for about half of the
duration of the `TICKF` rule (from our benchmarks).

## Decision

@nfrisby suggested two very helpful optimizations
(but we take full responsibility for the implementation).

The first suggestion was to create a thunk for the pool distribution on the first epoch
boundary when the computation is able to be performed.
This way the node will only have to pay the computational cost once, instead of at every
slot in a new epoch until the first block is produced.
In particular, we can add a lazy field to the `SnapShots` record to memoize the computation.

The second suggestion was to strip down `TICKF` to only what is required to compute the 'LedgerView'.

Preliminary benchmarks from the consensus team show that these two optimizations remove the
problematic behavior on the epoch boundary.

## Consequences

The most dramatic consequence of these two decisions is the rapid leader checks on the epoch
boundary.

On the negative side, we have now diverged further from the formal specification
(though it is semantically the same).
Furthermore, the `TICKF` implementation is less obviously correct, as it has to duplicate logic.
To combat the problem that `TICKF` could diverge from `TICK` with respect to the
ledger view, we added a property test.
