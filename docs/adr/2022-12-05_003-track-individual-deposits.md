---
slug: 3
title: |
  3. Track individual deposits
authors: [Jared Corduan]
tags: [Accepted]
---

## Status

Accepted

## Context

Individual deposits are not currently tracked by the ledger,
only the values as given in the protocol parameters.
This has several problems and needs to be addressed.

See #3113.

### What happens currently if the deposit amounts are changed?

We currently collect deposits for:
* stake credential registrations
* stake pool registrations

The amount in Lovelace is determined by two protocol parameters.
In the event of a change to these parameters, the reserves absorb the difference,
assuming it is large enough (otherwise the entire protocol parameter update is ignored).

In particular:

* If the deposit amount is raised, the appropriate number of Lovelace is removed from the
  reserves and added to the deposit pot. Users de-registering these resources will be given
  back a larger deposit than they originally gave.
* If the deposit amount is lowered, the appropriate number of Lovelace is added to the reserves
  and removed from the deposit pot. Users de-registering these resources will be given back a
  smaller deposit than they originally gave.

     
### Pros for this methods

* We do not have to track the value of each deposit paid by the many registered stake credentials
  (the stake pool deposits are less problematic since there are far fewer of them).
  This is not only less complexity, but also less memory used.

### Cons for this methods

* Most folks expect a deposit to be paid back exactly.
* We cannot increase the deposit amount once the reserves hits zero.
* If folks know that the deposit amount is soon to be increased,
  they can make free Lovelace by registering a lot of credentials.
* Because of the problems above, it is going to be incredibly hard to ever change the values.
* There is a serious issue involving hard forks explained below in its own subsection.

#### Serious flaw with the current design

Note the logic of the `NEWPP` rule. It checks three things:

1. There is a non-trivial update.
2. There is enough in the reserves to account for increasing the deposit values.
3. The maxBlockSize is not being set to something obviously problematic.

Note also that consensus makes the decision about whether or not to enact a hard fork based on the
protocol parameter update state two stability windows before the end of the epoch.

This means that if quorum is met regarding changing the major protocol version,
but the update violates 2. or 3. above, consensus will change the era but the ledger will not
change the major protocol version, leaving the ledger in a split-brain state.

We must therefore commit to accepting a protocol parameter update two stability windows from the
end of the epoch. This mean that:

* We must check the max block size at the time the update is provided
  (there is no good reason we are not doing it this way now).
* We must track the deposits and pay back exactly what was paid.

## Decision

Rewrite the ledger to track how much everyone deposited, and pay back exactly what they
originally deposited (we have the history, we just do not track it in the ledger state).

## Consequences

The already problematically large stake credential map will become larger,
but probably not more than one word (eight bytes) per stake credential if we are smart
about the implementation.

All the cons listed above, especially the issue involving hard forks, are addressed.
