# Ledger Events

## Overview

The main API function provided by the ledger, `applyBlockOpts`, can be configured to return ledger events by
setting the event policy to `EPReturn` in the `asoEvents` field of `ApplySTSOpts`.
The events come at the cost of performance, but are useful for understanding changes to the ledger state
after each block is applied. Note that there are no guarantees about the ordering of the events
for any given block.

Note thet when we say that an event only occurs on an epoch boundary, we mean that it only occurs on the first
block of a new epoch.

## List of Events

### `NewEpoch epoch`

This event signals the fact that the given block is the first block of a new epoch, namely `epoch`.

### `RetiredPools refundPools unclaimedPools epoch`

This event only occurs on epoch boundaries,
it provides information about pools which have been retired.
In particular, it provides the stake pool registration deposit amount
which is being returned.
Note that any given stake credential can be registered to multiple stake pools as the account
where the leader rewards are received.

When a stake pool retires, the refund is added to the stake pool's stake address,
provided the stake addresses is registered.
The field `refundPools` provides this information.
It is a map from stake credentials to another map, namely a map of pool IDs
to the deposit refund.

The field `unclaimedPools` is similar to `refundPools`, but captures the information
for those stake pools whose listed stake credential is not registered.


### `StakeDistEvent stakeMap`

A new stake distribution snapshot is taken on every epoch boundary,
corresponding to the "mark" snapshot as described in the Shelley ledger formal spec.
This event provides the new snapshot, `stakeMap`, which is
a map from registered stake credentials to the stake pool it is registered
to and the amount of stake (in lovelace) that it controls.

### `MirTransfer mir`

This event only occurs on epoch boundaries,
it describes any credits that are being added to the reward accounts from
MIR certificates.
The field `mir` is a record which contains:
* A map from stake credentials to lovelace, to be taken from the reserves.
* A map from stake credentials to lovelace, to be taken from the treasury.
* Additions to the reserves, being taken from the treasury.
* Additions to the treasury, being taken from the reserves.

### `NoMirTransfer mir availableReserves availableTreasury`

This event only occurs on epoch boundaries,
it describes a failed MIR certificate (due to a lack of funds in the reserves or treasury).
The field `mir` is the same as in the `MirTransfer` event.
The fields `availableReserves` and `availableTreasury` give the total available lovelace
that a MIR certificate can utilize from the reserves or the treasury (respectively).

### `RestrainedRewards e eraIgnored unregistered`

This event only occurs on epoch boundaries,
it describes rewards which are calculated by the ledger, but are not
delivered to a stake address.
The field `e` is the epoch number of the new epoch.
The field `eraIgnored` corresponds to the rewards not delivered
due to the Shelley bug described in Section 17.4 of the Shelley ledger specification.
The field `unregistered` corresponds to the rewards not delivered
due to the account being de-registered.

### `RupdEvent e rewards`

This event describes the new reward calculations done as a part of the
incremental reward calculation.
The field `e` is the epoch when these rewards become available
(the epoch following the one when the calculation is performed).
Some of these rewards may not end up being delivered, due to the reasons
described in the `RestrainedRewards` event.
The field `rewards` is a map from stake credentials to lovelace.

### `TotalRewardEvent e regRU`

This event only occurs on epoch boundaries, it contains all the rewards which
are delivered on the epoch boundary.
The field `e` is the epoch number of the new epoch.
It is related to the other rewards events by the property:
```
RupdEvent - RestrainedRewards = Total
```

### `TotalDeposits totalDeposits`

This event happens for every transaction, it contains the sum of all the deposits
paid by the given transaction (both stake credential registration deposits and
stake pool registration deposits).

### `RegisterPool poolID`
This event happens for every new stake pool registration certificate.
The field `poolID` is the stake pool ID.

### `ReregisterPool hk`
This event happens for every stake pool re-registration certificate
(a change in stake pool parameters).
The field `poolID` is the stake pool ID.

### `SuccessfulPlutusScriptsEvent debugInformation`
This event happens for every collection of Plutus scripts in a given transaction
which successfully execute.
The field `debugInformation` is a list of `PlutusDebug`, which is a record containing
all the information needed to re-run a Plutus script.

### `FailedPlutusScriptsEvent debugInformation`
This event is the same as `SuccessfulPlutusScriptsEvent`, except that it contains the
information for all the failed Plutus scripts in a single transaction.

## Notes / TODO

There appears to be multiple, redundant `NewEpoch` events.
