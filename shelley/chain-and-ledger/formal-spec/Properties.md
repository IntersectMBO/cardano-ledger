# Preservation of Value

Recall that there are six pots of money in the Shelley ledger:

* Circulation (total value of the UTxO)
* Deposits
* Fees
* Rewards (total value of the reward accounts)
* Reserves
* Treasury

For each transition sytem, we will list what pots are in scope,
describe how value moves between the pots,
and state any relevent properties (usually the preservation of ADA).

### Transitions with no pots in scope

* Up
* Ppup
* Avup
* Prtcl
* Overlay
* Updn
* Ocert
* Rupd

### UTXO, UTXOW

Pots in scope: Circulation, Deposits, Fees

Value can be transfered between Circulation and Deposits.
Value can also be transfered to the Fees, but Fees can only
be increased by this transition.

**Property** The value (Circulation + Deposits + Fees) increases by the sum
of the withdrawals in the transaction.

**Property** Fees does not decrease.

### DELEG, POOL, DELPL

Pots in scope: Rewards

**Property** The rewards to do not change.

### DELEGS

Pots in scope: Rewards

**Property** The reward pot is decreased by the sum of the withdrawals in the
transaction (in the environment).

### POOLREAP

Pots in scope: All

This transition returns a portion of the the pool certificate deposit to the correct
reward address, provided it is still registered. Otherwise it is given to the treasury.
The total is deducted from the deposit pot.

### LEDGER, LEDGERS, BBODY

Pots in scope: Circulation, Deposits, Fees, Rewards

The value lost by UTXO is balanced by the value gained by DELEGS.

**Property** The value (Circulation + Deposits + Fees + Rewards) is the same
before and after the transition.

### NEWEPOCH

Pots in scope: All

Besides using the EPOCH transition, NEWEPOCH applies a reward update.
The reward update decreases the reserves and the fee pot,
but increases the treasury and the reward pot.

**Property** The value (Reserves + Fees + Treasury + Rewards) is the same
before and after the transition.

**Property** The Circulation and Deposits do not change.

### SNAP

Pots in scope: Circulation, Deposits, Fees

The snapshot transition moves decayed deposits from the deposit pot to
the fee pot.

### NEWPP

Pots in scope: Circulation, Deposits, Fees, Treasury, Reserves

The new protocol parameter transition adusts the deposit pot to meet
the current obligation, and the difference is made up by the reserves.

### EPOCH

Pots in scope: All

With respect to the pots, the epoch transition does the combination
of SNAP, POOLREAP, and NEWPP.

**Property (Full Preservation of ADA)**
The value (Circulation + Deposit + Fees + Treasury + Rewards + Reserves) is the same
before and after the transition.

### EPOCH

Pots in scope: All

With respect to the pots, the epoch transition does the combination
of NEWEPOCH and RU.

**Property (Full Preservation of ADA)**

### CHAIN

Pots in scope: All

With respect to the pots, the epoch transition does the combination
of HEAD, PTRCL, and BBODY.

**Property (Full Preservation of ADA)**

# Time Traveling Header Properties

We need to adapt properties 1 -3, from section 8 of the byron chain spec, to Shelley.

# Update Properties

**Property**
There can be at most one change to the protocol parameters per epoch.

**Property**
If there are no pending future application versions,
there will not be a change to the version for at least SlotsPerEpoch.

**Property**
Software versions increase lexicographically.

**Property**
If there are only four gen keys acive, there can be no new future
application version or protocol parameters.

**Property**
The protocol parameter update system and the application version update system
are independent. (The transition system of one does not effect the state of the other.)

**Property**
The protocol parameter update state is always empty at begining of epoch.

**Property**
The keys (of type Slot) of the following two mappings are always past the current slot:
the future application versions (favs) and the future genesis delegation mapping (fdms).

**Property**
The size of the mappings PPUpdate, inside the update state, is always at most six.

**Property**
The size of the mappings AVUpdate, inside the update state, is always at most seven.

# Deposits Properties

**Property**
The deposits pot is always greater that the current obligation
(ie the total amount of coin needed to refund every stake registration and
pool registration certificate that are currently registered).
In particular, the UTXO and POOLREAP rule can never result in a
negative value for deposits.

# Staking Properties

**Property**
If no stake keys are registered, the rewards from the reward update
will always sum to zero.

**Property**
If no stake pools are registered, the rewards from the reward update
will always sum to zero.

**Property**
The sum of stake in the stake snapshots is always at most forty-five billion ADA.

**Property**
The following delegation mappings always has the same size:
`stdelegs`, `rewards`, and `ptrs`.
(We can maybe even say more, the key set of `stdelegs` is the same
as the range of `ptrs`, which also corresponds one-one with the reward addresses
in `rewards`. Moreover, the key set of `delegations` is a subset of of that of `stdelegs`.

**Property**
If all stake keys and pools deregister, then, assuming that no one registers anything,
by epoch `e+1`, where `e` is the max epoch in the stake pool retirement mapping,
the delegation state will be nearly empty:
`stDelegs`, `rewards`, `delegations`, `ptrs`, `stpools`, `poolParams`, `retiring`.
are all the empty map. (The map `cs` will have size seven, for the genesis keys.)


# Entropy Properties

**Property**
In the absence of the extra entropy parameter,
the epoch nonce is what you get from combining the blocks leading up to it
(and stopping `SlotsPrior`-many slots in the previous epoch).

# Decentralization Properties

**Property**
The overlay schedule is obeyed: no blocks are produced during the silent blocks,
and only core nodes makes blocks on the overlay slots.

# Rewards Properties

**Property**
At the start of each epoch, the reward update is set to NOTHING.
Moreover, the reward update will change exactly once during the epoch,
to a non-NOTHING value.

# Block Header Properties

**Property**
The body size and block body hash listed in the block header are correct.

# Block Count Properties

**Property**
The number of blocks made in an epoch is equal to number of active overlay slots plus
the number the size of the `BlocksMade` mapping.

**Property**
The number blocks made in an epoch is never greater than the number of slots in an epoch.

# Authorization Properties

TODO - Without just restating predicates already in our rules, how can we
state properties stating that UTxO transfer, certificates, etc, are properly authorized?
