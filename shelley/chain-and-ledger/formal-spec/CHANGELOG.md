# Changelog

## 2019-03-22
- The blockchain layer for the spec has been completed and polished.
- There are now operational certificates and key evolving signatures.

## 2019-03-01
- Added the blockchain layer to the spec for Praos, including a new top-level transition CHAIN.

## 2019-02-20
- Calculating the stake distribution uses relations.
- The prose now makes heavy use of bullet point lists that follow the order in the tables.
- Helper functions have been separated and labeled as such.
- The "Rewards Ledger Update" section was removed, and the logic was placed elsewhere.
The predicate in transition `DELRWDS`, namely checking that the reward withdrawal in a transaction
matched that in the ledger state, was moved to the base case of `DELEGS`.
The state transformation, namely zeroing out the appropriate rewards,
was also moved to the base case of `DELEGS`.
- Added a list of contributors.
- The `UTxOEP` and `ACCNT` transition systems were combined.
- The predicate in `POOLCLEAN` that requires that the current retiring pools not be the empty set
- `POOLCLEAN` was renamed to `POOLREAP`.
has been removed
- The `NEWPP` transition now performs a no-op when costs are not met
(so that the transition system does not halt).
- All the definitions regarding addresses are now in a single table.
- Enterprise addresses were added.
- All the definitions regarding the protocol parameters are now in a single table.
- `E_max` is now a protocol parameter.
- The certificate pointers are now constructed in the `DELEGS` transition.
- `Coin` is now defined as an alias for the integers.
- All the definitions regarding transactions are now in a single table.
- Transactions are now a concrete type.
- Transaction witnesses are now defined as a mapping from `VKey`s to `Sig`.
- We now require that the transaction witnesses be exactly the minimal set that is needed.
- The `Allocs` type is now split into `StakeKeys` and `StakePools`.
- Support for pool owners was added. All rewards for pool oweners go to the reward account
registered in the pool certificate.
- Unrealized rewards are now given to the treasury.
- Some values marked as belonging to the unit interval have been changed to non-negative reals.
- Reward accounts are now included in the stake distribution.
- The reward calculation no longer returns the updated moving averages.
- Several references to the delegation design document were added.
- When a pool retires, all delegations to the pool are removed from the delegation mapping.
- The main epoch boundary transition system is now split into two transitions.
There is now a top level transition `SNAP` which calculates and saves the last three stake distributions.
This transition does _not_ happen on the epoch boundary.
The transition which does occur on the epoch boundary now uses the stake distribution snapshots
for the reward calculation.
- The number of slots per epoch is now a global constant.
- The epoch boundary transition now uses an epoch number instead of a slot in the environment.
It represents the upcomming epoch number.
- This changelog was added.


## 2019-01-03
This update was cosmetic. 

- Descriptions were added to the reward calculations.
- Several spelling and grammar mistakes were fixed.
- A few typos in the reward calculation were fixed.
- Git references where added to the title page.
- Color and bolding was added to the state transitions.

## 2018-12-21
The initial formal specification of the ledger was released.
