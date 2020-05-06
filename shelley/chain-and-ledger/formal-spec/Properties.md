# Ledger and Epoch State Validity

We only care that the properties below are satisfied for _valid_ ledger states, and
more generally, valid _epoch_ states. Checking things for invalid states should
not be performed. As the STS rule system we have defined
is deterministic, all valid states can be reached using the transitions in the system,
and the only states that are valid are those that can be described by a sequence
of rule applications (i.e. a composition of valid transitions).

# Preservation of Value

Recall that there are six pots of money in the Shelley ledger:

* Circulation (total value of the UTxO)
* Deposits
* Fees
* Rewards (total value of the reward accounts)
* Reserves
* Treasury

For each transition system, we will list what pots are in scope,
describe how value moves between the pots,
and state any relevant properties (usually the preservation of ADA).

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

Value can be transferred between Circulation and Deposits.
Value can also be transferred to the Fees, but Fees can only
be increased by this transition.

**Property** The value (Circulation + Deposits + Fees) increases by the sum
of the withdrawals in the transaction. Note that Circulation decreases
due to transaction fees and certificate deposits, and can increase
through the certificate refunds.

**Property** Fees does not decrease.

### DELEG, POOL, DELPL

Pots in scope: Rewards

**Property** The rewards to do not change (both as an aggregated value
and as individual balances).

*Note:* here we consider elements that are not present to have a value of 0. On
the implementation there is a difference between an element which is not present
in the rewards map and an element with a 0 rewards balance.

### DELEGS

Pots in scope: Rewards

**Property** The reward pot is decreased by the sum of the withdrawals in the
transaction (in the environment).

### POOLREAP

Pots in scope: All

This transition returns a portion of the the pool certificate deposit to the correct
reward address, provided it is still registered. Otherwise it is given to the treasury.
The total is deducted from the deposit pot.

**Property** The value Deposits is non-negative.

**Property (Full Preservation of ADA)**
The value (Circulation + Deposit + Fees + Treasury + Rewards + Reserves) is the same
before and after the transition.

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

**Property** The Deposits value decreases by the amount that Fees increases.

### NEWPP

Pots in scope: Circulation, Deposits, Fees, Treasury, Reserves

The new protocol parameter transition adjusts the deposit pot to meet
the current obligation, and the difference is made up by the reserves.

**Property** The value (Deposits + Reserves) is the same
before and after the transition. Note that it is possible for Deposits
to increase or decrease.

**Property** The values Circulation, Fees, and Treasury do not change.

### EPOCH

Pots in scope: All

With respect to the pots, the EPOCH transition uses each of the following
once: SNAP, POOLREAP, and NEWPP.

**Property (Full Preservation of ADA)**

### TICK

Pots in scope: All

With respect to the pots, the TICK transition uses each of the following
once: NEWEPOCH and RU.

**Property (Full Preservation of ADA)**

### CHAIN

Pots in scope: All

With respect to the pots, the CHAIN transition uses each of the following
of HEAD, PTRCL, and BBODY.

**Property (Full Preservation of ADA)**

### Slots and Epochs

As an easy consequence of the Full Preservation of ADA for the CHAIN
transition is the following:

**Property (Full Preservation of ADA)** The total amount of ADA in the system
(Circulation + Deposit + Fees + Treasury + Rewards + Reserves),
remains constant at each slot and at each epoch.

# Time Traveling Header Properties

We need to adapt properties 1 -3, from section 8 of the byron chain spec, to Shelley.

# Update Properties

**Property**
There can be at most one change to the protocol parameters per epoch.
Moreover, the protocol parameter update state is always empty at begining of epoch.

**Property**
If there are no pending future application versions,
there will not be a change to the version for at least SlotsPerEpoch.

**Property**
Updating the software versions, without updating the protocol version,
results in no change to the transition systems.
Note that changes to the transition system resulting from a new
protocol version will be difficult to state formally, since this
depends on logic in the software changing the ledger rules.

**Definition**
Let **num-genesis** be the number of genesis nodes
(concretely this value is seven).

**Definition**
Let **quorum** be the number of genesis nodes needed for consensus
on votes (concretely this value is five).

**Property**
If there are only (quorum -1)-many gen keys active, there can be no new future
application version or protocol parameters.

**Property**

**Property**
The keys (of type Slot) of the following two mappings are always past the current slot:
the future application versions (favs) and the future genesis delegation mapping (fGenDelegs).
The favs slots can appear in any current or future epoch, but the fGenDelegs slots
can be at most one epoch into the future.

**Property**
The size of the mappings PPUpdate, inside the update state, is always at most (num-genesis - 1).

**Property**
The size of the mappings AVUpdate, inside the update state, is always at most num-genesis.

# Epoch Boundary Transition Properties

**Property** The `NEWEPOCH` transition can always be invoked at the epoch boundary
(i.e. when `e = e_l + 1`). Thus, the transitions it depends on, `SNAP`, `POOLREAP`, `NEWPP`,
and `EPOCH`, can always be invoked as well. Note that when no blocks are produced,
the `CHAIN` rule is blocked and `NEWEPOCH` never fires.

Transitions `SNAP`, `POOLREAP`, and `EPOCH` have no preconditions in the
antecedents of their rules. `NEWPP` has two associated rules, and the disjunction of the
preconditions in these rules is a tautology. We justify
the non-blocking of these rules by this reasoning.

# Deposits Properties

**Property**
The deposits pot is always greater that the current obligation
(ie the total amount of coin needed to refund every stake registration and
pool registration certificate that are currently registered).
In particular, the UTXO and POOLREAP rule can never result in a
negative value for deposits.

# Staking Properties

**Consistency Property for Boundary Case**
If no stake keys are registered, the rewards from the reward update
will always sum to zero.

**Property**
If no stake pools are registered, the rewards from the reward update
will always sum to zero.

**Property**
The sum of stake in the stake snapshots is always at most forty-five billion ADA.

**Property**
The following delegation mappings always has the same size:
`stkCreds`, `rewards`, and `ptrs`.
Moreover, the key set of `stkCreds` is the same
as the range of `ptrs`, which also corresponds one-one with the reward addresses
in `rewards`. Finally, the key set of `delegations` is a subset of that of `stkCreds`.

**Property**
If all stake keys and pools deregister, then, assuming that no one registers anything,
by epoch `e+1`, where `e` is the max epoch in the stake pool retirement mapping,
the delegation state will be nearly empty. More precisely,
the mappings `stkCreds`, `rewards`, `delegations`, `ptrs`, `stpools`, `poolParams`,
and `retiring` are all the empty map.
(The map `cs` will have size seven, for the genesis keys.)

# Genesis Node Property

**Property**
The size of the genesis delegation mapping `genDelegs` is always num-genesis.
Note that the value num-genesis can be given as the size of the
mapping inherited from Byron.


# Entropy Properties

**Consistency Property**
In the absence of the extra entropy parameter,
the epoch nonce is what you get from combining the blocks leading up to it
(and stopping `StabilityWindow`-many slots in the previous epoch).

# Decentralization Properties

**Consistency Property**
The overlay schedule is obeyed: no blocks are produced during the silent blocks,
and only core nodes makes blocks on the overlay slots.

# Rewards Properties

**Property**
At the start of each epoch, the reward update is set to NOTHING.
Moreover, the reward update will change exactly once during the epoch,
to a non-NOTHING value.

**Property**
All members of stake pools that did not meet their pledges will receive zero
rewards for the epoch.

# Block Header Properties

**Consistency Property**
The body size and block body hash listed in the block header are correct.
Correct refers to the two predicates given in the BBODY transition.

# Block Count Properties

**Property**
The number of blocks made in an epoch is equal to number of active overlay slots plus
the sum of the values in the `BlocksMade` mapping.

**Property**
The number blocks made in an epoch is never greater than the number of slots in an epoch.

# Authorization Properties

TODO - Without just restating predicates already in our rules, how can we
state properties stating that UTxO transfer, certificates, etc, are properly authorized?


# Praos Properties

**_The following Properties are taken from the Ouroboros Praos Document.  Not all of them will reflect
into ledger properties, but we should record them somewhere since they will be overall concerns that
should be driving tests/proofs. _**  

Persistence and Liveness seem to be the key properties of interest.

Some questions:

What does semisynchronous actually mean?  
What implications does this protocol have for performance?
What are acceptable values for various system protocol parameters?

The following are examples of things that should be part of some overview document

(1) potentially, multiple slot leaders may be elected for a particular slot (forming a slot leader set);

(2) frequently, slots will have no leaders assigned to them; and

(3) a priori, only a slot leader is aware that it is indeed a leader for a given slot; this assignment is unknown to all the other stakeholders—including other slot leaders of the same slot—until the other stakeholders receive a valid block from this slot leader.



**Independent aggregation property (Property 2)**

Page 10:  The probability of a stakeholder becoming a slot leader in a particular slot is independent of whether this stakeholder acts as a single party in the protocol, or splits its stake among several “virtual” parties.

_This is a technical property that may have some deeper implications.
Invariance of selection rule under arbitrary reassignment of stake._

**Strong consistency between theoretical and real world experiments**

Page 11: Any property of the protocol that we prove true in the hybrid experiment (such as achieving common prefix, chain growth and chain quality) will remain true (with overwhelming probability) in the setting where FVRF and FKES are replaced by their real-world implementations—in the so-called real experiment (p.11).  Argued in Theorems 1 & 2.


**"Small" Divergence**

Page 16: With high probability, the characteristic strings induced by protocol πSPoS have small divergence and hence provide strong guarantees on common prefix.

_"small" needs to be quantified - is this an absolute measure, or relative to a period of time (epoch, system, slot)
- we might be able to ensure specific levels of divergence throughout a slot for example_

**Subadditivity of φ**

Page 17: Proposition 1. The function φf (α) satisfies the following properties. 􏰍􏰎

φ
􏰓α =1−􏰔(1−φ (α))≤􏰓φ (α), α ≥0, fififii
iii
φf(α) = φf(α) ≥ α, α ∈ [0,1].
(5)
(6)

_Proposition 1 needs to be embedded in the spec (true by design and construction?)_

**Common prefix**

Page 19: There is a low probability of violating the common prefix condition.

Theorem 5 (Common prefix). Let k,R,∆ ∈ N and ε ∈ (0,1). Let A be an α-dominated adversary against the protocol πSPoS for some α satisfying α(1−f)∆ ≥ (1+ε)/2. Then the probability that A, when executed in a ∆-semisynchronous environment, makes πSPoS violate the common prefix property with parameter k throughout a period of R slots is no more than exp(ln R + ∆ − Ω(k)). The constant hidden by the Ω(·)-notation depends on ε.

_This is a key property.  It may be necessary to test this rather than proving it.  It should be embedded by design in the spec.  Note that the three properties in this section use exponentials. How does this relate to the non-integer calcs?_


**Chain growth**

Page 20: The length of the chain grows by at least the number of slots.

Theorem 6 (Chain growth). Let k, R, ∆ ∈ N and ε ∈ (0, 1). Let A be an α-dominated adversary against the protocol πSPoS for some α > 0. Then the probability that A, when executed in a ∆-semi- synchronous environment, makes πSPoS violate the chain growth property with parameters s ≥ 4∆ and τ = cα/4 throughout a period of R slots, is no more than exp (−cαs/(20∆) + ln R∆ + O(1)), where c denotes the constant c:=c(f,∆)=f(1−f)∆.

_This is a key property that is worth verifying/proving.  It might also form the basis for progress/productivity._



**Chain quality**

Page 21: There is a low probability of violating the chain property condition.

Theorem 7 (Chain quality). Let k, R, ∆ ∈ N and ε ∈ (0, 1). Let A be an α-dominated adversary against the protocol πSPoS for some α > 0 satisfying α(1−f)∆ ≥ (1+ε)/2. Then the probability that A, when executed in a ∆-semisynchronous environment, makes πSPoS violate the chain quality property with parameters k and μ = 1/k throughout a period of R slots, is no more than exp(ln R − Ω(k)).

_Another key property.  Approach should be similar to common prefix._

**Probability Calculations**

Page 21 contains two displayed conditions that could be used to generate tests to confirm correct probabilities.

R∆ exp(−cα(s − 3∆)/(20∆)) = exp(−cα(s − 3∆)/(20∆) + ln R∆)

Pr [g(x) ≤ b(x)] ≤ Pr [g(x) ≤ b(x)]
x←Df x←Dαf

**Theorem 8**

Page 22: Every adaptive adversary A that corrupts at most (1−α)-fraction of stake throughout the whole execution is α-dominated.

_Is this related to the slot leadership?
Is the corruption monitored/verified/avoided somehow?  Or is it just a condition/assumption?_


**Participation**

Page 25: It is sufficient for an honest stakeholder to join at the beginning of each epoch, determine whether she belongs to the slot leader set for any slots within this epoch (using the Eval interface of FVRF), and then come online and act on those slots while maintaining online presence at least every k slots.

_It's not obvious from this whether a non-participating actor could disrupt the system (e.g. by
causing timeslot problems).  Does non-participation imply loss of benefit?  Do we need to assure
adequate participation by all stakeholders?  Can we use the assumption to help ensure progress?
Also what happens if a slot leader fails to participate?
Some of these discussions may be in the design document?_

**Theorem 9**

Page 26: The protocol πDPoS, with access to Fτ,r , with τ ≤ 8k/(1 + ε) satisfies persistence with parameters RLB
k and liveness under specific conditions.

_Theorem 9 is the main persistence and liveness property.
I assume this has been evaluated empirically (graph of probabilities/simulation).
Are there any false independence assumptions?
Liveness refers to honest actors and transactions.  If there are no honest transactions,
there will presumably be no growth?
What assumptions are being made here in terms of deadlock, availability etc.
(these could have a major impact on system viability_


**Key pair correctness**

Page 30: Correctness: for every key pair (KES.sk1,KES.vk) ← Gen(1k,T), every message m and every
time period j ≤ T , VerifyKES.vk (m, SignKES.skj (m)) = 1

_Check assumptions.  The proof should be by construction and design if the spec is consistent with the Praos document._


**Test Properties**

Page 33/34:  Various possible attacks are given.  The protocol implementation should defend against these.

_These look like ways to drive test case generation_


#Multi-signature properties

**Sufficient Signatures are Provided to authorise Multi-Signature Transactions**

Outputs of transactions that require multiple signatures will be "locked" against use until at least the
required number of signatures is provided.

_This should come by construction from the rules in the multi-sig spec._
