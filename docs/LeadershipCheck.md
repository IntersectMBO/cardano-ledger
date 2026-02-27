# Background on Leadership Checks in Praos

In Praos, we want the target of 1 block every 20 seconds. This is why we have in the Shelley genesis file:

```json
"activeSlotsCoeff": 0.05  // = 1 / 20 [blocks / sec]
```

## The Lottery Mechanism

To let this desired rate of block production emerge, each node runs a local lottery at each slot where **1 lovelace = 1 ticket**.

Note that each lovelace is fungible and we only know how much stake a pool has. Questions like "which lovelace won that slot" thus make no sense. Since we cannot run a direct lottery that selects one specific winning lovelace from the total stake, we need an alternative approach that produces the same probability distribution. The solution is to model each lovelace as an independent trial with some win probability `x`, which is precisely a **repeated Bernoulli trial**.

An important consequence of this emulation is that **multiple pools can win the same slot** (since each pool independently checks if they won). However, on average we still achieve the target rate of 1 block every 20 seconds across the network.

## Probability Derivation

A Bernoulli trial has two outcomes (win with probability `x`, or lose with probability `1-x`).

Then the odds that a pool with n tickets wins is given by:

```
P("Pool with n tickets wins") = 1 - P("None of the n tickets win")
                              = 1 - P("one ticket does not win")^n
                              = 1 - (1-x)^n
```

Now we should remember that we target the block rate of `f = 0.05 [blocks/sec]`. So in the hypothetical case where one pool holds all stake, we should get that:

```
P("Pool with all stake wins") = 1 - P("Pool has no tickets that win")
                              = 1 - (1-x)^n
                              = 1 - (1-x)^totalActiveStake
                              = f
```

If we solve for `x` we get that the probability that 1 ticket wins in terms of our desired `f` is given by:

```
x = 1 - (1 - f)^(1/totalActiveStake)
```

Now back to the case where stake is distributed, we can use this ideal definition of `x` just derived via:

```
P("Pool with n tickets wins") = 1 - (1- x)^n
                              = 1 - (1 - (1 - (1 - f)^(1/totalActiveStake)))^n
                              = 1 - (1 - f)^(n/totalActiveStake)
```

And if we define `σ = (n / totalActiveStake)`, then we get:

```
P("Pool with stake ratio σ wins") = 1 - (1 - f)^σ
```

## Implementation Using VRF

So how do we use this formula to locally let a pool emulate that it has a winning ticket and be able to verify that computation when validating the block corresponding to that slot?

This is done using a **VRF (Verifiable Random Function)**, which is a keyed hash function. It takes as input:
- A message/preimage
- A VRF secret key of the pool

It outputs:
- A hash/digest
- A proof

The hash/digest has similar properties to a normal hash like collision resistance, but also that **it's uniformly distributed**! If the message/preimage changes, the output digest also completely changes.

Very important: this VRF also allows anyone to verify that the VRF public key of the pool and only that pool could have created that digest (using the proof). This method binds the producer to the outcome, and thus this digest can be viewed as a random draw of the pool from a uniformly distributed random variable.

## The Leadership Check

The last trick is noting that we can compare this uniformly distributed picked value (which is a value between `0` and `2^{the digest size in bits}`) to the above Bernoulli trial probability. If it is below that value, a pool had the odds in its favor as the uniformly distributed value was below the repeated Bernoulli trial odds.

So if we map the VRF digest `out` to a value `0 ≤ p ≤ 1` by normalizing it, we get that we should check:

```
p < 1 - (1 - f)^σ    AND    verifyVRFOutput(Pubkey, out, proof)
```

Where:
- `p` is the normalized VRF output
- `f` is the active slots coefficient (e.g., 0.05)
- `σ` is the pool's stake ratio
- `verifyVRFOutput` ensures the VRF proof is valid for the given public key
