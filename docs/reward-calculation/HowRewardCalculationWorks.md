# Introduction

This document explains how the Reward Calculation is implemented in the Cardano Ledger. We strongly
suggest the reader read and understand chapter 11 "Rewards and the Epoch Boundary" in the 
[Shelley Spec](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/shelley-ledger.pdf) that
covers related material on the phases of the reward cycle.  
[Additional info from the spec](https://intersectmbo.github.io/formal-ledger-specifications/site/Ledger.Conway.Specification.Rewards.html)



 The Reward Calculation has 5 parts.

# How The Reward Calculation might work if time did not matter

Rewards are analagous to interest on a bank account. If you deposit funds in a bank account, periodically the bank pays you a percentage
of the amount on deposit as interest. So over time your account balance grows.

In the Cardano Ledger, the UTxO plays the role of bank accounts, and the pool operators play the role of banks. Pools make money by
making blocks on the chain. For each block they make, they get a fixed percent of the fees collected for processing the
transactions that make up the block. The probability that a pool is chosen to make a block, is proportional to the amount
of Coin in the UTxO that has been delegated to that pool. So it is in the interest of each Pool to entice the owners of UTxO entries
to delegate the Coin in their UTxO entries to that Pool, inorder to increase their percentage of the total delegated Coin. 
The enticement is the promise of Rewards to the UTxO entry owners that delegate to their Pool. 
The actual amount of rewards is computed by a computation that involves PoolParameters that can be set differently by each pool.
Rewards are paid at each epoch boundary. An epoch is roughly 5 days, so rewards are paid out about every 5 days.

What data must be available to compute rewards.

1. The UTxO. Each `UTxO` entry records 
      - The entry owner. This is a Haskell datatype (Credential 'Staking), which is a cryptographic hash of the owner's identity
      - The amount of the entry in Coin. This is always non-zero and positive.
      - Staking information about which Pool (if any) the UTxO entry is delegated to. This may be deliberately left blank, or it is
        a Haskell datatype (KeyHash 'Pool), which is another cryptographic hash, this time of the Pool owner's identity
      - Other information not related to the Reward calculation

2. Information about which staking credentials are registered to participate in staking, and which Pool they delegate to.
This is a Haskell Datatype (Map (Credential 'Staking)  (KeyHash StakePool)), which we will call the `stakeDelegationMap`.
Only registered credentials will receive rewards.

3. Information about which pool operators are currently registered to operate pools, and the parameters under which the pools operate.
This is a Haskell Datatype (Map (KeyHash StakePool) PoolParams), which we will call the `poolParamsMap` . Rewards for UTxO entries delegated
to non registered Pools are distributed to either the Treasury or the Reserves.

4. Information about which pools are currently scheduled to retire, and when.
This is a Haskell Datatype (Map (KeyHash StakePool) EpochNo), which we will call the `retiringMap` .

If time was not an issue, at the end of each epoch, we would compute the rewards in several steps. For each step, we either give 
a link to the Ledger code base where that code is illustrated, or we give a short Haskell function that illustrates 
how it might be computed in the ShelleyEra. Other eras would be very similar, but the TxOut for that Era would have more
fields than just the Addr and Coin.


1. Aggregate the total `Coin` in the `UTxO` for each (Credential 'Staking) that appears somewhere in the UTxO, resulting in
a (Map (Credential 'Staking) Coin). This only requires the UTxO. At this point we do not care if the Credential is registered
in the `stakeDelegationMap` . We call this the `aggregateStakeMap` .

```
-- | Loop through all the TxOut in the UTxO, if the Addr in the TxOut has a `StakeRefBase` credential
--   then add the (cred,coin) pair to the answer map.
aggregateStakeMap :: UTxO ShelleyEra -> Map (Credential 'Staking) Coin
aggregateStakeMap (UTxO utxoMap) = loop (Map.toList utxoMap) Map.empty
  where loop ((_,ShelleyTxOut @ShelleyEra (Addr _network _payCred stakeRef) coin):more) aggrAns =
           case stakeRef of
             StakeRefBase cred -> Map.insertWith (<>) cred coin aggrAns
             _ -> loop more aggrAns
        loop _ aggrAns = aggrAns   
```


2. For each pool operator compute the set of Credentials that delegate stake to that pool, resulting in
a (Map (KeyHash 'StakePool) (Set (Credential 'Staking))). This requires the UTxO and the stakeDelegationMap. 
At this point we don't care if the pool is registered in the `poolParamsNap`. This is called the `whoPaysMap`.

```
-- | Loop through all the TxOut in the UTxO, if the Addr in the TxOut has a `StakeRefBase` credential
--   then lookup if that credential delegates to some Pool, if it does, add the credential 
--   to the answer map for that Pool.
whoPaysMap :: UTxO ShelleyEra -> 
              Map (Credential 'Staking) (KeyHash StakePool) -> 
              Map (KeyHash 'StakePool) (Set (Credential 'Staking))
whoPaysMap (UTxO utxoMap) stakeDelegationMap = loop (Map.toList utxoMap) Map.empty
  where loop ((_,ShelleyTxOut @ShelleyEra (Addr _network _payCred stakeRef) coin):more) aggrAns =
           case stakeRef of
             StakeRefBase cred -> case Map.lookup cred stakeDelegationMap of
                     Nothing -> loop more aggrAns
                     Just keyHashPool -> Map.insertWith (Set.union) keyHashPool (Set.singleton cred) aggrAns 
             _ -> loop more aggrAns
        loop _ aggrAns = aggrAns  
```        

3. Aggregate the total Coin delegated by registered credentials to each registered Pool, resulting in 
(Map (KeyHash 'StakePool) Coin). This requires the results from steps 1 and 2. This is called the `stakeDistributionMap`.
This will not be used in the rest of the Reward calculation, but will be used to compute the ercentage of stake controlled by each pool, to choose Pools to make blocks in a following epoch.

```
stakeDistrMap :: Map (KeyHash 'StakePool) (Set (Credential 'Staking)) -> 
                 Map (Credential 'Staking) Coin -> 
                 Map (KeyHash 'StakePool) Coin
stakeDistrMap whoPaysMap aggregateStakeMap = Map.map setCredToCoin whoPaysMap
   where setCredToCoin :: Set (Credential Staking) -> Coin
         setCredToCoin set = loop (Set.toList set) (Coin 0)
           where loop [] totalcoin = totalcoin
                 loop (cred : more) totalcoin = case Map.lookup cred aggregateStakeMap of
                    Nothing -> loop more totalcoin
                    Just coin -> loop more (coin <> totalcoin)
```


4. For each Pool in the domain of `whoPaysMap`, and each Credential in the range, compute a Reward owed to that Credential by that Pool,
resulting in (Set Reward). This requires the `whoPaysMap` the `poolParamMap`, the `stakeDelegationMap`, and the `aggregateStakeMap`, because the amount in the reward depends on the PoolParameters of the Pool and the Coin amount delegated by that Credential to that Pool. We must also check
that each (Credential 'Staking) and (KeyHash 'StakePool) are currently registered, to make a valid Reward.
[Source code for computing a Reward from Coin and PoolParam values.](https://github.com/IntersectMBO/cardano-ledger/blob/a7fb33a2b2922933eb6dd7e2420363291f6d4903/eras/shelley/impl/src/Cardano/Ledger/Shelley/Rewards.hs#L260)  
[Source code for `rewardStakePoolMember`](https://github.com/IntersectMBO/cardano-ledger/blob/a7fb33a2b2922933eb6dd7e2420363291f6d4903/eras/shelley/impl/src/Cardano/Ledger/Shelley/RewardUpdate.hs#L251)

5. Apply the Rewards to the `rewardsBalanceMap`, increasing the balance for each Credential.
[Source code for applying the Rewards](https://github.com/IntersectMBO/cardano-ledger/blob/a7fb33a2b2922933eb6dd7e2420363291f6d4903/eras/shelley/impl/src/Cardano/Ledger/Shelley/LedgerState/IncrementalStake.hs#L97)

The problem is, as time passes the `UTxO`, the `stakeDelegationMap`, and the `poolParamsMap` are subject to change when a new Block is made.
So unless the whole calculation can be completed in the time between the last block in an epoch and the first block in the next
epoch, the calculations could be inaccurate. That is why we snap shot those values, before we begin computing the Rewards. The time to complete the reward calculation is currently in the order of many 10's of seconds, and the time to distribute the rewards several seconds. The time between blocks is an order of magnitude smaller than that, so we must resort to strategies that compute and apply the rewards in parallel with the progress
of block production.


# How Reward Calculation Works in the implementation

1) Random selection of the pool operators who will be assigned the opportunity to mint each block in the next Epoch.
   Each block is assigned a single random StakePool, with likelihood proportional to the stake delegated to that StakePool.
   This proportion is called the Stake Distribution, which can change with each applied transaction.
   To get a stable Stake Distribution we need to make a Snapshot at the point in the Epoch, 
   when all blocks have been assigned a StakePool. This point is called the stability point, 
   as no assignment will change from this point to the end of the block. So the idea
   is to take a snapshot anytime after the stability point, and use it in the rest of the Reward Calculation.
2) Registration (using certificates) of User's Stake credentials and StakePools can take place in the same
   Epoch that the StakePools are assigned blocks. Only registered Users and
   StakePools participate in the Reward Calculation. 
3) Snapshots are taken at the end of this Epoch, remembering 
     - the current registration status of Users and StakePools.
     - the Stake Distribution used for selection of which StakePools will make blocks in the next epoch
     - the Fee Pot
     - other data that cannot change over the Reward Calculation
   when the Reward Calculation is computed.
4) The calculation begins, using the 4 steps outlined below, resulting in a RewardUpdate.
5) The Reward Update is applied to the current NewEpochState, distributing Rewards to those stake credentials
   that were registered to StakePools, and only where the corresponding StakePool was registered as a pool operator.

The Reward system works as follows. With every transaction, the transaction author has the opportunity
to delegate the `Stake` (the `Coin` part of each `TxOut` in the transaction) to one of the `StakePools`
by adding a non null `StakeReference` to the `Addr` part of the `Output`

At the end of an `Epoch`, registered stake, that is delegated in the address of a TxOut in the UTxO,
is marked as deserving a reward in a `RewardUpdate`. Applying a `RewardUpdate`  means increasing the amount of `Coin` associated
with the `StakeCredential` in the Rewards map. This is similar to earning interest on
the amount of money in a bank deposit. Here are the basic data structures that encode this information


```
-- | A TxOut can be made in every Era using this function, other functions can 
--   add other (non staking) features to the TxOut
mkBasicTxOut :: Addr -> Value era -> TxOut era

data StakeReference
  = StakeRefBase !(Credential 'Staking)
  | StakeRefPtr !Ptr  -- Ptr are no longer allowed in the Conway and later Eras
  | StakeRefNull


data Addr
  = Addr Network (Credential 'Payment) StakeReference
  | AddrBootstrap BootstrapAddress  
       -- `AddrBootstrap` has an implicit `StakeRefNull` StakeReference
       -- No (Credential `Staking) are delegated in a AddrBootstrap

data RewardAccount = RewardAccount
  { raNetwork :: !Network
  , raCredential :: !(Credential 'Staking)
  }

```  

In order for rewards to be paid, the (Credential 'Staking) must be associated with two things

1. A `(KeyHash 'StakePool)`. The hash of the entity that will pay the reward
2. A `RewardAccount`. Information about where to store the reward

The `StakePool` must also be associated with a Pool operator for rewards to be paid.

These associations are created by registering the Credential and the StakePool using various
Certificates, that are part of the current (or earlier) transactions that were applied to the system.

The actual values are stored in internal maps found in the type family `CertState` stored in the ledger state

```

Map (Credential 'Staking) (StrictMaybe (KeyHash 'StakePool)) -- The User registration map
Map (Credential 'Staking) Coin                               -- The User Rewards map
Map (KeyHash 'StakePool) PoolParams                          -- The StakePool registration map
```

A `Reward` contains information about a computed reward, that has yet to be applied to the internal Rewards map.

```
data RewardType = MemberReward   -- A User
                | LeaderReward   -- A StakePool operator
  
data Reward = Reward
  { rewardType :: !RewardType              -- to be paid to a User or a StakePool operator
  , rewardPool :: !(KeyHash 'StakePool)    -- which Stakepool operator will pay this reward
  , rewardAmount :: !Coin                  -- the amount of the reward.
  }
```  

The Reward calculation has several parts

1. Sum the total `Coin` for each unique `StakeReference` in the `UTxO`

    ```
    UTxO era -> Map (Credential 'Staking) Coin
    ```

2. For each `StakePool` compute the set of `StakeReference` that delegate stake to that pool.

    ```
    Map (KeyHash 'StakePool) (Set (Credential 'Staking))
    ```

3. For each pool and each stake reference to that pool, compute a `Reward` for that reference

    ```
    Map (Credential 'Staking) (Set Reward)

4. Pay the rewards for every pool and stake reference
   
    ```
    Map (Credential 'Staking) (Set Reward) -> EpochState era -> EpochState era
    ````

## What makes it complicated and hard

The reward calculation takes place on the Epoch boundary, each node has at most several seconds to compute all 4 parts.
Using naive strategies to compute the four steps will far exceed few second budget available. 
The problem is that the amount of data is very large.

1. The UTxO has tens of millions of entries
2. The set of staking credentials has millions of entries
3. The number of stake pools numbers in the thousands.


In order to address this problem the reward calculation uses several strategies.

1. Use incremental computation to compute the changes to the UTxO and total coin for each StakeReference 
in lock step, so step one is not necessary. We call this the "InstantStake" calculation, as it computes
two different things (both stored in the UTxOState) simultaneously 
     - The InstantStake `(Map (Credential 'Staking) Coin)
     - The UTxO

There are strong invariants that must be maintained between these two maps.   

2. Spread the calculation over several Epochs, perhaps computing each part at one Epoch boundary, and delaying
the actual payment for 2, 3 , or 4 Epochs. This is what is discussed in chapter 11 "Rewards and the Epoch Boundary" in the 
[Shelley Spec](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/shelley-ledger.pdf) Shelley Spec.

  

3. Structure expensive computation into many pure steps that can be started and stopped when the node is otherwise idle.
   we call this pulsing, and it too must have many invariants.
     - It must be a pure computation
     - The computation must be serializeable, because if a node fails, we must be able to restart this 
       computation from the last checkpoint.
     - It must compute the same result as if we ran it all at once, not breaking it into many starts and stops. 

We look closely at each of these strategies in turn, studying the data structures and algorithms that implement them.

## The InstantStake strategy.

This strategy computes the InstantStake `(Map (Credential 'Staking) Coin)` and the `UTxO` simultaneously.
It is an instance of incremental computation, as the InstantStake is a pure function of the `UTxO`.
It works by observing every change to the `UTxO`, and if that change could alter the `InstantStake`, it makes
corresponding changes to the InstantStake that keep the two in lockstep. The `UTxO` and the `InstantStake` are 
both fields in the `UTxOState`. 

```
data UTxOState era = UTxOState
  { utxosUtxo :: !(UTxO era)
  , utxosDeposited :: !Coin
  , utxosFees :: !Coin
  , utxosGovState :: !(GovState era)
  , utxosInstantStake :: !(InstantStake era)
  , utxosDonation :: !Coin
  }
```  

A valid (or self-consistent) `UTxOState{utxosUtxo, utxosDeposited , utxosFees  , utxosGovState, utxosInstantStake, utxosDonation}`
maintains an invariant between the `utxosUtxo` and `utxosInstantStake` fields. The `utxosInstantStake` field is
the aggregation of Coin over the StakeReferences in the UTxO. It can be computed by a pure
function from the `utxosUtxo` field.  This is captured by the methods of class `EraStake` because the computations
are slightly different in Eras that support `Ptr` (Shelley, Allegra, Mary, Alonzo, Babbage) from Eras
that do not support Ptr (Conway, Dijkstra).

```
class EraStake era
  where
  -- | This is the current stake in the system. The important part of this stake is that not all of
  -- it is active. Any stake credential that is not registred will not contribute to the active
  -- stake, however it will be part of the InstantStake. Throughout an epoch it is not relevant
  -- which part of the stake is active, because it is only when we take the snaphot that we resolve
  -- all the active stake.
  type InstantStake era = (r :: Type) | r -> era

  -- | Add new UTxO to the `InstantStake`. This is invoked for every new TxOut that is added to the
  -- ledger state
  addInstantStake :: UTxO era -> InstantStake era -> InstantStake era

  -- | Delete spent UTxO from the `InstantStake`. This is invoked for every TxOut that is removed
  -- from the ledger state
  deleteInstantStake :: UTxO era -> InstantStake era -> InstantStake era  
```

The invariant we need to maintain is

```
invariant :: UTxOState era -> Bool
invariant UTxOState{utxosUtxo, utxosDeposited , utxosFees  , utxosGovState, utxosInstantStake, utxosDonation} =
    utxosInstantStake == addInstantStake utxosUtxo mempty
```  

To compute the UTxO and the InstantStake in lockstep, we observing every change to the UTxO, and if that change 
could alter the InstantStake, we make corresponding changes to the InstantStake that keep the invariant True.
Luckily the UTxO is only ever changed in the STS instance for `(EraRule "UTXO" era)`,
and this is captured in one function `updateUTxOState`. Here is the function, with changes to
the non-(UTxO and InstantStake) fields elided for brevity.

```
updateUTxOState pp utxos txBody certState govState depositChangeEvent txUtxODiffEvent = do
  let UTxOState {utxosUtxo, utxosDeposited, utxosFees, utxosDonation} = utxos
      UTxO utxo = utxosUtxo
      !utxoAdd = txouts txBody -- These will be inserted into the UTxO
      {- utxoDel  = txins txb ◁ utxo -}
      !(utxoWithout, utxoDel) = extractKeys utxo (txBody ^. inputsTxBodyL)
      {- newUTxO = (txins txb ⋪ utxo) ∪ outs txb -}
      newUTxO = utxoWithout `Map.union` unUTxO utxoAdd
      deletedUTxO = UTxO utxoDel
  depositChangeEvent depositChange
  txUtxODiffEvent deletedUTxO utxoAdd
  pure $!
    UTxOState
      { utxosUtxo = UTxO newUTxO
      , utxosInstantStake =
          deleteInstantStake deletedUTxO (addInstantStake utxoAdd (utxos ^. instantStakeL))
      }
```      

We noted above that the type family InstantStake is different for different Eras. This is because
Eras before Conway supported `Ptr`s. A `Ptr` is a reference to an `Addr`, and aggregating the `Coin` for each
Stake address must follow the reference. Unfortunately, the references may change so the `InstantStake` for
Eras with `Ptr`s must store the references and only resolve them when the aggregation is used in step 2. Here
are the two `EraStake` instances.

### Shelley, Allegra, Mary, Alonzo, Babbage InstantStake

The type family `InstantStake` datatype. Note how the `sisPtrStake` field stores the `Ptr` for resolution
later, when we know what the address the `Ptr` points to.

```
data ShelleyInstantStake era = ShelleyInstantStake
  { sisCredentialStake :: !(Map.Map (Credential 'Staking) (CompactForm Coin))
  , sisPtrStake :: !(Map.Map Ptr (CompactForm Coin))
  }
```  

The function `applyUTxOShelleyInstantStake` can be used for either adding or deleting a `Coin` to the aggregation.
The function parameter `f`, directs how to make an appropriate action. This action will be effected by passing the
partial application of the local function `(keepOrDeleteCompact coinFromTxOut)` to the function `Map.alter`
`keepOrDeleteCompact` will use the function `f` to take one of the following actions.

1. Override the Coin in a (Credential,Coin) entry already in the map with a new Coin 
2. Insert a new (Credentia,Coin) pair in the map
3. Delete a (Credentia,Coin) pair from the map


```
applyUTxOShelleyInstantStake ::
  EraTxOut era =>
  (CompactForm Coin -> CompactForm Coin -> CompactForm Coin) ->
  UTxO era ->
  ShelleyInstantStake era ->
  ShelleyInstantStake era
applyUTxOShelleyInstantStake f (UTxO u) instantStake =
  Map.foldl' accum instantStake u
  where
    keepOrDeleteCompact :: CompactCoin -> Credential `Staking -> (Maybe CompactCoin -> Maybe CompactCoin)
    keepOrDeleteCompact new cred = case cred
      Nothing ->                    -- The `cred` is not in the map, so we might insert or leave it not in the map.
        case new of
          CompactCoin 0 -> Nothing  -- The aggregation is `0`, so Nothing means do not add it
          final -> Just final       -- The aggregation is not `0`, so Just means add the pair (cred,final) to the map  
      Just old ->                   -- The `cred` is in the map, with value `old`
        case old `f` new of         -- combine the two with `f`
          CompactCoin 0 -> Nothing  -- The aggregation is `0`, so Nothing means do not add it
          final -> Just final       -- The aggregation is not `0`, so Just means add the pair (cred, old `f` new) to the map 
    accum ans@(ShelleyInstantStake {sisCredentialStake, sisPtrStake}) out =
      let cc = out ^. compactCoinTxOutL     -- lookup the Coin inside the TxOut
       in case out ^. addrTxOutL of         -- lookup the `Addr`
            Addr _ _ (StakeRefPtr stakingPtr) ->
              ans
                { sisPtrStake = Map.alter (keepOrDeleteCompact cc) stakingPtr sisPtrStake -- Resolve the `Ptr`
                }
            Addr _ _ (StakeRefBase stakingKeyHash) -> -- 
              ans
                { sisCredentialStake = Map.alter (keepOrDeleteCompact cc) stakingKeyHash sisCredentialStake
                }
            _other -> ans
```            

So we see by using either `(+)` or `(-)` as the argument to `applyUTxOShelleyInstantStake` we can 
either remove `Coin` value from the aggregation, or add `Coin` value to the aggregation.

```
addShelleyInstantStake ::
  EraTxOut era => UTxO era -> ShelleyInstantStake era -> ShelleyInstantStake era
addShelleyInstantStake = applyUTxOShelleyInstantStake (coerce ((+) @Word64))

deleteShelleyInstantStake ::
  EraTxOut era => UTxO era -> ShelleyInstantStake era -> ShelleyInstantStake era
deleteShelleyInstantStake = applyUTxOShelleyInstantStake (coerce ((-) @Word64))

instance EraStake ShelleyEra where
  type InstantStake ShelleyEra = ShelleyInstantStake ShelleyEra
  addInstantStake = addShelleyInstantStake
  deleteInstantStake = deleteShelleyInstantStake
```  

### Conway Dijkstra InstantStake

The type family `InstantStake` for Eras after Babbage (that have no `Ptr`s)
is simpler, since it does not need the Map of `Ptrs` that need to be resolved.

```
newtype ConwayInstantStake era = ConwayInstantStake
  { cisCredentialStake :: Map.Map (Credential 'Staking) (CompactForm Coin)
  }
```

The application function for `Conway` `functionapplyUTxOConwayInstantStake` is similar
to the application function for `Shelley`  `applyUTxOShelleyInstantStake`, except it
has one fewer cases, as it is not possible for and `Addr` in the `Conway` era to have  
`StakeRefPtr` `StakeReference`. 

```
applyUTxOConwayInstantStake ::
  EraTxOut era =>
  (CompactForm Coin -> CompactForm Coin -> CompactForm Coin) ->
  UTxO era ->
  ConwayInstantStake era ->
  ConwayInstantStake era
applyUTxOConwayInstantStake f (UTxO u) instantInstantStake =
  Map.foldl' accum instantInstantStake u
  where
    keepOrDeleteCompact new = \case
      Nothing ->
        case new of
          CompactCoin 0 -> Nothing
          final -> Just final
      Just old ->
        case old `f` new of
          CompactCoin 0 -> Nothing
          final -> Just final
    accum ans@(ConwayInstantStake {cisCredentialStake}) out =
      let cc = out ^. compactCoinTxOutL
       in case out ^. addrTxOutL of                                -- Note only 2 `Addr` cases are necessary
            Addr _ _ (StakeRefBase stakingKeyHash) ->
              ConwayInstantStake
                { cisCredentialStake = Map.alter (keepOrDeleteCompact cc) stakingKeyHash cisCredentialStake
                }
            _other -> ans

addConwayInstantStake ::
  EraTxOut era => UTxO era -> ConwayInstantStake era -> ConwayInstantStake era
addConwayInstantStake = applyUTxOConwayInstantStake (coerce ((+) @Word64))

deleteConwayInstantStake ::
  EraTxOut era => UTxO era -> ConwayInstantStake era -> ConwayInstantStake era
deleteConwayInstantStake = applyUTxOConwayInstantStake (coerce ((-) @Word64))

instance EraStake ConwayEra where
  type InstantStake ConwayEra = ConwayInstantStake ConwayEra
  addInstantStake = addConwayInstantStake
  deleteInstantStake = deleteConwayInstantStake

```
##  Spread the calculation over several Epochs

This strategy follows the advice, if you can't do it in one `Epoch` then do it in four.
It exploits that over an Epoch (approximately 5 days) most nodes are mostly idle. Use some of this idle time
between `Epoch` boundaries to things that cannot do in the few second time budget at the `Epoch` boundary.
Also if you spread it over n-`Epoch`s you have n-(few second budgets) to work with. The complication is that
computation depends on the state of the system at the beginning of the Reward Calculation, and that state
changes many times in the course of the n-`Epochs`. This requires the use of Snapshots, making temporary copies
of things at the beginning of the calculation, that cannot change, as the rest of the system progresses.

It also means the Reward calculation must be pipelined, and that requires a queue of SnapShots (Mark, Set, Go).


##  Tools for breaking the calculation into many pure steps that can be started and stopped.

The module `Data.Pulse` provides a library for breaking a large computation to 
series of smaller ones, which can be scheduled by the library.

```
-- | let T be a Pulse structure. A Pulse struture
--   is abstracted over a monad: m, and an answer type: a,
--   so the concrete type of a pulse structure is written: (T m a).
--   The Pulsable class supplies operations on the structure
--   that allow its computation to be split into many discrete
--   steps. One does this by running: "pulse p" or "pulseM p",
--   depending upon whether the computation is monadic or not,
--   to run a discrete step.  The scheduling infrastructure needs
--   to know nothing about what is going on inside the pulse structure.
class Pulsable (pulse :: (Type -> Type) -> Type -> Type) where
  done :: pulse m ans -> Bool
  current :: pulse m ans -> ans
  pulseM :: Monad m => pulse m ans -> m (pulse m ans)
  completeM :: Monad m => pulse m ans -> m ans
  completeM p =
    if done p
      then pure (current p)
      else do p' <- pulseM p; completeM p
```      
 
In order to make a `Pulesable` instance we need to identify
the monad `m` and the answer type `ans`. The monad type is the
underlying Monad in the STS rules which is `ShelleyBase`, and the answer type
is `RewardAns` which has two maps as components


```
type RewardEvent = Map (Credential 'Staking) (Set Reward)

-- | The result of reward calculation is a pair of aggregate Maps.
--   One for the accumulated answer, and one for the answer since the last pulse
data RewardAns = RewardAns
  { accumRewardAns :: !(Map (Credential 'Staking) Reward)
  , recentRewardAns :: !RewardEvent
  }
```

## The Pulsable Instance

The next step is to make an instance where we define the methods `done`, `current`,  `pulseM` and `completeM`

```
-- | The type of a Pulser which uses 'rewardStakePoolMember' as its underlying function.
--     'rewardStakePool' will be partially applied to the component of type
--     (FreeVars c) when pulsing. Note that we use two type equality (~) constraints
--     to fix both the monad 'm' and the 'ans' type, to the context where we will use
--     the type as a Pulser. The type must have 'm' and 'ans' as its last two
--     parameters so we can make a Pulsable instance.
--     RSLP = Reward Serializable Listbased Pulser
data RewardPulser (m :: Type -> Type) ans where
  RSLP ::
    (ans ~ RewardAns, m ~ ShelleyBase) =>
    !Int ->
    !FreeVars ->
    !(VMap.VMap VMap.VB VMap.VP (Credential 'Staking) (CompactForm Coin)) ->
    !ans ->
    RewardPulser m ans

-- Because of the constraints on the Constructor RSLP, there is really only one inhabited
-- type:  (RewardPulser c ShelleyBase (RewardAns c))
-- All of the instances are at that type. Though only the CBOR instances need make that explicit.

clearRecent :: RewardAns -> RewardAns
clearRecent (RewardAns accum _) = RewardAns accum Map.empty

instance Pulsable RewardPulser where
  done (RSLP _n _free zs _ans) = VMap.null zs
  current (RSLP _ _ _ ans) = ans
  pulseM p@(RSLP n free balance (clearRecent -> ans)) =
    if VMap.null balance
      then pure p
      else do
        let !(steps, !balance') = VMap.splitAt n balance
            ans' = VMap.foldlWithKey (rewardStakePoolMember free) ans steps
        pure $! RSLP n free balance' ans'
  completeM (RSLP _ free balance (clearRecent -> ans)) =
    pure $ VMap.foldlWithKey (rewardStakePoolMember free) ans balance
```    

## The Phases of a pulsing computation

To prevent a huge pause in the Reward Calculation we spread out some of the computation
over many blocks in the idle time of a node. We do this in 3 steps

1. The Start Step of the Reward Calculation is a pure computation, computing and combining some parameters which
become fixed at the time when we reach the stability point. One of these parameters is a Pulser,
i.e. a computation that when pulseM'ed computes a portion of what is required, so that the whole 
computation can be spread out in time. Note that the Start Step can be very cheap, as we just collect the data
that must be held constant, and create the thunks that will execute when the system is pulsed.

2. The Pulse Step. Run the pulser for a bit. It returns a PulsingRewardUpdate, which is one of two things
     - The final result, if the pulser has completed
     - Enough information to restart the pulser for the next pulse
         ```
         data PulsingRewUpdate
            = Pulsing !RewardSnapShot !Pulser -- Pulsing work still to do
            | Complete !RewardUpdate -- Pulsing work completed, ultimate goal reached
         ```       

3. The Complete Step. Clean up loose ends, and return the `RewardUpdate`
which can be used to update the `NewEpochState`

The 3 steps are combined in this function

```
-- | To create a reward update, run all 3 phases
--   This function is not used in the rules, so it ignores RewardEvents
--   but it illustrates the three phases and the looping behavior of the second phase
--   through a recursive call. It is also used in some tests.
createRUpd ::
  forall era.
  (EraGov era, EraCertState era) =>
  EpochSize ->
  BlocksMade ->
  EpochState era ->
  Coin ->
  ActiveSlotCoeff ->
  NonZero Word64 ->
  ShelleyBase RewardUpdate
createRUpd slotsPerEpoch blocksmade epstate maxSupply asc secparam = do
  let step1 = startStep slotsPerEpoch blocksmade epstate maxSupply asc secparam
  (step2, _event) <- pulseStep step1
  case step2 of
    Complete r -> pure r
    Pulsing rewsnap pulser -> fst <$> completeRupd (Pulsing rewsnap pulser)
```           

In the STS system, the three phases are packed into one transition funtion `rupdTransisiton`
which is called on every call to the STS RUPD rule. The three phases
are ordered by observing the current slot and returning a `RewardTiming` value.
[source code](https://github.com/IntersectMBO/cardano-ledger/blob/1b1d6cc83946db56e98e999e07c20e54eca86338/eras/shelley/impl/src/Cardano/Ledger/Shelley/Rules/Rupd.hs#L125)


```
-- | The Goldilocks labeling of when to do the reward calculation.
data RewardTiming = RewardsTooEarly | RewardsJustRight | RewardsTooLate

determineRewardTiming :: SlotNo -> SlotNo -> SlotNo -> RewardTiming
determineRewardTiming currentSlot startAfterSlot endSlot
  | currentSlot > endSlot = RewardsTooLate
  | currentSlot <= startAfterSlot = RewardsTooEarly
  | otherwise = RewardsJustRight
```

1. A Timing of `RewardsToEarly` is a No-op in the TickRule where pulsing is concerned
2. A Timing of `RewardsJustRight` means take a StartStep, or a PulseStep
3. A Timing of `RewardsTooLate` means no time is left, so the rest of the pulsing is forced.

```
rupdTransition :: (EraGov era, EraCertState era) => TransitionRule (ShelleyRUPD era)
rupdTransition = do
  TRC (RupdEnv b es, ru, s) <- judgmentContext
  (slotsPerEpoch, slot, slotForce, maxLL, asc, k, e) <- liftSTS $ do
    ei <- asks epochInfoPure
    sr <- asks randomnessStabilisationWindow
    let e = epochInfoEpoch ei s
        slotsPerEpoch = epochInfoSize ei e
        slot = epochInfoFirst ei e +* Duration sr
    maxLL <- asks maxLovelaceSupply
    asc <- asks activeSlotCoeff
    k <- asks securityParameter -- Maximum number of blocks we are allowed to roll back
    return (slotsPerEpoch, slot, slot +* Duration sr, maxLL, asc, k, e)
  let maxsupply = Coin (fromIntegral maxLL)
  case determineRewardTiming s slot slotForce of
    -- Waiting for the stability point, do nothing, keep waiting
    RewardsTooEarly -> pure SNothing
    -- More blocks to come, get things started or take a step
    RewardsJustRight ->
      case ru of
        SNothing ->
          -- This is the first opportunity to pulse, so start pulsing.
          -- SJust <$> tellLeaderEvents (e + 1) (fst (startStep slotsPerEpoch b es maxsupply asc k))
          (pure . SJust) (startStep slotsPerEpoch b es maxsupply asc k)
        (SJust p@(Pulsing _ _)) -> do
          -- We began pulsing earlier, so run another pulse
          (ans, event) <- liftSTS $ pulseStep p
          tellRupd "Pulsing Rupd" (RupdEvent (succ e) event)
          pure (SJust ans)
        (SJust p@(Complete _)) -> pure (SJust p)
    -- Time to force the completion of the pulser so that downstream tools such as db-sync
    -- have time to see the reward update before the epoch boundary rollover.
    RewardsTooLate ->
      case ru of
        SNothing -> do
          -- Nothing has been done, so start, and then complete the pulser. We hope this is very rare.
          let pulser = startStep slotsPerEpoch b es maxsupply asc k
          (reward, event) <- liftSTS $ completeStep pulser
          tellRupd "Starting too late" (RupdEvent (succ e) event)
          pure (SJust reward)
        SJust p@(Pulsing _ _) -> do
          -- We have been pulsing, but we ran out of time, so complete the pulser.
          (reward, event) <- liftSTS $ completeStep p
          tellRupd "Completing too late" (RupdEvent (succ e) event)
          pure (SJust reward)
        complete@(SJust (Complete _)) -> pure complete
```