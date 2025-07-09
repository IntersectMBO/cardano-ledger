# How Reward Calculation Works

This document explains how the Reward Calculation is implemented in the Cardano Ledger. We strongly
suggest the reader read and understand chapter 11 "Rewards and the Epoch Boundary" in the 
[Shelley Spec](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/shelley-ledger.pdf) that
covers related material on the phases of the reward cycle.

1) Random selection of the pool operators who will be assigned the opportunity to mint each block in the next Epoch.
   Each block is assigned a single random StakePool, with likelihood proportional to the stake delegated to that StakePool.
   This proportion is called the Stake Distribution, which can change with each applied transaction.
   To get a stable Stake Distribution we need to make a Snapshot at the point in the Epoch, 
   when all blocks have been assigned a StakePool. This point is called the stability point, 
   as no assignment will change from this point to the end of the block. So the idea
   is to take a snapshot anytime after the stability point, and use it in the the rest of the Reward Calculation.
2) Registration (using certificates) of User's Stake credentials and StakePools can take place in the same
   Epoch that the StakePools are assigned blocks. Only registered Users and
   StakePools participate in the Reward Calculation. 
3) Snapshots are taken at the end of this Epoch, remembering 
     - the current registration status of Users and StakePools.
     - the Stake Distribution used for selection of which StakePools will make blocks in the next epoch
     - the Fee Pot
     - other data that cannot change over the Reward Calulation
   when the Reward Calculation is computed.
4) The calculation begins, using the 4 steps outlined below, resulting in a RewardUpdate.
5) The Reward Update is applied to the current NewEpochState, distributing Rewards to those stake credentials
   that were registered to StakePools, and only where the corresponding StakePool was registered as a pool operator.

The Reward system works as follows. When constructing a new transaction, the transaction author has the opportunity
to delegate the `Stake` (the `Coin` part of each `TxOut` in the transaction) to one of the `StakePools`
by adding a non null `StakeReference` to the `Addr` part of the `Output`

At the end of an `Epoch`, registered stake, that is delegated in a the address of a TxOut in the UTxO,
is marked as deserving a reward in a `RewardUpdate`. Applying a `RewardUpdate`  means increasing the amount of `Coin` associated
with the `StakeCredential` in the Rewards map. This is similar to earning interest on
the amount of money in a bank deposit. Here are the basic data stuctures that encode this information


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
  | AddrBootstrap BootstrapAddress  -- Bootstrap Addr have an implicit `StakeRefNull` StakeReference
                                    -- No (Credential `Staking) are delegated in a Bootstrap Addr

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
2. The set of staking cerdentials has millions of entries
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
   we call this pulsing, and it two must have many invariants.
     - It must be a pure computation
     - The computation must be serializeable, because if a node fails, we must be able to restart this 
       computation from the last checkpoint.
     - it must compute the same result as if we ran it all at once, not braking it into many starts and stops. 

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
that do not support Ptr (Conway, Dikstra).

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
    utxosInstantStake == addInstanrStake utxosUtxo mempty
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
Stake address must follow the reference. Unfortuantely, the references may change so the `InstantStake` for
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

-- Override the Coin in a (Credential,Coin) entry already in the map with a new Coin 
-- Insert a new (Credentia,Coin) pair in the map
-- Delete a (Credentia,Coin) pair from the map


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

### Conway Dikstra InstantStake

The type family `InstantStake` for Eras after Babbage (that have no `Ptr`s)
is simpler, since it does not need the Map of `Ptrs` that need to be resolved.

```
newtype ConwayInstantStake era = ConwayInstantStake
  { cisCredentialStake :: Map.Map (Credential 'Staking) (CompactForm Coin)
  }
```

The application function for `Conway` `functionapplyUTxOConwayInstantStake` is simliar
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
Also if you spead it over n-`Epoch`s you have n-(few second budgets) to work with. The complication is that
computation depends on the state of the system at the beginning of the Reward Calculation, and that state
changes many times in the course of n-`Epochs`. This requires the use of Snapshots, making temporary copies
of things at the beginning of the calculation, that cannot change, as the rest of the system progresses.

It also means the Reward calculation must be pipelined, and that requires a queue of SnapShots (Mark, Set, Go).


##  Break the calculation into many pure steps that can be started and stopped.

The module `Data.Pulse` provovides a library for breaking a large computation to 
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


