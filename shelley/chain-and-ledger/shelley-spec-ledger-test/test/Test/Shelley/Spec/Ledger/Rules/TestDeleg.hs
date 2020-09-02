{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Shelley.Spec.Ledger.Rules.TestDeleg
  ( credentialMappingAfterDelegation,
    credentialRemovedAfterDereg,
    rewardZeroAfterReg,
    rewardsSumInvariant,
    instantaneousRewardsAdded,
    instantaneousRewardsValue,
  )
where

import Control.Iterate.SetAlgebra (dom, eval, rng, (∈), (∉), (◁))
import Control.State.Transition.Trace
  ( SourceSignalTarget,
    signal,
    source,
    target,
    pattern SourceSignalTarget,
  )
import Data.Foldable (fold)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map.Strict as Map (difference, filter, keysSet, lookup, (\\))
import qualified Data.Maybe as Maybe (maybe)
import Data.Set (Set)
import qualified Data.Set as Set (isSubsetOf, singleton, size)
import Shelley.Spec.Ledger.API (DELEG)
import Shelley.Spec.Ledger.BaseTypes ((==>))
import Shelley.Spec.Ledger.Coin (Coin, pattern Coin)
import Shelley.Spec.Ledger.Credential (Credential)
import Shelley.Spec.Ledger.Keys
  ( KeyHash,
    KeyRole (..),
  )
import Shelley.Spec.Ledger.LedgerState
  ( DState (..),
    InstantaneousRewards (..),
  )
import Shelley.Spec.Ledger.TxData
  ( MIRPot (..),
    pattern DCertDeleg,
    pattern DCertMir,
    pattern DeRegKey,
    pattern Delegate,
    pattern Delegation,
    pattern MIRCert,
    pattern RegKey,
  )
import Test.QuickCheck (Property, conjoin, counterexample, property)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( C,
  )

-------------------------------
-- helper accessor functions --
-------------------------------

getStDelegs :: DState C -> Set (Credential 'Staking C)
getStDelegs = \x -> eval (dom (_rewards x))

getRewards :: DState C -> Map (Credential 'Staking C) Coin
getRewards = _rewards

getDelegations :: DState C -> Map (Credential 'Staking C) (KeyHash 'StakePool C)
getDelegations = _delegations

--------------------------
-- Properties for DELEG --
--------------------------

-- | Check that a newly registered key has a reward of 0.
rewardZeroAfterReg ::
  [SourceSignalTarget (DELEG C)] ->
  Property
rewardZeroAfterReg tr =
  conjoin $
    map credNewlyRegisteredAndRewardZero tr
  where
    credNewlyRegisteredAndRewardZero (SourceSignalTarget d d' (DCertDeleg (RegKey hk))) =
      counterexample
        "a newly registered key should have a reward of 0"
        ( eval (hk ∉ getStDelegs d)
            ==> ( eval (hk ∈ getStDelegs d')
                    && Maybe.maybe True (== mempty) (Map.lookup hk (getRewards d'))
                )
        )
    credNewlyRegisteredAndRewardZero _ = property ()

-- | Check that when a stake credential is deregistered, it will not be in the
-- rewards mapping or delegation mapping of the target state.
credentialRemovedAfterDereg ::
  [SourceSignalTarget (DELEG C)] ->
  Property
credentialRemovedAfterDereg tr =
  conjoin $
    map removedDeregCredential tr
  where
    removedDeregCredential
      SourceSignalTarget
        { signal = DCertDeleg (DeRegKey cred),
          target = d'
        } =
        counterexample
          "a deregistered stake key should not be in the reward and delegation mappings"
          ( eval (cred ∉ getStDelegs d')
              && eval (cred ∉ dom (getRewards d'))
              && eval (cred ∉ dom (getDelegations d'))
          )
    removedDeregCredential _ = property ()

-- | Check that a registered stake credential get correctly delegated when
--  applying a delegation certificate.
credentialMappingAfterDelegation ::
  [SourceSignalTarget (DELEG C)] ->
  Property
credentialMappingAfterDelegation tr =
  conjoin $
    map delegatedCredential tr
  where
    delegatedCredential
      SourceSignalTarget
        { signal = DCertDeleg (Delegate (Delegation cred to)),
          target = d'
        } =
        let credImage = eval (rng (Set.singleton cred ◁ getDelegations d'))
         in eval (cred ∈ getStDelegs d')
              && eval (to ∈ credImage)
              && Set.size credImage == 1
    delegatedCredential _ = True

-- | Check that the sum of rewards does not change and that each element that is
-- either removed or added has a zero balance.
rewardsSumInvariant ::
  [SourceSignalTarget (DELEG C)] ->
  Property
rewardsSumInvariant tr =
  conjoin $
    map rewardsSumZeroDiff tr
  where
    rewardsSumZeroDiff
      SourceSignalTarget
        { source = d,
          target = d'
        } =
        let rew = _rewards d
            rew' = _rewards d'
            sumRew = foldl' (<>) mempty
         in -- sum of rewards is not changed
            sumRew rew == sumRew rew'
              && null (Map.filter (/= Coin 0) $ rew `Map.difference` rew') -- dropped elements had a zero reward balance
              && null (Map.filter (/= Coin 0) $ rew' `Map.difference` rew) -- added elements have a zero reward balance

-- | Check that an accepted MIR certificate adds all entries to the `irwd` mapping
instantaneousRewardsAdded ::
  [SourceSignalTarget (DELEG C)] ->
  Property
instantaneousRewardsAdded ssts =
  conjoin (map checkMIR ssts)
  where
    checkMIR :: SourceSignalTarget (DELEG C) -> Property
    checkMIR (SourceSignalTarget _ t sig) =
      case sig of
        DCertMir (MIRCert ReservesMIR irwd) ->
          property $ Map.keysSet irwd `Set.isSubsetOf` Map.keysSet (iRReserves $ _irwd t)
        DCertMir (MIRCert TreasuryMIR irwd) ->
          property $ Map.keysSet irwd `Set.isSubsetOf` Map.keysSet (iRTreasury $ _irwd t)
        _ -> property ()

-- | Check that an accepted MIR certificate adds the overall value in the
-- certificate to the existing value in the `irwd` map, overwriting any entries
-- that already existed.
instantaneousRewardsValue ::
  [SourceSignalTarget (DELEG C)] ->
  Property
instantaneousRewardsValue ssts =
  conjoin (map checkMIR ssts)
  where
    checkMIR :: SourceSignalTarget (DELEG C) -> Property
    checkMIR (SourceSignalTarget s t sig) =
      case sig of
        DCertMir (MIRCert ReservesMIR irwd) ->
          property $
            ( (fold $ (iRReserves $ _irwd s) Map.\\ irwd)
                <> (fold $ irwd)
                == (fold $ (iRReserves $ _irwd t))
            )
        DCertMir (MIRCert TreasuryMIR irwd) ->
          property $
            ( (fold $ (iRTreasury $ _irwd s) Map.\\ irwd)
                <> (fold $ irwd)
                == (fold $ (iRTreasury $ _irwd t))
            )
        _ -> property ()
