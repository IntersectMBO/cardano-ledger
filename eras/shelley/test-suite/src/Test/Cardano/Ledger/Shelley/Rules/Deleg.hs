{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Cardano.Ledger.Shelley.Rules.Deleg (
  keyRegistration,
  keyDeRegistration,
  keyDelegation,
  rewardsSumInvariant,
  checkInstantaneousRewards,
)
where

import Cardano.Ledger.Coin (addDeltaCoin)
import Cardano.Ledger.Shelley.API (ShelleyDELEG)
import Cardano.Ledger.Shelley.Core
import qualified Cardano.Ledger.Shelley.HardForks as HardForks (allowMIRTransfer)
import Cardano.Ledger.Shelley.LedgerState (
  DState (..),
  InstantaneousRewards (..),
  delegations,
  rewards,
 )
import Cardano.Ledger.Shelley.Rules (DelegEnv (..))
import Cardano.Ledger.Shelley.TxBody (
  MIRPot (..),
  MIRTarget (..),
  pattern DCertDeleg,
  pattern DCertMir,
  pattern DeRegKey,
  pattern Delegate,
  pattern Delegation,
  pattern MIRCert,
  pattern RegKey,
 )
import qualified Cardano.Ledger.UMapCompact as UM
import Control.SetAlgebra (eval, rng, (∈))
import Control.State.Transition.Trace (
  SourceSignalTarget,
  signal,
  source,
  target,
  pattern SourceSignalTarget,
 )
import Data.Foldable (fold)
import Data.List (foldl')
import qualified Data.Map.Strict as Map (difference, filter, keysSet, (\\))
import qualified Data.Set as Set (isSubsetOf, singleton, size)
import Lens.Micro.Extras (view)
import Test.QuickCheck (Property, conjoin, counterexample, property)

-- | Check stake key registration
keyRegistration :: SourceSignalTarget (ShelleyDELEG era) -> Property
keyRegistration
  SourceSignalTarget
    { signal = (DCertDeleg (RegKey hk))
    , target = targetSt
    } =
    conjoin
      [ counterexample
          "a newly registered key should have a reward account"
          ((UM.member hk (rewards targetSt)) :: Bool)
      , counterexample
          "a newly registered key should have a reward account with 0 balance"
          ((UM.rdReward <$> UM.lookup hk (rewards targetSt)) == Just mempty)
      ]
keyRegistration _ = property ()

-- | Check stake key de-registration
keyDeRegistration :: SourceSignalTarget (ShelleyDELEG era) -> Property
keyDeRegistration
  SourceSignalTarget
    { signal = (DCertDeleg (DeRegKey hk))
    , target = targetSt
    } =
    conjoin
      [ counterexample
          "a deregistered stake key should no longer be in the rewards mapping"
          ((UM.notMember hk (rewards targetSt)) :: Bool)
      , counterexample
          "a deregistered stake key should no longer be in the delegations mapping"
          ((UM.notMember hk (delegations targetSt)) :: Bool)
      ]
keyDeRegistration _ = property ()

-- | Check stake key delegation
keyDelegation :: SourceSignalTarget (ShelleyDELEG era) -> Property
keyDelegation
  SourceSignalTarget
    { signal = (DCertDeleg (Delegate (Delegation from to)))
    , target = targetSt
    } =
    let fromImage = eval (rng (Set.singleton from `UM.domRestrictedView` delegations targetSt))
     in conjoin
          [ counterexample
              "a delegated key should have a reward account"
              ((UM.member from (rewards targetSt)) :: Bool)
          , counterexample
              "a registered stake credential should be delegated"
              ((eval (to ∈ fromImage)) :: Bool)
          , counterexample
              "a registered stake credential should be uniquely delegated"
              (Set.size fromImage == 1)
          ]
keyDelegation _ = property ()

-- | Check that the sum of rewards does not change and that each element
-- that is either removed or added has a zero balance.
rewardsSumInvariant :: SourceSignalTarget (ShelleyDELEG era) -> Property
rewardsSumInvariant
  SourceSignalTarget {source, target} =
    let sourceRewards = UM.compactRewView (dsUnified source)
        targetRewards = UM.compactRewView (dsUnified target)
        rewardsSum = foldl' (<>) mempty
     in conjoin
          [ counterexample
              "sum of rewards should not change"
              (rewardsSum sourceRewards == rewardsSum targetRewards)
          , counterexample
              "dropped elements have a zero reward balance"
              (null (Map.filter (/= UM.CompactCoin 0) $ sourceRewards `Map.difference` targetRewards))
          , counterexample
              "added elements have a zero reward balance"
              (null (Map.filter (/= UM.CompactCoin 0) $ targetRewards `Map.difference` sourceRewards))
          ]

checkInstantaneousRewards ::
  EraPParams era =>
  DelegEnv era ->
  SourceSignalTarget (ShelleyDELEG era) ->
  Property
checkInstantaneousRewards
  denv
  SourceSignalTarget {source, signal, target} =
    case signal of
      DCertMir (MIRCert ReservesMIR (StakeAddressesMIR irwd)) ->
        conjoin
          [ counterexample
              "a ReservesMIR certificate should add all entries to the `irwd` mapping"
              (Map.keysSet irwd `Set.isSubsetOf` Map.keysSet (iRReserves $ dsIRewards target))
          , counterexample
              "a ReservesMIR certificate should add the total value to the `irwd` map, overwriting any existing entries"
              ( if (HardForks.allowMIRTransfer . view ppProtocolVersionL . ppDE $ denv)
                  then -- In the Alonzo era, repeated fields are added

                    ( (fold $ iRReserves $ dsIRewards source)
                        `addDeltaCoin` (fold irwd)
                        == (fold $ (iRReserves $ dsIRewards target))
                    )
                  else -- Prior to the Alonzo era, repeated fields overridden

                    ( (fold $ (iRReserves $ dsIRewards source) Map.\\ irwd)
                        `addDeltaCoin` (fold irwd)
                        == (fold $ (iRReserves $ dsIRewards target))
                    )
              )
          ]
      DCertMir (MIRCert TreasuryMIR (StakeAddressesMIR irwd)) ->
        conjoin
          [ counterexample
              "a TreasuryMIR certificate should add all entries to the `irwd` mapping"
              (Map.keysSet irwd `Set.isSubsetOf` Map.keysSet (iRTreasury $ dsIRewards target))
          , counterexample
              "a TreasuryMIR certificate should add* the total value to the `irwd` map"
              ( if HardForks.allowMIRTransfer . view ppProtocolVersionL . ppDE $ denv
                  then -- In the Alonzo era, repeated fields are added

                    ( (fold $ iRTreasury $ dsIRewards source)
                        `addDeltaCoin` (fold irwd)
                        == (fold $ (iRTreasury $ dsIRewards target))
                    )
                  else -- Prior to the Alonzo era, repeated fields overridden

                    ( (fold $ (iRTreasury $ dsIRewards source) Map.\\ irwd)
                        `addDeltaCoin` (fold irwd)
                        == (fold $ (iRTreasury $ dsIRewards target))
                    )
              )
          ]
      _ -> property ()
